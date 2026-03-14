# =========================================================
# Module: tme_cluster
# =========================================================
tme_clusterUI <- function(id) {
  ns <- NS(id)
  bs4TabItem(
    tabName = "tme_cluster",
    h3(HTML(
      "Tme_cluster
      <span style='font-size:80%; color:#333;'>:
       Performs tumor microenvironment clustering analysis.</span>"
      )),
    tme_clusterBodyUI(id)
  )
}

# ---- Body UI ----
tme_clusterBodyUI <- function(id, include_upload = TRUE, only_table = FALSE) {
  ns <- NS(id)

  fluidRow(
    # Left Panel: Parameters / Upload / Download
    column(
      width = 3,
      bs4Card(
        title = "Parameter",
        status = "gray",
        headerBorder = TRUE,
        collapsible = TRUE,
        solidHeader = TRUE,
        width = 12,
        maximizable = TRUE,

        # Upload (Conditional)
        if (isTRUE(include_upload)) {
          tagList(
            uploadUI(ns("upload")) %>%
              helper(
                type    = "markdown",
                icon    = "question-circle",
                size    = "m",
                colour  = "#007bff",
                content = "demo_stad_sig"
              )
          )
        },


        div(
          style = "text-align: center; margin-top: 10px;",
          actionButton(
            inputId = ns("run_tme_cluster"),
            label = "Run Analysis",
            class = "btn-primary"
          )
        ),

        # Feature Selection
        pickerInput(
          inputId = ns("tme_cluster_features"),
          label = "Features",
          choices = NULL, # Initialized empty
          multiple = TRUE,
          options = pickerOptions(
            actionsBox = TRUE, #
            liveSearch = TRUE,
            size = 10,
            selectedTextFormat = NULL,
            style = "btn-outline-secondary",
            dropupAuto = FALSE,
            container = "body",
            title = "Please select features..."
          )
        ),

        numericInput(
          inputId = ns("tme_cluster_min_nc"),
          label = "Min Clusters",
          value = 2,
          min = 2,
          max = 20,
          step = 1
        ),

        numericInput(
          inputId = ns("tme_cluster_max_nc"),
          label = "Max Clusters",
          value = 6,
          min = 2,
          max = 20,
          step = 1
        )
      ),

      dataDownloadUI(ns("download"))
    ),


    # Right Panel: Plots and Data
    column(
      width = 9,
      bs4Card(
        title = "Plot and Data",
        status = "gray",
        headerBorder = TRUE,
        collapsible = TRUE,
        solidHeader = TRUE,
        width = 12,
        maximizable = TRUE,

        do.call(
          tabBox,
          c(
            list(width = 12, side = "left"),
            # 逻辑：如果不是仅表格模式，显示Plot, Data, Cluster
            if (!isTRUE(only_table)) {
              list(
                tabPanel("Plot", 
                  plotDownloadUI(ns("plot_download")),
                  uiOutput(ns("tme_cluster_plot_container"))
                ),
                tabPanel("Data", dataTableUI(ns("tbl_data"))),
                tabPanel("Cluster", textOutput(ns("tme_cluster_cluster")))
              )
            } else {
              # 逻辑：如果是仅表格模式，只显示 Data, Cluster
              list(
                tabPanel("Data", dataTableUI(ns("tbl_data"))),
                tabPanel("Cluster", textOutput(ns("tme_cluster_cluster")))
              )
            }
          )
        )
      )
    )
  )
}


# ---- Server ----
tme_clusterServer <- function(id, external_eset = NULL, only_table = FALSE) {
  moduleServer(id, function(input, output, session) {

    # 1. 获取输入数据
    input_data <- if (!is.null(external_eset)) {
      external_eset
    } else {
      uploadServer("upload")
    }
    
    # 2. 监听数据变化，更新 Features 选项
    observeEvent(input_data(), {
      req(input_data())
      data <- input_data()
    
      colnames(data) <- gsub("\\.", "-", colnames(data)) # 清洗列名
      
      all_cols <- colnames(data) 
      is_num <- sapply(data, is.numeric)
      pool_numeric <- all_cols[is_num]
      
      # 排除临床列
      blacklist_pattern <- "time|status|os|age"
      is_clinical <- grepl(blacklist_pattern, pool_numeric, ignore.case = TRUE)
      
      numeric_cols <- pool_numeric[!is_clinical] 
      
      updatePickerInput(
        session = session,
        inputId = "tme_cluster_features",
        choices = numeric_cols,
        selected = numeric_cols
      )
    })

    tme_cluster_result <- reactiveVal(NULL)

    # 3. 运行分析
    observeEvent(input$run_tme_cluster, {
      req(input_data())
      data <- input_data() # 获取原始数据
      
      colnames(data) <- gsub("\\.", "-", colnames(data)) 
      
      if (is.null(input$tme_cluster_features) || length(input$tme_cluster_features) == 0) {
        showNotification("Please select at least one feature.", type = "error")
        return(NULL)
      }

      withProgress(message = "Running tme_cluster analysis...", value = 0, {
        setProgress(0.5, message = "Clustering...")

        result <- tryCatch({
          # 确保 tme_cluster 函数返回的是一个 data.frame，包含 ID, features, 和 cluster 列
          tme_cluster(
            input     = data, 
            features  = input$tme_cluster_features,
            pattern   = NULL,
            id        = "ID",
            method    = "kmeans",
            min_nc    = input$tme_cluster_min_nc,
            max.nc    = input$tme_cluster_max_nc
          )
        }, error = function(e) {
          showNotification(paste("Error:", e$message), type = "error")
          return(NULL)
        })
        
        req(result)

        tme_cluster_result(result)
        setProgress(1, message = "Finished")
      })
    })

    # 4. Outputs
    
    # 表格 (Always)
    dataTableServer("tbl_data", tme_cluster_result)
    
    # 分组统计文本 (Always - 移出if块，确保两种模式都能看到)
    output$tme_cluster_cluster <- renderText({
      req(tme_cluster_result())
      res <- tme_cluster_result()
      
      # 检查是否有 cluster 列，防止报错
      if ("cluster" %in% colnames(res)) {
        cluster_tbl <- table(res$cluster)
        paste0("Group: ", paste(names(cluster_tbl), cluster_tbl, sep = ": ", collapse = "; "))
      } else {
        "No cluster column found."
      }
    })

    # 图形 (Conditional - 仅在 only_table=FALSE 时渲染)
    if (!isTRUE(only_table)) { 
      
      # --- 1. 定义绘图逻辑 (Reactive) ---
      plot_reactive <- reactive({
        req(tme_cluster_result())
        sig_heatmap(
          input    = tme_cluster_result(),
          features = input$tme_cluster_features, 
          group    = "cluster"
        )
      })

      # --- 2. 接收下载模块返回的尺寸 (Controller) ---
      dims <- plotDownloadServer(
        id = "plot_download", 
        plot_reactive = plot_reactive,
        filename_prefix = "tme_cluster_heatmap"
      )

      # --- 3. 屏幕绘图 (Painter) ---
      output$tme_cluster_plot <- renderPlot({
        plot_reactive()
      })

      # --- 4. 动态容器 (Container) ---
      output$tme_cluster_plot_container <- renderUI({
        req(plot_reactive())
        
        div(style = "overflow: auto;",
            plotOutput(session$ns("tme_cluster_plot"), 
                       width = paste0(dims$width(), "px"), 
                       height = paste0(dims$height(), "px"))
        )
      })
    }

    # 数据下载 (Always)
    dataDownloadServer("download", data_reactive = tme_cluster_result, filename_prefix = "tme_cluster_result")

    # 返回结果 (Table)
    return(tme_cluster_result)
  })
}
