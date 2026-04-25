# =========================================================
# Module: find_outlier_samples
# =========================================================

# ---- Full UI ----
find_outlier_samplesUI <- function(id) {
  ns <- NS(id)
  bs4TabItem(
    tabName = "find_outlier_samples",
    h3(HTML(
      "Find_outlier_samples
      <span style='font-size:80%; color:#333;'>:
       Identify and remove outlier samples.</span>"
      )),
    find_outlier_samplesBodyUI(id)
  )
}


# ---- Body UI ----
find_outlier_samplesBodyUI <- function(id, include_upload = TRUE) {
  ns <- NS(id)

  fluidRow(
    # 左侧参数 / 上传 / 下载区域
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

        if (isTRUE(include_upload)) {
          tagList(
            uploadUI(ns("upload")) %>%
              helper(
                type    = "markdown",
                icon    = "question-circle",
                size    = "m",
                colour  = "#007bff",
                content = "demo_stad_tpm"
              )
          )
        },

        div(
          style = "text-align: center; margin-top: 15px; margin-bottom: 5px;",
          actionButton(
            inputId = ns("run_find_outlier_samples"),
            label = "Run Analysis",
            class = "btn-primary"
          )
        )
      ),

      # 数据下载放置在左侧，保持风格一致
      dataDownloadUI(ns("download_data"))
    ),

    # 右侧图表和数据区域
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
        tabBox(
          width = 12,
          side = "left",
          # Tab 1: 清洗后的数据
          tabPanel("Data_clean", dataTableUI(ns("tbl_data"))),
          
          # Tab 2: 异常检测图
          tabPanel(
            "Plot", 
            plotDownloadUI(ns("plot_download")),
            uiOutput(ns("find_outlier_samples_plot_container"))
          ),
          
          # Tab 3: 异常样本列表
          tabPanel(
            "Outliers",
            tags$b("Identified Outlier Samples:"),
            verbatimTextOutput(ns("outlier_text"))
          )
        )
      )
    )
  )
}


# ---- Server ----
find_outlier_samplesServer <- function(id, external_eset = NULL) {
  moduleServer(id, function(input, output, session) {
    
    # 1. 输入数据逻辑
    input_data <- if (!is.null(external_eset)) {
      external_eset
    } else {
      uploadServer("upload")
    }
    
    # 缓存结果
    res_clean_data <- reactiveVal(NULL)   # 清洗后的数据
    res_outliers   <- reactiveVal(NULL)   # 异常样本名
    res_plot_obj   <- reactiveVal(NULL)   # 绘图对象
    
    # 2. 主逻辑
    observeEvent(input$run_find_outlier_samples, {
      req(input_data())
      
      withProgress(message = "Detecting outlier samples...", value = 0, {
        setProgress(0.2, message = "Reading data...")
        
        data <- input_data()
        
        # 基础预处理
        if(!is.null(data)) colnames(data) <- gsub("\\.", "-", colnames(data))
        
        setProgress(0.5, message = "Running Analysis...")
        
        # 开启虚拟画板防止报错
        pdf(file = NULL)
        dev.control(displaylist = "enable")
        
        outliers_vec <- NULL
        plot_recorded <- NULL
        run_success <- FALSE
        
        tryCatch({
          # 调用函数 (直接画图，不返回 Plot 对象，需 recordPlot 捕获)
          # 注意：find_outlier_samples 返回的是异常样本的名称向量
          outliers_vec <- find_outlier_samples(
            eset        = data,
            yinter      = -3,
            project     = NULL,
            plot_hculst = FALSE,
            show_plot   = TRUE,
            save        = FALSE
          )
          
          # 捕获图像
          plot_recorded <- recordPlot()
          run_success <- TRUE
          
        }, error = function(e) {
          showNotification(paste("Error:", e$message), type = "error", duration = 8)
        })
        
        dev.off() # 关闭画板
        
        if (run_success) {
          # 处理异常样本剔除
          if (!is.null(outliers_vec) && length(outliers_vec) > 0) {
            # 过滤掉异常列
            data_clean <- data[, !(colnames(data) %in% outliers_vec), drop = FALSE]
          } else {
            # 没有异常样本，保持原样
            data_clean <- data
            outliers_vec <- "No outliers detected."
          }
          
          # 更新缓存
          res_clean_data(data_clean)
          res_outliers(outliers_vec)
          res_plot_obj(plot_recorded)
          
          setProgress(1, message = "Finished")
        } else {
          setProgress(1, message = "Failed")
        }
      })
    })
    
    # 3. 输出部分
    # 3.1 表格输出 (Clean Data)
    dataTableServer("tbl_data", res_clean_data)
    
    # 3.2 文本输出 (Outliers List)
    output$outlier_text <- renderPrint({
      req(res_outliers())
      cat(paste(res_outliers(), collapse = ", "))
    })
    
    # 3.3 图形下载与展示
    dims <- plotDownloadServer(
      id = "plot_download", 
      plot_reactive = res_plot_obj, 
      filename_prefix = "find_outlier_samples_plot"
    )
    
    output$find_outlier_samples_plot <- renderPlot({
      req(res_plot_obj())
      replayPlot(res_plot_obj())
    })
    
    output$find_outlier_samples_plot_container <- renderUI({
      req(res_plot_obj())
      div(style = "overflow: auto;",
          plotOutput(session$ns("find_outlier_samples_plot"), 
                     width = paste0(dims$width(), "px"), 
                     height = paste0(dims$height(), "px"))
      )
    })
    
    # 3.4 数据下载 (Clean Data)
    dataDownloadServer(
      "download_data",
      data_reactive   = res_clean_data,
      filename_prefix = "clean_data_no_outliers"
    )
    
    return(res_clean_data)
  })
}