# =========================================================
# Module: cell_bar_plot
# =========================================================

# ---- Full UI ----
cell_bar_plotUI <- function(id) {
  ns <- NS(id)
  bs4TabItem(
    tabName = "cell_bar_plot",
    h3(HTML(
      "Cell_bar_plot
      <span style='font-size:80%; color:#333;'>:
       Visualizes cell composition or immune infiltration profiles across samples.</span>"
      )),
    cell_bar_plotBodyUI(id)
  )
}

# ---- Body UI ----
cell_bar_plotBodyUI <- function(id, include_upload = TRUE) {
  ns <- NS(id)

  fluidRow(
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
          tagList(uploadUI(ns("upload")) %>%
              helper(
                type    = "markdown",
                icon    = "question-circle",
                size    = "m",
                colour  = "#007bff",
                content = "demo_stad_epic"
              )
          )
        },

        div(
          style = "text-align: center;",
          actionButton(
            inputId = ns("run_cell_bar_plot"),
            label = "Run Analysis",
            class = "btn-primary"
          )
        ),
        br(),

        textInput(
          inputId = ns("cell_bar_plot_id"),
          label = "ID column",
          value = "ID"
        ),

        # Features 选择 (改为 PickerInput)
        pickerInput(
          inputId = ns("cell_bar_plot_features"),
          label = "Features",
          choices = NULL, # 初始化为空
          multiple = TRUE,
          options = pickerOptions(
            actionsBox = TRUE,       # 显示 Select All / Deselect All
            liveSearch = TRUE,       # 允许搜索
            size = 10,               # 下拉框显示条数
            selectedTextFormat = NULL, # 选多了显示数量
            style = "btn-outline-secondary",
            dropupAuto = FALSE,      # 强制向下弹出
            container = "body",
            title = "Please select features..."
          )
        ),
        br(),
        tags$p(
          style = "color: #555; font-style: italic; font-size: 90%; margin-top: -10px;",
          "Note: Supports data from deconvolution methods such as CIBERSORT, EPIC, and quanTIseq."
        ),

        numericInput(
          inputId = ns("cell_bar_plot_n"),
          label = "Sample Number",
          value = 10,
          min = 1,
          step = 1
        ),

        textAreaInput(
          inputId = ns("custom_cols"),
          label = "Colors",
          value = "",
          placeholder = "e.g., #E64B35, #4DBBD5, #00A087\nSeparate by comma",
          rows = 3,
          resize = "vertical"
        ),
        br(),
          tags$p(
          style = "color: #555; font-style: italic; font-size: 90%; margin-top: -10px;",
          "Input hex codes or color names separated by comma."
          ),

        textInput(
          inputId = ns("cell_bar_plot_title"),
          label = "Title",
          value = "Cell Fraction"
        ),

        selectInput(
          ns("cell_bar_plot_legend.position"),
          label = "Legend Position",
          choices = c("bottom", "top", "left", "right"),
          selected = "bottom"
        ),

        selectInput(
          ns("cell_bar_plot_palette"),
          label = "Palette",
          choices = c("1", "2", "3", "4"),
          selected = "3"
        ),

        selectInput(
          ns("cell_bar_plot_coord_flip"),
          label = "Coord Flip",
          choices = c("True" = "T", "False" = "F"),
          selected = "T"
        )
      )
    ),

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
          tabPanel(
            "Plot", 
            plotDownloadUI(ns("plot_download")),
            uiOutput(ns("cell_bar_plot_container"))
          )
        )
      )
    )
  )
}


# ---- Server ----
cell_bar_plotServer <- function(id, external_eset = NULL) {
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
      cat("===== 数据结构 =====\n")
  print(str(data))
  cat("===== 每列类型 =====\n")
  print(sapply(data, class))
      
      all_cols <- colnames(data)
      is_num <- sapply(data, function(x) {
    is.numeric(x) || all(!is.na(suppressWarnings(as.numeric(x))))
    })
      cat("===== is_num =====\n")
      print(is_num)
      
      pool_numeric <- all_cols[is_num] # 初始数字池
      
      blacklist_pattern <- "(^|_)time|status|os|id(_|$)"
      is_clinical <- grepl(blacklist_pattern, pool_numeric, ignore.case = TRUE)
      
      numeric_cols <- pool_numeric[!is_clinical]      # 纯特征 (Features)
      non_numeric_cols <- setdiff(all_cols, numeric_cols) # ID / Clinical
      
      print(numeric_cols)  # 🔍 debug

      # 更新 Features 下拉框
      updatePickerInput(
        session = session,
        inputId = "cell_bar_plot_features",
        choices = numeric_cols,
        selected = numeric_cols
      )
    })

    cell_bar_plot_result <- reactiveVal(NULL)

    # 3. 运行分析
    observeEvent(input$run_cell_bar_plot, {
      req(input_data())
      data <- input_data()
      req(nrow(data) > 0)

      # 校验
      if (nchar(input$cell_bar_plot_id) == 0) {
        showNotification("Please provide ID column name.", type = "error")
        return(NULL)
      }

      if (is.null(input$cell_bar_plot_features) || length(input$cell_bar_plot_features) == 0) {
        showNotification("Please select at least one feature.", type = "error")
        return(NULL)
      }

      withProgress(message = "Generating cell_bar_plot...", value = 0, {
        
        cols_vec <- NULL
        raw_cols_text <- input$custom_cols
        
        if (!is.null(raw_cols_text) && trimws(raw_cols_text) != "") {
          # 1. 按逗号、分号或换行符分割
          split_cols <- unlist(strsplit(raw_cols_text, "[,;\n]"))
          # 2. 去除首尾空格
          split_cols <- trimws(split_cols)
          # 3. 去除空字符串
          cols_vec <- split_cols[split_cols != ""]
          
          if(length(cols_vec) == 0) cols_vec <- NULL
        }

        n_limit <- input$cell_bar_plot_n
        if (n_limit > nrow(data)) {
           n_limit <- nrow(data)
           showNotification(paste("Warning: Sample Number exceeds data rows. Using all", n_limit, "samples."), type = "warning")
        } # 确保不超界：如果 n > 总行数，就用总行数
        data_subset <- data[1:n_limit, , drop = FALSE] # 截取前 n 行数据

        setProgress(0.5, message = "Plotting...")

        pp <- tryCatch({
          cell_bar_plot(
            input           = data_subset,
            id              = input$cell_bar_plot_id,
            features        = input$cell_bar_plot_features, # Picker 选中的列名
            pattern         = NULL,                         # 不再需要 pattern
            cols = cols_vec,
            title           = input$cell_bar_plot_title,
            legend.position = input$cell_bar_plot_legend.position,
            coord_flip      = input$cell_bar_plot_coord_flip == "T",
            palette         = as.numeric(input$cell_bar_plot_palette)
          )
        }, error = function(e) {
          setProgress(1, message = "Error")
          showNotification(
            paste("Error during cell_bar_plot():", e$message),
            type = "error",
            duration = 8
          )
          return(NULL)
        })
        
        req(pp)

        cell_bar_plot_result(pp)
        setProgress(1, message = "Done.")
      })
    })

    # 1. 调用模块并接收返回值 dims
    dims <- plotDownloadServer(
      "plot_download",
      plot_reactive = cell_bar_plot_result,
      filename_prefix = "cell_bar_plot"
    )
    
    # 2. 负责画图 (ID 对应 session$ns("cell_bar_plot"))
    output$cell_bar_plot <- renderPlot({
      req(cell_bar_plot_result())
      cell_bar_plot_result()
    })

    # 3. 动态渲染 UI 容器
    output$cell_bar_plot_container <- renderUI({
      req(cell_bar_plot_result())
      
      div(style = "overflow: auto;",
          plotOutput(session$ns("cell_bar_plot"), 
                     width = paste0(dims$width(), "px"), 
                     height = paste0(dims$height(), "px"))
      )
    })

    return(cell_bar_plot_result)
  })
}