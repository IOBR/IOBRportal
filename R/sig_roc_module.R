# =========================================================
# Module: sig_roc
# =========================================================

sig_rocUI <- function(id) {
  ns <- NS(id)
  bs4TabItem(
    tabName = "sig_roc",
    h3(HTML(
      "Sig_roc
      <span style='font-size:80%; color:#333;'>:
       Generates ROC curves for significant signatures.</span>"
      )),
    sig_rocBodyUI(id)
  )
}

sig_rocBodyUI <- function(id, include_upload = TRUE) {
  ns <- NS(id)

  fluidRow(
    # 左侧参数区域
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

        # 上传模块
        if (isTRUE(include_upload)) {
          tagList(uploadUI(ns("upload"))%>%
              helper(
                type    = "markdown",
                icon    = "question-circle",
                size    = "m",
                colour  = "#007bff",
                content = "demo_stad_com"
              )
          )
        },

        # 运行按钮
        div(
          style = "text-align: center; margin-top: 0px; margin-bottom: 0px;",
          actionButton(
            inputId = ns("run_sig_roc"),
            label = "Run Analysis",
            class = "btn-primary"
          )
        ),

        # --- 修改点：自动识别 Response (单选) ---
        pickerInput(
          inputId = ns("response"),
          label = "Status",
          choices = NULL,
          multiple = FALSE,
          options = pickerOptions(
            liveSearch = TRUE,
            size = 10,
            style = "btn-outline-secondary",
            title = "Select outcome column..."
          )
        ),

        # --- 修改点：自动识别 Variables (多选) ---
        pickerInput(
          inputId = ns("variables"),
          label = "Variables",
          choices = NULL,
          multiple = TRUE,
          options = pickerOptions(
            actionsBox = TRUE, # 全选
            liveSearch = TRUE,
            size = 10,
            selectedTextFormat = "count > 2",
            style = "btn-outline-secondary",
            title = "Select variables...",
            dropupAuto = FALSE,
            container = "body"
          )
        ),

        # Palette 选择
        selectInput(
          inputId = ns("palette"),
          label = "Palette",
          choices = c(
            "nrc" = "nrc", "jama" = "jama", "aaas" = "aaas", "jco" = "jco",
            "paired1" = "paired1", "paired2" = "paired2", 
            "paired3" = "paired3", "paired4" = "paired4",
            "accent" = "accent", "set2" = "set2"
          ),
          selected = "jama"
        ),

        # Alpha 滑块
        sliderInput(
          inputId = ns("alpha"),
          label = "Transparency",
          min = 0, max = 1, value = 1, step = 0.01
        ),

        # Compare 选项
        selectInput(
          inputId = ns("compare"),
          label = "Compare",
          choices = c("True" = "T", "False" = "F"),
          selected = "T"
        ),
        
        # 仅当 Compare 为 T 时显示的条件面板 (可选优化，这里直接显示)
        selectInput(
          inputId = ns("compare_method"),
          label = "Compare Method",
          choices = c("Bootstrap" = "bootstrap", "Delong" = "delong", "Venkatraman" = "venkatraman"),
          selected = "bootstrap"
        ),

        # Smooth 选项
        selectInput(
          inputId = ns("smooth"),
          label = "Smooth",
          choices = c("True" = "T", "False" = "F"),
          selected = "T"
        ),

        # Title 输入
        textInput(
          inputId = ns("main"),
          label = "Title",
          value = "",
          placeholder = "Title (optional)"
        )
      )
    ),

    # 右侧绘图区域
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
          tabPanel(
            "Plot",
            plotDownloadUI(ns("plot_download")),
            uiOutput(ns("plot_container"))
          )
        )
      )
    )
  )
}


sig_rocServer <- function(id, external_eset = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # 1. 数据源处理 (内部上传 或 外部传入)
    input_data <- if (!is.null(external_eset)) {
      external_eset
    } else {
      uploadServer("upload")
    }

    # 2. 数据加载后的自动识别逻辑
    observeEvent(input_data(), {
      req(input_data())
      data <- input_data()
      
      # 替换非法字符，保证列名安全
      colnames(data) <- gsub("\\.", "-", colnames(data))

      all_cols <- gsub("\\.", "-", colnames(data))
      is_num <- sapply(data, is.numeric)
      pool_numeric <- all_cols[is_num] # 初始数字池
      blacklist_pattern <- "time|status|os|event|censored|days|months|years|fustat|futime|rfs|pfs|dfs"
      is_clinical <- grepl(blacklist_pattern, pool_numeric, ignore.case = TRUE)
      numeric_cols <- pool_numeric[!is_clinical]
      non_numeric_cols <- grep("time|status", all_cols[is_num], ignore.case = TRUE, value = TRUE)

      # 更新 Variables 选择器
      updatePickerInput(
        session = session,
        inputId = "variables",
        choices = numeric_cols, # 允许用户选择所有数值列
        selected = character(0) # 默认不选中，让用户选
      )

      # 更新 Response 选择器
      updatePickerInput(
        session = session,
        inputId = "response",
        choices = non_numeric_cols
      )

      potential_status <- grep("os_status", non_numeric_cols, ignore.case = TRUE, value = TRUE)
      if(length(potential_status) > 0) updatePickerInput(session, "response", selected = potential_status[1])
    })

    # 3. 运行分析
    plot_result <- reactiveVal(NULL)

    observeEvent(input$run_sig_roc, {
      req(input_data())
      
      # [验证] 检查必填项
      if (is.null(input$response) || input$response == "") {
        showNotification("Please select a Response column.", type = "error")
        return(NULL)
      }
      if (is.null(input$variables) || length(input$variables) == 0) {
        showNotification("Please select at least one Variable.", type = "error")
        return(NULL)
      }

      withProgress(message = "Running sig_roc analysis...", value = 0, {
        
        data <- input_data()
        colnames(data) <- gsub("\\.", "-", colnames(data)) # 再次确保列名一致
        
        # 验证列是否存在
        req_cols <- c(input$response, input$variables)
        if (!all(req_cols %in% colnames(data))) {
          showNotification("Selected columns not found in data.", type = "error")
          return(NULL)
        }

        setProgress(0.4, message = "Generating plot...")

        # 调用 sig_roc 函数
        # 注意：这里不再需要 gsub 处理括号和逗号，因为 input$variables 已经是向量
        result <- tryCatch({
          sig_roc(
            data = data,
            response = input$response,
            variables = input$variables, # 直接传入向量
            main = if (input$main == "") NULL else input$main,
            palette = input$palette,
            alpha = input$alpha,
            compare = (input$compare == "T"),
            smooth = (input$smooth == "T"),
            compare_method = input$compare_method
          )
        }, error = function(e) {
          setProgress(1)
          showNotification(paste("Error:", e$message), type = "error", duration = 8)
          return(NULL)
        })

        req(result)
        plot_result(result) # 保存结果
        
        setProgress(1, message = "Plot ready.")
      })
    })

    dims <- plotDownloadServer(
      id = "plot_download",
      plot_reactive = plot_result,
      filename_prefix = "sig_roc_plot"
    )

    # --- 2. (Painter) 负责画图的核心逻辑 ---
    output$plot_output <- renderPlot({
      req(plot_result())
      plot_result()
    })

    # --- 3. (Container) 动态渲染 UI 容器 ---
    output$plot_container <- renderUI({
      req(plot_result())
      
      div(style = "overflow: auto;",
          plotOutput(session$ns("plot_output"), # 对应上面 renderPlot 的 ID
                     width = paste0(dims$width(), "px"),
                     height = paste0(dims$height(), "px"))
      )
    })
    
    return(plot_result)
  })
}
