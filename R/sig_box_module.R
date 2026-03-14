# =========================================================
# Module: sig_box
# =========================================================

# ---- Full UI ----
sig_boxUI <- function(id) {
  ns <- NS(id)
  bs4TabItem(
    tabName = "sig_box",
    h3(HTML(
      "Sig_box
      <span style='font-size:80%; color:#333;'>:
       Creates customizable boxplots for signatures or variables.</span>"
      )),
    sig_boxBodyUI(id)
  )
}


# ---- Body UI ----
sig_boxBodyUI <- function(id, include_upload = TRUE) {
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
            uploadUI(ns("upload"))%>%
              helper(
                type    = "markdown",
                icon    = "question-circle",
                size    = "m",
                colour  = "#007bff",
                content = "demo_stad_com"
              )
          )
        },

        div(
          style = "text-align: center; margin-top: 0px; margin-bottom: 0px;",
          actionButton(
            inputId = ns("run_sig_box"),
            label = "Run Analysis",
            class = "btn-primary"
          )
        ),

        pickerInput(
          inputId = ns("sig_box_variable"),
          label = "Groups",  
          choices = NULL,      # 初始化为空
          multiple = FALSE,    # 单选
          options = pickerOptions(
            liveSearch = TRUE,
            size = 10,
            style = "btn-outline-secondary", 
            dropupAuto = FALSE,
            container = "body"
          )
        ),        

        pickerInput(
          inputId = ns("sig_box_signature"),
          label = "Signature", 
          choices = NULL,      # 初始化为空
          multiple = FALSE,    # 单选
          options = pickerOptions(
            liveSearch = TRUE,
            size = 10,
            style = "btn-outline-secondary", 
            dropupAuto = FALSE,
            container = "body"
          )
        ),

        selectInput(
          inputId = ns("sig_box_scale"),
          label = "Scale",
          choices = c("True" = "T", "False" = "F"),
          selected = "F"
        ),

        selectInput(
          inputId = ns("sig_box_palette"),
          label = "Palette",
          choices = c(
            "nrc" = "nrc", "jama" = "jama", "aaas" = "aaas",
            "jco" = "jco", "paired1" = "paired1", "paired2" = "paired2",
            "paired3" = "paired3", "paired4" = "paired4",
            "accent" = "accent", "set2" = "set2"
          ),
          selected = "jama"
        ),

        textAreaInput(
          inputId = ns("sig_box_custom_cols"),
          label = "Colors",
          value = "",
          placeholder = "e.g., #E64B35, #4DBBD5, #00A087\nSeparate by comma",
          rows = 3,
          resize = "vertical"
        ),
        br(),
          tags$p(
          style = "color: #555; font-style: italic; font-size: 90%; margin-top: -10px;",
          "Input hex codes or color names separated by comma. If filled, 'Palette' will be ignored."
          ),

        selectInput(
          inputId = ns("sig_box_jitter"),
          label = "Show Jitter",
          choices = c("True" = "T", "False" = "F"),
          selected = "T"
        ),
      
        selectInput(
          inputId = ns("sig_box_show_pvalue"),
          label = "Show P-value",
          choices = c("True" = "T", "False" = "F"),
          selected = "T"
        ),

        sliderInput(
          inputId = ns("sig_box_size_of_pvalue"),
          label = "P-value Size",
          min = 0, max = 20, value = 6, step = 1
        ),

        sliderInput(
          inputId = ns("sig_box_point_size"),
          label = "Point Size",
          min = 0, max = 20, value = 5, step = 1
        ),

        sliderInput(
          inputId = ns("sig_box_size_of_font"),
          label = "Font Size",
          min = 0, max = 20, value = 10, step = 1
        ),

        sliderInput(
          inputId = ns("sig_box_angle_x_text"),
          label = "X-axis Text Angle",
          min = 0, max = 90, value = 0, step = 5
        ),

        sliderInput(
          inputId = ns("sig_box_hjust"),
          label = "X-axis Text Justification",
          min = 0, max = 1, value = 0.5, step = 0.1
        )
      )
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
          tabPanel(
            "Plot", 
            plotDownloadUI(ns("plot_download")),
            uiOutput(ns("sig_box_plot_container")))
        )
      )
    )
  )
}


sig_boxServer <- function(id, external_eset = NULL) {
  moduleServer(id, function(input, output, session) {
    
    # 1. 输入数据逻辑 (保持不变)
    input_data <- if (!is.null(external_eset)) {
      external_eset
    } else {
      uploadServer("upload")
    }
    
    # ============================================================
    # [新增] 动态更新 Signature 和 Variable 的选项
    # ============================================================
    observeEvent(input_data(), {
      req(input_data())
      data <- input_data()
      
      all_cols <- gsub("\\.", "-", colnames(data))
      is_num <- sapply(data, is.numeric)
      non_numeric_cols <- all_cols[!is_num]
      non_numeric_cols <- setdiff(non_numeric_cols, "ID") #只有字符，删去ID列

      pool_numeric <- all_cols[is_num] # 初始数字池
      blacklist_pattern <- "time|status|os|id"
      is_clinical <- grepl(blacklist_pattern, pool_numeric, ignore.case = TRUE)
      numeric_cols <- pool_numeric[!is_clinical]
      
      updatePickerInput(
        session = session, 
        inputId = "sig_box_signature", 
        choices = numeric_cols, 
        selected = character(0)
      )
      
      updatePickerInput(
        session = session, 
        inputId = "sig_box_variable", 
        choices = non_numeric_cols, 
        selected = character(0)
      )
    })
    
    sig_box_result <- reactiveVal(NULL)
    
    # 主逻辑
    observeEvent(input$run_sig_box, {
      req(input_data())
      
      if (is.null(input$sig_box_signature) || input$sig_box_signature == "" || 
          is.null(input$sig_box_variable) || input$sig_box_variable == "") {
        showNotification("Please select both signature and variable columns.", type = "error")
        return(NULL)
      }
      
      withProgress(message = "Generating boxplot...", value = 0, {
        setProgress(0.2, message = "Reading data...")
        
        data <- input_data()

        colnames(data) <- gsub("\\.", "-", colnames(data))
        
        # 检查列是否存在
        if (!(input$sig_box_signature %in% colnames(data))) {
          showNotification("Signature column not found in data.", type = "error")
          return(NULL)
        }
        if (!(input$sig_box_variable %in% colnames(data))) {
          showNotification("Variable column not found in data.", type = "error")
          return(NULL)
        }

        # --- [新增] 解析自定义颜色 ---
        custom_cols_vec <- NULL
        raw_cols_text <- input$sig_box_custom_cols
        
        if (!is.null(raw_cols_text) && trimws(raw_cols_text) != "") {
          # 1. 按逗号、分号或换行符分割
          split_cols <- unlist(strsplit(raw_cols_text, "[,;\n]"))
          # 2. 去除首尾空格
          split_cols <- trimws(split_cols)
          # 3. 去除空字符串
          custom_cols_vec <- split_cols[split_cols != ""]
          
          if(length(custom_cols_vec) == 0) custom_cols_vec <- NULL
        }
        # ---------------------------
        
        setProgress(0.5, message = "Plotting...")
        
        pp <- tryCatch({
          sig_box(
            data             = data,
            signature        = input$sig_box_signature,
            variable         = input$sig_box_variable,
            scale            = input$sig_box_scale == "T",
            angle_x_text     = input$sig_box_angle_x_text,
            hjust            = input$sig_box_hjust,
            palette          = input$sig_box_palette,
            cols             = custom_cols_vec,
            jitter           = input$sig_box_jitter == "T",
            point_size       = input$sig_box_point_size,
            size_of_font     = input$sig_box_size_of_font,
            size_of_pvalue   = input$sig_box_size_of_pvalue,
            show_pvalue      = input$sig_box_show_pvalue == "T",
            return_stat_res  = FALSE
          )
        }, error = function(e) {
          setProgress(1, message = "Error")
          showNotification(
            paste("Error during sig_box():", e$message),
            type = "error",
            duration = 8
          )
          return(NULL)
        })
        
        req(pp)
        
        setProgress(1, message = "Finished")
        
        sig_box_result(pp)
      })
    })
    
    # 1. 调用模块并接收返回值 dims
    dims <- plotDownloadServer(
      id = "plot_download", 
      plot_reactive = sig_box_result, 
      filename_prefix = "sig_box_plot"
    )
    
    # 2. 负责画图 (id 必须是 sig_box_plot)
    output$sig_box_plot <- renderPlot({
      req(sig_box_result())
      sig_box_result()
    })

    # 3. 动态渲染 UI 容器
    output$sig_box_plot_container <- renderUI({
      req(sig_box_result())
      
      div(style = "overflow: auto;",
          plotOutput(session$ns("sig_box_plot"), 
                     width = paste0(dims$width(), "px"), 
                     height = paste0(dims$height(), "px"))
      )
    })
    
    return(sig_box_result)
  })
}