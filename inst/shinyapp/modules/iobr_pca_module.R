# =========================================================
# Module: iobr_pca
# =========================================================

# ---- Full UI ----
iobr_pcaUI <- function(id) {
  ns <- NS(id)
  bs4TabItem(
    tabName = "iobr_pca",
    h3(HTML(
      "IOBR_pca
      <span style='font-size:80%; color:#333;'>:
       Perform Principal Component Analysis (PCA).</span>"
      )),
    iobr_pcaBodyUI(id)
  )
}


# ---- Body UI ----
iobr_pcaBodyUI <- function(id, include_upload = TRUE) {
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

        uploadUI(ns("upload_matrix"))%>%
              helper(
                type    = "markdown",
                icon    = "question-circle",
                size    = "m",
                colour  = "#007bff",
                content = "demo_stad_tpm"
              ),

        uploadUI(ns("upload_pdata"))%>%
              helper(
                type    = "markdown",
                icon    = "question-circle",
                size    = "m",
                colour  = "#007bff",
                content = "demo_stad_pdata"
              ),

        div(
          style = "text-align: center; margin-top: 15px; margin-bottom: 5px;",
          actionButton(
            inputId = ns("run_iobr_pca"),
            label = "Run Analysis",
            class = "btn-primary"
          )
        ),

        textInput(
          inputId = ns("iobr_pca_id_pdata"),
          label = "ID Column (Pdata)",
          value = "ID",
          placeholder = "Column name (e.g., ID, Sample)"
        ),

        # 动态更新的 Group 选择器
        pickerInput(
          inputId = ns("iobr_pca_group"),
          label = "Group (Pdata)",
          choices = NULL,       # 初始化为空
          multiple = FALSE,
          options = pickerOptions(
            liveSearch = TRUE,
            size = 10,
            style = "btn-outline-secondary", 
            dropupAuto = FALSE,
            container = "body"
          )
        ),

        selectInput(
          inputId = ns("iobr_pca_is.matrix"),
          label = "Matrix",
          choices = c("True" = "T", "False" = "F"),
          selected = "T"
        ),

        selectInput(
          inputId = ns("iobr_pca_scale"),
          label = "Scale",
          choices = c("True" = "T", "False" = "F"),
          selected = "T"
        ),

        selectInput(
          inputId = ns("iobr_pca_is_log"),
          label = "Log",
          choices = c("True" = "T", "False" = "F"),
          selected = "F"
        ),

        selectInput(
          inputId = ns("iobr_pca_geom_ind"),
          label = "Sample Display",
          choices = c("Point" = "point", "Text" = "text", "Both" = "both"),
          selected = "point"
        ),

        selectInput(
          inputId = ns("iobr_pca_palette"),
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
          "Input hex codes or color names separated by comma. If filled, 'Palette' will be ignored."
          ),

        selectInput(
          inputId = ns("iobr_pca_repel"),
          label = "Repel Labels",
          choices = c("True" = "T", "False" = "F"),
          selected = "F"
        ),

        sliderInput(
          inputId = ns("iobr_pca_ncp"),
          label = "Components Number",
          min = 2, max = 20, value = 5, step = 1
        ),

        fluidRow(
          column(6, 
             selectInput(
               inputId = ns("iobr_pca_axis_x"),
               label = "X-axis",
               choices = as.character(1:5),
               selected = "1"
             )
          ),
          column(6,
             selectInput(
               inputId = ns("iobr_pca_axis_y"),
               label = "Y-axis",
               choices = as.character(1:5),
               selected = "2"
             )
          )
        ),

        selectInput(
          inputId = ns("iobr_pca_addEllipses"),
          label = "Add Ellipses",
          choices = c("True" = "T", "False" = "F"),
          selected = "T"
        )
      )
    ),

    # 右侧图表区域
    column(
      width = 9,
      bs4Card(
        title = "Plot Result",
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
            uiOutput(ns("iobr_pca_plot_container"))
          )
        )
      )
    )
  )
}


# ---- Server ----
iobr_pcaServer <- function(id, external_eset = NULL) {
  moduleServer(id, function(input, output, session) {
    
    # 1. 输入数据逻辑
    # 表达矩阵
    input_data <- if (!is.null(external_eset)) {
      external_eset
    } else {
      uploadServer("upload_matrix")
    }
    
    # Pdata (分组信息)
    input_pdata <- uploadServer("upload_pdata")
    
    # 2. 动态更新 Group 选项
    observeEvent(input_pdata(), {
      req(input_pdata())
      pdata <- input_pdata()
      
      # 清洗列名
      all_cols <- gsub("\\.", "-", colnames(pdata))
      is_num <- sapply(pdata, is.numeric)
      non_numeric_cols <- all_cols[!is_num]
      non_numeric_cols <- setdiff(non_numeric_cols, "ID") #只有字符，删去ID列

      updatePickerInput(
        session = session,
        inputId = "iobr_pca_group",
        choices = non_numeric_cols,
        selected = character(0) # 默认不选
      )
    })
    
    iobr_pca_result <- reactiveVal(NULL)
    
    # 3. 主逻辑
    observeEvent(input$run_iobr_pca, {
      req(input_data())
      
      withProgress(message = "Running IOBR PCA...", value = 0, {
        setProgress(0.2, message = "Reading and processing data...")
        
        data <- input_data()
        pdata <- input_pdata() # 可能是 NULL
        
        # 数据列名清洗
        if(!is.null(data)) colnames(data) <- gsub("\\.", "-", colnames(data))
        if(!is.null(pdata)) colnames(pdata) <- gsub("\\.", "-", colnames(pdata))
        
        cols_vec <- "normal"
        raw_cols_text <- input$custom_cols
        
        if (!is.null(raw_cols_text) && trimws(raw_cols_text) != "") {
          # 1. 按逗号、分号或换行符分割
          split_cols <- unlist(strsplit(raw_cols_text, "[,;\n]"))
          # 2. 去除首尾空格
          split_cols <- trimws(split_cols)
          # 3. 去除空字符串
          cols_vec <- split_cols[split_cols != ""]
          
          if (length(split_cols) > 0) {
           cols_vec <- split_cols
         }
        }

        setProgress(0.5, message = "Calculating PCA...")
        
        pp <- tryCatch({
          # 准备坐标轴参数
          axes_vec <- c(as.numeric(input$iobr_pca_axis_x), as.numeric(input$iobr_pca_axis_y))
          
          iobr_pca(
            data        = data,
            is.matrix   = input$iobr_pca_is.matrix == "T",
            scale       = input$iobr_pca_scale == "T",
            is.log      = input$iobr_pca_is_log == "T",
            pdata       = pdata,
            id_pdata    = input$iobr_pca_id_pdata,
            group       = input$iobr_pca_group,
            geom.ind    = input$iobr_pca_geom_ind,
            palette     = input$iobr_pca_palette,
            cols = cols_vec,
            repel       = input$iobr_pca_repel == "T",
            ncp         = input$iobr_pca_ncp,
            axes        = axes_vec,
            addEllipses = input$iobr_pca_addEllipses == "T"
          )
        }, error = function(e) {
          setProgress(1, message = "Error")
          message("Error during iobr_pca(): ", conditionMessage(e))
          print(e)
          showNotification(
          paste("Error during iobr_pca():", conditionMessage(e)),
          type = "error",
          duration = 8
          )
         return(NULL)
        })
        
        req(pp)
        
        setProgress(1, message = "Finished")
        iobr_pca_result(pp)
      })
    })
    
    # 4. 图形下载与展示模块
    # 调用模块并接收返回值 dims
    dims <- plotDownloadServer(
      id = "plot_download", 
      plot_reactive = iobr_pca_result, 
      filename_prefix = "iobr_pca_plot"
    )
    
    # 负责画图
    output$iobr_pca_plot <- renderPlot({
      req(iobr_pca_result())
      iobr_pca_result()
    })

    # 动态渲染 UI 容器
    output$iobr_pca_plot_container <- renderUI({
      req(iobr_pca_result())
      
      div(style = "overflow: auto;",
          plotOutput(session$ns("iobr_pca_plot"), 
                     width = paste0(dims$width(), "px"), 
                     height = paste0(dims$height(), "px"))
      )
    })
    
    return(iobr_pca_result)
  })
}