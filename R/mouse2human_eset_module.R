# =========================================================
# Module: mouse2human_eset
# =========================================================

# ---- Full UI ----
mouse2human_esetUI <- function(id) {
  ns <- NS(id)
  bs4TabItem(
    tabName = "mouse2human_eset",
    h3(HTML(
      "Mouse2human_eset
      <span style='font-size:80%; color:#333;'>:
       Convert mouse gene symbols in an expression set to human homologs.</span>"
      )),
    mouse2human_esetBodyUI(id)
  )
}


# ---- Body UI ----
mouse2human_esetBodyUI <- function(id, include_upload = TRUE) {
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

        # 1. Upload
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
            inputId = ns("run_mouse2human_eset"),
            label = "Run Analysis",
            class = "btn-primary"
          )
        ),
        
        selectInput(
          inputId = ns("mouse2human_eset_source"),
          label = "Source",
          choices = c(
            "Ensembl" = "ensembl",
            "Local" = "local"
          ),
          selected = "local"
        ),

        selectInput(
          inputId = ns("mouse2human_eset_is_matrix"),
          label = "Matrix",
          choices = c(
            "True" = "T",
            "False" = "F"
          ),
          selected = "T"
        ),

        textInput(
          inputId = ns("mouse2human_eset_column_of_symbol"),
          label = "Symbol Column",
          value = "",  
          placeholder = "Column name (e.g., symbol)"
        ),
        br(),
        tags$p(
          style = "color: #555; font-style: italic; font-size: 90%; margin-top: -10px;",
          "The parameter must be specified if 'Matrix' is FALSE."
        )
      ),
      
      # 数据下载组件
      dataDownloadUI(ns("download_data"))
    ),
    
    # 右侧数据区域
    column(
      width = 9,
      bs4Card(
        title = "Result Data",
        status = "gray",
        headerBorder = TRUE,
        collapsible = TRUE,
        solidHeader = TRUE,
        width = 12,
        maximizable = TRUE,
        tabBox(
          width = 12,
          side = "left",
          tabPanel("Data", dataTableUI(ns("tbl_data")))
        )
      )
    )
  )
}


# ---- Server ----
mouse2human_esetServer <- function(id, external_eset = NULL) {
  moduleServer(id, function(input, output, session) {
    
    # 1. 输入数据逻辑
    input_data <- if (!is.null(external_eset)) {
      external_eset
    } else {
      uploadServer("upload")
    }
    
    # 缓存结果
    res_data <- reactiveVal(NULL)
    
    # 2. 主逻辑
    observeEvent(input$run_mouse2human_eset, {
      req(input_data())
      
      withProgress(message = "Converting mouse to human symbols...", value = 0, {
        setProgress(0.2, message = "Reading data...")
        
        eset <- input_data()
        
        # 基础清洗
        if(!is.null(eset)) colnames(eset) <- gsub("\\.", "-", colnames(eset))
        
        # 提取参数
        is_matrix_val <- input$mouse2human_eset_is_matrix == "T"
        source_val <- input$mouse2human_eset_source
        column_val <- input$mouse2human_eset_column_of_symbol
        
        # 校验参数
        if (!is_matrix_val && (is.null(column_val) || column_val == "")) {
          showNotification("Please specify the column containing gene symbols when 'Matrix' is FALSE.", type = "error")
          return(NULL)
        }
        
        setProgress(0.5, message = "Converting...")
        
        result <- tryCatch({
          # 调用函数
          mouse2human_eset(
            eset = eset,
            source = source_val,
            is_matrix = is_matrix_val,
            column_of_symbol = if (is_matrix_val) NULL else column_val
          )
        }, error = function(e) {
          setProgress(1, message = "Error")
          showNotification(paste("Error:", e$message), type = "error", duration = 8)
          return(NULL)
        })
        
        req(result)
        
        # 缓存结果
        res_data(result)
        
        setProgress(1, message = "Finished")
      })
    })
    
    # 3. 输出部分
    # 3.1 表格输出
    dataTableServer("tbl_data", res_data)
    
    # 3.2 数据下载
    dataDownloadServer(
      "download_data",
      data_reactive   = res_data,
      filename_prefix = "mouse2human_result"
    )
    
    return(res_data)
  })
}