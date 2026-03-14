# =========================================================
# Module: remove_duplicate_genes
# =========================================================

# ---- Full UI ----
remove_duplicate_genesUI <- function(id) {
  ns <- NS(id)
  bs4TabItem(
    tabName = "remove_duplicate_genes",
    h3(HTML(
      "Remove_duplicate_genes
      <span style='font-size:80%; color:#333;'>:
       Aggregate duplicate gene symbols using Mean, SD, or Sum.</span>"
      )),
    remove_duplicate_genesBodyUI(id)
  )
}


# ---- Body UI ----
remove_duplicate_genesBodyUI <- function(id, include_upload = TRUE) {
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
            inputId = ns("run_remove_duplicate_genes"),
            label = "Run Analysis",
            class = "btn-primary"
          )
        ),

        # 保留原有的 textInput，不使用动态更新组件
        textInput(
          inputId = ns("remove_duplicate_genes_column_of_symbol"),
          label = "Symbol Column",
          value = "id",  
          placeholder = "Column name (e.g., Symbol)"
        ),

        selectInput(
          inputId = ns("remove_duplicate_genes_method"),
          label = "Method",
          choices = c(
            "Mean" = "mean",
            "SD" = "sd",
            "Sum" = "sum"
          ),
          selected = "mean"
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
remove_duplicate_genesServer <- function(id, external_eset = NULL) {
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
    observeEvent(input$run_remove_duplicate_genes, {
      req(input_data())
      
      withProgress(message = "Removing duplicates...", value = 0, {
        setProgress(0.2, message = "Reading data...")
        
        data <- input_data()
        
        # 基础清洗
        if(!is.null(data)) colnames(data) <- gsub("\\.", "-", colnames(data))
        
        # 检查输入的列名是否存在
        col_symbol <- input$remove_duplicate_genes_column_of_symbol
        if (!(col_symbol %in% colnames(data))) {
          showNotification(paste("Column", col_symbol, "not found in data."), type = "error")
          return(NULL)
        }

        setProgress(0.5, message = "Processing...")
        
        result <- tryCatch({
          # 调用函数 (假设函数已在环境中加载)
          remove_duplicate_genes(
            eset = data,
            column_of_symbol = col_symbol,
            method = input$remove_duplicate_genes_method
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
      filename_prefix = "remove_duplicate_genes_result"
    )
    
    return(res_data)
  })
}