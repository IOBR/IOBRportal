# =========================================================
# Module: remove_batcheffect
# =========================================================

# ---- Full UI ----
remove_batcheffectUI <- function(id) {
  ns <- NS(id)
  bs4TabItem(
    tabName = "remove_batcheffect",
    h3(HTML(
      "Remove_batcheffect
      <span style='font-size:80%; color:#333;'>:
       Integrate and correct batch effects from multiple datasets.</span>"
      )),
    remove_batcheffectBodyUI(id)
  )
}


# ---- Body UI ----
remove_batcheffectBodyUI <- function(id, include_upload = TRUE) {
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

        # Dataset 1 (Primary)
        if (isTRUE(include_upload)) {
          tagList(
            uploadUI(ns("upload1")) %>%
              helper(
                type    = "markdown",
                icon    = "question-circle",
                size    = "m",
                colour  = "#007bff",
                content = "demo_eset_stad"
              )
          )
        },        

        uploadUI(ns("upload2")),
        uploadUI(ns("upload3")),

        div(
          style = "text-align: center; margin-top: 15px; margin-bottom: 5px;",
          actionButton(
            inputId = ns("run_remove_batcheffect"),
            label = "Run Analysis",
            class = "btn-primary"
          )
        ),

        selectInput(
          inputId = ns("remove_batcheffect_idType"),
          label = "ID Type",
          choices = c("Ensembl" = "ensembl", "Symbol" = "symbol"),
          selected = "ensembl"
        ),

        selectInput(
          inputId = ns("remove_batcheffect_data_type"),
          label = "Data Type",
          choices = c("Array" = "array", "Count" = "count", "Tpm" = "tpm"),
          selected = "count"
        )
      ),

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
          tabPanel("Data", dataTableUI(ns("tbl_data"))),
          tabPanel(
            "Plot", 
            plotDownloadUI(ns("plot_download")),
            uiOutput(ns("remove_batcheffect_plot_container"))
          )
        )
      )
    )
  )
}


remove_batcheffectServer <- function(id, external_eset = NULL) {
  moduleServer(id, function(input, output, session) {
    
    # 1. 输入数据逻辑
    eset1_input <- if (!is.null(external_eset)) {
      external_eset
    } else {
      uploadServer("upload1")
    }
    
    eset2_input <- uploadServer("upload2")
    eset3_input <- uploadServer("upload3") 

    # 缓存结果
    remove_batcheffect_result <- reactiveVal(NULL)   # 存数据
    remove_batcheffect_plot_obj <- reactiveVal(NULL) # 存图

    # 主逻辑
    observeEvent(input$run_remove_batcheffect, {
      # 必须要有 d1 和 d2
      req(eset1_input(), eset2_input()) 

      withProgress(message = "Running remove_batcheffect analysis...", value = 0, {
        setProgress(0.2, message = "Reading input files...")

        d1 <- eset1_input()
        d2 <- eset2_input()
        
        # 安全获取 d3：因为它是 optional 的，如果用户没上传，uploadServer内部的req可能会阻断
        # 这里用 tryCatch 尝试获取，如果失败(没上传)则设为 NULL
        d3 <- tryCatch({ eset3_input() }, error = function(e) NULL)
        
        # 列名清洗
        if(!is.null(d1)) colnames(d1) <- gsub("\\.", "-", colnames(d1))
        if(!is.null(d2)) colnames(d2) <- gsub("\\.", "-", colnames(d2))
        if(!is.null(d3)) colnames(d3) <- gsub("\\.", "-", colnames(d3))

        setProgress(0.5, message = "Running IOBR::remove_batcheffect...")

        # --- 核心修改：开启虚拟画板 ---
        # 1. 开启一个不会显示在屏幕上的 pdf 设备
        pdf(file = NULL) 
        # 2. 确保开启记录功能
        dev.control(displaylist = "enable") 
        
        result_data <- NULL
        run_success <- FALSE

        tryCatch({
          # 3. 运行分析函数
          result_data <- remove_batcheffect(
            eset1       = d1,
            eset2       = d2,
            eset3       = d3,
            id_type     = input$remove_batcheffect_idType,
            data_type   = input$remove_batcheffect_data_type,
            cols        = "normal",
            palette     = "jama",
            log2        = TRUE,
            check_eset  = TRUE,
            adjust_eset = TRUE,
            repel       = FALSE,
            path        = NULL 
          )
          
          # 4. 捕获当前设备上的图像
          p_recorded <- recordPlot()
          
          # 存入 reactive
          remove_batcheffect_result(result_data)
          remove_batcheffect_plot_obj(p_recorded)
          run_success <- TRUE
          
        }, error = function(e) {
          showNotification(paste("Error:", e$message), type = "error", duration = 8)
        })
        
        # 5. 关闭设备，释放资源
        dev.off() 
        # -----------------------------

        if(run_success){
          setProgress(1, message = "Finished")
        } else {
          setProgress(1, message = "Failed")
        }
      })
    })

    # 2. 表格输出
    dataTableServer("tbl_data", remove_batcheffect_result)

    # 3. 数据下载
    dataDownloadServer(
      "download_data",
      data_reactive   = remove_batcheffect_result,
      filename_prefix = "remove_batcheffect_result"
    )

    # 4. 图形下载与展示
    dims <- plotDownloadServer(
      id = "plot_download",
      plot_reactive = remove_batcheffect_plot_obj,
      filename_prefix = "remove_batcheffect_PCA"
    )

    output$remove_batcheffect_plot <- renderPlot({
      req(remove_batcheffect_plot_obj())
      replayPlot(remove_batcheffect_plot_obj())
    })

    output$remove_batcheffect_plot_container <- renderUI({
      req(remove_batcheffect_plot_obj())
      div(style = "overflow: auto;",
          plotOutput(session$ns("remove_batcheffect_plot"), 
                     width = paste0(dims$width(), "px"), 
                     height = paste0(dims$height(), "px"))
      )
    })

    return(remove_batcheffect_result)
  })
}