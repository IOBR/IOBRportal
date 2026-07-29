# =========================================================
# Module: lr_cal
# =========================================================

# ---- Full UI ----
lr_calUI <- function(id) {
  ns <- NS(id)
  bs4TabItem(
    tabName = "lr_cal",
    h3(HTML(
      "LR_cal
      <span style='font-size:80%; color:#333;'>:
       Quantifies ligand-receptor interactions in the tumor microenvironment.</span>"
      )),
    lr_calBodyUI(id)
  )
}

# ---- Body UI ----
lr_calBodyUI <- function(id, include_upload = TRUE) {
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
          tagList(
            uploadUI(ns("upload")) %>%
              helper(
                type    = "markdown",
                icon    = "question-circle",
                size    = "m",
                colour  = "#007bff",
                content = "demo_eset_stad"
              )
          )
        },
        

        div(
          style = "text-align: center; margin-top: 10px;",
          actionButton(
            inputId = ns("run_lr_cal"),
            label = "Run Analysis",
            class = "btn-primary"
          )
        ),

        selectInput(
          inputId = ns("lr_cal_data_type"),
          label = "Data Type",
          choices = c("Count" = "count", "TPM" = "tpm"),
          selected = "count"
        ),

        selectInput(
          inputId = ns("lr_cal_id_type"),
          label = "ID Type",
          choices = c("Ensembl" = "ensembl", "Symbol" = "symbol"),
          selected = "ensembl"
        ),
        br(),
        tags$p(
          style = "color: #555; font-style: italic; font-size: 90%; margin-top: -10px;",
          "Note: LR_cal will take a long time."
          )
      ),

      dataDownloadUI(ns("download"))
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
          side = "left",
          tabPanel("Data", dataTableUI(ns("tbl_data")))
        )
      )
    )
  )
}

# ---- Server ----
lr_calServer <- function(id, external_eset = NULL) {
  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    input_data <- if (!is.null(external_eset)) {
      external_eset
    } else {
      uploadServer("upload")
    }

    # 用于缓存结果
    lr_cal_result <- reactiveVal(NULL)

    observeEvent(input$run_lr_cal, {
      req(input_data())

      withProgress(message = "Running LR_cal analysis...", value = 0, {
        setProgress(0.2, message = "Reading data...")
        data <- input_data()

        req(nrow(data) > 0)
        colnames(data) <- gsub("\\.", "-", colnames(data))  #列名标准化

        setProgress(0.5, message = "Running LR_cal...")
    
        # 数据预处理：缺失值处理
        data_clean <- data
        data_clean[is.na(data_clean)] <- 0
        data_clean <- data_clean[complete.cases(data_clean), ]
        
        result <- tryCatch({
          LR_cal(
            eset        = data_clean,
            data_type   = input$lr_cal_data_type,
            id_type     = input$lr_cal_id_type,
            cancer_type = "pancan"
          )
        }, error = function(e) {
          msg <- conditionMessage(e)
          print(msg)
          traceback()
          setProgress(1, message = "Error")
          showNotification(
            paste("Error during LR_cal():", msg),
            type = "error",
            duration = 8
          )
          return(NULL)
        })
        
        req(result)

        if (is.matrix(result)) result <- as.data.frame(result)

        lr_cal_result(result)
        setProgress(1, message = "Finished")
      })
    })

    # 渲染 DataTable
    dataTableServer("tbl_data", lr_cal_result)

    # 下载模块
    dataDownloadServer(
      "download",
      data_reactive = lr_cal_result,
      filename_prefix = "lr_cal_result"
    )

    return(lr_cal_result)
  })
}
