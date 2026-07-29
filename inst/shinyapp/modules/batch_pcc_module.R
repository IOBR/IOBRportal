# =========================================================
# Module: batch_pcc
# =========================================================

batch_pccUI <- function(id) {
  ns <- NS(id)
  bs4TabItem(
    tabName = "batch_pcc",
    h3(HTML(
      "Batch_pcc
      <span style='font-size:80%; color:#333;'>:
       Computes partial correlations while adjusting for a third variable.</span>"
      )),     
    batch_pccBodyUI(id)
  )
}

batch_pccBodyUI <- function(id, include_upload = TRUE) {
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
            uploadUI(ns("upload"))%>%
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
            inputId = ns("run_batch_pcc"),
            label = "Run Analysis",
            class = "btn-primary"
          )
        ),

        pickerInput(
          inputId = ns("batch_pcc_interferenceid"),
          label = "Control Variable)",
          choices = NULL,       
          multiple = FALSE,     
          options = pickerOptions(
            liveSearch = TRUE,
            size = 10,
            style = "btn-outline-secondary",
            dropupAuto = FALSE,
            container = "body",
            title = "Select control variable..."
          )
        ),

        pickerInput(
          inputId = ns("batch_pcc_target"),
          label = "Target Variable",
          choices = NULL,
          multiple = FALSE,     
          options = pickerOptions(
            liveSearch = TRUE,
            size = 10,
            style = "btn-outline-secondary",
            dropupAuto = FALSE,
            container = "body"
          )
        ),

        pickerInput(
          inputId = ns("batch_pcc_feature"),
          label = "Features",
          choices = NULL,
          multiple = TRUE,      # <--- 关键：批量分析
          options = pickerOptions(
            actionsBox = TRUE,  # 开启全选/反选
            liveSearch = TRUE,
            size = 10,
            selectedTextFormat = "count > 3",
            style = "btn-outline-secondary",
            dropupAuto = FALSE,
            container = "body",
            title = "Please select multiple..."
          )
        ),        

        selectInput(
          inputId = ns("batch_pcc_method"),
          label = "Method",
          choices = c(
            "Pearson"   = "pearson",
            "Spearman"  = "spearman",
            "Kendall"   = "kendall"
          ),
          selected = "pearson"
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


batch_pccServer <- function(id, external_eset = NULL) {
  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    input_data <- if (!is.null(external_eset)) {
      external_eset
    } else {
      uploadServer("upload")
    }

    # ============================================================
    # [新增] 动态更新 Interference, Target, Feature 的选项
    # ============================================================
    observeEvent(input_data(), {
      req(input_data())
      data <- input_data()
      
      all_cols <- gsub("\\.", "-", colnames(data))
      is_num <- sapply(data, is.numeric)
      pool_numeric <- all_cols[is_num] # 初始数字池
      blacklist_pattern <- "(^|_)time|status|os|id(_|$)"
      is_clinical <- grepl(blacklist_pattern, pool_numeric, ignore.case = TRUE)
      numeric_cols <- pool_numeric[!is_clinical]
      non_numeric_cols <- setdiff(all_cols, numeric_cols)
      
      # 1. 更新 Interference ID
      updatePickerInput(
        session = session,
        inputId = "batch_pcc_interferenceid",
        choices = numeric_cols,
        selected = character(0) 
      )
      
      # 2. 更新 Target
      updatePickerInput(
        session = session,
        inputId = "batch_pcc_target",
        choices = numeric_cols,
        selected = character(0)
      )
      
      # 3. 更新 Feature
      updatePickerInput(
        session = session,
        inputId = "batch_pcc_feature",
        choices = numeric_cols,
        selected = numeric_cols
      )
    })

    batch_pcc_result <- reactiveVal(NULL)

    observeEvent(input$run_batch_pcc, {
      req(input_data())

      if (is.null(input$batch_pcc_interferenceid) || input$batch_pcc_interferenceid == "") {
        showNotification("Please select an Interference ID column.", type = "error")
        return(NULL)
      }
      if (is.null(input$batch_pcc_target) || input$batch_pcc_target == "") {
        showNotification("Please select a Target column.", type = "error")
        return(NULL)
      }
      if (is.null(input$batch_pcc_feature) || length(input$batch_pcc_feature) == 0) {
        showNotification("Please select at least one Feature.", type = "error")
        return(NULL)
      }

      withProgress(message = "Running batch PCC analysis...", value = 0, {
        setProgress(0.2, message = "Reading data...")

        data <- input_data()
        
        colnames(data) <- gsub("\\.", "-", colnames(data))

        setProgress(0.5, message = "Processing PCC...")

        interferenceid <- input$batch_pcc_interferenceid
        target         <- input$batch_pcc_target
        features       <- input$batch_pcc_feature 
        method         <- input$batch_pcc_method

        req_cols <- c(interferenceid, target, features)
        if (!all(req_cols %in% colnames(data))) {
          showNotification("Selected columns not found in data.", type = "error")
          return(NULL)
        }

        clean_features <- setdiff(features, c(interferenceid, target))
        
        if (length(clean_features) == 0) {
           showNotification("No valid features remaining after removing target/interference columns.", type = "error")
           return(NULL)
        }

        result <- tryCatch({
          batch_pcc(
            input          = data,
            interferenceid = interferenceid,
            target         = target,
            features       = clean_features, 
            method         = method
          )
        }, error = function(e) {
          setProgress(1, message = "Error")
          showNotification(
            paste("Error during batch_pcc():", e$message),
            type = "error",
            duration = 8
          )
          return(NULL)
        })
        
        req(result)

        batch_pcc_result(result)
        setProgress(1, message = "Finished")
      })
    })

    dataTableServer("tbl_data", batch_pcc_result)

    dataDownloadServer(
      "download",
      data_reactive = batch_pcc_result,
      filename_prefix = "batch_pcc_result"
    )

    return(batch_pcc_result)
  })
}
