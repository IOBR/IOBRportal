# =========================================================
# Module: batch_surv
# =========================================================

# ---- Full UI ----
batch_survUI <- function(id) {
  ns <- NS(id)
  bs4TabItem(
    tabName = "batch_surv",
    h3(HTML(
      "Batch_surv
      <span style='font-size:80%; color:#333;'>:
       Runs survival Cox analysis for multiple features.</span>"
      )),     
    batch_survBodyUI(id)
  )
}


# ---- BodyUI ----
batch_survBodyUI <- function(id, include_upload = TRUE) {
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
                content = "demo_stad_com"
              )
          )
        },

        div(
          style = "text-align: center; margin-top: 10px;",
          actionButton(
            inputId = ns("run_batch_surv"),
            label = "Run Analysis",
            class = "btn-primary"
          )
        ),

        pickerInput(
          inputId = ns("batch_surv_variable"),
          label = "Variables",
          choices = NULL,       # Server 端填充
          multiple = TRUE,      # <--- 关键：批量分析必须支持多选
          options = pickerOptions(
            actionsBox = TRUE,  # 必须开启：允许一键全选/反选
            liveSearch = TRUE,  # 允许搜索
            size = 10,
            selectedTextFormat = "count > 3", # 选多了只显示数量
            style = "btn-outline-secondary",  # 保持统一风格
            dropupAuto = FALSE,
            container = "body",
            title = "Please select multiple..."
          )
        ),

        pickerInput(
          inputId = ns("batch_surv_status"),
          label = "Status",
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
          inputId = ns("batch_surv_time"),
          label = "Time",
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

        selectInput(
          inputId = ns("batch_surv_best_cutoff"),
          label = "best_cutoff",
          choices = c("True" = "T", "False" = "F"),
          selected = "F"
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
batch_survServer <- function(id, external_eset = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    input_data <- if (!is.null(external_eset)) {
      external_eset
    } else {
      uploadServer("upload")
    }

    observeEvent(input_data(), {
      req(input_data())
      data <- input_data()
      data$status <- NULL

      all_cols <- gsub("\\.", "-", colnames(data))
      is_num <- sapply(data, is.numeric)
      pool_numeric <- all_cols[is_num] # 初始数字池
      blacklist_pattern <- "time|status|os|event|censored|days|months|years|fustat|futime|rfs|pfs|dfs"
      is_clinical <- grepl(blacklist_pattern, pool_numeric, ignore.case = TRUE)
      numeric_cols <- pool_numeric[!is_clinical]
      non_numeric_cols <- grep("time|status", all_cols[is_num], ignore.case = TRUE, value = TRUE)
      
      updatePickerInput(
        session = session,
        inputId = "batch_surv_variable",
        choices = numeric_cols,
        selected = numeric_cols
      )
      
      updatePickerInput(
        session = session,
        inputId = "batch_surv_time",
        choices = non_numeric_cols
      )
      
      updatePickerInput(
        session = session,
        inputId = "batch_surv_status",
        choices = non_numeric_cols
      )

      potential_time <- grep("time", non_numeric_cols, ignore.case = TRUE, value = TRUE)
      if(length(potential_time) > 0) updatePickerInput(session, "batch_surv_time", selected = potential_time[1])
      
      potential_status <- grep("os_status", non_numeric_cols, ignore.case = TRUE, value = TRUE)
      if(length(potential_status) > 0) updatePickerInput(session, "batch_surv_status", selected = potential_status[1])
    })

    batch_surv_result <- reactiveVal(NULL)
    
    observeEvent(input$run_batch_surv, {
      req(input_data())
      

      if (is.null(input$batch_surv_variable) || length(input$batch_surv_variable) == 0) {
        showNotification("Please select at least one Variable.", type = "error")
        return(NULL)
      }
      if (is.null(input$batch_surv_time) || input$batch_surv_time == "") {
        showNotification("Please select a Time column.", type = "error")
        return(NULL)
      }
      if (is.null(input$batch_surv_status) || input$batch_surv_status == "") {
        showNotification("Please select a Status column.", type = "error")
        return(NULL)
      }

      withProgress(message = "Running batch survival analysis...", value = 0, {
        setProgress(0.2, message = "Reading data...")
        
        data <- input_data()
        data$status <- NULL
        req(nrow(data) > 0)
        
        colnames(data) <- gsub("\\.", "-", colnames(data))
        
        # 二次检查列是否存在
        req_cols <- c(input$batch_surv_variable, input$batch_surv_time, input$batch_surv_status)
        if (!all(req_cols %in% colnames(data))) {
          showNotification("Selected columns not found in data.", type = "error")
          return(NULL)
        }
        
        setProgress(0.5, message = "Processing survival analysis...")
        
        variable_vec <- input$batch_surv_variable
        time_col     <- input$batch_surv_time
        status_col   <- input$batch_surv_status
        best_cutoff  <- input$batch_surv_best_cutoff == "T"

        # 调用 batch_surv
        result <- tryCatch({
          batch_surv(
            pdata       = data,
            variable    = variable_vec, # 直接传入向量
            time        = time_col,
            status      = status_col,
            best_cutoff = best_cutoff
          )
        }, error = function(e) {
          setProgress(1, message = "Error")
          showNotification(
            paste("Error during batch_surv():", e$message),
            type = "error",
            duration = 8
          )
          return(NULL)
        })
        
        req(result)
        batch_surv_result(result)
        setProgress(1, message = "Finished")
      })
    })

    dataTableServer("tbl_data", batch_surv_result)
    
    dataDownloadServer(
      "download",
      data_reactive = batch_surv_result,
      filename_prefix = "batch_surv_result"
    )
    
    return(batch_surv_result)
  })
}