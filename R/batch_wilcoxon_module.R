# =========================================================
# Module: batch_wilcoxon
# =========================================================

batch_wilcoxonUI <- function(id) {
  ns <- NS(id)
  bs4TabItem(
    tabName = "batch_wilcoxon",
    h3(HTML(
      "Batch_wilcoxon
      <span style='font-size:80%; color:#333;'>:
       Runs Wilcoxon rank-sum tests on multiple features (for two groups).</span>"
      )),     
    batch_wilcoxonBodyUI(id)
  )
}

batch_wilcoxonBodyUI <- function(id, include_upload = TRUE) {
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
            inputId = ns("run_batch_wilcoxon"),
            label = "Run Analysis",
            class = "btn-primary"
          )
        ),

        pickerInput(
          inputId = ns("batch_wilcoxon_target"),
          label = "Group(=2)",
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
          inputId = ns("batch_wilcoxon_feature"),
          label = "Features",
          choices = NULL,
          multiple = TRUE,      
          options = pickerOptions(
            actionsBox = TRUE,  
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
          inputId = ns("batch_wilcoxon_feature_manipulation"),
          label = "Feature Manipulation",
          choices = c(
            "True" = "T",
            "False" = "F"
          ),
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


batch_wilcoxonServer <- function(id, external_eset = NULL) {
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
        inputId = "batch_wilcoxon_target",
        choices = non_numeric_cols,
        selected = character(0) 
      )
      
      updatePickerInput(
        session = session,
        inputId = "batch_wilcoxon_feature",
        choices = numeric_cols,
        selected = numeric_cols
      )
    })

    batch_wilcoxon_result <- reactiveVal(NULL)

    observeEvent(input$run_batch_wilcoxon, {
      req(input_data())
      
      if (is.null(input$batch_wilcoxon_target) || input$batch_wilcoxon_target == "") {
        showNotification("Please select a Target column.", type = "error")
        return(NULL)
      }
      if (is.null(input$batch_wilcoxon_feature) || length(input$batch_wilcoxon_feature) == 0) {
        showNotification("Please select at least one Feature.", type = "error")
        return(NULL)
      }

      withProgress(message = "Running batch Wilcoxon...", value = 0, {
        setProgress(0.2, message = "Reading data...")

        data <- input_data() 

        colnames(data) <- gsub("\\.", "-", colnames(data))

        setProgress(0.5, message = "Processing Wilcoxon...")

        target <- input$batch_wilcoxon_target
        features <- input$batch_wilcoxon_feature 
        feature_manipulation <- input$batch_wilcoxon_feature_manipulation == "T"

        # 二次校验列是否存在
        req_cols <- c(target, features)
        if (!all(req_cols %in% colnames(data))) {
          showNotification("Selected columns not found in data.", type = "error")
          return(NULL)
        }
        
        clean_features <- setdiff(features, target)
        
        if (length(clean_features) == 0) {
           showNotification("No valid features selected (Target cannot be a feature).", type = "error")
           return(NULL)
        }

        result <- tryCatch({
          batch_wilcoxon(
            data                 = data,
            target               = target,
            feature              = clean_features, 
            feature_manipulation = feature_manipulation
          )
        }, error = function(e) {
          setProgress(1, message = "Error")
          showNotification(
            paste("Error during batch_wilcoxon():", e$message),
            type = "error",
            duration = 8
          )
          return(NULL)
        })

        req(result)

        batch_wilcoxon_result(result)
        setProgress(1, message = "Finished")
      })
    })

    dataTableServer("tbl_data", batch_wilcoxon_result)

    dataDownloadServer(
      "download",
      data_reactive = batch_wilcoxon_result,
      filename_prefix = "batch_wilcoxon_result"
    )

    return(batch_wilcoxon_result)
  })
}