# =========================================================
# Module: sig_surv_plot
# =========================================================

sig_surv_plotUI <- function(id) {
  ns <- NS(id)
  bs4TabItem(
    tabName = "sig_surv_plot",
    h3(HTML(
      "Sig_surv_plot
      <span style='font-size:80%; color:#333;'>:
       Generates multiple KM survival curves for genes or signatures.</span>"
      )),
    sig_surv_plotBodyUI(id)
  )
}


sig_surv_plotBodyUI <- function(id, include_upload = TRUE) {
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

        # 上传组件
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
            inputId = ns("run_sig_surv_plot"),
            label = "Run Analysis",
            class = "btn-primary"
          )
        ),

        pickerInput(
          inputId = ns("sig_surv_plot_signature"),
          label = "Signature",
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
          inputId = ns("sig_surv_plot_status"),
          label   = "Status",
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
          inputId = ns("sig_surv_plot_time"),
          label   = "Time",
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

        textInput(
          inputId = ns("sig_surv_plot_ID"),
          label   = "ID",
          value   = "ID"
        ),

        selectInput(
          inputId = ns("sig_surv_plot_time_type"),
          label   = "Time Type",
          choices = c("Month" = "month", "Day" = "day"),
          selected = "month"
        ),

        selectInput(
          inputId = ns("sig_surv_plot_palette"),
          label   = "Palette",
          choices = c("nrc","jama","aaas","jco","paired1","paired2",
                      "paired3","paired4","accent","set2"),
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
          )
      )
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
          tabPanel(
            "Plot", 
            plotDownloadUI(ns("plot_download")),
            uiOutput(ns("sig_surv_plot_container"))
          )
        )
      )
    )
  )
}


sig_surv_plotServer <- function(id, external_eset = NULL) {
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
      pool_numeric <- all_cols[is_num] # 初始数字池
      blacklist_pattern <- "(^|_)(time|status|os|event|censored|days|months|years|fustat|futime|rfs|pfs|dfs)(_|$)"
      is_clinical <- grepl(blacklist_pattern, pool_numeric, ignore.case = TRUE)
      numeric_cols <- pool_numeric[!is_clinical]
      non_numeric_cols <- grep("time|status", all_cols[is_num], ignore.case = TRUE, value = TRUE)

      updatePickerInput(
        session = session,
        inputId = "sig_surv_plot_signature",
        choices = numeric_cols,
        selected = character(0) 
      )
      
      updatePickerInput(
        session = session,
        inputId = "sig_surv_plot_time",
        choices = non_numeric_cols
      )
      
      updatePickerInput(
        session = session,
        inputId = "sig_surv_plot_status",
        choices = non_numeric_cols
      )

      potential_time <- grep("time", non_numeric_cols, ignore.case = TRUE, value = TRUE)
      if(length(potential_time) > 0) updatePickerInput(session, "sig_surv_plot_time", selected = potential_time[1])
      
      potential_status <- grep("os_status", non_numeric_cols, ignore.case = TRUE, value = TRUE)
      if(length(potential_status) > 0) updatePickerInput(session, "sig_surv_plot_status", selected = potential_status[1])
    })

    plot_result <- reactiveVal(NULL)

    observeEvent(input$run_sig_surv_plot, {
      req(input_data())

      if (is.null(input$sig_surv_plot_signature) || input$sig_surv_plot_signature == "") {
        showNotification("Please select a Signature column.", type = "error")
        return(NULL)
      }
      if (is.null(input$sig_surv_plot_time) || input$sig_surv_plot_time == "") {
        showNotification("Please select a Time column.", type = "error")
        return(NULL)
      }
      if (is.null(input$sig_surv_plot_status) || input$sig_surv_plot_status == "") {
        showNotification("Please select a OS_status column.", type = "error")
        return(NULL)
      }

      withProgress(message = "Running sig_surv_plot analysis...", value = 0, {
        data <- input_data()
        req(nrow(data) > 0)

        colnames(data) <- gsub("\\.", "-", colnames(data))
        
        if (!all(c(input$sig_surv_plot_signature, input$sig_surv_plot_time, input$sig_surv_plot_status) %in% colnames(data))) {
           showNotification("Selected columns not found in data.", type = "error")
           return(NULL)
        }

        cols_vec <- NULL
        raw_cols_text <- input$custom_cols
        
        if (!is.null(raw_cols_text) && trimws(raw_cols_text) != "") {
          # 1. 按逗号、分号或换行符分割
          split_cols <- unlist(strsplit(raw_cols_text, "[,;\n]"))
          # 2. 去除首尾空格
          split_cols <- trimws(split_cols)
          # 3. 去除空字符串
          cols_vec <- split_cols[split_cols != ""]
          
          if(length(cols_vec) == 0) cols_vec <- NULL
        }

        setProgress(0.5, message = "Performing survival analysis...")

        pp <- tryCatch({
          sig_surv_plot(
            input_pdata = data,
            signature   = input$sig_surv_plot_signature, 
            ID          = input$sig_surv_plot_ID,
            cols = cols_vec,        
            time        = input$sig_surv_plot_time,      
            status      = input$sig_surv_plot_status,    
            time_type   = input$sig_surv_plot_time_type,
            palette     = input$sig_surv_plot_palette,
            show_col    = FALSE,
            save_path = NULL
          )
        }, error = function(e) {
          setProgress(1, message = "Error")
          showNotification(
            paste("Error during sig_surv_plot():", e$message),
            type = "error",
            duration = 8
          )
          return(NULL)
        })
        
        req(pp)

        plot_result(pp$plots)

        setProgress(1, message = "Plot ready.")
      })
    })

    output$sig_surv_plot_output <- renderPlot({
      req(plot_result()) 
      print(plot_result())
    })

    dims <- plotDownloadServer(
      id = "plot_download", 
      plot_reactive = plot_result, 
      filename_prefix = "sig_surv_plot"
    )

    output$sig_surv_plot_container <- renderUI({
      req(plot_result())
      
      div(style = "overflow: auto;",
          plotOutput(session$ns("sig_surv_plot_output"), 
                     width = paste0(dims$width(), "px"), 
                     height = paste0(dims$height(), "px"))
      )
    })

    return(plot_result)
  })
}