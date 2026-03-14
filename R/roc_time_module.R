# =========================================================
# Module: roc_time
# =========================================================

roc_timeUI <- function(id) {
  ns <- NS(id)
  bs4TabItem(
    tabName = "roc_time",
    h3(HTML(
      "Roc_time
      <span style='font-size:80%; color:#333;'>:
       Generates time-dependent ROC curves with AUC values.</span>"
      )),
    roc_timeBodyUI(id)
  )
}

roc_timeBodyUI <- function(id, include_upload = TRUE) {
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
          tagList(uploadUI(ns("upload"))%>%
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
            inputId = ns("run_roc_time"),
            label = "Run Analysis",
            class = "btn-primary"
          )
        ),
        
        pickerInput(
          inputId = ns("vars"),
          label = "Variables (Max 3)", 
          choices = NULL,      
          multiple = TRUE,     
          options = pickerOptions(
            actionsBox = TRUE, 
            liveSearch = TRUE,
            size = 10,
            selectedTextFormat = "count > 2",
            style = "btn-outline-secondary",
            dropupAuto = FALSE,
            container = "body",
            maxOptions = 3, 
            maxOptionsText = "Limit reached (max 3 variables)",
            title = "Please select 1-3 variables..." 
          )
        ),

        pickerInput(
          inputId = ns("status"),
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
          inputId = ns("time"),
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

        sliderInput(
          ns("time_point"),
          "Time Point",
          min = 1,
          max = 31,
          value = 12,
          step = 1
        ),

        selectInput(
          ns("time_type"),
          "Time Type",
          choices = c(
            "Month" = "month",
            "Day" = "day"
          ),
          selected = "month"
        ),

        selectInput(
          ns("palette"),
          "Palette",
          choices = c(
            "nrc",
            "jama",
            "aaas",
            "jco",
            "paired1",
            "paired2",
            "paired3",
            "paired4",
            "accent",
            "set2"
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

        textInput(
          ns("main"),
          "Title",
          value = "",
          placeholder = "optional"
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
            # 1. 顶部插入标准化工具栏
            plotDownloadUI(ns("plot_download")),
            # 2. 替换为动态 UI 容器
            uiOutput(ns("roc_time_plot_container"))
          )
        )
      )
    )
  )
}

roc_timeServer <- function(id, external_eset = NULL) {
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
      blacklist_pattern <- "time|status|os|event|censored|days|months|years|fustat|futime|rfs|pfs|dfs"
      is_clinical <- grepl(blacklist_pattern, pool_numeric, ignore.case = TRUE)
      numeric_cols <- pool_numeric[!is_clinical]
      non_numeric_cols <- grep("time|status", all_cols[is_num], ignore.case = TRUE, value = TRUE)
      
      # 3. 更新 Variables (必须是数值)
      updatePickerInput(
        session = session, 
        inputId = "vars", 
        choices = numeric_cols, # <--- 只给看数字列
        selected = character(0)
      )
      
      # 4. 更新 Time (必须是数值)
      updatePickerInput(
        session = session, 
        inputId = "time", 
        choices = non_numeric_cols
      )
      
      updatePickerInput(
        session = session, 
        inputId = "status", 
        choices = non_numeric_cols
      )

      potential_time <- grep("time", non_numeric_cols, ignore.case = TRUE, value = TRUE)
      if(length(potential_time) > 0) updatePickerInput(session, "time", selected = potential_time[1])
      
      potential_status <- grep("os_status", non_numeric_cols, ignore.case = TRUE, value = TRUE)
      if(length(potential_status) > 0) updatePickerInput(session, "status", selected = potential_status[1])
    })

    plot_result <- reactiveVal(NULL)

    observeEvent(input$run_roc_time, {
      req(input_data())

      # [验证] 检查输入是否为空
      if (is.null(input$vars) || length(input$vars) == 0) {
        showNotification("Please select at least one Variable.", type = "error")
        return(NULL)
      }
      if (is.null(input$time) || input$time == "") {
        showNotification("Please select a Time column.", type = "error")
        return(NULL)
      }
      if (is.null(input$status) || input$status == "") {
        showNotification("Please select a Status column.", type = "error")
        return(NULL)
      }

      withProgress(message = "Running roc_time analysis...", value = 0, {
        
        data <- input_data()
        req(nrow(data) > 0)

        colnames(data) <- gsub("\\.", "-", colnames(data))
        
        req_cols <- c(input$vars, input$time, input$status)
        if (!all(req_cols %in% colnames(data))) {
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

        setProgress(0.4)

        vars_vec <- input$vars 

        result <- tryCatch({
          roc_time(
            input      = data,
            vars       = vars_vec,     
            time       = input$time,   
            status     = input$status, 
            time_point = input$time_point,
            time_type  = input$time_type,
            palette    = input$palette,
            cols = cols_vec,
            main       = if (input$main == "") NULL else input$main
          )
        }, error = function(e) {
          setProgress(1, message = "Error")
          showNotification(
            paste("Error during roc_time():", e$message),
            type = "error",
            duration = 8
          )
          return(NULL)
        })
        
        req(result)
        plot_result(result)

        setProgress(1, message = "Finished")
      })
    })

    output$plot_output <- renderPlot({
      req(plot_result())
      print(plot_result())
    })

    dims <- plotDownloadServer(
      id = "plot_download",
      plot_reactive = plot_result,
      filename_prefix = "roc_time_plot"
    )

    output$roc_time_plot_container <- renderUI({
      req(plot_result())
      
      div(style = "overflow: auto;",
          plotOutput(session$ns("plot_output"), 
                     width = paste0(dims$width(), "px"), 
                     height = paste0(dims$height(), "px"))
      )
    })

    return(plot_result)
  })
}
