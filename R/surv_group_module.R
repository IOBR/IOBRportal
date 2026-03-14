# =========================================================
# Module: surv_group
# =========================================================

# ---- Full UI ----
surv_groupUI <- function(id) {
  ns <- NS(id)
  bs4TabItem(
    tabName = "surv_group",
    h3(HTML(
      "Surv_group
      <span style='font-size:80%; color:#333;'>: 
      Generates Kaplan-Meier survival plots for categorical grouping variables.</span>"
    )),    
    surv_groupBodyUI(id)
  )
}

# ---- Body UI ----
surv_groupBodyUI <- function(id, include_upload = TRUE) {
  ns <- NS(id)

  fluidRow(
    # 左侧参数栏
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
          style = "text-align: center; margin-top: 10px;",
          actionButton(
            inputId = ns("run_surv_group"),
            label = "Run Analysis",
            class = "btn-primary"
          )
        ),

        textInput(
          inputId = ns("surv_group_ID"),
          label = "ID column",
          value = "ID"
        ),

        pickerInput(
          inputId = ns("surv_group_target_group"),
          label = "Target Group",
          choices = NULL,
          multiple = FALSE, # 单选
          options = pickerOptions(
            liveSearch = TRUE,             # 允许搜索
            size = 10,                     # 显示条数
            style = "btn-outline-secondary", # <--- 关键：找回这个框的样式
            dropupAuto = FALSE,            # 强制向下
            container = "body",
            title = "Select group column..."
          )
        ),

        pickerInput(
          inputId = ns("surv_group_status"),
          label = "Status",
          choices = NULL,
          multiple = FALSE, # 单选
          options = pickerOptions(
            liveSearch = TRUE,
            size = 10,
            style = "btn-outline-secondary", # <--- 关键：找回这个框的样式
            dropupAuto = FALSE,
            container = "body",
            title = "Select status column..."
          )
        ), 

        pickerInput(
          inputId = ns("surv_group_time"),
          label = "Time",
          choices = NULL,
          multiple = FALSE, # 单选
          options = pickerOptions(
            liveSearch = TRUE,
            size = 10,
            style = "btn-outline-secondary", # <--- 关键：找回这个框的样式
            dropupAuto = FALSE,
            container = "body",
            title = "Select time column..."
          )
        ),       

        selectInput(
          inputId = ns("surv_group_time_type"),
          label = "Time Type",
          choices = c("Years", "Months", "Days"),
          selected = "Months"
        ),

        selectInput(
          inputId = ns("surv_group_palette"),
          label = "Palette",
          choices = c("nrc", "aaas", "jco", "jama", "paired1", "paired2", "paired3", "paired4", "accent", "set2"),
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

        numericInput(
          inputId = ns("surv_group_font.size.table"),
          label = "Font Size (Table)",
          value = 5,
          min = 1,
          max = 20
        )
      )
    ),

    # 右侧绘图栏
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
            uiOutput(ns("surv_group_plot_container"))
          )
        )
      )
    )
  )
}

# ---- Server ----
surv_groupServer <- function(id, external_eset = NULL) {
  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    # 1. 获取输入数据
    input_data <- if (!is.null(external_eset)) {
      external_eset
    } else {
      uploadServer("upload")
    }

    # 2. 监听数据变化，自动更新下拉框
    observeEvent(input_data(), {
      req(input_data())
      data <- input_data()
      
      all_cols <- gsub("\\.", "-", colnames(data))
      is_num <- sapply(data, is.numeric)
      group_cols <- all_cols[!is_num]
      group_cols <- setdiff(group_cols, "ID") #只有字符，删去ID列

      pool_numeric <- all_cols[is_num] # 初始数字池
      blacklist_pattern <- "time|status|os|event|censored|days|months|years|fustat|futime|rfs|pfs|dfs"
      is_clinical <- grepl(blacklist_pattern, pool_numeric, ignore.case = TRUE)
      numeric_cols <- pool_numeric[!is_clinical]
      non_numeric_cols <- grep("time|status", all_cols[is_num], ignore.case = TRUE, value = TRUE)


      updatePickerInput(session, "surv_group_target_group", choices = group_cols)
      updatePickerInput(session, "surv_group_time",         choices = non_numeric_cols)
      updatePickerInput(session, "surv_group_status",       choices = non_numeric_cols)
      
      # 尝试自动选中可能的 Time/Status 列 (提升体验)
      potential_time <- grep("time", non_numeric_cols, ignore.case = TRUE, value = TRUE)
      if(length(potential_time) > 0) updatePickerInput(session, "surv_group_time", selected = potential_time[1])
      
      potential_status <- grep("os_status", non_numeric_cols, ignore.case = TRUE, value = TRUE)
      if(length(potential_status) > 0) updatePickerInput(session, "surv_group_status", selected = potential_status[1])
    })

    surv_group_result <- reactiveVal(NULL)

    # 3. 运行分析
    observeEvent(input$run_surv_group, {
      req(input_data())
      data <- input_data()
      req(nrow(data) > 0)
      
      # 再次清洗列名，确保一致
      colnames(data) <- gsub("\\.", "-", colnames(data))

      # 简单的空值校验
      if (is.null(input$surv_group_target_group) || input$surv_group_target_group == "") {
        showNotification("Please select a Target Group.", type = "error"); return(NULL)
      }

      withProgress(message = "Running surv_group analysis...", value = 0, {

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

        setProgress(0.5, message = "Plotting...")

        pp <- tryCatch({
          surv_group(
            input_pdata       = data,
            target_group      = input$surv_group_target_group, # 这是一个列名字符串
            ID                = input$surv_group_ID,
            time              = input$surv_group_time,         # 列名字符串
            status            = input$surv_group_status,       # 列名字符串
            time_type         = input$surv_group_time_type,
            palette           = input$surv_group_palette,
            cols = cols_vec,
            font.size.table   = input$surv_group_font.size.table
          )
        }, error = function(e) {
          setProgress(1, message = "Error")
          showNotification(
            paste("Error during surv_group():", e$message),
            type = "error",
            duration = 8
          )
          return(NULL)
        })
        
        req(pp)

        surv_group_result(pp)
        setProgress(1, message = "Finished")
      })
    })

    output$surv_group_plot_output <- renderPlot({
      req(surv_group_result())
      surv_group_result()
    })

    dims <- plotDownloadServer(
      id = "plot_download",
      plot_reactive = surv_group_result,
      filename_prefix = "surv_group_plot"
    )

    output$surv_group_plot_container <- renderUI({
      req(surv_group_result())
      
      div(style = "overflow: auto;",
          plotOutput(session$ns("surv_group_plot_output"), 
                     width = paste0(dims$width(), "px"), 
                     height = paste0(dims$height(), "px"))
      )
    })

    return(surv_group_result)
  })
}