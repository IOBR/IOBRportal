# =========================================================
# Module: percent_bar_plot
# =========================================================

# ---- UI ----
percent_bar_plotUI <- function(id) {
  ns <- NS(id)
  bs4TabItem(
    tabName = "percent_bar_plot",
    h3(HTML(
      "Percent_bar_plot
      <span style='font-size:80%; color:#333;'>:
       Plots percentage bar charts for categorical variables.</span>"
      )),
    percent_bar_plotBodyUI(id)
  )
}

# ---- Body UI ----
percent_bar_plotBodyUI <- function(id, include_upload = TRUE) {
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
                content = "demo_stad_pdata"
              )
          )
        },


        div(
          style = "text-align: center; margin-top: 10px;",
          actionButton(
            inputId = ns("run_percent_bar_plot"),
            label = "Run Analysis",
            class = "btn-primary"
          )
        ),
        br(),

        pickerInput(
          inputId = ns("percent_bar_plot_x"),
          label = "X Variable",
          choices = NULL,
          multiple = FALSE, # 单选
          options = pickerOptions(
            liveSearch = TRUE,
            size = 10,
            style = "btn-outline-secondary", 
            dropupAuto = FALSE,
            container = "body",
            title = "Select X column..."
          )
        ),

        pickerInput(
          inputId = ns("percent_bar_plot_y"),
          label = "Y Variable",
          choices = NULL,
          multiple = FALSE, # 单选
          options = pickerOptions(
            liveSearch = TRUE,
            size = 10,
            style = "btn-outline-secondary", 
            dropupAuto = FALSE,
            container = "body",
            title = "Select Y column..."
          )
        ),

        selectInput(
          inputId = ns("percent_bar_plot_palette"),
          label = "Palette",
          choices = c("nrc","jama","aaas","jco","paired1","paired2","paired3","paired4","accent","set2"),
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
          inputId = ns("percent_bar_plot_title"),
          label = "Title",
          value = "title(optional)"
        ),

        sliderInput(
          inputId = ns("percent_bar_plot_axis_angle"),
          label = "Axis Angle",
          min = 0, max = 90,
          value = 0, step = 1
        ),

        selectInput(
          inputId = ns("percent_bar_plot_coord_flip"),
          label = "Coord Flip",
          choices = c("True" = "T", "False" = "F"),
          selected = "F"
        ),

        selectInput(
          inputId = ns("percent_bar_plot_add_Freq"),
          label = "Add Frequency",
          choices = c("True" = "T", "False" = "F"),
          selected = "T"
        ),

        sliderInput(
          inputId = ns("percent_bar_plot_size_freq"),
          label = "Freq Font Size",
          min = 0, max = 20,
          value = 8, step = 1
        ),

        sliderInput(
          inputId = ns("percent_bar_plot_legend_size_text"),
          label = "Legend Text Size",
          min = 0, max = 40,
          value = 10, step = 1
        ),

        selectInput(
          inputId = ns("percent_bar_plot_add_sum"),
          label = "Add Summary",
          choices = c("True" = "T", "False" = "F"),
          selected = "T"
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
            uiOutput(ns("percent_bar_plot_container"))
          )
        )
      )
    )
  )
}


# ---- Server ----
percent_bar_plotServer <- function(id, external_eset = NULL) {
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
      
      # E. 更新 UI (X 和 Y 通常都是分类变量)
      updatePickerInput(session, "percent_bar_plot_x", choices = non_numeric_cols)
      updatePickerInput(session, "percent_bar_plot_y", choices = non_numeric_cols)
    })

    percent_bar_plot_result <- reactiveVal(NULL)

    # 3. 运行分析
    observeEvent(input$run_percent_bar_plot, {
      req(input_data())
      
      # 校验
      if (is.null(input$percent_bar_plot_x) || input$percent_bar_plot_x == "") {
        showNotification("Please select X variable.", type = "error"); return(NULL)
      }
      if (is.null(input$percent_bar_plot_y) || input$percent_bar_plot_y == "") {
        showNotification("Please select Y variable.", type = "error"); return(NULL)
      }

      withProgress(message = "Running percent_bar_plot analysis...", value = 0, {
        setProgress(0.3, message = "Reading input data...")

        data <- input_data()
        req(nrow(data) > 0)

        # 再次确保列名一致
        colnames(data) <- gsub("\\.", "-", colnames(data))

        # 颜色
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

        setProgress(0.6, message = "Plotting...")

        pp <- tryCatch({
          percent_bar_plot(
            input            = data,
            x                = input$percent_bar_plot_x, # Picker值
            y                = input$percent_bar_plot_y, # Picker值
            title            = if (input$percent_bar_plot_title %in% c("title(optional)", "")) NULL else input$percent_bar_plot_title,
            palette          = if (input$percent_bar_plot_palette == "") NULL else input$percent_bar_plot_palette,
            color = cols_vec,
            axis_angle       = input$percent_bar_plot_axis_angle,
            coord_flip       = input$percent_bar_plot_coord_flip == "T",
            add_Freq         = input$percent_bar_plot_add_Freq == "T",
            size_freq        = input$percent_bar_plot_size_freq,
            legend.size.text = input$percent_bar_plot_legend_size_text,
            add_sum          = input$percent_bar_plot_add_sum == "T",
            print_result     = TRUE,
            round.num        = 2
          )
        }, error = function(e) {
          setProgress(1, message = "Error")
          showNotification(
            paste("Error during percent_bar_plot():", e$message),
            type = "error",
            duration = 8
          )
          return(NULL)
        })
        
        req(pp)

        percent_bar_plot_result(pp)
        setProgress(1, message = "Plot ready.")
      })
    })

    dims <- plotDownloadServer(
      "plot_download",
      plot_reactive = percent_bar_plot_result,
      filename_prefix = "percent_bar_plot"
    )

    output$percent_bar_plot_output <- renderPlot({
      req(percent_bar_plot_result())
      percent_bar_plot_result()
    })

    # 3. 动态渲染 UI 容器
    output$percent_bar_plot_container <- renderUI({
      req(percent_bar_plot_result())
      
      div(style = "overflow: auto;",
          plotOutput(session$ns("percent_bar_plot_output"), 
                     width = paste0(dims$width(), "px"), 
                     height = paste0(dims$height(), "px"))
      )
    })

    return(percent_bar_plot_result)
  })
}