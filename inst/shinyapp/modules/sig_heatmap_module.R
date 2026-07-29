# =========================================================
# Module: sig_heatmap
# =========================================================

# ---- Full UI ----
sig_heatmapUI <- function(id) {
  ns <- NS(id)
  bs4TabItem(
    tabName = "sig_heatmap",
    h3(HTML(
      "Sig_heatmap
      <span style='font-size:80%; color:#333;'>:
       Draws customizable heatmaps grouped by annotations or conditions.</span>"
      )),
    sig_heatmapBodyUI(id)
  )
}


# ---- BodyUI ----
sig_heatmapBodyUI <- function(id, include_upload = TRUE, show_top_n = FALSE) {
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
          style = "text-align:center; margin-top:10px;",
          actionButton(ns("run_sig_heatmap"), "Run Analysis", class = "btn-primary")
        ),

        # 2. Group 选择 (单选)
        pickerInput(
            inputId = ns("sig_heatmap_group"),
            label = "Groups",
            choices = NULL, # 初始化为空，Server 端填充
            multiple = FALSE,
            options = pickerOptions(
              liveSearch = TRUE,  # 允许搜索列名
              size = 10,
              style = "btn-outline-secondary",
              dropupAuto = FALSE,      # 禁止自动向上弹出，强制向下
              container = "body"
            )
        ),
          
        # 3. Feature 选择 (多选，带 Select All)
        pickerInput(
            inputId = ns("sig_heatmap_features"),
            label = "Features",
            choices = NULL, # 初始化为空
            multiple = TRUE,
            options = pickerOptions(
              actionsBox = TRUE,       # 显示 Select All / Deselect All
              liveSearch = TRUE,       # 允许搜索基因名
              size = 10,               # 下拉框显示条数
              selectedTextFormat = "count > 3", # 选多了就显示 "5 items selected"
              style = "btn-outline-secondary",
              dropupAuto = FALSE,      # 禁止自动向上弹出，强制向下
              container = "body",
              title = "Please select multiple..."
            )
        ),

        # 展示top n
        if (isTRUE(show_top_n)) {
          tagList(
            numericInput(
             inputId = ns("surv_top_n"), 
             label = "Auto-select", 
             value = 20, 
             min = 5, 
             max = 100, 
             step = 1,
             width = "300px"
            ),
          br(),
          tags$p(
          style = "color: #555; font-style: italic; font-size: 90%; margin-top: -10px;",
          "The signatures will be auto-selected")
          )
        },  


        selectInput(
          ns("sig_heatmap_scale"),
          label = "Scale",
          choices = c("True" = "T", "False" = "F"),
          selected = "F"
        ),

        selectInput(
          ns("sig_heatmap_palette"),
          label = "Palette",
          choices = c("1", "2", "3", "4", "5", "6"),
          selected = "2"
        ),

        selectInput(
          ns("sig_heatmap_palette_group"),
          label = "Palette Group",
          choices = c(
            "nrc", "jama", "aaas", "jco",
            "paired1", "paired2", "paired3", "paired4",
            "accent", "set2"
          ),
          selected = "jama"
        ),

        textAreaInput(
          inputId = ns("custom_cols"),
          label = "Colors Group",
          value = "",
          placeholder = "e.g., #E64B35, #4DBBD5, #00A087\nSeparate by comma",
          rows = 3,
          resize = "vertical"
        ),
        br(),
          tags$p(
          style = "color: #555; font-style: italic; font-size: 90%; margin-top: -10px;",
          "Input hex codes or color names separated by comma. If filled, 'Palette Group' will be ignored."
          ),

        textAreaInput(
          inputId = ns("custom_heatmap_cols"),
          label = "Colors Heatmap",
          value = "",
          placeholder = "e.g., #E64B35, #4DBBD5, #00A087\nSeparate by comma",
          rows = 3,
          resize = "vertical"
        ),
        br(),
          tags$p(
          style = "color: #555; font-style: italic; font-size: 90%; margin-top: -10px;",
          "Provide at least 3 colors separated by comma. Used for heatmap low-mid-high values."
          ),

        # sliderInput(ns("sig_heatmap_size_col"), "Column Font Size", min = 5, max = 20, value = 10, step = 1),
        # sliderInput(ns("sig_heatmap_angle_col"), "Column Text Angle", min = 0, max = 90, value = 90, step = 5),
        sliderInput(ns("sig_heatmap_size_row"), "Row Font Size", min = 5, max = 20, value = 8, step = 1)
      )
    ),

    column(
      width = 9,
      bs4Card(
        title = "Plot",
        status = "gray",
        headerBorder = TRUE,
        collapsible = TRUE,
        solidHeader = TRUE,
        width = 12,
        maximizable = TRUE,
        tabBox(
            width = 12,
            side = "left",  # 标签显示在左边
            tabPanel(
              "Plot",
              plotDownloadUI(ns("download")),
              uiOutput(ns("sig_heatmap_plot_container"))
            )
          )
      )
    )
  )
}


# ---- Server ----
sig_heatmapServer <- function(id, external_eset = NULL, ordered_ids = reactive(NULL)) {
  moduleServer(id, function(input, output, session) {

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
      non_numeric_cols <- setdiff(non_numeric_cols, "ID")

      pool_numeric <- all_cols[is_num] # 初始数字池
      blacklist_pattern <- "(^|_)(time|status|os|event|censored|days|months|years|fustat|futime|rfs|pfs|dfs)(_|$)"
      is_clinical <- grepl(blacklist_pattern, pool_numeric, ignore.case = TRUE)
      numeric_cols <- pool_numeric[!is_clinical]

      updatePickerInput(
        session = session,
        inputId = "sig_heatmap_features",
        label   = "Select Features", 
        choices = numeric_cols,     
        selected = character(0)
      )
      
      updatePickerInput(
        session = session,
        inputId = "sig_heatmap_group",
        choices = non_numeric_cols, 
        selected = character(0)
      )
    })

    observe({
      req(ordered_ids()) # 等待上游传数据
      
      # 1. 获取所有排好序的 ID
      all_ids <- ordered_ids()
      
      # 2. 获取用户想看多少个 (如果没有输入，默认 20)
      n <- if (!is.null(input$surv_top_n)) input$surv_top_n else 20
      
      # 3. 截取 Top N
      top_n_ids <- head(all_ids, n)
      
      # 4. 检查这些 ID 是否在当前表达矩阵里 (防止报错)
      valid_ids <- top_n_ids[top_n_ids %in% gsub("\\.", "-", colnames(input_data()))]
      
      # 5. 更新下拉框
      if(length(valid_ids) > 0) {
        updatePickerInput(
          session = session,
          inputId = "sig_heatmap_features",
          selected = valid_ids
        )
      }
    })

    sig_heatmap_plot <- reactiveVal(NULL)

    observeEvent(input$run_sig_heatmap, {
      req(input_data())
      
      if (is.null(input$sig_heatmap_group) || input$sig_heatmap_group == "") {
        showNotification("Please select a Group column.", type = "error")
        return(NULL)
      }
      
      if (is.null(input$sig_heatmap_features) || length(input$sig_heatmap_features) == 0) {
        showNotification("Please select at least one Signature.", type = "error")
        return(NULL)
      }

      withProgress(message = "Generating heatmap...", value = 0, {
        setProgress(0.2, message = "Parsing input...")

        data <- input_data()
        req(nrow(data) > 0)

        colnames(data) <- gsub("\\.", "-", colnames(data))
        features <- input$sig_heatmap_features
        group_col <- input$sig_heatmap_group

        valid_features <- features[features %in% colnames(data)]
        
        if (length(valid_features) == 0) {
          showNotification("Selected features not found in data.", type = "error")
          return(NULL)
        }
        
        if (!(group_col %in% colnames(data))) {
          showNotification("Selected Group column not found in data.", type = "error")
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

        heatmap_cols_vec <- NULL
        raw_heatmap_cols_text <- input$custom_heatmap_cols
        if (!is.null(raw_heatmap_cols_text) && trimws(raw_heatmap_cols_text) != "") {
          split_heatmap_cols <- unlist(strsplit(raw_heatmap_cols_text, "[,;\n]"))
          split_heatmap_cols <- trimws(split_heatmap_cols)
          heatmap_cols_vec <- split_heatmap_cols[split_heatmap_cols != ""]

          if (length(heatmap_cols_vec) == 0) heatmap_cols_vec <- NULL
  
          if (!is.null(heatmap_cols_vec) && length(heatmap_cols_vec) < 3) {
            showNotification("Heatmap colors require at least 3 colors.", type = "error")
            return(NULL)
          }
        }

        setProgress(0.5, message = "Running sig_heatmap...")

        pp <- tryCatch({
          sig_heatmap(
            input             = data,
            id                = "ID",         
            features          = valid_features,
            group             = group_col,      
            condition        = NULL,
            id_condition     = "vars",
            col_condition    = "condiction",
            cols_condition   = NULL,
            scale             = (input$sig_heatmap_scale == "T"),
            palette           = as.numeric(input$sig_heatmap_palette),
            cols_heatmap      = heatmap_cols_vec,
            palette_group     = input$sig_heatmap_palette_group,
            show_col          = FALSE,
            show_palettes     = FALSE,
            cols_group        = cols_vec,
            show_plot         = FALSE,
            width             = 8,
            height            = 6,
            # size_col          = input$sig_heatmap_size_col,
            size_row          = input$sig_heatmap_size_row,
            # angle_col         = input$sig_heatmap_angle_col,
            column_title      = NULL,
            row_title         = NULL,
            show_heatmap_col_name = FALSE,
            path              = NULL,
            index             = 1
          )
        }, error = function(e) {
          setProgress(1, message = "Error")
          showNotification(
            paste("Error during sig_heatmap():", e$message),
            type = "error",
            duration = 8
          )
          return(NULL)
        })
        
        req(pp)

        sig_heatmap_plot(pp)
        setProgress(1, message = "Heatmap generation complete.")
      })
    })

    # 负责画图
    output$sig_heatmap_plot <- renderPlot({
      req(sig_heatmap_plot())
      sig_heatmap_plot()
    })

    # 负责调整大小
    dims <- plotDownloadServer("download", 
      plot_reactive = sig_heatmap_plot, 
      filename_prefix = "sig_heatmap")

    output$sig_heatmap_plot_container <- renderUI({
      req(sig_heatmap_plot())

      # overflow: auto;是出现滚动条
      div(style = "overflow: auto;",
          plotOutput(session$ns("sig_heatmap_plot"), 
                     width = paste0(dims$width(), "px"), 
                     height = paste0(dims$height(), "px"))
      )
    })

    return(sig_heatmap_plot)
  })
}