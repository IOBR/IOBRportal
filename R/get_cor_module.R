# =========================================================
# Module: get_cor
# =========================================================

# ---- Full UI ----
get_corUI <- function(id) {
  ns <- NS(id)
  bs4TabItem(
    tabName = "get_cor",
    h3(HTML(
      "Get_cor
      <span style='font-size:80%; color:#333;'>:
       Computes and plots correlation between two variables with regression options.</span>"
      )),
    get_corBodyUI(id)
  )
}

# ---- Body UI ----
get_corBodyUI <- function(id, include_upload = TRUE) {
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
          style = "text-align: center; margin-top: 0px; margin-bottom: 0px;",
          actionButton(
            inputId = ns("run_get_cor"),
            label = "Run Analysis",
            class = "btn-primary"
          )
        ),

        selectInput(
          inputId = ns("get_cor_is_matrix"),
          label = "Matrix",
          choices = c("True" = "T", "False" = "F"),
          selected = "F"
        ),

        # pickerInput(
        #   inputId = ns("get_cor_var1"),
        #   label = "Variable 1",
        #   choices = NULL,       
        #   multiple = FALSE,     
        #   options = pickerOptions(
        #     liveSearch = TRUE,
        #     size = 10,
        #     style = "btn-outline-secondary", 
        #     dropupAuto = FALSE,
        #     container = "body"
        #   )
        # ),

        # pickerInput(
        #   inputId = ns("get_cor_var2"),
        #   label = "Variable 2",
        #   choices = NULL,       
        #   multiple = FALSE, 
        #   options = pickerOptions(
        #     liveSearch = TRUE,
        #     size = 10,
        #     style = "btn-outline-secondary",
        #     dropupAuto = FALSE,
        #     container = "body"
        #   )
        # ),

        selectizeInput(
          inputId = ns("get_cor_var1"),
          label = "Variable 1",
          choices = NULL, 
          multiple = FALSE,
          options = list(placeholder = "Select variable...")
        ),

        selectizeInput(
          inputId = ns("get_cor_var2"),
          label = "Variable 2",
          choices = NULL, 
          multiple = FALSE,
          options = list(placeholder = "Select variable...")
        ),

                pickerInput(
          inputId = ns("get_cor_subtype"),
          label = "Subtype",
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
          inputId = ns("get_cor_scale"),
          label = "Scale",
          choices = c("True" = "T", "False" = "F"),
          selected = "F"
        ),

        selectInput(
          inputId = ns("get_cor_method"),
          label = "Method",
          choices = c("Pearson" = "pearson", "Spearman" = "spearman", "Kendall" = "kendall"),
          selected = "spearman"
        ),

        selectInput(
          inputId = ns("get_cor_show_result"),
          label = "Show Result",
          choices = c("True" = "T", "False" = "F"),
          selected = "T"
        ),

        textInput(
          inputId = ns("get_cor_col_line"),
          label = "Color Line(optional)",
          value = "",
          placeholder = "e.g., black/#E69F00"
        ),

        textAreaInput(
          inputId = ns("custom_cols"),
          label = "Subtype Colors",
          value = "",
          placeholder = "e.g., #E64B35, #4DBBD5, #00A087\nSeparate by comma",
          rows = 3,
          resize = "vertical"
        ),
        br(),
          tags$p(
          style = "color: #555; font-style: italic; font-size: 90%; margin-top: -10px;",
          "Input hex codes or color names separated by comma. If you have group like subtype."
          ),

        textInput(
          inputId = ns("get_cor_title"),
          label = "Title(optional)",
          value = "",
          placeholder = "e.g., correlation"
        ),

        sliderInput(
          inputId = ns("get_cor_title_size"),
          label = "Title Size(optional)",
          min = 0.5, max = 5, value = 1.5, step = 0.1
        ),

        sliderInput(
          inputId = ns("get_cor_point_size"),
          label = "Point Size",
          min = 0, max = 10, value = 4, step = 0.1
        ),

        sliderInput(
          inputId = ns("get_cor_alpha"),
          label = "Point Transparency",
          min = 0, max = 1, value = 0.5, step = 0.05
        ),

        sliderInput(
          inputId = ns("get_cor_text_size"),
          label = "Text Size",
          min = 6, max = 30, value = 10, step = 1
        ),

        sliderInput(
          inputId = ns("get_cor_axis_angle"),
          label = "Axis Label Angle",
          min = 0, max = 90, value = 0, step = 5
        ),

        sliderInput(
          inputId = ns("get_cor_hjust"),
          label = "Hjust",
          min = 0, max = 1, value = 0, step = 0.1
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
            uiOutput(ns("get_cor_plot_container"))
          )
        )
      )
    )
  )
}

# ---- Server ----
get_corServer <- function(id, external_eset = NULL, target_cols = NULL) {
  moduleServer(id, function(input, output, session) {

    input_data <- if (!is.null(external_eset)) {
      external_eset
    } else {
      uploadServer("upload")
    }

    # observeEvent(input_data(), {
    #   req(input_data())
    #   data <- input_data()

    #   all_cols <- gsub("\\.", "-", colnames(data))
    #   is_num <- sapply(data, is.numeric)
    #   non_numeric_cols <- all_cols[!is_num]
    #   non_numeric_cols <- setdiff(non_numeric_cols, "ID") #只有字符，删去ID列

    #   pool_numeric <- all_cols[is_num] # 初始数字池
    #   blacklist_pattern <- "time|status|os|id"
    #   is_clinical <- grepl(blacklist_pattern, pool_numeric, ignore.case = TRUE)
    #   numeric_cols <- pool_numeric[!is_clinical]
      
    #   updatePickerInput(
    #     session = session,
    #     inputId = "get_cor_var1",
    #     choices = numeric_cols,
    #     selected = character(0) 
    #   )
      
    #   updatePickerInput(
    #     session = session,
    #     inputId = "get_cor_var2",
    #     choices = numeric_cols,
    #     selected = character(0)
    #   )

    #   updatePickerInput(
    #     session = session,
    #     inputId = "get_cor_subtype",
    #     choices = non_numeric_cols,
    #     selected = character(0)
    #   )
    # })

    # --- 块 1：通用初始化 (负责 Var1, Var2 和 Subtype) ---
    observeEvent(input_data(), {
      req(input_data())
      data <- as.data.frame(input_data(), check.names = FALSE)
      all_cols <- gsub("\\.", "-", colnames(data))
      
      # 区分数值列(用于Var1/2) 和 字符列(用于Subtype)
      is_num <- unlist(sapply(data, is.numeric))
      numeric_cols <- all_cols[is_num]
      numeric_cols <- numeric_cols[!grepl("(^|_)(time|status|os|event|censored|days|months|years|fustat|futime|rfs|pfs|dfs)(_|$)", numeric_cols, ignore.case = TRUE)]
      
      non_numeric_cols <- all_cols[!is_num]
      non_numeric_cols <- setdiff(non_numeric_cols, "ID")

      # 初始化 Var1 & Var2 (默认全量数值列)
      updateSelectizeInput(session, "get_cor_var1", choices = numeric_cols, selected = character(0), server = TRUE)
      updateSelectizeInput(session, "get_cor_var2", choices = numeric_cols, selected = character(0), server = TRUE)
      
      # 初始化 Subtype (保持 pickerInput)
      updatePickerInput(session, "get_cor_subtype", choices = non_numeric_cols, selected = character(0))
    })

    # --- 块 2：Workflow 模式限定 Var1 (Target) ---
    observeEvent(input_data(), {
      req(input_data(), isTRUE(target_cols))
      
      data <- as.data.frame(input_data(), check.names = FALSE)
      all_numeric_cols <- gsub("\\.", "-", colnames(data))
      
      # 锁定 Var1 只能选 Signature
      sig_pool <- intersect(all_numeric_cols, names(signature_collection))
      sig_pool <- unique(c(sig_pool, "TMEscore_plus", "TMEscore_CIR"))
      
      updateSelectizeInput(
        session, 
        "get_cor_var1", 
        choices = sig_pool, 
        selected = sig_pool[1], 
        server = TRUE
      )
    })

    # --- 块 3：联动 Var2 (Feature) ---
    observeEvent(input$get_cor_var1, {
      req(isTRUE(target_cols), input$get_cor_var1)
      
      sig_name <- input$get_cor_var1
      sig_pool <- names(signature_collection)
      
      # 只有当 Var1 是 Signature 时才执行联动
      if (sig_name %in% sig_pool) {
        
        # 1. 准备“纯基因池” (Var2 的选项)
        data <- as.data.frame(input_data(), check.names = FALSE)
        all_numeric_cols <- gsub("\\.", "-", colnames(data))
        is_num <- unlist(sapply(data, is.numeric))
        numeric_cols_only <- all_numeric_cols[is_num]
        blacklist <- "(^|_)(time|status|os|event|censored|days|months|years|fustat|futime|rfs|pfs|dfs|TMEscore_plus|TMEscore_CIR)(_|$)"
        clean_numeric_cols <- numeric_cols_only[!grepl(paste(blacklist, collapse="|"), numeric_cols_only, ignore.case = TRUE)]
        gene_pool <- setdiff(clean_numeric_cols, sig_pool)
        
        # 2. 匹配基因 (找该 Signature 下的第一个基因作为默认值)
        target_genes <- signature_collection[[sig_name]]
        valid_genes <- intersect(target_genes, gene_pool)
        
        # 默认选中第一个匹配到的基因 (防止空值)
        default_gene <- if(length(valid_genes) > 0) valid_genes[1] else gene_pool[1]
        
        # 3. 更新 Var2
        updateSelectizeInput(
          session, 
          "get_cor_var2", 
          choices = gene_pool,     # 选项里只留基因
          selected = default_gene, # 自动选中第一个相关基因
          server = TRUE
        )
      }
    })

    get_cor_result <- reactiveVal(NULL)

    observeEvent(input$run_get_cor, {
      req(input_data())
      
      if (is.null(input$get_cor_var1) || input$get_cor_var1 == "") {
        showNotification("Please select Var1.", type = "error")
        return(NULL)
      }
      if (is.null(input$get_cor_var2) || input$get_cor_var2 == "") {
        showNotification("Please select Var2.", type = "error")
        return(NULL)
      }

      subtype_val <- input$get_cor_subtype
      if (!is.null(subtype_val) && subtype_val == "") subtype_val <- NULL

      withProgress(message = "Running correlation analysis...", value = 0, {
        setProgress(0.2, "Reading data...")

        data <- input_data()

        colnames(data) <- gsub("\\.", "-", colnames(data))

        if (!all(c(input$get_cor_var1, input$get_cor_var2) %in% colnames(data))) {
          showNotification("Selected variables not found in data.", type = "error")
          return(NULL)
        }

        var1 <- input$get_cor_var1
        var2 <- input$get_cor_var2
        col_line <- if (input$get_cor_col_line == "") NULL else input$get_cor_col_line
        title <- if (input$get_cor_title == "") NULL else input$get_cor_title

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

        setProgress(0.5, "Generating plot...")

        pp <- tryCatch({
          get_cor(
            eset            = data,
            is.matrix       = input$get_cor_is_matrix == "T",
            var1            = var1,
            var2            = var2,
            scale           = input$get_cor_scale == "T",
            subtype = subtype_val,
            na.subtype.rm = TRUE,
            color_subtype = cols_vec,
            method          = input$get_cor_method,
            show_cor_result = input$get_cor_show_result == "T",
            col_line        = col_line,
            title           = title,
            point_size      = input$get_cor_point_size,
            alpha           = input$get_cor_alpha,
            title_size      = input$get_cor_title_size,
            text_size       = input$get_cor_text_size,
            axis_angle      = input$get_cor_axis_angle,
            hjust           = input$get_cor_hjust,
            path = NULL,
            save_plot = FALSE
          )
        }, error = function(e) {
          setProgress(1, message = "Error")
          showNotification(
            paste("Error during get_cor():", e$message),
            type = "error",
            duration = 8
          )
          return(NULL)
        })
        
        req(pp)

        setProgress(1, "Finished")
        get_cor_result(pp)
      })
    })

    output$get_cor_plot_output <- renderPlot({
      req(get_cor_result())
      get_cor_result()
    })

    dims <- plotDownloadServer(
      id = "plot_download",
      plot_reactive = get_cor_result,
      filename_prefix = "get_cor_plot"
    )

    output$get_cor_plot_container <- renderUI({
      req(get_cor_result())
      
      div(style = "overflow: auto;",
          plotOutput(session$ns("get_cor_plot_output"), 
                     width = paste0(dims$width(), "px"), 
                     height = paste0(dims$height(), "px"))
      )
    })

    return(get_cor_result)
  })
}