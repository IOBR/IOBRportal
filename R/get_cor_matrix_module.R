# =========================================================
# Module: get_cor_matrix
# =========================================================

# ---- Full UI ----
get_cor_matrixUI <- function(id) {
  ns <- NS(id)
  bs4TabItem(
    tabName = "get_cor_matrix",
      h3(HTML(
      "Get_cor_matrix
      <span style='font-size:80%; color:#333;'>:
       Calculates and displays a correlation matrix for variable groups.</span>"
      )),
    get_cor_matrixBodyUI(id)
  )
}


# ---- Body UI ----
get_cor_matrixBodyUI <- function(id, include_upload = TRUE) {
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
            inputId = ns("run_get_cor_matrix"),
            label = "Run Analysis",
            class = "btn-primary"
          )
        ),

        selectInput(
          inputId = ns("get_cor_matrix_is_matrix"),
          label = "Matrix",
          choices = c("True" = "T", "False" = "F"),
          selected = "F"
        ),

        # pickerInput(
        #   inputId = ns("get_cor_matrix_feas1"),
        #   label = "Features Set1",
        #   choices = NULL,       
        #   multiple = TRUE,     
        #   options = pickerOptions(
        #     actionsBox = TRUE,  
        #     liveSearch = TRUE,
        #     size = 10,
        #     selectedTextFormat = "count > 3",
        #     style = "btn-outline-secondary",
        #     dropupAuto = FALSE,
        #     container = "body",
        #     title = "Please select multiple..." 
        #   )
        # ),

        # pickerInput(
        #   inputId = ns("get_cor_matrix_feas2"),
        #   label = "Features Set2",
        #   choices = NULL,       
        #   multiple = TRUE,      
        #   options = pickerOptions(
        #     actionsBox = TRUE,
        #     liveSearch = TRUE,
        #     size = 10,
        #     selectedTextFormat = "count > 3",
        #     style = "btn-outline-secondary",
        #     dropupAuto = FALSE,
        #     container = "body",
        #     title = "Please select multiple..."
        #   )
        # ),

        selectizeInput(
          inputId = ns("get_cor_matrix_feas1"),
          label = "Features Set1",
          choices = NULL, 
          multiple = TRUE, 
          options = list(
            placeholder = "Type to search...",
            plugins = list('remove_button')
          )
        ),

        selectizeInput(
          inputId = ns("get_cor_matrix_feas2"),
          label = "Features Set2",
          choices = NULL, 
          multiple = TRUE,
          options = list(
            placeholder = "Type to search...",
            plugins = list('remove_button')
          )
        ),

        selectInput(
          inputId = ns("get_cor_matrix_scale"),
          label = "Scale",
          choices = c("True" = "T", "False" = "F"),
          selected = "F"
        ),

        selectInput(
          inputId = ns("get_cor_matrix_method"),
          label = "Method",
          choices = c("Pearson" = "pearson", "Spearman" = "spearman", "Kendall" = "kendall"),
          selected = "pearson"
        ),

        sliderInput(
          inputId = ns("get_cor_matrix_font.size.star"),
          label = "font.size.star",
          min = 0, max = 20, value = 8, step = 0.5
        ),

        sliderInput(
          inputId = ns("get_cor_matrix_font.size"),
          label = "font.size",
          min = 0, max = 30, value = 15, step = 1
        ),

        selectInput(
          inputId = ns("get_cor_matrix_fill_by_cor"),
          label = "fill_by_cor",
          choices = c("True" = "T", "False" = "F"),
          selected = "F"
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
            uiOutput(ns("get_cor_matrix_plot_container"))
          )
        )
      )
    )
  )
}


# ---- Server ----
get_cor_matrixServer <- function(id, external_eset = NULL, target_cols = NULL) {
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
    #     inputId = "get_cor_matrix_feas1",
    #     choices = numeric_cols,
    #     selected = character(0) 
    #   )
      
    #   updatePickerInput(
    #     session = session,
    #     inputId = "get_cor_matrix_feas2",
    #     choices = numeric_cols,
    #     selected = character(0)
    #   )
    # })

    # --- 块 1：通用初始化 (所有数值列) ---
    observeEvent(input_data(), {
      req(input_data())
      data <- as.data.frame(input_data(), check.names = FALSE)
      all_cols <- gsub("\\.", "-", colnames(data))
      
      # 提取数值列
      is_num <- unlist(sapply(data, is.numeric))
      numeric_cols <- all_cols[is_num]
      numeric_cols <- numeric_cols[!grepl("time|status|os|id", numeric_cols, ignore.case = TRUE)]

      # 初始化两个框
      updateSelectizeInput(session, "get_cor_matrix_feas1", choices = numeric_cols, selected = character(0), server = TRUE)
      updateSelectizeInput(session, "get_cor_matrix_feas2", choices = numeric_cols, selected = character(0), server = TRUE)
    })

    # --- 块 2：Workflow 模式限定 Set 1 (Target) ---
    observeEvent(input_data(), {
      req(input_data(), isTRUE(target_cols))
      
      data <- as.data.frame(input_data(), check.names = FALSE)
      all_numeric_cols <- gsub("\\.", "-", colnames(data))
      
      # 仅保留 Signature 名字
      sig_pool <- intersect(all_numeric_cols, names(signature_collection))
      
      updateSelectizeInput(
        session, 
        "get_cor_matrix_feas1", 
        choices = sig_pool, 
        selected = sig_pool[1], 
        server = TRUE
      )
    })

    # --- 块 3：联动 Set 2 (自动勾选 Genes) ---
    observeEvent(input$get_cor_matrix_feas1, {
      req(isTRUE(target_cols), input$get_cor_matrix_feas1)
      
      # 获取用户选中的所有 Signatures (这是一个向量)
      selected_sigs <- input$get_cor_matrix_feas1
      sig_pool <- names(signature_collection)
      
      # 检查：只有当选中的全部都是 Signature 时才执行联动
      # (或者只要包含 Signature 就执行，看你需求，这里写的是严格模式)
      if (all(selected_sigs %in% sig_pool)) {
        
        # 1. 准备“纯基因池” (Set 2 的选项)
        data <- as.data.frame(input_data(), check.names = FALSE)
        all_numeric_cols <- gsub("\\.", "-", colnames(data))
        is_num <- unlist(sapply(data, is.numeric))
        numeric_cols_only <- all_numeric_cols[is_num]
        blacklist <- c("ID", "time", "status", "os", "TMEscor_CIR", "TMEscore_plus") # (这里用正则过滤更彻底)
        clean_numeric_cols <- numeric_cols_only[!grepl(paste(blacklist, collapse="|"), numeric_cols_only, ignore.case = TRUE)]
        gene_pool <- setdiff(clean_numeric_cols, sig_pool)

        # 2. 【核心修改】合并所有选中 Signature 的基因
        # signature_collection[selected_sigs] 会返回一个由多个列表组成的列表
        # unlist 把它们铺平，unique 去除重复的基因
        all_target_genes <- unique(unlist(signature_collection[selected_sigs]))
        
        # 3. 匹配数据中存在的基因
        valid_genes <- intersect(all_target_genes, gene_pool)
        
        # 4. 更新 Set 2
        updateSelectizeInput(
          session, 
          "get_cor_matrix_feas2", 
          choices = gene_pool,     
          selected = valid_genes, # 自动勾选所有相关基因
          server = TRUE
        )
      }
    })

    get_cor_matrix_result <- reactiveVal(NULL)

    observeEvent(input$run_get_cor_matrix, {
      req(input_data())
      
      if (is.null(input$get_cor_matrix_feas1) || length(input$get_cor_matrix_feas1) == 0) {
        showNotification("Please select at least one feature for Set 1.", type = "error")
        return(NULL)
      }
      if (is.null(input$get_cor_matrix_feas2) || length(input$get_cor_matrix_feas2) == 0) {
        showNotification("Please select at least one feature for Set 2.", type = "error")
        return(NULL)
      }

      withProgress(message = "Running get_cor_matrix...", value = 0, {
        setProgress(0.2, message = "Reading data...")

        data <- input_data()
        
        colnames(data) <- gsub("\\.", "-", colnames(data))

        feas1 <- input$get_cor_matrix_feas1
        feas2 <- input$get_cor_matrix_feas2
        
        req_cols <- c(feas1, feas2)
        if (!all(req_cols %in% colnames(data))) {
          showNotification("Selected features not found in data.", type = "error")
          return(NULL)
        }

        setProgress(0.5, message = "Plotting...")

        pp <- tryCatch({
          get_cor_matrix(
            data             = data,
            feas1            = feas1, 
            feas2            = feas2, 
            method           = input$get_cor_matrix_method,
            is.matrix        = input$get_cor_matrix_is_matrix == "T",
            scale            = input$get_cor_matrix_scale == "T",
            font.size.star   = input$get_cor_matrix_font.size.star,
            font.size        = input$get_cor_matrix_font.size,
            fill_by_cor      = input$get_cor_matrix_fill_by_cor == "T"
          )
        }, error = function(e) {
          setProgress(1, message = "Error")
          showNotification(
            paste("Error during get_cor_matrix():", e$message),
            type = "error",
            duration = 8
          )
          return(NULL)
        })

        req(pp)

        get_cor_matrix_result(pp)
        setProgress(1, message = "Finished")
      })
    })

    output$get_cor_matrix_plot_output <- renderPlot({
      req(get_cor_matrix_result())
      # 注意：相关性矩阵通常返回 ggplot 或类似的图形对象
      print(get_cor_matrix_result())
    })

    dims <- plotDownloadServer(
      id = "plot_download",
      plot_reactive = get_cor_matrix_result,
      filename_prefix = "get_cor_matrix"
    )

    output$get_cor_matrix_plot_container <- renderUI({
      req(get_cor_matrix_result())
      
      div(style = "overflow: auto;",
          plotOutput(session$ns("get_cor_matrix_plot_output"), 
                     width = paste0(dims$width(), "px"), 
                     height = paste0(dims$height(), "px"))
      )
    })

    return(get_cor_matrix_result)
  })
}
