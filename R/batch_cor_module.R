# =========================================================
# Module: batch_cor
# =========================================================

# ---- Full UI ----
batch_corUI <- function(id) {
  ns <- NS(id)
  bs4TabItem(
    tabName = "batch_cor",
    h3(HTML(
      "Batch_cor
      <span style='font-size:80%; color:#333;'>:
       Calculates Pearson or Spearman correlations across multiple feature pairs.</span>"
      )),    
    batch_corBodyUI(id)
  )
}

# ---- Body UI ----
batch_corBodyUI <- function(id, include_upload = TRUE) {
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
            inputId = ns("run_batch_cor"),
            label = "Run Analysis",
            class = "btn-primary"
          )
        ),

        # pickerInput(
        #   inputId = ns("batch_cor_target"),
        #   label = "Target",
        #   choices = NULL,       # Server 端填充
        #   multiple = FALSE,     # 单选
        #   options = pickerOptions(
        #     liveSearch = TRUE,
        #     size = 10,
        #     style = "btn-outline-secondary", # 保持统一风格
        #     dropupAuto = FALSE,
        #     container = "body"
        #   )
        # ),

        # pickerInput(
        #   inputId = ns("batch_cor_feature"),
        #   label = "Features",
        #   choices = NULL,       # Server 端填充
        #   multiple = TRUE,      # <--- 关键：批量分析
        #   options = pickerOptions(
        #     actionsBox = TRUE,  # 开启全选/反选
        #     liveSearch = TRUE,
        #     size = 10,
        #     selectedTextFormat = "count > 3",
        #     style = "btn-outline-secondary",
        #     dropupAuto = FALSE,
        #     container = "body",
        #     title = "Please select multiple..."
        #   )
        # ),

        # --- 改动 1: 使用 selectizeInput (Target) ---
        selectizeInput(
          inputId = ns("batch_cor_target"),
          label = "Target",
          choices = NULL, # Server端填充
          multiple = FALSE,
          options = list(placeholder = "Select a signature...")
        ),
        
        # --- 改动 2: 使用 selectizeInput (Features) ---
        selectizeInput(
          inputId = ns("batch_cor_feature"),
          label = "Features",
          choices = NULL, # Server端填充
          multiple = TRUE,
          options = list(
            placeholder = "Type to search or auto-select...",
            plugins = list('remove_button') # 允许删除选中项
          )
        ),

        fluidRow(
          column(
            width = 6,
            actionButton(
              inputId = ns("batch_cor_select_all"),
              label = "Select All",
              class = "btn-outline-primary btn-sm",
              width = "100%"
            )
          ),
          column(
            width = 6,
            actionButton(
              inputId = ns("batch_cor_clear_all"),
              label = "Clear",
              class = "btn-outline-secondary btn-sm",
              width = "100%"
            )
          )
        ),
        br(),
        uiOutput(ns("batch_cor_feature_mode_note")),

        selectInput(
          inputId = ns("batch_cor_method"),
          label = "Method",
          choices = c("Spearman" = "spearman", "Pearson" = "pearson"),
          selected = "spearman"
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
batch_corServer <- function(id, external_eset = NULL, target_cols = NULL) {
  moduleServer(id, function(input, output, session) {

    ns <- session$ns

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
    #   pool_numeric <- all_cols[is_num] # 初始数字池
    #   blacklist_pattern <- "time|status|os"
    #   is_clinical <- grepl(blacklist_pattern, pool_numeric, ignore.case = TRUE)
    #   numeric_cols <- pool_numeric[!is_clinical]
    #   non_numeric_cols <- setdiff(all_cols, numeric_cols)
      
    #   updatePickerInput(
    #     session = session,
    #     inputId = "batch_cor_target",
    #     choices = numeric_cols,
    #     selected = character(0) 
    #   )
      
    #   updatePickerInput(
    #     session = session,
    #     inputId = "batch_cor_feature",
    #     choices = numeric_cols,
    #     selected = numeric_cols
    #   )
    # })

    # observeEvent(input_data(), {
    #   req(input_data())
    #   # 防止列名被自动修改
    #   data <- as.data.frame(input_data(), check.names = FALSE)

    #   all_cols <- gsub("\\.", "-", colnames(data))
    #   is_num <- unlist(sapply(data, is.numeric))
    #   pool_numeric <- all_cols[is_num]
    #   blacklist_pattern <- "time|status|os"
    #   is_clinical <- grepl(blacklist_pattern, pool_numeric, ignore.case = TRUE)
    #   numeric_cols <- pool_numeric[!is_clinical]
      
    #   # 更新选项
    #   updateSelectizeInput(session, "batch_cor_target", choices = numeric_cols, selected = numeric_cols[1], server = TRUE)
    #   updateSelectizeInput(session, "batch_cor_feature", choices = numeric_cols, selected = character(0), server = TRUE)
    # })

    feature_choices_pool <- reactiveVal(character(0))
    use_all_features <- reactiveVal(FALSE)

    output$batch_cor_feature_mode_note <- renderUI({
      if (!isTRUE(use_all_features())) return(NULL)

      tags$p(
        style = "color: #555; font-style: italic; font-size: 90%; margin-top: -10px;",
        tags$span(class = "badge badge-primary", "All mode"),
        "Note: Running all selected features may take a long time."
      )
    })

    observeEvent(input_data(), {
      req(input_data())
      # 防止列名被自动修改
      data <- as.data.frame(input_data(), check.names = FALSE)

      all_cols <- gsub("\\.", "-", colnames(data))
      is_num <- unlist(sapply(data, is.numeric))
      pool_numeric <- all_cols[is_num]
      blacklist_pattern <- "time|status|os|id"
      is_clinical <- grepl(blacklist_pattern, pool_numeric, ignore.case = TRUE)
      numeric_cols <- pool_numeric[!is_clinical]
      
      choices_target <- numeric_cols
      choices_feature <- numeric_cols
      
      feature_choices_pool(numeric_cols) # 缓存当前 Features 候选池
      use_all_features(FALSE)

      # 更新选项
      updateSelectizeInput(session, "batch_cor_target", choices = numeric_cols, selected = numeric_cols[1], server = TRUE)
      updateSelectizeInput(session, "batch_cor_feature", choices = numeric_cols, selected = character(0), server = TRUE)
    })

    # observeEvent(input$batch_cor_target, {
    #   # 1. 开关检查：只有 target_cols = TRUE 时才执行
    #   req(isTRUE(target_cols)) 
    #   req(input$batch_cor_target)
      
    #   sig_name <- input$batch_cor_target
      
    #   # 【安全检查】：确保选中的 Target 确实在 Signature 集合里
    #   if (exists("signature_collection") && sig_name %in% names(signature_collection)) {
        
    #     # 2. 获取基因列表
    #     target_genes <- signature_collection[[sig_name]]
        
    #     # 3. 获取当前数据的列名
    #     data <- as.data.frame(input_data(), check.names = FALSE)
    #     current_cols <- gsub("\\.", "-", colnames(data))
        
    #     # 4. 匹配并勾选
    #     valid_genes <- intersect(target_genes, current_cols)
        
    #     if (length(valid_genes) > 0) {
    #       updateSelectizeInput(session, "batch_cor_feature", selected = valid_genes)
    #     }
    #   }
    # })

    # --- 块 2：针对 Workflow 模式限定 Target 选项 (Observe 1) ---
    observeEvent(input_data(), {
      req(input_data(), isTRUE(target_cols))
      
      data <- as.data.frame(input_data(), check.names = FALSE)
      all_numeric_cols <- gsub("\\.", "-", colnames(data))
      
      # 仅保留数据中存在的 Signature 名字
      sig_pool <- intersect(all_numeric_cols, names(signature_collection))
      
      # 强制将 Target 框锁定为 Signature 列表
      updateSelectizeInput(
        session, 
        "batch_cor_target", 
        choices = sig_pool, 
        selected = sig_pool[1], 
        server = TRUE
      )
    })

    # --- 块 3：处理联动勾选与基因池清洗 (Observe 2) ---
    observeEvent(input$batch_cor_target, {
      req(isTRUE(target_cols), input$batch_cor_target)
      
      sig_name <- input$batch_cor_target
      # sig_pool <- names(signature_collection) 
      sig_pool <- gsub("\\.", "-", names(signature_collection))

      # 只有当选中的确实是 Signature 时才执行
      if (sig_name %in% sig_pool) {
        
        # 1. 准备“纯基因池”：从所有数值列中剔除 Signature 名字
        data <- as.data.frame(input_data(), check.names = FALSE)
        all_numeric_cols <- gsub("\\.", "-", colnames(data))
        is_num <- unlist(sapply(data, is.numeric))
        numeric_cols_only <- all_numeric_cols[is_num]
        blacklist <- c("ID", "time", "status", "os", "TMEscor_CIR", "TMEscore_plus") # (这里用正则过滤更彻底)
        clean_numeric_cols <- numeric_cols_only[!grepl(paste(blacklist, collapse="|"), numeric_cols_only, ignore.case = TRUE)]
        gene_pool <- setdiff(clean_numeric_cols, sig_pool)
        
        feature_choices_pool(gene_pool)   # 当前 Features 候选池改为剔除 signature 后的 gene_pool
        use_all_features(FALSE)           # target切换后，退出全选模式

        # 2. 匹配当前数据中存在的基因 (解决 TAP2 问题)
        target_genes <- signature_collection[[sig_name]]
        valid_genes <- intersect(target_genes, gene_pool)
        
        # 3. 同时更新 Feature 的选项（只留基因）和勾选状态
        updateSelectizeInput(
          session, 
          "batch_cor_feature", 
          choices = gene_pool,    # 下拉框只显示基因，干干净净
          selected = valid_genes,  # 自动点亮匹配到的基因
          server = TRUE
        )
      }
    })

    # --- 全选模式：不把所有项塞进 selectize，只打标记 ---
    observeEvent(input$batch_cor_select_all, {
      pool <- feature_choices_pool()
      req(length(pool) > 0)
      use_all_features(TRUE)

      showNotification(
        paste0("All features will be used at run time (n = ",
               length(setdiff(pool, input$batch_cor_target)), ")."),
        type = "message",
        duration = 3
      )
    })

    # --- 清空：退出全选模式，并清空手动选择 ---
    observeEvent(input$batch_cor_clear_all, {
      use_all_features(FALSE)
      updateSelectizeInput(
        session,
        "batch_cor_feature",
        selected = character(0),
        server = TRUE
      )
    })

    # --- 用户手动改 Features 时，退出全选模式 ---
    observeEvent(input$batch_cor_feature, {
      use_all_features(FALSE)
    }, ignoreInit = TRUE)    

    batch_cor_result <- reactiveVal(NULL)

    observeEvent(input$run_batch_cor, {
      req(input_data())
      
      if (is.null(input$batch_cor_target) || input$batch_cor_target == "") {
        showNotification("Please select a Target variable.", type = "error")
        return(NULL)
      }
      if (!isTRUE(use_all_features()) &&
        (is.null(input$batch_cor_feature) || length(input$batch_cor_feature) == 0)) {
        showNotification("Please select at least one Feature (or click Select All).", type = "error")
        return(NULL)
      }

      withProgress(message = "Running batch correlation analysis...", value = 0, {
        setProgress(0.2, message = "Reading data...")
        data <- input_data()

        req(nrow(data) > 0)

        colnames(data) <- gsub("\\.", "-", colnames(data))  

        setProgress(0.5, message = "Processing correlation analysis...")

        target <- input$batch_cor_target
        method <- input$batch_cor_method

        if (isTRUE(use_all_features())) {
          # 全选模式：运行时使用当前池子，并剔除 target 自己
          feature <- setdiff(feature_choices_pool(), target)
        } else {
          # 手动模式：使用用户在组件中选中的项
          feature <- input$batch_cor_feature
        }

        if (length(feature) == 0) {
          showNotification("No valid features available for correlation.", type = "error")
          return(NULL)
        }

        req_cols <- c(target, feature)

        result <- tryCatch({
          batch_cor(
            data    = data,
            target  = target,
            feature = feature, 
            method  = method
          )
        }, error = function(e) {
          setProgress(1, message = "Error")
          showNotification(
            paste("Error during batch_cor():", e$message),
            type = "error",
            duration = 8
          )
          return(NULL)
        })
        
        req(result)

        batch_cor_result(result)
        setProgress(1, message = "Finished")
      })
    })

    dataTableServer("tbl_data", batch_cor_result)

    dataDownloadServer(
      "download",
      data_reactive = batch_cor_result,
      filename_prefix = "batch_cor_result"
    )

    return(batch_cor_result)
  })
}