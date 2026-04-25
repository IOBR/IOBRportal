# =========================================================
# Module: prepare_immunotherapy_data
# Description: Unified module for fetching TCGA Signature or TME data
# =========================================================

prepare_immunotherapy_dataUI <- function(id) {
  ns <- NS(id)
  bs4TabItem(
    tabName = "prepare_immunotherapy_data",
    prepare_immunotherapy_dataBodyUI(id)
  )
}

prepare_immunotherapy_dataBodyUI <- function(id) {
  ns <- NS(id)
  
  fluidRow(
    # --- 左侧：筛选和参数 ---
    column(
      width = 3,
      
      # --- Card 1: 筛选控制器 ---
      bs4Card(
        title = "Choose Data Source",
        status = "gray",
        headerBorder = TRUE,
        collapsible = TRUE,
        solidHeader = TRUE,
        width = 12,
        maximizable = TRUE,
        
        # 1. 癌症类型
        pickerInput(
          inputId = ns("sel_cancer"),
          label   = "Cancer Type",
          choices = NULL,
          multiple = FALSE,  #改成单选
          options = pickerOptions(
            liveSearch = TRUE,
            actionsBox = TRUE, 
            size = 10,
            style = "btn-outline-secondary",
            dropupAuto = FALSE,
            container = "body"
          )
        ),
        
        # 2. 治疗方案 (多选)
        pickerInput(
          inputId = ns("sel_treatment"),
          label   = "Treatment",
          choices = NULL,
          multiple = TRUE, 
          options = pickerOptions(
            liveSearch = TRUE,
            actionsBox = TRUE,
            size = 10,
            style = "btn-outline-secondary",
            dropupAuto = FALSE,
            container = "body"
          )
        ),
        
        # 3. 药物 (多选)
        pickerInput(
          inputId = ns("sel_drug"),
          label   = "Drug",
          choices = NULL,
          multiple = TRUE, 
          options = pickerOptions(
            liveSearch = TRUE,
            actionsBox = TRUE,
            size = 10,
            style = "btn-outline-secondary",
            dropupAuto = FALSE,
            container = "body"
          )
        ),
        
        # 4. 采样时间 (多选)
        pickerInput(
          inputId = ns("sel_time"),
          label   = "Timepoint",
          choices = NULL,
          multiple = TRUE, 
          options = pickerOptions(
            liveSearch = TRUE,
            actionsBox = TRUE,
            size = 10,
            style = "btn-outline-secondary",
            dropupAuto = FALSE,
            container = "body"
          )
        ),

        pickerInput(
          inputId = ns("sel_datasets"),
          label   = "Confirm Datasets",
          choices = NULL,
          multiple = TRUE,
          options = pickerOptions(
            liveSearch = TRUE, 
            actionsBox = TRUE, 
            selectedTextFormat = "count > 3", # 选中多项时显示数量
            countSelectedText = "{0} datasets selected",
            size = 10, 
            style = "btn-outline-secondary",
            dropupAuto = FALSE, 
            container = "body"
          )
        )
      ),
      
      # --- Card 2: 方法参数 ---
      bs4Card(
        title = "Method Parameters",
        status = "gray",
        headerBorder = TRUE,
        collapsible = TRUE,
        solidHeader = TRUE,
        width = 12,
        maximizable = TRUE,

        # 分析模式
        selectInput(
          inputId = ns("data_type"),
          label   = "Analysis Mode",
          choices = c("Calculate Sigscore" = "sig", "Deconvolute TME" = "tme"),
          selected = "sig"
        ),

        # Signature 参数
        conditionalPanel(
          condition = paste0("input['", ns("data_type"), "'] == 'sig'"),
          selectInput(
            inputId = ns("calculate_sig_score_method"),
            label   = "Method",
            choices = c("PCA" = "PCA", "ssGSEA" = "ssgsea", "Z-score" = "zscore"),
            selected = "PCA"
          )
        ),
        
        # TME 参数
        conditionalPanel(
          condition = paste0("input['", ns("data_type"), "'] == 'tme'"),
          selectInput(
            inputId = ns("deconvo_tme_method"),
            label   = "Method",
            choices = c(
              "CIBERSORT"      = "CIBERSORT",
              "CIBERSORT(abs)" = "CIBERSORT-ABS",
              "EPIC"           = "EPIC",
              "quanTIseq"      = "quantiseq",
              "ESTIMATE"       = "estimate",
              "TIMER"          = "TIMER",
              "MCPcounter"     = "MCPcounter",
              "IPS"            = "IPS",
              "xCell"          = "xcell",
              "Integration"    = "integration" 
            ),
            selected = "CIBERSORT"
          )
        )
      ),

        div(
          style = "text-align: center;",
          actionButton(
            inputId = ns("submit"),
            label   = "Submit",
            icon    = icon("paper-plane"),
            class   = "btn-primary",
            style   = "width: 180px;"
          )
        )     
    ),
    
    # --- 右侧：只展示最终结果 (删除了 Meta Tab) ---
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
          side  = "left",
          tabPanel(
            title = "Data", 
            dataTableUI(ns("expr_data"))
          )
        )
      )
    )
  )
}

# =========================================================
# Server Section
# =========================================================
prepare_immunotherapy_dataServer <- function(id, pool) {
  moduleServer(id, function(input, output, session) {
    
    # 结果容器
    data_results <- reactiveVal(NULL)
    
    # 1. 读取 Meta 数据
    meta_file <- "data/cohorts_immunotherpy.xlsx"
    
    meta_df <- reactive({
      req(file.exists(meta_file))
      df <- readxl::read_excel(meta_file, col_types = "text")
      df %>% dplyr::mutate(across(everything(), ~trimws(.))) 
    })
    
    # =========================================================
    # 2. 级联筛选逻辑
    # =========================================================
    
    # 2.1 初始化 Cancer
    observe({
      df <- meta_df()
      req(df)
      all_cancers <- sort(unique(df$cancer_type))
      if ("Pan-Cancer" %in% all_cancers) {
        others <- setdiff(all_cancers, "Pan-Cancer")
        all_cancers <- c(others, "Pan-Cancer")
      }
      updatePickerInput(session, "sel_cancer", choices = all_cancers, selected = character(0))
    })
    
    # 2.2 Cancer -> Treatment
    observeEvent(input$sel_cancer, ignoreNULL = FALSE, {
      df <- meta_df()
      req(df)
      if (is.null(input$sel_cancer)) {
        updatePickerInput(session, "sel_treatment", choices = character(0), selected = character(0))
      } else {
        treats <- df %>% dplyr::filter(cancer_type %in% input$sel_cancer) %>% dplyr::pull(treatment) %>% unique() %>% sort()
        sel_val <- if(length(treats) == 1) treats else character(0)
        updatePickerInput(session, "sel_treatment", choices = treats, selected = sel_val)
      }
    })
    
    # 2.3 Treatment -> Drug
    observeEvent(c(input$sel_cancer, input$sel_treatment), ignoreNULL = FALSE, {
      df <- meta_df()
      req(df)
      if (is.null(input$sel_cancer)) {
        updatePickerInput(session, "sel_drug", choices = character(0), selected = character(0))
      } else {
        current_data <- df %>% dplyr::filter(cancer_type %in% input$sel_cancer)
        if (!is.null(input$sel_treatment)) current_data <- current_data %>% dplyr::filter(treatment %in% input$sel_treatment)
        drugs <- current_data %>% dplyr::pull(drug) %>% unique() %>% sort()
        sel_val <- if(length(drugs) == 1) drugs else character(0)
        updatePickerInput(session, "sel_drug", choices = drugs, selected = sel_val)
      }
    })
    
    # 2.4 Drug -> Timepoint
    observeEvent(c(input$sel_cancer, input$sel_treatment, input$sel_drug), ignoreNULL = FALSE, {
      df <- meta_df()
      req(df)
      if (is.null(input$sel_cancer)) {
        updatePickerInput(session, "sel_time", choices = character(0), selected = character(0))
      } else {
        current_data <- df %>% dplyr::filter(cancer_type %in% input$sel_cancer)
        if (!is.null(input$sel_treatment)) current_data <- current_data %>% dplyr::filter(treatment %in% input$sel_treatment)
        if (!is.null(input$sel_drug))      current_data <- current_data %>% dplyr::filter(drug %in% input$sel_drug)
        times <- current_data %>% dplyr::pull(time) %>% unique() %>% sort()
        sel_val <- if(length(times) == 1) times else character(0)
        updatePickerInput(session, "sel_time", choices = times, selected = sel_val)
      }
    })
    
    # 2.5
    observeEvent(c(input$sel_cancer, input$sel_treatment, input$sel_drug, input$sel_time), ignoreNULL = FALSE, {
      df <- meta_df()
      req(df)
      
      # 如果没选癌症，清空列表
      if (is.null(input$sel_cancer)) {
        updatePickerInput(session, "sel_datasets", choices = character(0), selected = character(0))
        return()
      }
      
      # --- 级联筛选 ---
      # 基于所有 4 个条件进行严格过滤
      filtered_data <- df %>% dplyr::filter(cancer_type %in% input$sel_cancer)
      
      if (!is.null(input$sel_treatment)) filtered_data <- filtered_data %>% dplyr::filter(treatment %in% input$sel_treatment)
      if (!is.null(input$sel_drug))      filtered_data <- filtered_data %>% dplyr::filter(drug %in% input$sel_drug)
      if (!is.null(input$sel_time))      filtered_data <- filtered_data %>% dplyr::filter(time %in% input$sel_time)
      
      matched_ids <- sort(unique(filtered_data$id))
      
      # --- 更新 UI ---
      # 【核心修改】：choices 和 selected 完全一致
      # 这样列表里只会显示符合条件的数据集，且默认全选。用户依然可以手动取消勾选。
      updatePickerInput(
        session, 
        "sel_datasets", 
        choices  = matched_ids, 
        selected = matched_ids
      )
    })

    # =========================================================
    # 3. 提交逻辑
    # =========================================================
    observeEvent(input$submit, {
      req(input$data_type)
      
      # 获取选中的数据集ID
      target_ids <- input$sel_datasets
      
      # 防御性检查
      if (is.null(target_ids) || length(target_ids) == 0) {
        shinyWidgets::sendSweetAlert(
          session = session, 
          title = "Warning", 
          text = "Please confirm at least one dataset in Box 5.", 
          type = "warning"
        )
        return()
      }
      
      # 重置结果
      data_results(NULL)
      
      withProgress(message = "Processing...", value = 0, {
        
        tryCatch({
          
          if (!dbIsValid(pool)) stop("Database connection lost.")
          
          tbl_ref <- tbl(pool, "cohort_data")
          all_columns <- colnames(tbl_ref)
          subset_data <- NULL
          
          # ---------------------------------------------------------
          # Branch A: Signature
          # ---------------------------------------------------------
          if (input$data_type == "sig") {
            req(input$calculate_sig_score_method)
            setProgress(0.2, message = "Inspecting Signature columns...")
            
            # 按计算方法后缀匹配
            target_suffix <- input$calculate_sig_score_method 
            
            data_cols <- setdiff(all_columns, c("Dataset", "ID"))
            target_cols <- grep(target_suffix, data_cols, value = TRUE, ignore.case = TRUE)
            
            if (length(target_cols) == 0) stop("No columns found matching suffix.")
            
            setProgress(0.4, message = "Querying Database...")
            cols_to_select <- c("Dataset", "ID", target_cols)
            
            subset_data <- tbl_ref %>% 
              dplyr::filter(Dataset %in% target_ids) %>% 
              dplyr::select(dplyr::all_of(cols_to_select)) %>% 
              collect()
            
            if (nrow(subset_data) == 0) stop("No data found for selected filters.")
            
            clean_pattern <- paste0("_", target_suffix, "$") 
            colnames(subset_data) <- gsub(clean_pattern, "", colnames(subset_data), ignore.case = TRUE)
            
          # ---------------------------------------------------------
          # Branch B: TME
          # ---------------------------------------------------------
          } else {
            req(input$deconvo_tme_method)
            setProgress(0.2, message = "Inspecting TME columns...")
            
            target_method <- input$deconvo_tme_method
            if (target_method == "integration") {
              target_pattern <- "CIBERSORT|EPIC|quantiseq|xCell|estimate|TIMER|MCPcounter|IPS"
            } else {
              target_pattern <- target_method
            }
            
            data_cols <- setdiff(all_columns, c("Dataset", "ID"))
            target_cols <- grep(target_pattern, data_cols, value = TRUE, ignore.case = TRUE)
            
            if (target_method == "CIBERSORT") {
              abs_cols <- grep("ABS", target_cols, value = TRUE, ignore.case = TRUE)
              target_cols <- setdiff(target_cols, abs_cols)
            }
            
            if (length(target_cols) == 0) stop("No columns found matching method.")
            
            setProgress(0.4, message = "Querying Database...")
            cols_to_select <- c("Dataset", "ID", target_cols)
            
            subset_data <- tbl_ref %>% 
              dplyr::filter(Dataset %in% target_ids) %>% 
              dplyr::select(dplyr::all_of(cols_to_select)) %>% 
              collect()
            
            if (nrow(subset_data) == 0) stop("No data found for selected filters.")
          }
          
          # ---------------------------------------------------------
          # Common: Data Cleaning
          # ---------------------------------------------------------
          is_informative <- function(x) {
            # 1. 剔除全是 NA 的列
            if (all(is.na(x))) return(FALSE)
            # 2. 剔除内容完全一样的列 (常量列)
            if (length(unique(stats::na.omit(x))) <= 1) return(FALSE)
            return(TRUE)
          }
            
          # 应用清洗
          subset_data <- subset_data %>% 
            dplyr::select(where(is_informative))
          
          # ---------------------------------------------------------
          # Finalize & Output (Inside tryCatch)
          # ---------------------------------------------------------
          setProgress(0.9, message = "Finalizing...")
          
          # 成功弹窗 (在 tryCatch 内部)
          shinyWidgets::sendSweetAlert(
            session = session, 
            title = "Success!", 
            text = paste0("Loaded ", nrow(subset_data), " samples."), 
            type = "success"
          )
          
          # 更新结果 (在 tryCatch 内部)
          attr(subset_data, "data_type") <- input$data_type
          data_results(subset_data)
          
        }, error = function(e) {
          # 错误处理
          shinyWidgets::sendSweetAlert(
            session = session, 
            title = "Error", 
            text = paste("Failed:", e$message), 
            type = "error"
          )
          return(NULL)
        })
      }) 
    })

    # 4. Outputs
    output_expr_data <- reactive({ 
      req(data_results())
      data_results() 
    })
    dataTableServer("expr_data", output_expr_data)
    
    return(data_results)
  })
}
