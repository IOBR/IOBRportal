prepare_cancercohort_dataUI <- function(id) {
  ns <- NS(id)
  bs4TabItem(
    tabName = "prepare_cancercohort_data",
    prepare_cancercohort_dataBodyUI(id)
  )
}


# ---- BodyUI ----
prepare_cancercohort_dataBodyUI <- function(id) {
  ns <- NS(id)
  
  fluidRow(
    column(
      width = 3,
      
      # --- Card 1: 基础数据选择 ---
      bs4Card(
        title = "Choose Data Source",
        status = "gray",
        headerBorder = TRUE,
        collapsible = TRUE,
        solidHeader = TRUE,
        width = 12,
        maximizable = TRUE,
        
        # 癌症类型单选
        pickerInput(
          inputId = ns("sel_cancer"),
          label   = "Cancer Type",
          choices = NULL,
          multiple = FALSE, 
          options = pickerOptions(
            liveSearch = TRUE,
            actionsBox = TRUE, 
            size = 10,
            style = "btn-outline-secondary",
            dropupAuto = FALSE,
            container = "body"
          )
        ),
        
        # 不同的队列，可以多选
        pickerInput(
          inputId = ns("sel_cohort"),
          label   = "Cohorts",
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
        )
      ),
      
      # --- Card 2: 方法参数 (动态变化) ---
      bs4Card(
        title = "Method Parameters",
        status = "gray",
        headerBorder = TRUE,
        collapsible = TRUE,
        solidHeader = TRUE,
        width = 12,
        maximizable = TRUE,
        
        # 模式选择 (Signature 或 TME)
        selectInput(
          inputId = ns("data_type"),
          label   = "Analysis Mode",
          choices = c(
            "Calculate Sigscore"  = "sig",
            "Deconvolute TME"  = "tme"
          ),
          selected = "sig"
        ),

        # === 场景 A: Signature 模式参数 ===
        conditionalPanel(
          condition = paste0("input['", ns("data_type"), "'] == 'sig'"),
          
          selectInput(
            inputId = ns("calculate_sig_score_method"),
            label   = "Scoring Method",
            choices = c("PCA" = "pca", "ssGSEA" = "ssgsea"),
            selected = "pca"
          ),
          
          # PCA 子选项
          conditionalPanel(
            condition = paste0("input['", ns("calculate_sig_score_method"), "'] == 'pca'"),
            selectInput(
              inputId = ns("calculate_sig_score_signature_pca"),
              label   = "Signature Set",
              choices = c(
                "TME"        = "signature_tme",
                "Metabolism" = "signature_metabolism",
                "Collection" = "signature_collection"
              ),
              selected = "signature_tme"
            )
          ),
          
          # ssGSEA 子选项
          conditionalPanel(
            condition = paste0("input['", ns("calculate_sig_score_method"), "'] == 'ssgsea'"),
            selectInput(
              inputId = ns("calculate_sig_score_signature_ssgsea"),
              label   = "Signature Set",
              choices = c(
                "Go_bp"    = "go_bp",
                "Go_cc"    = "go_cc",
                "Go_mf"    = "go_mf",
                "KEGG"     = "kegg",
                "Hallmark" = "hallmark",
                "Reactome" = "reactome"
              ),
              selected = "hallmark"
            )
          )
        ),
        
        # === 场景 B: TME 模式参数 ===
        conditionalPanel(
          condition = paste0("input['", ns("data_type"), "'] == 'tme'"),
          
          selectInput(
            inputId = ns("deconvo_tme_method"),
            label   = "Algorithm",
            choices = c(
              "CIBERSORT"      = "CIBERSORT",
              "CIBERSORT(abs)" = "CIBERSORT-ABS",
              "EPIC"           = "EPIC",
              "quanTIseq"      = "quantiseq",
              "ESTIMATE"       = "estimate",
              "TIMER"          = "TIMER",
              "MCPcounter"     = "MCPcounter",
              "xCell"          = "xcell",
              "Integration"    = "integration" 
            ),
            selected = "CIBERSORT"
          )
        )
      ),
      
      # --- 提交按钮 (公共) ---
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
    
    # --- 右侧结果展示 (公共) ---
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
          tabPanel("Data", dataTableUI(ns("expr_data")))
        )
      )
    )
  )
}


prepare_cancercohort_dataServer <- function(id, pool) {
  moduleServer(id, function(input, output, session) {
    
    # 结果容器
    data_results <- reactiveVal(NULL)
    
    # =========================================================
    # 1. 动态 UI 更新 (利用 DuckDB 索引，速度极快)
    # =========================================================
    
    # 1.1 初始化：获取所有癌种
    observe({
      # 直接查库，因为有索引，瞬间完成
      cancers <- tbl(pool, "cohort_data") %>% 
        distinct(CancerType) %>% 
        pull(CancerType) %>% 
        sort()
      
      updatePickerInput(session, "sel_cancer", choices = cancers)
    })
    
    # 1.2 联动：选了癌种 -> 更新该癌种下的队列
    observeEvent(input$sel_cancer, {
      req(input$sel_cancer)
      
      # 根据癌种筛选队列
      cohorts <- tbl(pool, "cohort_data") %>% 
        filter(CancerType == !!input$sel_cancer) %>% 
        distinct(Dataset) %>% 
        pull(Dataset) %>% 
        sort()
      
      # 默认全选，或者留空让用户选
      updatePickerInput(session, "sel_cohort", choices = cohorts, selected = cohorts[1])
    })
    
    # =========================================================
    # 2. 核心提交逻辑
    # =========================================================
    observeEvent(input$submit, {
      req(input$sel_cancer)
      req(input$sel_cohort) # 必须选队列
      req(input$data_type)
      
      # 重置结果
      data_results(NULL)
      
      withProgress(message = "Processing Query...", value = 0, {
        
        tryCatch({
          
          # 建立数据库引用指针 (不耗内存)
          tbl_ref <- tbl(pool, "cohort_data")
          subset_data <- NULL
          
          # ---------------------------------------------------------
          # Branch A: Signature Scoring (内置列表 + 列名匹配)
          # ---------------------------------------------------------
          if (input$data_type == "sig") {
            
            setProgress(0.2, message = "Parsing Signature selection...")
            
            # 1. 确定要用的 Signature 集合名称
            method <- input$calculate_sig_score_method
            target_sig_name_str <- if (method == "pca") {
              input$calculate_sig_score_signature_pca
            } else {
              input$calculate_sig_score_signature_ssgsea
            }
            req(target_sig_name_str)
            
            # 2. 获取 R 环境中的列表对象 (必须在 global.R 中预加载这些 list!)
            if (!exists(target_sig_name_str)) {
              stop(paste("Signature list object not found in R environment:", target_sig_name_str))
            }
            target_sig_list <- get(target_sig_name_str)
            sig_names_needed <- names(target_sig_list)
            
            # 3. 数据库查询
            setProgress(0.4, message = paste("Querying Signature data for", input$sel_cancer, "..."))
            
            # 构造要提取的列：基础列 + 签名列
            # 使用 any_of 防止某些签名在数据库里还没算(容错)
            subset_data <- tbl_ref %>% 
              filter(CancerType == !!input$sel_cancer,
                     Dataset %in% !!input$sel_cohort) %>% 
              select(CancerType, Dataset, ID, any_of(sig_names_needed)) %>% 
              collect()
            
            if (ncol(subset_data) <= 3) stop("No matching signature columns found in database.")
            
          # ---------------------------------------------------------
          # Branch B: TME Deconvolution (正则匹配列名)
          # ---------------------------------------------------------
          } else { 
            
            req(input$deconvo_tme_method)
            setProgress(0.2, message = "Inspecting TME columns...")
            
            # 1. 获取数据库所有列名 (dbListFields 比 colnames(tbl) 更快)
            all_columns <- dbListFields(pool, "cohort_data")
            
            # 2. 构造正则
            target_method <- input$deconvo_tme_method
            target_pattern <- if (target_method == "integration") {
              "CIBERSORT|EPIC|quantiseq|xCell|estimate|TIMER|MCPcounter|IPS"
            } else {
              target_method
            }
            
            # 3. 匹配列名
            # 先排除掉基础信息列，只在指标里找
            data_cols_pool <- setdiff(all_columns, c("CancerType", "Dataset", "ID"))
            target_cols <- grep(target_pattern, data_cols_pool, value = TRUE, ignore.case = TRUE)
            
            # 4. 特殊处理：CIBERSORT 去除 ABS
            if (target_method == "CIBERSORT") {
              abs_cols <- grep("ABS", target_cols, value = TRUE, ignore.case = TRUE)
              target_cols <- setdiff(target_cols, abs_cols)
            }
            
            if (length(target_cols) == 0) stop(paste("No columns found matching:", target_method))
            
            # 5. 数据库查询
            setProgress(0.4, message = "Querying TME data...")
            
            cols_to_select <- c("CancerType", "Dataset", "ID", target_cols)
            
            subset_data <- tbl_ref %>% 
              filter(CancerType == !!input$sel_cancer,
                     Dataset %in% !!input$sel_cohort) %>% 
              select(all_of(cols_to_select)) %>% 
              collect()
          }
          
          is_informative <- function(x) {
            # 1. 剔除全是 NA 的列
            if (all(is.na(x))) return(FALSE)
            
            # 2. 剔除内容完全一样的列 (常量列)
            if (length(unique(stats::na.omit(x))) <= 1) return(FALSE)
            
            return(TRUE)
          }
            
          # 应用于 subset_data (以防万一有全NA或全0的数据)
          subset_data <- subset_data %>% 
            dplyr::select(where(is_informative))

          if (nrow(subset_data) == 0) stop("No data found for the selected cohorts.")
          
          setProgress(0.8, message = "Finalizing data...")
          
          # 成功弹窗
          shinyWidgets::sendSweetAlert(
            session = session,
            title = "Success",
            text = paste0("Loaded ", nrow(subset_data), " samples. "),
            type = "success"
          )
          
          # 返回结果 (直接是一个 dataframe)
          attr(subset_data, "data_type") <- input$data_type
          data_results(subset_data)
          
        }, error = function(e) {
          # 错误弹窗
          shinyWidgets::sendSweetAlert(
            session = session, title = "Error", text = e$message, type = "error"
          )
          return(NULL)
        })
      })
    })
    
    # =========================================================
    # 3. 将结果传递给通用 Table Module
    # =========================================================
    
    # 这里的 output_expr_data 需要是一个 reactive，返回 dataframe
    output_expr_data <- reactive({
      req(data_results())
      data_results()
    })
    
    # 调用你的通用表格模块
    dataTableServer("expr_data", output_expr_data)
    
    # 预留：如果有绘图模块，也可以在这里传
    # plotServer("plot_preview", output_expr_data)
    
    return(data_results)
  })
}