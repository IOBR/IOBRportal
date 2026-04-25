# =========================================================
# Module: prepare_tcga_data
# Description: Unified module for fetching TCGA Signature or TME data
# =========================================================

prepare_tcga_dataUI <- function(id) {
  ns <- NS(id)
  bs4TabItem(
    tabName = "prepare_tcga_data",
    prepare_tcga_dataBodyUI(id)
  )
}


# ---- BodyUI ----
prepare_tcga_dataBodyUI <- function(id) {
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
        
        # 1. 癌种选择 (公共)
        selectInput(
          inputId = ns("tcga_cancer"),
          label   = "Cancer Type",
          choices = c(
            "ACC" = "ACC",
            "BLCA" = "BLCA",
            "BRCA" = "BRCA",
            "CESC" = "CESC",
            "CHOL" = "CHOL",
            "COAD" = "COAD",
            "DLBC" = "DLBC",
            "ESCA" = "ESCA",
            "GBM" = "GBM",
            "HNSC" = "HNSC",
            "KICH" = "KICH",
            "KIRC" = "KIRC",
            "KIRP" = "KIRP",
            "LAML" = "LAML",
            "LGG" = "LGG",
            "LIHC" = "LIHC",
            "LUAD" = "LUAD",
            "LUSC" = "LUSC",
            "MESO" = "MESO",
            "OV" = "OV",
            "PAAD" = "PAAD",
            "PCPG" = "PCPG",
            "PRAD" = "PRAD",
            "READ" = "READ",
            "SARC" = "SARC",
            "SKCM" = "SKCM",
            "STAD" = "STAD",
            "TGCT" = "TGCT",
            "THCA" = "THCA",
            "THYM" = "THYM",
            "UCEC" = "UCEC",
            "UCS" = "UCS",
            "UVM" = "UVM",
            "Pan-Cancer" = "Pancancer" #更新
          ),
          selected = "ACC",
          multiple = FALSE
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
        
        # 2. 模式选择 (Signature 或 TME)
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
              "CIBERSORT"  = "CIBERSORT",
              "EPIC"       = "EPIC",
              "quanTIseq"  = "quantiseq",
              "xCell"      = "xCell",
              "ESTIMATE"   = "estimate",
              "TIMER"      = "TIMER",
              "MCPcounter" = "MCPcounter",
              "IPS"        = "IPS",
              "Integration" = "integration" #更新
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
          # 统一命名为 "Data"，因为可能是 Expression 也可能是 Score
          tabPanel("Data", dataTableUI(ns("expr_data"))),
          tabPanel("Clinical Data", dataTableUI(ns("clinical_data")))
        )
      )
    )
  )
}


# =========================================================
# Server Logic
# =========================================================

prepare_tcga_dataServer <- function(id, pool) {
  moduleServer(id, function(input, output, session) {
    
    # Store the final result list(expr, pdata)
    data_results <- reactiveVal(NULL)
    
    observeEvent(input$submit, {
      req(input$tcga_cancer)
      req(input$data_type)
      
      withProgress(message = "Processing...", value = 0, {
        
        final_list <- tryCatch({
          
          # --- 1. Connection Check ---
          if (input$data_type == "sig") {
  
  setProgress(0.2, message = "Parsing Signature selection...")
  
  method <- input$calculate_sig_score_method
  target_sig_name_str <- if (method == "pca") {
    input$calculate_sig_score_signature_pca
  } else {
    input$calculate_sig_score_signature_ssgsea
  }
  req(target_sig_name_str)

  # target_sig_list <- sig_map[[target_sig_name_str]]
  # sig_names_needed <- names(target_sig_list)
  # 数据库模块只需要这些 names 去匹配 DuckDB 中已经预计算好的列
  sig_names_needed <- load_iobr_signature_names(target_sig_name_str) 
  
  if (is.null(sig_names_needed) || length(sig_names_needed) == 0) {
    stop(paste("Signature object has no valid names:", target_sig_name_str))
  }
  
  setProgress(0.4, message = paste("Querying Signature data for", input$tcga_cancer, "..."))
  
  tbl_sig <- tbl(pool, "signature_table")
  all_columns <- colnames(tbl_sig)
  matched_cols <- intersect(sig_names_needed, all_columns)
  
  if (length(matched_cols) == 0) {
    stop(
      paste0(
        "No signature columns matched in database for ",
        target_sig_name_str,
        ". Example expected names: ",
        paste(head(sig_names_needed, 5), collapse = ", ")
      )
    )
  }
  
  subset_data <- tbl_sig %>% 
    dplyr::filter(CancerType == !!input$tcga_cancer) %>% 
    dplyr::select(ID, dplyr::all_of(matched_cols)) %>% 
    collect()
  
  if (nrow(subset_data) == 0) {
    stop(paste("No signature data found for:", input$tcga_cancer))
  }
            
          # =========================================================
          # BRANCH B: TME Deconvolution Logic (Updated for Integration)
          # =========================================================
          } else { # input$data_type == "tme"
            
            req(input$deconvo_tme_method)
            setProgress(0.2, message = "Inspecting TME columns...")
            
            # 1. Get all columns to find matches
            tbl_ref <- tbl(pool, "signature_table")
            all_columns <- colnames(tbl_ref)
            
            # 2. Define pattern based on selection
            # --- 支持 Integration ---
            if (input$deconvo_tme_method == "integration") {
              # 如果选了 Integration，构造一个匹配所有 8 种方法的正则
              # 使用 | (OR) 连接所有方法名
              target_pattern <- "CIBERSORT|EPIC|quantiseq|xCell|estimate|TIMER|MCPcounter|IPS"
            } else {
              # 否则，只匹配选中的那一种方法
              target_pattern <- input$deconvo_tme_method
            }
            
            # 3. Grep for columns
            target_cols <- grep(target_pattern, all_columns, value = TRUE, ignore.case = TRUE)
            
            if (length(target_cols) == 0) {
              stop(paste0("No columns found matching pattern: ", target_pattern))
            }
            
            # 4. Query Database (Pancancer compatible)
            setProgress(0.4, message = paste("Querying TME data for", input$tcga_cancer, "..."))
            
            cols_to_select <- c("ID", target_cols)
            
            subset_data <- tbl_ref %>% 
              dplyr::filter(CancerType == !!input$tcga_cancer) %>% 
              dplyr::select(dplyr::all_of(cols_to_select)) %>% 
              collect()
            
            if (nrow(subset_data) == 0) stop(paste("No TME data found for:", input$tcga_cancer))
          }
          
          # =========================================================
          # COMMON LOGIC: Process IDs & Merge Clinical
          # =========================================================
          
          # --- 5. Process IDs (substr 12 chars, distinct) ---
          if ("ID" %in% colnames(subset_data)) {
            subset_data <- subset_data %>% 
              dplyr::mutate(ID = substr(ID, 1, 12)) %>% 
              dplyr::distinct(ID, .keep_all = TRUE)
          }
          
          # --- 6. Fetch Clinical Data ---
          setProgress(0.7, message = "Fetching Clinical data...")
          
          target_patients <- subset_data$ID
          
          pdata <- tbl(pool, "tbl_clinical") %>% 
            dplyr::filter(ID %in% target_patients) %>% 
            collect() %>% 
            dplyr::distinct(ID, .keep_all = TRUE)
          
          # --- 7. Intersect and Match ---
          common_ids <- intersect(subset_data$ID, pdata$ID)
          
          if (length(common_ids) == 0) {
            stop("No valid samples remaining after merging clinical data.")
          }
          
          subset_data <- subset_data[match(common_ids, subset_data$ID), ]
          pdata       <- pdata[match(common_ids, pdata$ID), ]
          
          is_informative <- function(x) {
            # 1. 剔除全是 NA 的列
            if (all(is.na(x))) return(FALSE)
            
            # 2. 剔除内容完全一样的列 (常量列)
            if (length(unique(stats::na.omit(x))) <= 1) return(FALSE)
            
            return(TRUE)
          }
          
          # 应用于 pdata (临床数据中最常见这种常量列)
          pdata <- pdata %>% 
            dplyr::select(where(is_informative))
            
          # 应用于 subset_data (以防万一有全NA或全0的数据)
          subset_data <- subset_data %>% 
            dplyr::select(where(is_informative))

          setProgress(0.9, message = "Finalizing...")
          
          # --- 8. Return List ---
          list(
            expr  = subset_data, 
            pdata = pdata,
            type  = input$data_type        
          )
          
        }, error = function(e) {
          shinyWidgets::sendSweetAlert(
            session = session, title = "Error", text = paste("Failed:", e$message), type = "error"
          )
          return(NULL)
        })
        
        # --- 9. Success Handling ---
        if (!is.null(final_list)) {
          data_results(final_list) 
          
          setProgress(1, message = "Done!")
          
          mode_msg <- if(input$data_type == "sig") "Signature" else "TME"
          
          shinyWidgets::sendSweetAlert(
            session = session,
            title = "Success!",
            text = paste0("Loaded ", nrow(final_list$expr), " samples. "),
            type = "success"
          )
        }
      }) 
    })
    
    # --- 10. Render Tables ---
    output_expr_data <- reactive({
      req(data_results())
      data_results()$expr 
    })
    dataTableServer("expr_data", output_expr_data)
    
    output_clin_data <- reactive({
      req(data_results())
      data_results()$pdata 
    })
    dataTableServer("clinical_data", output_clin_data)
    
    return(data_results)
  })
}