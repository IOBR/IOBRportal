# =========================================================
# Module: prepare_othercohort_data
# Description: Unified module for fetching CPTAC/TARGET Data
# =========================================================

# ---- UI Function ----
prepare_othercohort_dataUI <- function(id) {
  ns <- NS(id)
  bs4TabItem(
    tabName = "prepare_othercohort_data",
    prepare_othercohort_dataBodyUI(id)
  )
}

# ---- BodyUI Function ----
prepare_othercohort_dataBodyUI <- function(id) {
  ns <- NS(id)
  
  fluidRow(
    column(
      width = 3,
      
      # --- Card 1: Data Source Selection ---
      bs4Card(
        title = "Choose Data Source",
        status = "gray",
        headerBorder = TRUE,
        collapsible = TRUE,
        solidHeader = TRUE,
        width = 12,
        maximizable = TRUE,
        
        # 1. Cohort Selection
        selectInput(
          inputId = ns("cohort_selector"),
          label   = "Select Cohort",
          choices = c("CPTAC", "TARGET"),
          selected = "CPTAC"
        ),
        
        # 2. Cancer Type Selection (Conditional)
        
        # === Scenario A: CPTAC List ===
        conditionalPanel(
          condition = paste0("input['", ns("cohort_selector"), "'] == 'CPTAC'"),
          selectInput(
            inputId = ns("cptac_cancer"),
            label   = "Cancer Type",
            choices = c(
              "BRCA" = "BRCA", "COAD" = "COAD", "GBM"  = "GBM",
              "HNSC" = "HNSC", "KIRC" = "KIRC", "LUAD" = "LUAD",
              "LUSC" = "LUSC", "OV"   = "OV",   "PAAD" = "PAAD",
              "UCEC" = "UCEC"
            ),
            selected = "LUAD"
          )
        ),
        
        # === Scenario B: TARGET List ===
        conditionalPanel(
          condition = paste0("input['", ns("cohort_selector"), "'] == 'TARGET'"),
          selectInput(
            inputId = ns("target_cancer"),
            label   = "Cancer Type",
            choices = c(
              "GNB"  = "GNB",
              "LAML" = "LAML",
              "NBL"  = "NBL"
            ),
            selected = "LAML"
          )
        )
      ),
      
      # --- Card 2: Method Parameters (Dynamic) ---
      bs4Card(
        title = "Method Parameters",
        status = "gray",
        headerBorder = TRUE,
        collapsible = TRUE,
        solidHeader = TRUE,
        width = 12,
        maximizable = TRUE,

        # 3. Analysis Mode Selection
        selectInput(
          inputId = ns("data_type"),
          label   = "Analysis Mode",
          choices = c(
            "Calculate Sigscore" = "sig",
            "Deconvolute TME"    = "tme"
          ),
          selected = "sig"
        ),
        
        # === Scenario A: Signature Parameters ===
        conditionalPanel(
          condition = paste0("input['", ns("data_type"), "'] == 'sig'"),
          selectInput(
            inputId = ns("calculate_sig_score_method"),
            label   = "Method",
            choices = c("PCA" = "PCA", "ssGSEA" = "ssgsea", "Z-score" = "zscore"),
            selected = "PCA"
          )
        ),
        
        # === Scenario B: TME Parameters ===
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
              "Integration"    = "integration" 
            ),
            selected = "CIBERSORT"
          )
        )
      ),
      
      # --- Submit Button ---
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
    
    # --- Right Side: Results ---
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

prepare_othercohort_dataServer <- function(id, pool) {
  moduleServer(id, function(input, output, session) {
    
    data_results <- reactiveVal(NULL)
    
    observeEvent(input$submit, {
      req(input$data_type)
      
      # 1. Determine the actual Cancer Type based on Cohort Selection
      current_cancer_type <- if (input$cohort_selector == "CPTAC") {
        input$cptac_cancer
      } else {
        input$target_cancer
      }
      req(current_cancer_type)
      
      withProgress(message = paste0("Processing ", input$cohort_selector, " (", current_cancer_type, ")..."), value = 0, {
        
        final_list <- tryCatch({
          
          if (!dbIsValid(pool)) stop("Database connection lost.")
          
          subset_data <- NULL
          tbl_ref <- tbl(pool, "iobr_data") 
          all_columns <- colnames(tbl_ref)
          
          # =========================================================
          # BRANCH A: Signature Scoring Logic
          # =========================================================
          if (input$data_type == "sig") {
            
            req(input$calculate_sig_score_method)
            setProgress(0.2, message = "Inspecting Signature columns...")
            
            # 按计算方法后缀匹配
            # Construct pattern (e.g., "_PCA$")
            target_pattern <- paste0("_", input$calculate_sig_score_method, "$")
            
            # Find columns
            target_cols <- grep(target_pattern, all_columns, value = TRUE, ignore.case = TRUE)
            
            if (length(target_cols) == 0) {
              stop(paste0("No columns found matching suffix: ", target_pattern))
            }
            
            # Query Data
            setProgress(0.4, message = "Querying Database...")
            cols_to_select <- c("ID", target_cols)
            
            subset_data <- tbl_ref %>% 
              dplyr::filter(CancerType == !!current_cancer_type) %>% 
              dplyr::select(dplyr::all_of(cols_to_select)) %>% 
              collect()
            
            if (nrow(subset_data) == 0) stop(paste("No signature data found for:", current_cancer_type))
            
            # Clean Column Names (remove suffix)
            colnames(subset_data) <- gsub(target_pattern, "", colnames(subset_data), ignore.case = TRUE)
            
            
          # =========================================================
          # BRANCH B: TME Deconvolution Logic
          # =========================================================
          } else { 
            req(input$deconvo_tme_method)
            setProgress(0.2, message = "Inspecting TME columns...")
            
            # Define pattern
            if (input$deconvo_tme_method == "integration") {
              target_pattern <- "CIBERSORT|EPIC|quantiseq|xCell|estimate|TIMER|MCPcounter|IPS"
            } else {
              target_pattern <- input$deconvo_tme_method
            }
            
            # Initial column grab
            target_cols <- grep(target_pattern, all_columns, value = TRUE, ignore.case = TRUE)
            
            # [CRITICAL FIX] Handle CIBERSORT vs CIBERSORT-ABS ambiguity
            if (input$deconvo_tme_method == "CIBERSORT") {
              abs_cols <- grep("ABS", target_cols, value = TRUE, ignore.case = TRUE)
              target_cols <- setdiff(target_cols, abs_cols)
            }
            
            if (length(target_cols) == 0) {
              stop(paste0("No columns found matching: ", target_pattern))
            }
            
            setProgress(0.4, message = "Querying Database...")
            cols_to_select <- c("ID", target_cols)
            
            subset_data <- tbl_ref %>% 
              dplyr::filter(CancerType == !!current_cancer_type) %>% 
              dplyr::select(dplyr::all_of(cols_to_select)) %>% 
              collect()
              
            if (nrow(subset_data) == 0) stop(paste("No TME data found for:", current_cancer_type))
          }
          
          # =========================================================
          # COMMON: Clinical Merge
          # =========================================================
          if ("ID" %in% colnames(subset_data)) {
            subset_data <- subset_data %>% dplyr::distinct(ID, .keep_all = TRUE)
          }
          
          setProgress(0.7, message = "Fetching Clinical data...")
          target_patients <- subset_data$ID
          
          # Fetch matching clinical data
          pdata <- tbl(pool, "clinical_data") %>% 
            dplyr::filter(ID %in% target_patients) %>% 
            collect() %>% 
            dplyr::distinct(ID, .keep_all = TRUE) %>%
            dplyr::select(where(~ !all(is.na(.))))
          
          # Intersect IDs
          common_ids <- intersect(subset_data$ID, pdata$ID)
          if (length(common_ids) == 0) stop("No valid samples after merging clinical data.")
          
          # Align Data
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
          
          list(expr = subset_data, pdata = pdata, type = input$data_type, cohort = input$cohort_selector, cancer = current_cancer_type)
          
        }, error = function(e) {
          shinyWidgets::sendSweetAlert(session = session, title = "Error", text = paste("Failed:", e$message), type = "error")
          return(NULL)
        })
        
        if (!is.null(final_list)) {
          data_results(final_list) 
          shinyWidgets::sendSweetAlert(session = session, title = "Success!", 
                                       text = paste0("Loaded ", nrow(final_list$expr), " samples. "), 
                                       type = "success")
        }
      }) 
    })
    
    # Outputs
    output_expr_data <- reactive({ req(data_results()); data_results()$expr })
    dataTableServer("expr_data", output_expr_data)
    
    output_clin_data <- reactive({ req(data_results()); data_results()$pdata })
    dataTableServer("clinical_data", output_clin_data)
    
    return(data_results)
  })
}