# =========================================================
# Module: siggenes
# =========================================================
data("signature_collection", package = "IOBR")

workflow_siggenesUI <- function(id) {
  ns <- NS(id)
  
  bs4TabItem(
    tabName = "workflow_siggenes",
    # --- 标题区域 ---
    h3(HTML(
      "Signature-Gene Workflow
      <span style='font-size:80%; color:#333;'>:
      Explore relationships between specific signatures and genes.</span>"
    )) %>%
      helper(
        type = "markdown",
        icon = "question-circle",
        size = "m",
        colour = "#007bff",
        content = "workflow_siggenes_help" # 请确保有对应的 help md 文件
      ),
    
    tabsetPanel(
      id = ns("main_categories"),
      type = "pills", 
      
      # =========================================================
      # Part 1: Preprocessing & Calculation
      # =========================================================
      tabPanel(
        title = "Part 1 · Preprocessing & Features",
        icon = icon("flask"),
        br(),
        tabsetPanel(
          id = ns("tabs_part1"),
          type = "tabs",

          tabPanel("Counts to TPM", count2tpmBodyUI(ns("mod_tpm"), include_upload = TRUE)),
          tabPanel("Detect Outliers", find_outlier_samplesBodyUI(ns("mod_detcct_outlier"), include_upload = FALSE)),
          tabPanel("Calculate Signatures", calculate_sig_scoreBodyUI(ns("mod_signature"), include_upload = FALSE))
        )
      ),

      tabPanel(
        title = "Part 2 · Correlation",
        icon = icon("project-diagram"),
        br(),
        tabsetPanel(
          id = ns("tabs_part2"),
          type = "tabs",
          tabPanel("Batch Correlation",   batch_corBodyUI(ns("mod_batch_cor"), include_upload = FALSE)),
          tabPanel("Single Correlation",  get_corBodyUI(ns("mod_single_cor"), include_upload = FALSE)),
          tabPanel("Correlation Matrix",  get_cor_matrixBodyUI(ns("mod_cor_matrix"), include_upload = FALSE))
        )
      )
    )
  )
}


workflow_siggenesServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # =========================================================
    # Part 1: Preprocessing & Calculation
    # =========================================================
    res_tpm <- count2tpmServer("mod_tpm")
    res_outlier <- find_outlier_samplesServer("mod_detcct_outlier", external_eset = res_tpm$tpm)
    
    # 离群值处理逻辑
    clean_tpm <- reactive({
      if (!is.null(res_outlier())) {
        return(res_outlier())
      } else {
        return(res_tpm$tpm())
      }
    })
    
    res_sig_scores <- calculate_sig_scoreServer("mod_signature", external_eset = clean_tpm)
    
    # =========================================================
    # Part 2: Data Merging (构造 Sig + Gene 的大表)
    # =========================================================
    
    big_matrix <- reactive({
      req(clean_tpm(), res_sig_scores())
      
      # --- A. 处理 TPM 矩阵 (转置) ---
      # TPM: Rows=Genes, Cols=Samples -> 转置为 Samples x Genes
      tpm_mat <- t(clean_tpm())
      tpm_df <- as.data.frame(tpm_mat)
      tpm_df$ID <- rownames(tpm_df)

      # --- B. 处理 Signature 矩阵 ---
      sig_df <- as.data.frame(res_sig_scores())
      if (!"ID" %in% colnames(sig_df)) {
        sig_df$ID <- rownames(sig_df)
      }

      # --- C. 合并 (Merge by ID) ---
      # 结果结构: ID列 | Signature列... | Gene列...
      final_big_matrix <- merge(sig_df, tpm_df, by = "ID")

      return(final_big_matrix)
    })

    # =========================================================
    # Part 3: Downstream Analysis (Correlation)
    # =========================================================
    # 1. 批量相关性
    batch_corServer("mod_batch_cor", external_eset = big_matrix, target_cols = TRUE)
    
    # 2. 单个相关性
    get_corServer("mod_single_cor", external_eset = big_matrix, target_cols = TRUE)
    
    # 3. 相关性矩阵
    get_cor_matrixServer("mod_cor_matrix", external_eset = big_matrix, target_cols = TRUE)
  })
}