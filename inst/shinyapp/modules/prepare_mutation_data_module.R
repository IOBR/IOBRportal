# =========================================================
# Module: prepare_mutation_data
# =========================================================


# =========================================================
# UI
# =========================================================

prepare_mutation_dataUI <- function(id) {
  ns <- NS(id)
  
  bs4TabItem(
    tabName = "prepare_mutation_data",
    prepare_mutation_dataBodyUI(id)
  )
}


# ---- Body UI ----

prepare_mutation_dataBodyUI <- function(id) {
  ns <- NS(id)
  
  cancer_choices <- c(
    "ACC", "BLCA", "BRCA", "CESC", "CHOL", "COAD", "DLBC", "ESCA",
    "GBM", "HNSC", "KICH", "KIRC", "KIRP", "LAML", "LGG", "LIHC",
    "LUAD", "LUSC", "MESO", "OV", "PAAD", "PCPG", "PRAD", "READ",
    "SARC", "SKCM", "STAD", "TGCT", "THCA", "THYM", "UCEC", "UCS", "UVM"
  )
  
  fluidRow(
    column(
      width = 3,
      
      # -----------------------------------------------------
      # Card 1：癌种和Mutation类型
      # -----------------------------------------------------
      
      bs4Card(
        title = "Choose Data Source",
        status = "gray",
        headerBorder = TRUE,
        collapsible = TRUE,
        solidHeader = TRUE,
        maximizable = TRUE,
        width = 12,
        
        selectInput(
          inputId = ns("mutation_cancer"),
          label = "Cancer Type",
          choices = cancer_choices,
          selected = "LUAD",
          multiple = FALSE
        ),
        
        selectInput(
          inputId = ns("mutation_type"),
          label = "Mutation Type",
          choices = c(
            "All Mutations" = "all",
            "SNP" = "snp",
            "INDEL" = "indel",
            "Frameshift" = "frameshift"
          ),
          selected = "all",
          multiple = FALSE
        )
      ),
      
      # -----------------------------------------------------
      # Card 2：Signature数据
      # -----------------------------------------------------
      
      bs4Card(
        title = "Signature Data",
        status = "gray",
        headerBorder = TRUE,
        collapsible = TRUE,
        solidHeader = TRUE,
        maximizable = TRUE,
        width = 12,
        
        selectInput(
          inputId = ns("signature_method"),
          label = "Scoring Method",
          choices = c(
            "PCA" = "pca",
            "ssGSEA" = "ssgsea"
          ),
          selected = "pca"
        ),
        
        # PCA Signature集合
        conditionalPanel(
          condition = paste0("input['", ns("signature_method"), "'] == 'pca'"),
          
          selectInput(
            inputId = ns("signature_set_pca"),
            label = "Signature Set",
            choices = c(
              "TME" = "signature_tme",
              "Metabolism" = "signature_metabolism",
              "Collection" = "signature_collection"
            ),
            selected = "signature_tme"
          )
        ),
        
        # ssGSEA Signature集合
        conditionalPanel(
          condition = paste0("input['", ns("signature_method"), "'] == 'ssgsea'"),
          
          selectInput(
            inputId = ns("signature_set_ssgsea"),
            label = "Signature Set",
            choices = c(
              "GO Biological Process" = "go_bp",
              "GO Cellular Component" = "go_cc",
              "GO Molecular Function" = "go_mf",
              "KEGG" = "kegg",
              "Hallmark" = "hallmark",
              "Reactome" = "reactome"
            ),
            selected = "hallmark"
          )
        )
      ),
      
      # -----------------------------------------------------
      # Submit
      # -----------------------------------------------------
      
      div(
        style = "text-align: center;",
        
        actionButton(
          inputId = ns("submit"),
          label = "Submit",
          icon = icon("paper-plane"),
          class = "btn-primary",
          style = "width: 180px;"
        )
      )
    ),
    
    # -------------------------------------------------------
    # 右侧数据展示
    # -------------------------------------------------------
    
    column(
      width = 9,
      
      bs4Card(
        title = "Prepared Data",
        status = "gray",
        headerBorder = TRUE,
        collapsible = TRUE,
        solidHeader = TRUE,
        maximizable = TRUE,
        width = 12,
        
        tabBox(
          width = 12,
          side = "left",
          tabPanel("Signature Data", dataTableUI(ns("signature_data"))),
          tabPanel("Mutation Data", dataTableUI(ns("mutation_data"))),
          tabPanel("Clinical Data", dataTableUI(ns("clinical_data")))
        )
      )
    )
  )
}


# =========================================================
# Server
# =========================================================

prepare_mutation_dataServer <- function(id, pool) {
  moduleServer(id, function(input, output, session) {
    
    # 保存最终结果
    data_results <- reactiveVal(NULL)
    
    # -------------------------------------------------------
    # 从数据库读取并准备数据
    # -------------------------------------------------------
    
    load_mutation_dataset <- function() {
      
      selected_cancer <- input$mutation_cancer
      selected_mutation_type <- input$mutation_type
      selected_method <- input$signature_method
      
      selected_signature_set <- if (selected_method == "pca") {
        input$signature_set_pca
      } else {
        input$signature_set_ssgsea
      }
      
      req(selected_cancer)
      req(selected_mutation_type)
      req(selected_signature_set)
      
      # 临时获取数据库连接，函数结束后自动归还
      con <- pool::poolCheckout(pool)
      on.exit(pool::poolReturn(con), add = TRUE)
      
      
      # -----------------------------------------------------
      # 1. 获取Signature列名
      # -----------------------------------------------------
      signature_names <- load_iobr_signature_names(selected_signature_set)
      
      if (is.null(signature_names) || length(signature_names) == 0) {
        stop("Signature object has no valid names: ", selected_signature_set)
      }
      
      signature_reference <- dplyr::tbl(con, "signature_table")
      database_columns <- colnames(signature_reference)
      matched_signature_columns <- base::intersect(signature_names, database_columns)
      
      if (length(matched_signature_columns) == 0) {
        stop(
          paste0(
            "No Signature columns matched for ",
            selected_signature_set,
            ". Example expected names: ",
            paste(head(signature_names, 5), collapse = ", ")
          )
        )
      }
      
      
      # -----------------------------------------------------
      # 2. 读取Signature数据
      # -----------------------------------------------------      
      signature_data <- signature_reference %>%
        dplyr::filter(CancerType == !!selected_cancer) %>%
        dplyr::select(ID, dplyr::all_of(matched_signature_columns)) %>%
        dplyr::collect() %>%
        dplyr::mutate(ID = substr(as.character(ID), 1, 12)) %>%
        dplyr::distinct(ID, .keep_all = TRUE)
      
      if (nrow(signature_data) == 0) {
        stop("No Signature data found for: ", selected_cancer)
      }
      
      
      # -----------------------------------------------------
      # 3. 读取并恢复Mutation矩阵
      # -----------------------------------------------------
      setProgress(0.50, message = paste("Loading", selected_mutation_type, "Mutation matrix..."))
      
      mutation_record <- DBI::dbGetQuery(
        con,
        paste(
          "SELECT NSamples, NGenes, MutationMatrix",
          "FROM mutation_table",
          "WHERE CancerType = ? AND MutationType = ?"
        ),
        params = list(
          selected_cancer,
          selected_mutation_type
        )
      )
      
      if (nrow(mutation_record) != 1) {
        stop(
          "Expected one Mutation matrix for ",
          selected_cancer,
          " / ",
          selected_mutation_type,
          ", but found ",
          nrow(mutation_record)
        )
      }
      
      mutation_matrix <- unserialize(
        memDecompress(
          mutation_record$MutationMatrix[[1]],
          type = "gzip"
        )
      )
      
      if (!is.matrix(mutation_matrix)) {
        stop("Restored Mutation object is not a matrix")
      }
      
      if (anyNA(mutation_matrix)) {
        stop("Restored Mutation matrix contains NA")
      }
      
      if (any(!(mutation_matrix %in% c(0L, 1L)))) {
        stop("Restored Mutation matrix is not a valid binary matrix")
      }
      
      if (
        nrow(mutation_matrix) != mutation_record$NSamples[[1]] ||
        ncol(mutation_matrix) != mutation_record$NGenes[[1]]
      ) {
        stop("Restored Mutation matrix dimensions do not match database metadata")
      }
      
      if (
        is.null(rownames(mutation_matrix)) ||
        is.null(colnames(mutation_matrix))
      ) {
        stop("Mutation matrix is missing sample IDs or gene names")
      }
      
      mutation_ids <- substr(rownames(mutation_matrix), 1, 12)
      
      if (anyDuplicated(mutation_ids)) {
        stop("Duplicated patient IDs found after shortening Mutation IDs")
      }
      
      rownames(mutation_matrix) <- mutation_ids
      
      
      # -----------------------------------------------------
      # 4. 读取Clinical数据
      # -----------------------------------------------------
      setProgress(0.70, message = "Loading Clinical data...")
      
      clinical_data <- dplyr::tbl(con, "tbl_clinical") %>%
        dplyr::filter(CancerType == !!selected_cancer) %>%
        dplyr::collect() %>%
        dplyr::mutate(ID = substr(as.character(ID), 1, 12)) %>%
        dplyr::distinct(ID, .keep_all = TRUE)
      
      if (nrow(clinical_data) == 0) {
        stop("No Clinical data found for: ", selected_cancer)
      }
      
      
      # -----------------------------------------------------
      # 5. 匹配三类数据共有患者
      # -----------------------------------------------------     
      common_ids <- Reduce(
        base::intersect,
        list(
          signature_data$ID,
          rownames(mutation_matrix),
          clinical_data$ID
        )
      )
      
      if (length(common_ids) == 0) {
        stop(
          paste0(
            "No common patients among Signature, Mutation, ",
            "and Clinical data"
          )
        )
      }
      
      signature_data <- signature_data[
        match(common_ids, signature_data$ID),
        ,
        drop = FALSE
      ]
      
      mutation_matrix <- mutation_matrix[
        match(common_ids, rownames(mutation_matrix)),
        ,
        drop = FALSE
      ]
      
      clinical_data <- clinical_data[
        match(common_ids, clinical_data$ID),
        ,
        drop = FALSE
      ]
      
      
      # -----------------------------------------------------
      # 6. 删除全NA或常量列
      # -----------------------------------------------------
      
      keep_informative_columns <- function(data, always_keep = "ID") {
        
        informative <- vapply(
          data,
          function(x) {
            valid_values <- stats::na.omit(x)
            length(valid_values) > 0 &&
              length(unique(valid_values)) > 1
          },
          logical(1)
        )
        
        keep_columns <- names(data) %in% always_keep | informative
        data[, keep_columns, drop = FALSE]
      }
      
      signature_data <- keep_informative_columns(
        signature_data,
        always_keep = "ID"
      )
      
      clinical_data <- keep_informative_columns(
        clinical_data,
        always_keep = c("ID", "CancerType")
      )
      
      if (ncol(signature_data) <= 1) {
        stop(
          "No informative Signature columns remained ",
          "after patient matching"
        )
      }
      
      # -------------------------------------------------------
      # 生成Mutation–Survival数据
      # -------------------------------------------------------
      # 仅保留同时包含WT和Mutated样本的基因
      informative_genes <- colSums(mutation_matrix) > 0 & colSums(mutation_matrix) < nrow(mutation_matrix)

      if (!any(informative_genes)) stop("No informative Mutation genes remained for survival analysis")

      # 提取用于生存分析的0/1突变矩阵
      mutation_survival_numeric <- as.data.frame(mutation_matrix[, informative_genes, drop = FALSE], check.names = FALSE)

      # Batch Survival使用数值型0/1变量
      batch_surv_data <- cbind(clinical_data, mutation_survival_numeric)

      # Survival Group使用WT/Mutated因子变量
      mutation_survival_group <- mutation_survival_numeric
      mutation_survival_group[] <- lapply(mutation_survival_group, function(x) factor(ifelse(x >= 1, "Mutated", "WT"), levels = c("WT", "Mutated")))
      surv_group_data <- cbind(clinical_data, mutation_survival_group)

      # -----------------------------------------------------
      # 7. 返回Mutation Workflow需要的数据
      # -----------------------------------------------------
      
      list(
        signature_matrix = signature_data,
        mutation_matrix = mutation_matrix,
        pdata = clinical_data,
        batch_surv_data = batch_surv_data,
        surv_group_data = surv_group_data,
        cancer_type = selected_cancer,
        mutation_type = selected_mutation_type,
        signature_set = selected_signature_set
      )
    }
    
    
    # -------------------------------------------------------
    # Submit事件
    # -------------------------------------------------------
    
    observeEvent(input$submit, {
      
      withProgress(
        message = "Preparing TCGA Mutation data...",
        value = 0,
        {
          final_result <- tryCatch(
            load_mutation_dataset(),
            
            error = function(e) {
              shinyWidgets::sendSweetAlert(
                session = session,
                title = "Error",
                text = paste("Failed:", e$message),
                type = "error"
              )
              
              NULL
            }
          )
          
          if (!is.null(final_result)) {
            
            data_results(final_result)
            setProgress(1, message = "Done!")
            
            shinyWidgets::sendSweetAlert(
              session = session,
              title = "Success!",
              text = paste0(
                "Loaded ",
                nrow(final_result$signature_matrix),
                " matched samples, ",
                ncol(final_result$signature_matrix) - 1,
                " Signatures, and ",
                ncol(final_result$mutation_matrix),
                " mutated genes."
              ),
              type = "success"
            )
          }
        }
      )
    })
    
    
    # -------------------------------------------------------
    # Signature数据
    # -------------------------------------------------------
    
    output_signature_data <- reactive({
      req(data_results())
      data_results()$signature_matrix
    })
    
    dataTableServer("signature_data", output_signature_data)
    
    
    # -------------------------------------------------------
    # Mutation数据
    # -------------------------------------------------------
    
    output_mutation_data <- reactive({
      req(data_results())
      data_results()$mutation_matrix
    })
    
    dataTableServer("mutation_data", output_mutation_data)
    
    
    # -------------------------------------------------------
    # Clinical数据
    # -------------------------------------------------------
    
    output_clinical_data <- reactive({
      req(data_results())
      data_results()$pdata
    })
    
    dataTableServer("clinical_data", output_clinical_data)
    
    
    # 返回给Mutation Workflow
    return(data_results)
  })
}