# =========================================================
# Module: deconvo_tme
# =========================================================

# ---- Full UI ----
deconvo_tmeUI <- function(id) {
  ns <- NS(id)
  bs4TabItem(
    tabName = "deconvo_tme",
    h3(HTML(
      "Deconvo_tme
      <span style='font-size:80%; color:#333;'>:
       Performs immune deconvolution using multiple algorithms.</span>"
      )),
    deconvo_tmeBodyUI(id)
  )
}


# ---- BodyUI ----
deconvo_tmeBodyUI <- function(id, include_upload = TRUE) {
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
            uploadUI(ns("upload")) %>%
              helper(
                type    = "markdown",
                icon    = "question-circle",
                size    = "m",
                colour  = "#007bff",
                content = "demo_stad_tpm"
              )
          )
        },

        div(
            style = "text-align: center; margin-top: 10px;",  # 居中并添加顶部间距
            # Run 按钮
            actionButton(
              inputId = ns("run_deconvo_tme"), 
              label = "Run Analysis", 
              class = "btn-primary"
            )
        ),

        # 方法选择
        selectInput(
          inputId = ns("deconvo_tme_method"),
          label = "Method",
          choices = c(
            "CIBERSORT" = "cibersort",
            "EPIC" = "epic",
            "quanTIseq" = "quantiseq",
            "xCell" = "xcell",
            "ESTIMATE" = "estimate",
            "TIMER" = "timer",
            "MCPcounter" = "mcpcounter",
            "IPS" = "ips",
            "Integration" = "integration"
          ),
          selected = "epic"
        ),

        # ==== 参数卡片 CIBERSORT ====
        conditionalPanel(
          condition = paste0("input['", ns("deconvo_tme_method"), "'] == 'cibersort'"),
          selectInput(
            ns("deconvo_tme_arrays"),
            "Array",
            choices = c("True" = "T", "False" = "F"),
            selected = "F"
          ),
          br(),
          tags$p(
          style = "color: #555; font-style: italic; font-size: 90%; margin-top: -10px;",
          "Note: CIBERSORT will take a long time."
          ),

          sliderInput(
            ns("deconvo_tme_perm"),
            "Perm",
            min = 100,
            max = 10000,
            value = 100,
            step = 100,
            ticks = TRUE
          ),
          selectInput(
            ns("deconvo_tme_absolute"),
            "Absolute",
            choices = c("True" = "T", "False" = "F"),
            selected = "F"
          ),
          selectInput(
            ns("deconvo_tme_abs_method"),
            "Absolute Method",
            choices = c("Sigscore" = "sig.score", "No Sum-to-1" = "no.sumto1"),
            selected = "sig.score"
          )
        ),

        # ==== 参数卡片 EPIC ====
        conditionalPanel(
          condition = paste0("input['", ns("deconvo_tme_method"), "'] == 'epic'"),
          selectInput(
            ns("deconvo_tme_tumor"),
            "Tumor",
            choices = c("True" = "T", "False" = "F"),
            selected = "T"
          ),
          selectInput(
            ns("deconvo_tme_scale_mrna"),
            "Scale",
            choices = c("True" = "T", "False" = "F"),
            selected = "T"
          )
        ),

        # ==== 参数卡片 quanTIseq ====
        conditionalPanel(
          condition = paste0("input['", ns("deconvo_tme_method"), "'] == 'quantiseq'"),
          selectInput(
            ns("deconvo_tme_arrays_q"),
            "Array",
            choices = c("True" = "T", "False" = "F"),
            selected = "F"
          ),
          selectInput(
            ns("deconvo_tme_tumor"),
            "Tumor",
            choices = c("True" = "T", "False" = "F"),
            selected = "T"
          ),
          selectInput(
            ns("deconvo_tme_scale_mrna"),
            "Scale",
            choices = c("True" = "T", "False" = "F"),
            selected = "T"
          )
        ),

        # ==== 参数卡片 xCell ====
        conditionalPanel(
          condition = paste0("input['", ns("deconvo_tme_method"), "'] == 'xcell'"),
          selectInput(
            ns("deconvo_tme_arrays_x"),
            "Array",
            choices = c("True" = "T", "False" = "F"),
            selected = "F"
          )
        ),

        # ==== 参数卡片 ESTIMATE ====
        conditionalPanel(
          condition = paste0("input['", ns("deconvo_tme_method"), "'] == 'estimate'"),
          selectInput(
            ns("deconvo_tme_platform"),
            "Platform",
            choices = c(
              "affymetrix" = "affymetrix",
              "agilent" = "agilent",
              "illumina" = "illumina"
            ),
            selected = "affymetrix"
          )
        ),

        # ==== 参数卡片 TIMER ====
        conditionalPanel(
          condition = paste0("input['", ns("deconvo_tme_method"), "'] == 'timer'"),
          selectInput(
            ns("deconvo_tme_group_list"),
            "Cancer Type",
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
              "UCS" = "UCS"
            ),
            selected = "BRCA"  # Default selection
          )
        ),

        # ==== 参数卡片 Integration ====
        conditionalPanel(
          condition = paste0("input['", ns("deconvo_tme_method"), "'] == 'integration'"),
          br(),
          tags$p(
          style = "color: #555; font-style: italic; font-size: 90%; margin-top: -10px;",
          "Note: This pipeline runs ALL algorithms and calculates signatures. It may take a while."
          ),
          # 需要 Arrays 参数
          selectInput(
            ns("deconvo_int_arrays"), 
            "Array",
            choices = c("True" = "T", "False" = "F"),
            selected = "F"
          ),
          # 需要 Perm 参数
          sliderInput(
            ns("deconvo_int_perm"), 
            "Permutations",
            min = 100, max = 10000, value = 100, step = 100
          ),
          # 需要 Cancer Type (影响 TIMER)
          selectInput(
            ns("deconvo_int_tumor"), 
            "Cancer Type (for TIMER)",
            choices = c(
              "ACC", "BLCA", "BRCA", "CESC", "CHOL", "COAD", "DLBC", "ESCA", "GBM", 
              "HNSC", "KICH", "KIRC", "KIRP", "LAML", "LGG", "LIHC", "LUAD", "LUSC", 
              "MESO", "OV", "PAAD", "PCPG", "PRAD", "READ", "SARC", "SKCM", "STAD", 
              "TGCT", "THCA", "THYM", "UCEC", "UCS"
            ),
            selected = "BRCA"
          )
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
deconvo_tmeServer <- function(id, external_eset = NULL) {
  moduleServer(id, function(input, output, session) {

    input_data <- if (!is.null(external_eset)) {
      external_eset
    } else {
      uploadServer("upload")
    }

    deconvo_tme_result <- reactiveVal(NULL)

    observeEvent(input$run_deconvo_tme, {
      req(input_data())

      withProgress(message = "Running deconvo_tme analysis...", value = 0, {
        setProgress(0.2, message = "Reading input data...")

        data <- input_data()
        req(nrow(data) > 0)

        setProgress(0.5, message = "Running IOBR::deconvo_tme analysis...")

        method <- input$deconvo_tme_method
        result <- NULL

        result <- tryCatch({
          
          if (method == "cibersort") {
            IOBR::deconvo_tme(
              eset = data,
              method = method,
              arrays = input$deconvo_tme_arrays == "T",
              perm = input$deconvo_tme_perm,
              absolute.mode = input$deconvo_tme_absolute == "T",
              abs.method = input$deconvo_tme_abs_method
            )
            
          } else if (method == "epic") {
            IOBR::deconvo_tme(
              eset = data,
              method = method,
              tumor = input$deconvo_tme_tumor == "T",
              scale_mrna = input$deconvo_tme_scale_mrna == "T"
            )
            
          } else if (method == "quantiseq") {
            IOBR::deconvo_tme(
              eset = data,
              method = method,
              arrays = input$deconvo_tme_arrays_x == "T",
              tumor = input$deconvo_tme_tumor == "T",
              scale_mrna = input$deconvo_tme_scale_mrna == "T"
            )
            
          } else if (method == "xcell") {
            IOBR::deconvo_tme(
              eset = data,
              method = method,
              arrays = input$deconvo_tme_arrays_x == "T"
            )
            
          } else if (method == "estimate") {
            IOBR::deconvo_tme(
              eset = data,
              method = method,
              platform = input$deconvo_tme_platform
            )
            
          } else if (method == "timer") {
            IOBR::deconvo_tme(
              eset = data,
              method = method,
              group_list = rep(input$deconvo_tme_group_list, dim(data)[2])
            )
          
          } else if (method == "integration") {
            
            deconvo_integration(
              eset = data,
              array = input$deconvo_int_arrays == "T",
              tumor_type = input$deconvo_int_tumor,
              permutation = input$deconvo_int_perm
            )

          } else {
            IOBR::deconvo_tme(eset = data, method = method)
          } 
          
        }, error = function(e) {
          setProgress(1, message = "Error")
          showNotification(
            paste("Error during deconvo_tme():", e$message),
            type = "error",
            duration = 8
          )
          return(NULL)
        })

        req(result)

        colnames(data) <- gsub("\\.", "-", colnames(data))
        if (is.matrix(result)) result <- as.data.frame(result)

        setProgress(1, message = "Finished")
        deconvo_tme_result(result)
      })
    })

    dataTableServer("tbl_data", deconvo_tme_result)

    dataDownloadServer(
      "download",
      data_reactive   = deconvo_tme_result,
      filename_prefix = "deconvo_tme_result"
    )

    return(deconvo_tme_result)
  })
}


deconvo_integration <- function(eset, array, tumor_type, permutation = 100) {  
  # (1) CIBERSORT
  cibersort <- tryCatch({
    IOBR::deconvo_tme(eset = eset, method = "cibersort", arrays = array, perm = permutation)
  }, error = function(e) { warning("CIBERSORT failed"); return(NULL) })
  
  # (2) EPIC
  epic <- tryCatch({
    IOBR::deconvo_tme(eset = eset, method = "epic", arrays = array)
  }, error = function(e) { warning("EPIC failed"); return(NULL) })
  
  # (3) MCPcounter
  mcp <- tryCatch({
    IOBR::deconvo_tme(eset = eset, method = "mcpcounter")
  }, error = function(e) { warning("MCPcounter failed"); return(NULL) })
  
  # (4) xCell
  xcell <- tryCatch({
    IOBR::deconvo_tme(eset = eset, method = "xcell", arrays = array)
  }, error = function(e) { warning("xCell failed"); return(NULL) })
  
  # (5) ESTIMATE
  estimate <- tryCatch({
    IOBR::deconvo_tme(eset = eset, method = "estimate")
  }, error = function(e) { warning("ESTIMATE failed"); return(NULL) })
  
  # (6) TIMER
  timer <- tryCatch({
    IOBR::deconvo_tme(eset = eset, method = "timer", group_list = rep(tumor_type, dim(eset)[2]))
  }, error = function(e) { warning("TIMER failed"); return(NULL) })
  
  # (7) quanTIseq
  quantiseq <- tryCatch({
    IOBR::deconvo_tme(eset = eset, method = "quantiseq", tumor = TRUE, arrays = array, scale_mrna = TRUE)
  }, error = function(e) { warning("quanTIseq failed"); return(NULL) })
  
  # (8) IPS
  ips <- tryCatch({
    IOBR::deconvo_tme(eset = eset, method = "ips", plot = FALSE)
  }, error = function(e) { warning("IPS failed"); return(NULL) })
  
  
  # --- 合并 ---
  valid_results <- list(cibersort, epic, mcp, xcell, estimate, timer, quantiseq, ips)
  valid_results <- valid_results[!sapply(valid_results, is.null)]
  
  if (length(valid_results) == 0) {
    stop("All deconvolution methods failed.")
  }
  
  tme_combine <- Reduce(function(x, y) dplyr::inner_join(x, y, by = "ID"), valid_results)
  
  return(tme_combine)
}