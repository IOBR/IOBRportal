# =========================================================
# Module: count2tpm
# =========================================================

# ---- Full UI ----
count2tpmUI <- function(id) {
  ns <- NS(id)
  bs4TabItem(
    tabName = "count2tpm",
    h3(HTML(
      "Count2tpm
      <span style='font-size:80%; color:#333;'>:
       Converts raw gene counts to TPM.</span>"
      )),
    count2tpmBodyUI(id)
  )
}


# ---- BodyUI ----
count2tpmBodyUI <- function(id, include_upload = TRUE) {
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
                content = "demo_eset_stad"
              )
          )
        },
        
        div(
          style = "text-align: center; margin-top: 10px;",
          actionButton(
            inputId = ns("run_count2tpm"),
            label = "Run Analysis",
            class = "btn-primary"
          )
        ),
        
        selectInput(
          inputId = ns("count2tpm_org"),
          label = "Organism",
          choices = c(
            "Human" = "hsa",
            "Mouse" = "mmus"
          ),
          selected = "hsa"
        ),

        # Human: Ensembl / Entrez / Symbol
        conditionalPanel(
          condition = "input.count2tpm_org == 'hsa'",
          ns = ns,
          selectInput(
            inputId = ns("count2tpm_idType_hsa"),
            label = "ID Type",
            choices = c(
              "Ensembl" = "ensembl",
              "Entrez"  = "entrez",
              "Symbol"  = "symbol"
            ),
            selected = "ensembl"
          )
        ),
        
        # Mouse: Ensembl / MGI / Symbol
        conditionalPanel(
          condition = "input.count2tpm_org == 'mmus'",
          ns = ns,
          selectInput(
            inputId = ns("count2tpm_idType_mmus"),
            label = "ID Type",
            choices = c(
              "Ensembl" = "ensembl",
              "MGI"     = "mgi",
              "Symbol"  = "symbol"
            ),
            selected = "ensembl"
          )
        ),

        selectInput(
          inputId = ns("count2tpm_source"),
          label = "Source",
          choices = c(
            "Local" = "local",
            "Biomart" = "biomart"
          ),
          selected = "local"
        ),

        selectInput(
          inputId = ns("log2"),
          label = "Choose Log2",
          choices = c(
            "True" = "T",
            "False" = "F"
          ),
          selected = "T"
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
count2tpmServer <- function(id, external_eset = NULL) {
  moduleServer(id, function(input, output, session) {
    
    # 如果 workflow 传入 external_eset（reactive），则优先用它；
    input_data <- if (!is.null(external_eset)) {
      external_eset
    } else {
      uploadServer("upload")
    }
    
    count2tpm_result <- reactiveVal(NULL)

    current_idType <- reactive({
      req(input$count2tpm_org)

      if (input$count2tpm_org == "mmus") {
        req(input$count2tpm_idType_mmus)
        input$count2tpm_idType_mmus
      } else {
        req(input$count2tpm_idType_hsa)
        input$count2tpm_idType_hsa
      }
    })

    observeEvent(input$run_count2tpm, {
      req(input_data())
      
      withProgress(message = "Running count2tpm analysis...", value = 0, {
        setProgress(0.2, message = "Reading data...")
        
        data <- input_data()
        req(nrow(data) > 0)
        
        setProgress(0.5, message = "Converting to TPM...")
        
        tpm_data <- tryCatch({
          count2tpm(
            countMat = data,
            idType   = current_idType(),
            org      = input$count2tpm_org,
            source   = input$count2tpm_source
          )
        }, error = function(e) {
          setProgress(1, message = "Error")
          showNotification(
            paste("Error during count2tpm():", e$message),
            type = "error",
            duration = 8
          )
          return(NULL)
        })
        
        req(!is.null(tpm_data))
        req(tpm_data)
        if (input$log2 == "T") {
          setProgress(0.8, message = "Performing Log2 transformation...")
          # 假设 log2eset 是你已经加载的函数 (通常等同于 log2(x + 1))
          tpm_data <- log2eset(tpm_data)
        }

        colnames(tpm_data) <- gsub("\\.", "-", colnames(tpm_data))
        
        count2tpm_result(tpm_data)
        
        setProgress(1, message = "Finished")
      })
    })
    
    dataTableServer("tbl_data", count2tpm_result)
    
    dataDownloadServer(
      "download",
      data_reactive   = count2tpm_result,
      filename_prefix = "TPM_Results"
    )
    
    # return(count2tpm_result)
    return(list(
      tpm = count2tpm_result,
      raw = input_data
    ))
  })
}

