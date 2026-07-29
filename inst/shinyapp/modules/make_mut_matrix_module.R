# =========================================================
# Module: make_mut_matrix
# =========================================================

# ---- Full UI ----
make_mut_matrixUI <- function(id) {
  ns <- NS(id)
  bs4TabItem(
    tabName = "make_mut_matrix",
    h3(HTML(
      "Make_mut_matrix
      <span style='font-size:80%; color:#333;'>:
       Converts MAF mutation data into a binary mutation matrix.</span>"
      )),
    make_mut_matrixBodyUI(id)
  )
}


# ---- BodyUI ----
make_mut_matrixBodyUI <- function(id,include_upload = TRUE) {
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

        #仅当 include_upload=TRUE 时渲染上传组件
        if (isTRUE(include_upload)) {
          tagList(
            uploadUI(ns("upload")) %>%
              helper(
                type    = "markdown",
                icon    = "question-circle",
                size    = "m",
                colour  = "#007bff",
                content = "demo_maf"
              )
          )
        },

        div(
          style = "text-align: center; margin-top: 10px;",
          actionButton(
            inputId = ns("run_make_mut_matrix"),
            label = "Run Analysis",
            class = "btn-primary"
          )
        ),
        
        selectInput(
          inputId = ns("make_mut_matrix_isTCGA"),
          label = "TCGA",
          choices = c("True" = "T", "False" = "F"),
          selected = "T"
        ),

        selectInput(
          inputId = ns("make_mut_matrix_table_type"),
          label = "Type to show and download",
          choices = c(
            "All" = "all",
            "SNP" = "snp",
            "INDEL" = "indel",
            "Frameshift" = "frameshift"
          ),
          selected = "all"
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
make_mut_matrixServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    uploaded_data <- uploadServer("upload")
    make_mut_matrix_result <- reactiveVal(NULL)

    observeEvent(input$run_make_mut_matrix, {
      req(uploaded_data())  
      withProgress(message = "Running make_mut_matrix...", value = 0, {
        setProgress(0.5, message = "Processing mutation data...")

        result <- tryCatch({
          make_mut_matrix(
            maf      = uploaded_data(),
            isTCGA   = input$make_mut_matrix_isTCGA == "T",
            category = "multi"
          )
        }, error = function(e) {
          setProgress(1, message = "Error")
          showNotification(
            paste("Error during make_mut_matrix():", e$message),
            type = "error",
            duration = 8
          )
          return(NULL)
        })
        
        req(result)

        make_mut_matrix_result(result)
        setProgress(1, message = "Finished!")
      })
    })

    selected_tbl <- reactive({
      req(make_mut_matrix_result())
      make_mut_matrix_result()[[ input$make_mut_matrix_table_type ]]
    })

    dataTableServer("tbl_data", selected_tbl)

    dataDownloadServer(
      "download",
      data_reactive   = selected_tbl,
      filename_prefix = "mut_matrix"
    )

    return(selected_tbl)
  })
}
