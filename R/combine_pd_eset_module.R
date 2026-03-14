# =========================================================
# Module: combine_pd_eset
# =========================================================

# ---- BodyUI ----
combine_pd_esetBodyUI <- function(id, include_upload = TRUE) {
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
            uploadUI(ns("upload_pdata"))%>%
              helper(
                type    = "markdown",
                icon    = "question-circle",
                size    = "m",
                colour  = "#007bff",
                content = "demo_stad_pdata"
              )
          )
        },

        div(
          style = "text-align: center; margin-top: 10px;",
          actionButton(
            inputId = ns("run_combine_pd_eset"),
            label = "Run Analysis",
            class = "btn-primary"
          )
        )
      ),

      dataDownloadUI(ns("download"))
    ),

    column(
      width = 9,
      bs4Card(
        title = "Combined Data Preview",
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
combine_pd_esetServer <- function(id, external_eset = NULL) {
  moduleServer(id, function(input, output, session) {
    
    # external_eset 从上一步传入
    req(external_eset)
    eset_data <- external_eset
    
    # 用户上传 pdata
    pdata_data <- uploadServer("upload_pdata")

    combine_pd_eset_result <- reactiveVal(NULL)

    observeEvent(input$run_combine_pd_eset, {
      req(eset_data())
      req(pdata_data())

      withProgress(message = "Running combine_pd_eset analysis...", value = 0, {
        setProgress(0.2, message = "Reading input data...")

        eset  <- eset_data()
        pdata <- pdata_data()
        req(nrow(eset) > 0, nrow(pdata) > 0)

        combined_data <- tryCatch({
          if ("cluster" %in% colnames(eset)) {
            # 确保 pdata 中有 ID 列
            if (!"ID" %in% colnames(pdata)) {
              stop("pdata must contain an 'ID' column when using direct merge mode.")
            }
            
            # eset 中如果也有 ID 列，则转为行名
            if ("ID" %in% colnames(eset)) {
              eset <- tibble::column_to_rownames(eset, var = "ID")
            }
            
            eset <- tibble::rownames_to_column(as.data.frame(eset), var = "ID")
            
            setProgress(0.5, message = "Merging pdata and eset directly...")
            merge(eset, pdata, by = "ID", all.x = TRUE)
            
          } else {
            message("No 'cluster' column → using IOBR::combine_pd_eset")
            
            setProgress(0.5, message = "Preparing data for combine_pd_eset...")
            
            # 如果有 ID 列，则设为行名再转置
            if ("ID" %in% colnames(eset)) {
              eset <- tibble::column_to_rownames(eset, var = "ID") 
              eset <- t(eset)
            }
            
            combine_pd_eset(
              eset  = eset,
              pdata = pdata
            )
          }
        }, error = function(e) {
          setProgress(1, message = "Error")
          showNotification(
            paste("Error during merging pdata and eset:", e$message),
            type = "error",
            duration = 8
          )
          return(NULL)
        })

        req(combined_data)

        colnames(combined_data) <- gsub("\\.", "-", colnames(combined_data))
        combine_pd_eset_result(combined_data)

        setProgress(1, message = "Finished")
      })
    })

    dataTableServer("tbl_data", combine_pd_eset_result)

    dataDownloadServer(
      "download",
      data_reactive   = combine_pd_eset_result,
      filename_prefix = "combine_pd_eset_Results"
    )

    return(combine_pd_eset_result)
  })
}
