# =========================================================
# Workflow: Mutation
# =========================================================
workflow_mutationUI <- function(id) {
  ns <- NS(id)

  bs4TabItem(
    tabName = "workflow_mutation",

    h3(HTML(
      "Mutation Workflow
      <span style='font-size:80%; color:#333;'>:
       Mutation Matrix Construction, Mutation–Signature Analysis, and Survival Analysis.</span>"
    )) %>%
      helper(
        type = "markdown",
        icon = "question-circle",
        size = "m",
        colour = "#007bff",
        content = "workflow_mutation_help"
      ),

    tabsetPanel(
      id = ns("mutation_tabs"),
      type = "pills",

      tabPanel(
        title = "Part 1 · Build Mutation Matrix",
        icon = icon("table"),
        make_mut_matrixBodyUI(ns("mod_matrix"), include_upload = TRUE)
      ),

      tabPanel(
        title = "Part 2 · Find Mutations",
        icon = icon("search"),
        find_mutationsBodyUI(ns("mod_find"), include_upload = FALSE)
      ),

      tabPanel(
        title = "Part 3 · Survival Analysis",
        icon = icon("heartbeat"),

        tabsetPanel(
          id = ns("tabs_part3"),
          type = "tabs",
          tabPanel("Upload Clinical Data", upload_clinical_dataBodyUI(ns("mod_upload_clinical_data"), include_upload = TRUE)),
          tabPanel("Batch Survival",  batch_survBodyUI(ns("mod_batch_surv"), include_upload = FALSE)),
          tabPanel("Survival Group", surv_groupBodyUI(ns("mod_surv_group"), include_upload = FALSE))
        )
      )
    )
  )
}


# =========================================================
# Server
# =========================================================

workflow_mutationServer <- function(id) {
  moduleServer(id, function(input, output, session) {

    res_matrix <- make_mut_matrixServer("mod_matrix")

    find_mutationsServer("mod_find", external_mut_matrix = res_matrix)

    # Clinical + WT/Mutated Mutation数据
    res_surv_group_data <- upload_clinical_dataServer(
      "mod_upload_clinical_data",
      external_mut_matrix = res_matrix
    )

    # 将WT/Mutated转换成Batch Survival需要的0/1
    res_batch_surv_data <- reactive({
      req(res_surv_group_data())

      data <- as.data.frame(res_surv_group_data(), check.names = FALSE)

      mutation_columns <- names(data)[vapply(data, function(x) {
        values <- unique(stats::na.omit(as.character(x)))
        length(values) > 0 && all(values %in% c("WT", "Mutated"))
      }, logical(1))]

      informative_columns <- mutation_columns[vapply(data[mutation_columns], function(x) {
        length(unique(stats::na.omit(as.character(x)))) > 1
      }, logical(1))]

      constant_columns <- setdiff(mutation_columns, informative_columns)

      if (length(constant_columns) > 0) data <- data[, !colnames(data) %in% constant_columns, drop = FALSE]

      data[informative_columns] <- lapply(data[informative_columns], function(x) ifelse(as.character(x) == "Mutated", 1L, 0L))

      data
    })

    batch_survServer("mod_batch_surv", external_eset = res_batch_surv_data)
    surv_groupServer("mod_surv_group", external_eset = res_surv_group_data)
  })
}