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
       Mutation Matrix Construction and Find Mutations Analysis.</span>"
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

      # Pill 1: Build Matrix
      tabPanel(
        title = "Build Mutation Matrix",
        make_mut_matrixBodyUI(ns("mod_matrix"), include_upload = TRUE)
      ),

      # Pill 2: Find Mutations
      tabPanel(
        title = "Find Mutations",
        find_mutationsBodyUI(ns("mod_find"), include_upload = FALSE)
      )
    )
  )
}

# =========================================================
# Server
# =========================================================
workflow_mutationServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    res_matrix <- make_mut_matrixServer("mod_matrix")
    find_mutationsServer("mod_find", external_mut_matrix = res_matrix)
  })
}

