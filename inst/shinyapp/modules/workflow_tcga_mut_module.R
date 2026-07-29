# =========================================================
# Workflow: TCGA Mutation
# =========================================================

workflow_tcga_mutUI <- function(id) {
  ns <- NS(id)
  
  bs4TabItem(
    tabName = "workflow_tcga_mut",
    
    h3(HTML(
      "TCGA Mutation Workflow
      <span style='font-size:80%; color:#333;'>:
       Database-driven Mutation–Signature and Survival Analysis.</span>"
    )) %>%
      helper(
        type = "markdown",
        icon = "question-circle",
        size = "m",
        colour = "#007bff",
        content = "workflow_tcga_mut_help"
      ),
    
    tabsetPanel(
      id = ns("tcga_mutation_tabs"),
      type = "pills",
      
      tabPanel(
        title = "Part 1 · Data Preparation",
        icon = icon("database"),
        prepare_mutation_dataBodyUI(ns("mod_prepare"))
      ),
      
      tabPanel(
        title = "Part 2 · Find Mutations",
        icon = icon("search"),
        find_mutationsBodyUI(ns("mod_find"), include_upload = FALSE, include_signature_upload = FALSE)
      ),
      
      tabPanel(
        title = "Part 3 · Survival Analysis",
        icon = icon("heartbeat"),
        
        tabsetPanel(
          id = ns("tabs_part3"),
          type = "tabs",
          tabPanel("Batch Survival", batch_survBodyUI(ns("mod_batch_surv"), include_upload = FALSE)),
          tabPanel("Survival Group", surv_groupBodyUI(ns("mod_surv_group"), include_upload = FALSE))
        )
      )
    )
  )
}


# =========================================================
# Server
# =========================================================

workflow_tcga_mutServer <- function(id, pool) {
  moduleServer(id, function(input, output, session) {

    res_prepare <- prepare_mutation_dataServer("mod_prepare", pool = pool)

    res_signature_matrix <- reactive({ req(res_prepare()); res_prepare()$signature_matrix })
    res_mutation_matrix <- reactive({ req(res_prepare()); res_prepare()$mutation_matrix })
    res_batch_surv_data <- reactive({ req(res_prepare()); res_prepare()$batch_surv_data })
    res_surv_group_data <- reactive({ req(res_prepare()); res_prepare()$surv_group_data })

    find_mutationsServer("mod_find", external_mut_matrix = res_mutation_matrix, external_sig_matrix = res_signature_matrix)

    batch_survServer("mod_batch_surv", external_eset = res_batch_surv_data)
    surv_groupServer("mod_surv_group", external_eset = res_surv_group_data)
  })
}