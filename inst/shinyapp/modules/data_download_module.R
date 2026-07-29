# =========================================================
# Module: Data Download (with TXT option)
# =========================================================
dataDownloadUI <- function(id) {
  ns <- NS(id)
  bs4Card(
    title = "Data Download",
    status = "gray",
    headerBorder = TRUE,
    collapsible = TRUE,
    solidHeader = TRUE,
    width = 12,
    maximizable = TRUE,
    
    selectInput(
      inputId = ns("format"),
      label = "Download Format",
      choices = c("CSV" = "csv", "TSV" = "tsv", "TXT" = "txt", "Excel" = "xlsx"),
      selected = "txt"
    ),
    
    div(
      style = "text-align: center; margin-top: 10px;",
      downloadButton(
        outputId = ns("download"),
        label = "Download Results",
        class = "btn-primary",
        icon = icon("download")
      )
    )
  )
}


dataDownloadServer <- function(id, data_reactive, filename_prefix = "result") {
  moduleServer(id, function(input, output, session) {
    output$download <- downloadHandler(
      filename = function() {
        paste0(filename_prefix, ".", input$format)
      },
      content = function(file) {
        req(data_reactive())
        x <- data_reactive()
        
        # 1. 检查行名是否只是普通的序号 (1, 2, 3...)
        is_seq_rownames <- isTRUE(all.equal(rownames(x), as.character(1:nrow(x))))
        
        # 2. 准备导出的数据框
        if (is_seq_rownames) {
          # 如果是序号，直接导出内容，不要行名
          out_data <- x
        } else {
          # 如果是有意义的行名(如基因名)，将其转为名为 "rowname" 的第一列
          out_data <- cbind(rowname = rownames(x), x)
        }
        
        # --- [核心逻辑] 根据格式写入 ---
        if (input$format == "csv") {
          write.table(out_data, file, sep = ",", quote = FALSE, 
                      row.names = FALSE, # 注意：行名已经处理进数据了，这里设为 FALSE
                      na = "", fileEncoding = "UTF-8")
          
        } else if (input$format %in% c("tsv", "txt")) {
          write.table(out_data, file, sep = "\t", quote = FALSE, 
                      row.names = FALSE, 
                      na = "", fileEncoding = "UTF-8")
          
        } else if (input$format == "xlsx") {
          # 需要安装 writexl 包: install.packages("writexl")
          if (!requireNamespace("writexl", quietly = TRUE)) {
            showNotification("Package 'writexl' is required for Excel export.", type = "error")
            return(NULL)
          }
          writexl::write_xlsx(out_data, path = file)
        }
      }
    )
  })
}
