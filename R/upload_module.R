# =========================================================
# Module: File Upload
# =========================================================
uploadUI <- function(id, label = "Upload File") {
  ns <- NS(id)
  fileInput(
    inputId = ns("file"),
    label = label,
    multiple = FALSE,
    accept = NULL,
    width = "100%",
    buttonLabel = "Browse",
    placeholder = "Choose a file"
  )
}


uploadServer <- function(id) {
  moduleServer(id, function(input, output, session) { 
    reactive({
      req(input$file)
      ext <- tools::file_ext(input$file$name)

      withProgress(message = "Reading uploaded file...", value = 0, {
        setProgress(0.5, message = "Reading file...")
        
        data <- tryCatch({
          if (ext == "csv") {
            read.csv(input$file$datapath, header = TRUE, stringsAsFactors = FALSE, 
                     row.names = 1, check.names = FALSE, quote = "")
          } else if (ext == "tsv") {
            read.delim(input$file$datapath, header = TRUE, stringsAsFactors = FALSE, 
                       row.names = 1, check.names = FALSE, quote = "")
          } else if (ext %in% c("txt", "tab")) {
            read.table(input$file$datapath, header = TRUE, sep = "\t", 
                       row.names = 1, check.names = FALSE, quote = "")
          } else if (ext == "xlsx") {
            # 1. 读取 Excel 
            df <- readxl::read_excel(input$file$datapath)
            df <- as.data.frame(df)
            
            # 2. 检查第一列列名是否为 "rowname"
            if (colnames(df)[1] == "rowname") {
              rownames(df) <- df[, 1]     # 设为行名
              df <- df[, -1, drop = FALSE] # 删除该列
            }
            df
          }
        }, error = function(e) {
          showNotification(paste("Failed to read file:", e$message), type = "error")
          return(NULL)
        })

        setProgress(1, message = "File successfully loaded.")
        data 
      })
    })
  })
}


# uploadServer <- function(id) {
#   moduleServer(id, function(input, output, session) { 
#     reactive({
#       req(input$file)
#       ext <- tools::file_ext(input$file$name)

#       withProgress(message = "Reading uploaded file...", value = 0, {
#         setProgress(0.5, message = "Reading file...")
        
#         data <- tryCatch({
#           df <- NULL
          
#           # 1. 统一读取为 data.frame，暂时不设行名 (row.names = NULL)
#           if (ext == "csv") {
#             df <- read.csv(input$file$datapath, header = TRUE, stringsAsFactors = FALSE, 
#                            check.names = FALSE, row.names = NULL)
#           } else if (ext %in% c("tsv", "txt", "tab")) {
#             df <- read.table(input$file$datapath, header = TRUE, sep = "\t", stringsAsFactors = FALSE, 
#                              check.names = FALSE, row.names = NULL)
#           } else if (ext == "xlsx") {
#             if (!requireNamespace("readxl", quietly = TRUE)) stop("readxl package missing")
#             df <- readxl::read_excel(input$file$datapath)
#             df <- as.data.frame(df)
#           }
          
#           # 2. 【核心闭环逻辑】统一处理 rowname
#           # 检查第一列的名字是不是 "rowname"
#           if (!is.null(df) && ncol(df) > 0) {
#             first_col_name <- colnames(df)[1]
            
#             # 如果第一列叫 "rowname"
#             if (first_col_name == "rowname" || first_col_name == "X") {
              
#               # 检查是否有重复行名，防止报错
#               if (any(duplicated(df[, 1]))) {
#                 showNotification("Warning: 'rowname' column contains duplicates. Keeping default rownames.", type = "warning")
#               } else {
#                 # 把它变成行名
#                 rownames(df) <- df[, 1]
#                 # 删掉这一列
#                 df <- df[, -1, drop = FALSE]
#               }
#             }
#           }
#           df
#         }, error = function(e) {
#           showNotification(paste("Failed to read file:", e$message), type = "error")
#           return(NULL)
#         })

#         setProgress(1, message = "File successfully loaded.")
#         data 
#       })
#     })
#   })
# }


