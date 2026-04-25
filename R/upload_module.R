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


uploadServer <- function(id, mode = c("auto", "pdata", "eset")) {
  mode <- match.arg(mode)

  moduleServer(id, function(input, output, session) {
    reactive({
      req(input$file)
      ext <- tolower(tools::file_ext(input$file$name))

      withProgress(message = "Reading uploaded file...", value = 0, {
        setProgress(0.5, message = "Reading file...")

        data <- tryCatch({
          # =========================================================
          # 1. 先完整读取文件
          # 不直接用 row.names = 1
          # =========================================================
          if (ext == "csv") {
            df <- read.csv(
              input$file$datapath,
              header = TRUE,
              stringsAsFactors = FALSE,
              check.names = FALSE
            )

          } else if (ext == "tsv") {
            df <- read.delim(
              input$file$datapath,
              header = TRUE,
              stringsAsFactors = FALSE,
              check.names = FALSE
            )

          } else if (ext %in% c("txt", "tab")) {
            df <- read.table(
              input$file$datapath,
              header = TRUE,
              sep = "\t",
              stringsAsFactors = FALSE,
              check.names = FALSE,
              quote = ""
            )

          } else if (ext == "xlsx") {
            df <- readxl::read_excel(input$file$datapath)
            df <- as.data.frame(df, stringsAsFactors = FALSE)

          } else {
            stop("Unsupported file format.")
          }

          req(ncol(df) >= 1)

          # =========================================================
          # 2. 判断第一列是否需要转为行名
          # =========================================================
          first_col_name <- colnames(df)[1]

          is_blank_like_name <- is.na(first_col_name) ||
            first_col_name == "" ||
            grepl("^\\.\\.\\.[0-9]+$", first_col_name)

          is_eset_like <- FALSE

          if (ncol(df) >= 2) {
            first_col <- df[[1]]
            other_df  <- df[, -1, drop = FALSE]

            # 其余列是否大多数可转为数值
            other_numeric_ratio <- mean(vapply(other_df, function(x) {
              x_num <- suppressWarnings(as.numeric(as.character(x)))
              mean(!is.na(x_num)) > 0.9
            }, logical(1)))

            # 第一列是否像 feature ID
            first_unique <- !anyDuplicated(first_col)
            first_char   <- is.character(first_col) || is.factor(first_col)

            is_eset_like <- first_char && first_unique && (other_numeric_ratio > 0.8)
          }

          # =========================================================
          # 3. 按 mode 处理
          # =========================================================
          if (mode == "eset") {
            rownames(df) <- df[[1]]
            df <- df[, -1, drop = FALSE]

          } else if (mode == "pdata") {
            # 完整保留，不做行名转换
            df <- df

          } else if (mode == "auto") {
            # 自动模式：空列名 + 数值矩阵风格 → 转行名
            if (is_blank_like_name || tolower(first_col_name) %in% c("rowname", "rownames", "gene", "symbol", "genesymbol", "probe", "probeid")) {
              if (is_eset_like) {
                rownames(df) <- df[[1]]
                df <- df[, -1, drop = FALSE]
              }
            }
          }

          df

        }, error = function(e) {
          showNotification(
            paste("Failed to read file:", e$message),
            type = "error"
          )
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
#           if (ext == "csv") {
#             read.csv(input$file$datapath, header = TRUE, stringsAsFactors = FALSE, 
#                      row.names = 1, check.names = FALSE)
#           } else if (ext == "tsv") {
#             read.delim(input$file$datapath, header = TRUE, stringsAsFactors = FALSE, 
#                        row.names = 1, check.names = FALSE)
#           } else if (ext %in% c("txt", "tab")) {
#             read.table(input$file$datapath, header = TRUE, sep = "\t", 
#                        row.names = 1, check.names = FALSE)
#           } else if (ext == "xlsx") {
#             # 1. 读取 Excel 
#             df <- readxl::read_excel(input$file$datapath)
#             df <- as.data.frame(df)
            
#             # 2. 检查第一列列名是否为 "rowname"
#             if (colnames(df)[1] == "rowname") {
#               rownames(df) <- df[, 1]     # 设为行名
#               df <- df[, -1, drop = FALSE] # 删除该列
#             }
#             df
#           }
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


