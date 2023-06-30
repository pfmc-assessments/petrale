#' Make table of petrale ageing error matrices
#'
#' adapted from code in 2019 assessment
#' https://github.com/chantelwetzel-noaa/Petrale_2019/blob/master/8_Tables.Rmd#L584-L668
#'
#' @author Chantel R. Wetzel, Ian G. Taylor
#' @example 
#' \dontrun{
#'   table_ageing_error(model) # all CAP types
#'   table_ageing_error(model, matrices = c(6, 5, 7)) # all WDFW types
#' }

table_ageing_error <- function(
    model,
    ages = 0:30,
    matrices = c(8, 3, 4, 2),
    ageerr_names = c(
      "no error", # 1
      "CAP BB", # 2
      "CAP Surface", # 3
      "CAP Combo", # 4
      "WDFW Combo", # 5
      "WDFW Surface", # 6
      "WDFW BB", # 7
      "CAP Surface Pre-1990" # 8
    )
  ) {
  Age.Error <- model$age_error_mean[ages + 1, "type1"]
  col_names <- "True age"
  for (imatrix in matrices) {
    Age.Error <- cbind(
      Age.Error,
      round(model$age_error_mean[ages, paste0("type", imatrix)], 1),
      round(model$age_error_sd[ages, paste0("type", imatrix)], 1)
      #sprintf("%01d", model$age_error_mean[ages, imatrix]),
      #sprintf("%01d", model$age_error_sd[ages, imatrix])
    )
    col_names <- c(col_names, paste(ageerr_names[imatrix], c("Mean", "SD")))
  }

  Age.Error <- as.data.frame(Age.Error)
  colnames(Age.Error) <- col_names

  return(Age.Error)
#   hlines <- c(-1, 0, nrow(Age.Error))
#   #  addtorow = list()
#   #  addtorow$pos = list(0,0)
#   #  addtorow$command = c(" & \\multicolumn{2}{c}{Break and Burn} &  \\multicolumn{2}{c}{Surface} & \\multicolumn{2}{c}{Combo} & \\multicolumn{2}{c}{Surface Pre-1990} \\\\\n",
#   #                      "True Age & Mean & SD & Mean &  SD  & Mean &  SD & Mean & SD \\\\\n" )


#   # Index of abundance summary, create table
#   AgeError.table <- xtable(Age.Error,
#     caption = c("Estimated ageing error vectors applied to ages read by the Cooperative Aging Project lab used in the assessment model."),
#     label = "tab:age_error1"
#   )
#   # Add alignment
#   # align(AgeError.table) = c('lccccccccc')

#   # Print index summary table
#   print(AgeError.table,
#     include.rownames = FALSE,
#     caption.placement = "top",
#     # add.to.row = addtorow,
#     sanitize.text.function = function(x) {
#       x
#     }
#   )
}
