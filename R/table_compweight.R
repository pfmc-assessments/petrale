#' Table of length and age comp Francis weights
#' (doesn't yet include other data types)
#'
#' @param output A list from [r4ss::SS_output].
#' @param caption A character string for the caption.
#' @param caption_extra Additional model-specific information pasted
#' at the end of the caption
#' @param label A character string with the label for the table.
#' No underscores allowed.
#' @author Kelli F. Johnson, Ian G. Taylor
#'
table_compweight <- function(output,
                             caption = paste(
                               "Data weightings applied to length and age compositions",
                               "according to the `Francis' method. `N obs.' refers to the number of unique",
                               "composition vectors included in the likelihood, `N input' and `N adj.'",
                               "refer to the sample sizes of those vectors before and after being adjusted",
                               "by the the weights."
                             ),
                             label = "table-compweight-base") {
  dplyr::bind_rows(
    .id = "Type",
    Length = output[["Length_Comp_Fit_Summary"]],
    Age = output[["Age_Comp_Fit_Summary"]]
  ) %>%
    # dplyr::mutate(Fleet = get_fleet(col = "label_long")[match(Fleet_name, get_fleet(col = "fleet"))]) %>%
    dplyr::mutate(Fleet = output[["FleetNames"]][Fleet]) %>%
    dplyr::mutate("Sum N adj." = mean_Nsamp_adj * Npos) %>%
    dplyr::select(
      Type,
      Fleet,
      "Francis" = Curr_Var_Adj,
      "N obs." = Npos,
      "Mean N input" = mean_Nsamp_in,
      "Mean N adj." = mean_Nsamp_adj,
      "Sum N adj."
    ) %>%
    dplyr::mutate(Francis = sprintf("%.3f", Francis)) %>% # round to 2 places
    dplyr::mutate_at(5:7, ~ sprintf("%.1f", .x)) %>% # round to 1 places
    kableExtra::kbl(
      row.names = FALSE,
      longtable = FALSE, booktabs = TRUE,
      label = label,
      caption = caption,
      format = "latex",
      linesep = ""
    )
}
