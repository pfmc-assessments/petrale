#' Table of estimated time series
#'
#' Adapted from Lingcod 2021:
#' https://github.com/pfmc-assessments/lingcod/blob/main/R/table_ts.R
#'
#' @param output A list from [r4ss::SS_output].
#' @param caption A character string for the caption.
#' @param label A character string with the label for the table.
#' No underscores allowed.
#' @author Kelli F. Johnson, Ian G. Taylor
#' @examples
#' \dontrun{
#' table_pars(model)
#' }
table_ts <- function(
    output,
    caption = "Time series of population estimates for the base model.",
    label = "table-ts-base") {
  # to do - Could format the names
  smbiomassname <- paste0("Age-", output[["summary_age"]], "+ biomass (mt)")
  output[["sprseries"]] %>%
    # dplyr::filter(!is.na(Deplete)) %>% # removes start year of model
    dplyr::filter(Era %in% c("VIRG", "TIME")) %>%
    dplyr::select(
      Yr, # 1
      SSB, # 2
      Deplete, # 3
      Bio_Smry, # 4
      Dead_Catch, # 5
      Recruits, # 6
      SPR_report, # 7
      Tot_Exploit
    ) %>% # 8
    dplyr::mutate(Recruits = Recruits / 1e3) %>% # convert recruits from 1000s to millions
    dplyr::mutate_at(c(4, 5), ~ sprintf("%.0f", .x)) %>% # round to 0 places
    dplyr::mutate_at(c(2, 6), ~ sprintf("%.2f", .x)) %>% # round to 2 places
    dplyr::mutate_at(c(3, 7:8), ~ sprintf("%.3f", .x)) %>% # round to 3 places
    # dplyr::mutate(Yr = ifelse(row_number() == 1, "Equil.", Yr)) %>%
    kableExtra::kbl(
      align = "r",
      row.names = FALSE, escape = FALSE,
      longtable = TRUE, booktabs = TRUE,
      format = "latex",
      caption = caption,
      label = label,
      col.names = c(
        "Year",
        "Spawning output (trillions of eggs)",
        "Fraction unfished",
        paste0("Age-", output[["summary_age"]], "+ biomass (mt)"),
        "Dead catch (mt)",
        "Age-0 recruits (1000s)",
        "1-SPR",
        "Exploitation rate"
      )
    ) %>%
    kableExtra::column_spec(column = 2:8, width = "1.5cm") %>%
    kable_styling_sa4ss(kable_captioncontinue(caption)) # %>%
  # kableExtra::add_header_above(c(" ", "Biomass (mt)" = 4, "Numbers" = 1, "Rate" = 3))
}
