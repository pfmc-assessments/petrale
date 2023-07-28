# Projection table ----------------------------------------------------
# by Brian Langseth
# modified from https://github.com/pfmc-assessments/canary_2023/blob/main/code/figures_for_document.R

# tab = readr::read_csv('models/ "tables", "g_Projections_ES.csv")) |> data.frame()
# man = readr::read_csv(here('data/ACLs.csv')) |> data.frame()
# read gmt table
projection_table <- function(mod, file = "projections") {
  gmt_table <- read.csv("tables/GMT016-final specifications.csv", skip = 1)
  # read projection table
  tab <- read.csv(file.path(mod$inputs$dir, "tables_no_format/tables/g_Projections_ES.csv"))
  man <- gmt_table

  out <- cbind(
    tab$Year,
    c(round(man[man$YEAR %in% c(2023, 2024) & man$SPECIFICATION_TYPE == "OFL", "VAL"], 0), rep("-", 10)),
    c(round(man[man$YEAR %in% c(2023, 2024) & man$SPECIFICATION_TYPE == "ABC", "VAL"], 0), rep("-", 10)),
    c(round(man[man$YEAR %in% c(2023, 2024) & man$SPECIFICATION_TYPE == "ACL", "VAL"], 0), rep("-", 10)),
    c(round(tab[1:2, "ABC.Catch..mt."], 0), rep("-", 10)), # assumed removals
    c(rep("-", 2), round(tab[3:12, "Predicted.OFL..mt."], 0)),
    c(rep("-", 2), format(PEPtools::get_buffer(2023:2034, 0.5, 0.45)[-c(1, 2), 2])),
    c(rep("-", 2), round(round(tab[3:12, "Predicted.OFL..mt."], 2) * PEPtools::get_buffer(2023:2034, 0.5, 0.45)[-c(1, 2), 2], 0)),
    c(rep("-", 2), round(tab[3:12, "ABC.Catch..mt."], 0)),
    format(round(tab[, 5], 2)),
    format(round(tab[, 6], 3))
  )

  col_names <- c(
    "Year",
    "Adopted OFL (mt)",
    "Adopted ABC (mt)",
    "Adopted ACL (mt)",
    "Assumed removals (mt)",
    "OFL (mt)",
    "Buffer",
    "ABC",
    "ACL",
    "Spawning Output",
    "Fraction Unfished"
  )

  colnames(out) <- col_names

  write.csv(out, file.path("tables", paste0(file, ".csv")), row.names = FALSE)


  out.tex <- sa4ss::table_format(out,
    caption = "Projections of estimated OFL (mt), ABC (mt), resulting ACLs (mt) based on the 25-5 rule and applied buffers, and estimated spawning output in trillions of eggs, and spawning output relative to unfished for 2025-2034, with assumed removals in 2023 and 2024 based on recommended values from the Groundfish Management Team.",
    label = "project",
    row.names = FALSE,
    col_names = c("Year", "Adopted OFL (mt)", "Adopted ABC (mt)", "Adopted ACL (mt)", "Assumed catch (mt)", "OFL (mt)", "Buffer", "ABC", "ACL", "Spawn. Output", "Frac. Unfished"),
    # landscape = TRUE,
    # custom_width = TRUE,
    align = "l" # ,
    # col_to_adjust = c(5,10,11),
    # width = c("0.7cm", "0.7cm", "0.7cm")
  )
  writeLines(out.tex, file.path("documents/tex_tables", paste0(file, ".tex")))
}
