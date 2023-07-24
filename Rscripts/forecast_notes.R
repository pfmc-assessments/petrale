if (FALSE) {
  # make tables in CSV format
  SSexecutivesummary(mod.34.10, forecast_ofl = c(3763, 3563))
  # 
  save_loc <- file.path(mod.34.10$inputs$dir, "tex_tables")
  dir.create(save_loc)
  sa4ss::es_table_tex(
    dir = mod.34.10$inputs$dir,
    save_loc = save_loc
  )

  SSexecutivesummary(mod.34.10, forecast_ofl = c(3763, 3563), 
    format = FALSE, plotfolder = file.path(mod.34.10$inputs$dir, "tables_no_format"))

  # this doesn't seem to be doing anything
  sa4ss::es_table_tex(
    # dir = mod.34.10$inputs$dir,
    dir = "models/2023.a034.010_fixed_forecast_catch/tables/",
    csv_name = "table_label_projection_modified.csv",
    digits = 0,
    save_loc = save_loc
  )



} # end if (FALSE) to help with sourcing the commands below


### make decision table
#devtools::load_all();
caption <- "Decision table with 10-year projections. 'Mgmt' refers to the three management scenarios (A) the default harvest control rule $P^* = 0.45$, (B) [TODO: fill in something here], and (C) [TODO: fill in something here]. In each case the 2023 and 2024 catches are fixed at the ACLs which have been set for that year with estimated fleet allocation provided  by the GMT. The alternative states of nature ('Low', 'Base', and 'High') are provided in the columns, with Spawning Output ('Spawn', in trillions of eggs) and Fraction of unfished ('Frac') provided for each state. The colors of catch and fraction unfished are relative with lighter colors representing lower values."

tab <- table_decision(
  caption = caption,
  label = "es-decision",
  list(mod.34.10, mod.34.10, mod.34.10),
  list(mod.34.10, mod.34.10, mod.34.10),
  list(mod.34.10, mod.34.10, mod.34.10)
)
writeLines(tab, "documents/tex_tables/decision_table.tex")

# kableExtra::save_kable(file = "documents/tex_tables/decision_table.tex")

