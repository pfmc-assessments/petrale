if (FALSE) {
  get_mod(34,10) # forecast recruits based on long-term average
  get_mod(34,11) # forecast recruits based on stock-recruit

  # make tables in CSV format
  SSexecutivesummary(mod.34.10, forecast_ofl = c(3763, 3563))
  SSexecutivesummary(mod.34.11, forecast_ofl = c(3763, 3563))
  # 
  save_loc <- file.path(mod.34.10$inputs$dir, "tex_tables")
  dir.create(save_loc)
  sa4ss::es_table_tex(
    dir = mod.34.10$inputs$dir,
    save_loc = save_loc
  )
  save_loc <- file.path(mod.34.11$inputs$dir, "tex_tables")
  dir.create(save_loc)
  sa4ss::es_table_tex(
    dir = mod.34.11$inputs$dir,
    save_loc = save_loc
  )
  SSexecutivesummary(mod.34.10, forecast_ofl = c(3763, 3563), 
    format = FALSE, plotfolder = file.path(mod.34.10$inputs$dir, "tables_no_format"))
  SSexecutivesummary(mod.34.11, forecast_ofl = c(3763, 3563), 
    format = FALSE, plotfolder = file.path(mod.34.11$inputs$dir, "tables_no_format"))

  # make alternative projection table (depends on unformatted exec summary table above)
  projection_table(mod.34.10)
  projection_table(mod.34.11)

  dir.create('figures/forecasts')
  SSplotCatch(mod.34.10, maxyr = 2035, forecastplot = TRUE, subplots = 5, 
    print = TRUE, plot = FALSE, plotdir = "figures/forecasts")
  # attempt to add landings on top of total catch
  #SSplotCatch(mod.34.10, maxyr = 2035, forecastplot = TRUE, subplots = 5, add = TRUE, fleetcols = 
  #  rich.colors.short(2, alpha = 0.5))

  SSplotComparisons(SSsummarize(list(mod.34.10, mod.34.11)),
    endyrvec = 2035, xlim = c(2000, 2036), print = TRUE, plot = FALSE, plotdir = "figures/compare/forecast", legendlabels = c("Forecast recruitment based on long-term mean", "Forecast recruitment based on stock-recruit curve"))

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

