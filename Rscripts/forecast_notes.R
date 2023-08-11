if (FALSE) {
  get_mod(34,10) # forecast recruits based on long-term average
  get_mod(34,11) # forecast recruits based on stock-recruit

  # make tables in CSV format
  SSexecutivesummary(mod.34.10, forecast_ofl = c(3763, 3563))
  SSexecutivesummary(mod.34.11, forecast_ofl = c(3763, 3563))
  SSexecutivesummary(mod.34.920, forecast_ofl = c(3763, 3563))
  SSexecutivesummary(mod.34.921, forecast_ofl = c(3763, 3563))
  # 
  save_loc0 <- file.path(mod.34.920$inputs$dir, "tex_tables")
  save_loc1 <- file.path(mod.34.921$inputs$dir, "tex_tables")
  dir.create(save_loc0)
  dir.create(save_loc1)
  sa4ss::es_table_tex(
    dir = mod.34.920$inputs$dir,
    save_loc = save_loc0
  )
  sa4ss::es_table_tex(
    dir = mod.34.920$inputs$dir,
    save_loc = save_loc1
  )
  
  SSexecutivesummary(mod.34.920, forecast_ofl = c(3763, 3563), 
    format = FALSE, plotfolder = file.path(mod.34.920$inputs$dir, "tables_no_format"))
  SSexecutivesummary(mod.34.921, forecast_ofl = c(3763, 3563), 
    format = FALSE, plotfolder = file.path(mod.34.921$inputs$dir, "tables_no_format"))

  # make alternative projection table (depends on unformatted exec summary table above)
  projection_table(mod.34.920)
  projection_table(mod.34.921, file = "projections_Pstar40")

  dir.create('figures/forecasts')
  SSplotCatch(mod.34.10, maxyr = 2035, forecastplot = TRUE, subplots = 5, 
    print = TRUE, plot = FALSE, plotdir = "figures/forecasts")
  # attempt to add landings on top of total catch
  #SSplotCatch(mod.34.10, maxyr = 2035, forecastplot = TRUE, subplots = 5, add = TRUE, fleetcols = 
  #  rich.colors.short(2, alpha = 0.5))

  SSplotComparisons(SSsummarize(list(mod.34.10, mod.34.11)),
    endyrvec = 2035, xlim = c(2000, 2036), print = TRUE, plot = FALSE, plotdir = "figures/compare/forecast", legendlabels = c("Forecast recruitment based on long-term mean", "Forecast recruitment based on stock-recruit curve"))

  # states of nature

  # 75% interval for M is too small
  get_mod(34,1)
  M_info <- mod.34.1$parameters["NatM_uniform_Fem_GP_1", c("Value", "Parm_StDev")]
  M_info$Value + c(-1.15, 1.15) * M_info$Parm_StDev
  # [1] 0.1285798 0.1550462
  qnorm(c(0.125, 0.875), mean = M_info$Value, sd = M_info$Parm_StDev)
  # [1] 0.1285758 0.1550502
  
  # 75% intervals for Spawning Output in 2023
  # approach used in 2019 update
  B2023_info <- mod.34.1$derived_quants["SSB_2023", c("Value", "StdDev")]
  B2023_info
  #            Value   StdDev
  # SSB_2023 7.68553 0.680777
  B2023_info$StdDev / B2023_info$Value
  # 0.08857906

  B2023_states <- B2023_info$Value + c(-1.15, 1.15) * B2023_info$StdDev
  B2023_states
  # [1] 6.902636 8.468424
  mod.34.1$OFL_sigma
  # [1] 0.1389856
  mod.34.1$Pstar_sigma
  # [1] 0.08840604

  ## symetric uncertainty
  B2023_states_v3 <- B2023_info$Value + c(-1.15, 1.15) * mod.34.1$Pstar_sigma * B2023_info$Value
  B2023_states_v3
  # [1] 6.904166 8.466894

  # lognormal uncertainty is better
  B2023_states_v2 <- qlnorm(p = c(.125, 0.875), 
    meanlog = log(B2023_info$Value),
    sdlog =  mod.34.1$Pstar_sigma)
  B2023_states_v2
  # [1] 6.942358 8.508258
  
  # finding M values that match the SSB values
  SSB_vs_M <- read.csv("tables/SSB_vs_M.csv")
  lm1 <- lm(SSB_2023 ~ M, data = SSB_vs_M)
  lm1$coefficients
  # (Intercept)           M 
  #    6.181161   10.604555 

  M_lo1 <- (B2023_states[1] - lm1$coefficients[1]) / lm1$coefficients[2]
  M_hi1 <- (B2023_states[2] - lm1$coefficients[1]) / lm1$coefficients[2]
  c(M_lo1, M_hi1)
  # (Intercept) (Intercept)
  #  0.06803451  0.21568682

  M_lo2 <- (B2023_states_v2[1] - lm1$coefficients[1]) / lm1$coefficients[2]
  M_hi2 <- (B2023_states_v2[2] - lm1$coefficients[1]) / lm1$coefficients[2]
  c(M_lo2, M_hi2)
  # 0.07178023  0.21944313 

  M_lo3 <- (B2023_states_v3[1] - lm1$coefficients[1]) / lm1$coefficients[2]
  M_hi3 <- (B2023_states_v3[2] - lm1$coefficients[1]) / lm1$coefficients[2]
  c(M_lo3, M_hi3)
  # (Intercept) (Intercept) 
  #  0.06817871  0.21554262
  base_M  <- mod.34.1$parameters["NatM_uniform_Fem_GP_1","Value"]

  png("figures/forecasts/M_states.png", width = 5, height =5, res = 300, units = 'in', pointsize = 10)
  plot(SSB_vs_M, xlim = c(0.06, 0.24), ylim = c(6.8, 8.6),
    axes = FALSE, xlab = "Female M", ylab = "Spawning output in 2023 (trillions of eggs)")
  abline(lm1, col = 2)
  points(round(M_lo2, 3), B2023_states_v2[1], pch = 16, cex = 2, col = 3)
  points(round(M_hi2, 3), B2023_states_v2[2], pch = 16, cex = 2, col = 3)
  points(base_M, B2023_info$Value, pch = 16, cex = 2, col = 4)
  axis(1, at = c(round(M_lo2, 3), round(base_M, 3), round(M_hi2, 3)))
  axis(1, at = c(0.09, 0.11, 0.16, 0.18, 0.20))
  axis(2)
  box()
  dev.off()


  get_mod(34, 44)
  get_mod(34, 25)
  SSplotComparisons(SSsummarize(list(mod.34.1, mod.34.910, mod.34.930)), 
    uncertainty = c(TRUE, FALSE, FALSE),
    legendlabels = c("Base M = 0.142", "Low state M = 0.072", "High state M = 0.219"),
    plotdir = "figures/forecasts", plot = FALSE, print = TRUE, filenameprefix = "states_v1_")
  SSplotComparisons(SSsummarize(list(mod.34.920, mod.34.910, mod.34.930)), 
    uncertainty = c(TRUE, FALSE, FALSE), endyrvec = 2034,
    legendlabels = c("Base M = 0.142", "Low state M = 0.072", "High state M = 0.219"),
    plotdir = "figures/forecasts", plot = FALSE, print = TRUE, filenameprefix = "states_Pstar45_")
  SSplotComparisons(SSsummarize(list(mod.34.921, mod.34.911, mod.34.931)), 
    uncertainty = c(TRUE, FALSE, FALSE), endyrvec = 2034,
    legendlabels = c("Base M = 0.142", "Low state M = 0.072", "High state M = 0.219"),
    plotdir = "figures/forecasts", plot = FALSE, print = TRUE, filenameprefix = "states_Pstar40_")

# buffers for Pstart = 0.45 and 0.40
PEPtools::get_buffer(years = 2023:2034, sigma = 0.5, pstar = 0.45)
PEPtools::get_buffer(years = 2023:2034, sigma = 0.5, pstar = 0.40)

} # end if (FALSE) to help with sourcing the commands below


if (FALSE) {
  devtools::load_all()
  devtools::load_all("c:/github/r4ss")
  get_mod(34, 910)
  get_mod(34, 911)
  get_mod(34, 920)
  get_mod(34, 921)
  get_mod(34, 930)
  get_mod(34, 931)
}

### make decision table
#devtools::load_all();
caption <- "Decision table with 10-year projections. 'Mgmt' refers to the three management scenarios (A) the default harvest control rule $P^* = 0.45$, (B) harvest control rule with a lower $P^* = 0.40$. In each case the 2023 and 2024 catches are fixed at the ACLs which have been set for that year with estimated fleet allocation provided  by the GMT. The alternative states of nature ('Low', 'Base', and 'High' as discussed in the text) are provided in the columns, with Spawning Output ('Spawn', in trillions of eggs) and Fraction of unfished ('Frac') provided for each state."

tab <- table_decision(
  caption = caption,
  label = "es-decision",
  list(mod.34.910, mod.34.920, mod.34.930),
  list(mod.34.911, mod.34.921, mod.34.931)
)
writeLines(tab, "documents/tex_tables/decision_table.tex")

# CSV version
tab <- table_decision(
  tex = FALSE,
  caption = caption,
  label = "es-decision",
  list(mod.34.910, mod.34.920, mod.34.930),
  list(mod.34.911, mod.34.921, mod.34.931)
)
write.csv(tab, "tables/decision_table.csv", row.names = FALSE)
