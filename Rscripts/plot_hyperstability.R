# plot showing commercial CPUE and catchability
if (FALSE) {
  p19 <- SS_output('models/2019.001.099_base_again/', 
    printstats = FALSE, verbose = FALSE)
  SS_plots(p19, plot = 24, pheight_tall = 8)
  
  # confirm relationship between Q and biomass
  (pow_S = mod.2019.001.099$parameters["Q_power_WinterS(3)", "Value"])
  # [1] -0.850908
  (baseQ_S = exp(mod.2019.001.099$parameters["LnQ_base_WinterS(3)", "Value"]))
  # [1] 0.2562581
  (pow_N = mod.2019.001.099$parameters["Q_power_WinterN(1)", "Value"])
  # [1] -0.115009
  (baseQ_N = exp(mod.2019.001.099$parameters["LnQ_base_WinterN(1)", "Value"]))
  # [1] 0.0008567795
  
  mod.2019.001.099$cpue %>% 
    dplyr::filter(Fleet == 3, Yr < 2004) %>% 
    dplyr::select(Vuln_bio, Eff_Q) %>% 
    plot()
  x <- seq(0, 5000, 20)
  lines(x, baseQ_S*x^pow_S)
}

# plot showing hyperstability
cols <- rich.colors.short(n = 5)[-1]
# make colors darker for better visibility
cols <- adjustcolor(cols, offset = c(-.3, -.3, -.3, 0))

#cols[2] = adjustcolor(cols[2], offset = c(0, -.5, -.5, 0))
png("figures/hyperstability_2019.png", 
  width = 8, height = 8, res = 300, units = "in")
par(mfrow = c(2, 2))

SSplotIndices(mod.2019.001.099, fleets = 3, subplots = 2, col3 = cols[3])
title(main = "South")
SSplotIndices(mod.2019.001.099, fleets = 1, subplots = 2, col3 = cols[1])
title(main = "North")

plot(0, type = 'n', xlab = "Vulnerable biomass",
  ylab = "Index", las = 1,
  xlim = c(0, 5000), ylim = c(0, 1.5), xaxs = 'i', yaxs = 'i')
lines(x, x * baseQ_S*x^pow_S, col = cols[3], lwd = 3)
mod.2019.001.099$cpue %>% 
  dplyr::filter(Fleet == 3, Yr < 2004) %>% 
  dplyr::select(Vuln_bio, Exp) %>% 
  points(pch = 16)
mod.2019.001.099$cpue %>% 
  dplyr::filter(Fleet == 3, Yr < 2004) %>% 
  dplyr::select(Vuln_bio, Obs) %>% 
  points(pch = 1)
title(main = "South (1987-2003)")
legend('topleft', pch = c(1, 16), c("Observed", "Expected"), bty = 'n')

plot(0, type = 'n', xlab = "Vulnerable biomass",
  ylab = "Index",
  xlim = c(0, 5000), ylim = c(0, 1.5), xaxs = 'i', yaxs = 'i')
lines(x, x * baseQ_N*x^pow_N, col = cols[1], lwd = 3)
mod.2019.001.099$cpue %>% 
  dplyr::filter(Fleet == 1, Yr < 2004) %>% 
  dplyr::select(Vuln_bio, Exp) %>% 
  points(pch = 16)
mod.2019.001.099$cpue %>% 
  dplyr::filter(Fleet == 1, Yr < 2004) %>% 
  dplyr::select(Vuln_bio, Obs) %>% 
  points(pch = 1)
title(main = "North (1987-2003)")
dev.off()
