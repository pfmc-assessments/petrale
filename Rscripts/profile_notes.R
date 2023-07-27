h_vec <- c(0.99, 0.95, 0.90, 0.85, 0.80, 0.75, 0.70, 0.65, 0.60)
h_dir <- "C:/SS/Petrale/Petrale2023/petrale/models/2023.a034.001/diags_steep_est"
h_para <- "SR_BH_steep"
profile(
  dir = h_dir,
  string = "steep",
  profilevec = h_vec,
  newctlfile = "petrale_control.ss",
  exe = "ss_win",
  extras = "-nohess",
  usepar = TRUE,
  parstring = "SR_parm[2]", # steepness
  verbose = TRUE,
  show_in_console = TRUE
)
h_mods <- SSgetoutput(h_dir, keyvec = 1:length(h_vec))
h_summary <- SSsummarize(h_mods)

nwfscDiag::get_summary(mydir = h_dir, 
 para = h_para,
 vec = h_vec,
 name = paste0("profile_", h_para),
 profilemodels = h_mods,
 profilesummary = h_summary
)
nwfscDiag::profile_plot(
  mydir = h_dir,
  para = h_para,
  rep = mod.34.1,
  profilesummary = h_summary
)

# M profile
M_para <- "NatM_uniform_Fem_GP_1"
M_dir_down <- "C:/SS/Petrale/Petrale2023/petrale/models/2023.a034.001/diags_profile_M2_down"
M_vec_down <- seq(0.14, 0.09, -0.01)
# profile going down from base
profile(
  dir = M_dir_down,
  string = "NatM_uniform_Fem",
  profilevec = M_vec_down,
  oldctlfile = "control.ss_new",
  newctlfile = "petrale_control.ss",
  exe = "ss_win",
  extras = "-nohess",
  usepar = FALSE,
  #parstring = "MGparm[1]", # female M
  verbose = TRUE,
  show_in_console = TRUE
)
# profile going up from base
M_para <- "NatM_uniform_Fem_GP_1"
M_vec_up <- seq(0.15, 0.20, 0.01)
M_dir_up <- "C:/SS/Petrale/Petrale2023/petrale/models/2023.a034.001/diags_profile_M2_up"
profile(
  dir = M_dir_up,
  string = "NatM_uniform_Fem",
  profilevec = M_vec_up,
  oldctlfile = "control.ss_new",
  newctlfile = "petrale_control.ss",
  exe = "ss_win",
  extras = "-nohess",
  usepar = FALSE,
  #parstring = "MGparm[1]", # female M
  verbose = TRUE,
  show_in_console = TRUE
)

for (ipar in seq_along(M_vec_up)) {
  file.copy(from = file.path(M_dir_up, paste0("Report", ipar, ".sso")), 
            to = file.path(M_dir_down, paste0("Report", ipar + length(M_vec_down), ".sso")),
            overwrite = FALSE)
}

M_mods <- SSgetoutput(M_dir_down, keyvec = 1:(length(M_vec_down) + length(M_vec_up)),
getcomp = FALSE)
M_mods[["MLE"]] <- mod.34.1
M_summary <- SSsummarize(M_mods)
M_vec <- M_summary$pars %>% 
  dplyr::filter(Label == M_para) %>% 
  dplyr::select(1:M_summary$n) %>%
  as.numeric()
# sort models
M_mods2 <- M_mods[order(M_vec)]
M_summary <- SSsummarize(M_mods2)
M_vec <- M_summary$pars %>% 
  dplyr::filter(Label == M_para) %>% 
  dplyr::select(1:M_summary$n) %>%
  as.numeric()

SSB_vs_M <- data.frame(M = M_vec, 
SSB_2023 = as.numeric(M_summary$quants[M_summary$quants$Label == "SSB_2023", 1:13]))
write.csv(SSB_vs_M, "tables/SSB_vs_M.csv", row.names = FALSE)


nwfscDiag::get_summary(mydir = M_dir_down, 
 para = M_para,
 vec = M_vec,
 name = paste0("profile_", M_para),
 profilemodels = M_mods,
 profilesummary = M_summary
)
nwfscDiag::profile_plot(
  mydir = M_dir_down,
  para = M_para,
  rep = mod_base,
  profilesummary = M_summary
)





h_profile_mods <- SSgetoutput(keyvec = 1:6, dirvec = "models/2023.a022.016_hess/h_profile")
h_profile_summary <- SSsummarize(h_profile_mods)
SSplotProfile(h_profile_summary,
  print = TRUE,
  plotdir = "models/2023.a022.016_hess/h_profile"
)
PinerPlot(h_profile_summary, component = "Length_like", profile.string = "steep")
PinerPlot(h_profile_summary, component = "Age_like", profile.string = "steep")
PinerPlot(h_profile_summary, component = "Surv_like", profile.string = "steep")

profile_plot_values <- function(summary, profile_string, value_string,
  add = FALSE) {
    # combine pars and quants into one data frame
  combined <- rbind(summary$pars %>% dplyr::select(-recdev),
                    summary$quants)
  # get labels for parameter or derived quantitiy
  profile_string <- grep(profile_string, combined$Label, value = TRUE)
  value_string <- grep(value_string, combined$Label, value = TRUE)
  message("plotting ", profile_string, " vs ", value_string)
  # get values
  x <- combined %>%
    dplyr::filter(Label == profile_string) %>%
    dplyr::select(dplyr::starts_with("replist")) %>%
    as.numeric()
  y <- combined %>%
    dplyr::filter(Label == value_string) %>%
    dplyr::select(dplyr::starts_with("replist")) %>%
    as.numeric()
  # make plot
  if (!add) {
	plot(x = x, y = y, xlab = profile_string, ylab = value_string, 
	  type = "n'")
  }
  lines(x = x, y = y, type = "o")
}
profile_plot_values(h_profile_summary, "steep", "Dead_Catch_MSY")
profile_plot_values(h_profile_summary, "SR_BH_steep", "NatM_uniform_Fem")
profile_plot_values(h_profile_summary, "SR_BH_steep", "NatM_uniform_Mal", add = TRUE)




## R0 was run with nwfscDiag, but 2 steps were replaced manually
R0_dir <- "C:/SS/Petrale/Petrale2023/petrale/models/2023.a034.001/diags_profile_SR_LN(R0)_prior_like_0"
R0_para <- "SR_LN(R0)"
R0_mods <- SSgetoutput(R0_dir, keyvec = 1:10, getcomp = FALSE)
R0_mods2 <- R0_mods[c(4:1,5:10)]
R0_summary <- SSsummarize(R0_mods2)
R0_vec <- R0_summary$pars %>% 
  dplyr::filter(Label == "SR_LN(R0)") %>% 
  dplyr::select(starts_with("replist")) %>%
  as.numeric()

nwfscDiag::get_summary(mydir = R0_dir, 
 para = R0_para,
 vec = R0_vec,
 name = paste0("profile_", R0_para),
 profilemodels = R0_mods,
 profilesummary = R0_summary
)

nwfscDiag::profile_plot(
  mydir = R0_dir,
  para = R0_para,
  rep = mod.34.1,
  profilesummary = R0_summary
)

#### sigmaR profile
sigmaR_vec <- seq(0.3, 0.8, 0.1)
sigmaR_dir <- "models/2023.a034.001/diags_profile_sigmaR"
sigmaR_para <- "SR_sigmaR"
profile(
  dir = sigmaR_dir,
  string = sigmaR_para,
  profilevec = sigmaR_vec,
  newctlfile = "petrale_control.ss",
  exe = "ss",
  extras = "-nohess",
  usepar = FALSE,
  verbose = TRUE,
  show_in_console = TRUE,
  skipfinished = FALSE
)
sigmaR_mods <- SSgetoutput(sigmaR_dir, keyvec = 1:length(sigmaR_vec))
sigmaR_summary <- SSsummarize(sigmaR_mods)

sigmaR_vec <- c(0.3, 0.4, 0.6, 0.7, 0.8)
sigmaR_mods <- SSgetoutput(dirvec = paste0(sigmaR_dir, "/sigmaR_", sigmaR_vec))
get_mod(34, 610)
sigmaR_mods2 <- list(
  mod.34.610, 
  sigmaR_mods[[1]], 
  sigmaR_mods[[2]], 
  mod.34.1, 
  sigmaR_mods[[3]],
  sigmaR_mods[[4]],
  sigmaR_mods[[5]])
  
sigmaR_sum <- SSsummarize(sigmaR_mods2)
# leaving out unconverged sigmaR = 0.8 
sigmaR_sum2 <- SSsummarize(sigmaR_mods2[1:6])
sigmaR_sum$pars[sigmaR_sum$pars$Label == "SR_sigmaR", 1] <- 0

main_devs_SD <- rep(NA, 7)
alt_sigma_R <- rep(NA, 7)
for (i in 1:7) {
  print(i)
  main_devs_SD[i] <- sigmaR_mods2[[i]]$sigma_R_info$SD_of_devs[1]
  alt_sigma_R[i] <- sigmaR_mods2[[i]]$sigma_R_info$alternative_sigma_R[1]
}

SSplotProfile(sigmaR_sum, 
  models = 2:7, 
  profile.label = "SigmaR", 
  profile.string = "sigmaR",
  print = TRUE,
  plotdir = "figures/diags_model34",
  legendloc = "top")

for(component in c("Length_like", "Age_like", "Surv_like")) {
  PinerPlot(sigmaR_sum, 
  models = 2:6, 
  profile.label = "SigmaR", 
  profile.string = "sigmaR",
  component = component,
  main = paste("Changes in", gsub("_", " ", component), "by fleet"),
  print = TRUE,
  plotdir = "figures/diags_model34",
  legendloc = "top")
  
  file.copy("figures/diags_model34/profile_plot_likelihood.png", 
    paste0("figures/diags_model34/sigma_R_profile_", component, ".png"))
}

SSplotComparisons(sigmaR_sum2, 
  plot = FALSE,
  print = TRUE, 
  plotdir = "figures/compare/sigmaR_profile3",
  legendlabels = c("No recdevs", paste0("sigmaR = ", seq(0.3, 0.7, 0.1))))
# repeat plot 11 with legend in a different spot
SSplotComparisons(sigmaR_sum2, 
  subplots = 11, legendloc = 'topleft',
  plot = FALSE,
  print = TRUE, 
  plotdir = "figures/compare/sigmaR_profile3",
  legendlabels = c("No recdevs", paste0("sigmaR = ", seq(0.3, 0.7, 0.1))))

png("figures/diags_model34/sigma_R_fig2.png", res = 300, units = 'in', width = 5, height = 5, pointsize = 10)
plot(as.numeric(sigmaR_sum$pars[sigmaR_sum$pars$Label == "SR_sigmaR", 1:6]),
  as.numeric(alt_sigma_R[1:6]), xlim = c(0,.9), ylim = c(0,.9),
  xaxs = 'i', yaxs = 'i', pch = 16, col = 2,
  xlab = "sigmaR", ylab = "Value")
legend("topleft", col = c(2, 4), pch = 16,
  legend = c("'Alternative sigmaR' suggested by tuning algorithm",
    "SD of 'main' recdevs (1959-2020)"), bty = 'n')
points(as.numeric(sigmaR_sum$pars[sigmaR_sum$pars$Label == "SR_sigmaR", 1:6]),
  as.numeric(main_devs_SD[1:6]), pch = 16, col = 4)
axis(1, at = seq(0.1, 0.7, 0.2))
axis(2, at = seq(0.1, 0.7, 0.2))
abline(0, 1, col = 2, lty = 2)
abline(v = 0.5, h = 0.5, col = 'grey', lty = 3)
dev.off()


