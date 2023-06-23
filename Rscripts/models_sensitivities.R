#### sensitivities for report starting with model 2023.a034.001
# matching google sheet 
# https://docs.google.com/spreadsheets/d/1Qwp-jbNStukj9rFzI1c18ezf6BD-p7gjp5gH50mPkdQ/edit#gid=0

r4ss::copy_SS_inputs(dir.old = mod.34.1$inputs$dir,
  "models/2023.a034.301_DM", copy_exe = TRUE, overwrite = TRUE)
file.copy(file.path(mod.34.1$inputs$dir, "Report.sso"),
    "models/2023.a034.301_DM/Report.sso")
tune_comps(mod.34.1, dir = "models/2023.a034.301_DM", 
  option = "DM", niters_tuning = 1, init_run = FALSE)

dir <- "models/2023.a034.302_early_ages"
r4ss::copy_SS_inputs(dir.old = mod.34.1$inputs$dir,
  dir.new = dir, copy_exe = TRUE, overwrite = TRUE)
inputs <- SS_read(dir)
inputs$dat$agecomp <-
  inputs$dat$agecomp %>%
  dplyr::mutate(FltSvy = abs(FltSvy))
SS_write(inputs, dir = dir, overwrite = TRUE)
tune_comps(dir = dir, init_run = TRUE, niters_tuning = 1)

dir <- "models/2023.a034.603_all_main_devs"
r4ss::copy_SS_inputs(dir.old = mod.34.1$inputs$dir,
  dir.new = dir, copy_exe = TRUE, overwrite = TRUE)

# TODO: add plot or table of DM vs Francis weights

# command below is equivalent to mod.34.301 = SS_output("models/2023.a034.301")
get_mod(34,301)
get_mod(34,302)
mod_list <- list(mod.34.1, mod.34.301, mod.34.302)
mod_names <- c("Base", "Dirichlet-multinomial weights", "Include early age comps")
# make table
sens_make_table(num = 34, sens_mods = mod_list, plot = TRUE, 
  sens_type = "comp", sens_names = mod_names)



# plot(0, type = 'n', xlim = c(0,1.05), ylim = c(0,1.05), xaxs = 'i', yaxs = 'i')
# points(mod.34.1$Length_Comp_Fit_Summary$Curr_Var_Adj,
#   mod.34.301$Dirichlet_Multinomial_pars$"Theta/(1+Theta)"[1:4])
# points(mod.34.1$Age_Comp_Fit_Summary$Curr_Var_Adj,
#   mod.34.301$Dirichlet_Multinomial_pars$"Theta/(1+Theta)"[5:7])

