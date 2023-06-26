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
# (or maybe just summarize DM weight values in text)

# plot(0, type = 'n', xlim = c(0,1.05), ylim = c(0,1.05), xaxs = 'i', yaxs = 'i')
# points(mod.34.1$Length_Comp_Fit_Summary$Curr_Var_Adj,
#   mod.34.301$Dirichlet_Multinomial_pars$"Theta/(1+Theta)"[1:4])
# points(mod.34.1$Age_Comp_Fit_Summary$Curr_Var_Adj,
#   mod.34.301$Dirichlet_Multinomial_pars$"Theta/(1+Theta)"[5:7])


# command below is equivalent to mod.34.301 = SS_output("models/2023.a034.301")
get_mod(34,301)
get_mod(34,302)
mod_list <- list(mod.34.1, mod.34.301, mod.34.302)
mod_names <- c("Base", "Dirichlet-multinomial weights", "Include early age comps")
# make table
sens_make_table(num = 34, sens_mods = mod_list, plot = TRUE, 
  sens_type = "comp", sens_names = mod_names)


# BIOLOGY sensitivities
get_mod(34,501)
get_mod(34,502)
#get_mod(34,503) # not bothering to include fecundity change because of changes in scale
get_mod(34,504)
get_mod(34,505)
mod_list <- list(mod.34.1, mod.34.501, mod.34.502, # mod.34.503, 
  mod.34.504, mod.34.505)
mod_names <- c("Base", 
"2019 weight-length", 
"2019 maturity", 
#"2019 fecundity", 
"Est. age-0 frac. female",
"Frac. female + no sex offset"
)
# make table
sens_make_table(num = 34, sens_mods = mod_list, plot = TRUE, 
  sens_type = "bio", sens_names = mod_names, 
  legendncol = 1) # petrale declines too far so bumps into label

# SELECTIVITY sensitivities
get_mod(34,401)
get_mod(34,402)
get_mod(34,403) 
get_mod(34,404)
mod_list <- list(mod.34.1, mod.34.401, mod.34.402, mod.34.403, 
  mod.34.404)
mod_names <- c("Base", 
"Triennial allowed to be dome-shaped", 
"WCGBTS allowed to be dome-shaped", 
"Fisheries allowed to be dome-shaped", 
"All fleets allowed to be dome-shaped"
)
# make table
sens_make_table(num = 34, sens_mods = mod_list, plot = TRUE, 
  sens_type = "sel", sens_names = mod_names, 
  legendncol = 1) # petrale declines too far so bumps into label

# INDEX sensitivities
get_mod(34,201)
get_mod(34,202)
get_mod(34,203) 
get_mod(34,204)
get_mod(34,205)
mod_list <- list(mod.34.1, mod.34.201, mod.34.202, mod.34.203, 
  mod.34.204, mod.34.205)
mod_names <- c("Base", 
"Separate Q for late triennial", 
"Separate Q and selex for late triennial", 
"No 2004 triennial index obs.", 
"Extra SD for WCGBTS estimated",
"Fisheries CPUE included"
)
# make table
sens_make_table(num = 34, sens_mods = mod_list, plot = TRUE, 
  sens_type = "index", sens_names = mod_names, 
  legendncol = 1) # petrale declines too far so bumps into label
