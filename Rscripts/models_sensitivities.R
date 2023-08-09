#### sensitivities for report starting with model 2023.a034.001
# matching google sheet 
# https://docs.google.com/spreadsheets/d/1Qwp-jbNStukj9rFzI1c18ezf6BD-p7gjp5gH50mPkdQ/edit#gid=0

# Dirichlet-multinomial likelihood
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

# No special project samples from Oregon
# Created by temporarily modifying the Rscripts\process_pacfin_bds.R
# to change dir from "data-raw" to "data-raw_no_earlyOR"
# and keep_sample_type = c("M", "S") to keep_sample_type = c("M")

bdsages <- read.csv("data-raw_no_earlyOR/pacfin/forSS_annual/Age_for_SS3_30.Jun.2023_data_from_08.May.2023.csv")
files <- SS_read(mod.34.1$inputs$dir)
# compare tables
table(files$dat$agecomp$FltSvy, files$dat$agecomp$Ageerr)
  #      2   3   4   5   6   7   8
  # -4  16   0   0   0   0   3   0
  # -2   0   0   0   0   0   0  19
  # -1   0   0   0   0  31   0   3
  # 1   18   5  18   2   0  12   0
  # 2   15   0   3   0   0   0   0
  # 4  559   0   0   0   0 105   0
table(bdsages$fleet, bdsages$ageErr)
  #    2  3  4  5  6  7  8
  # 1 18  4 12  2 31 12  0
  # 2 15  0  3  0  0  0 19

# adding revised age comp to data file
names(bdsages) <- names(files$dat$agecomp)
files$dat$agecomp <- rbind(bdsages, files$dat$agecomp[!abs(files$dat$agecomp$FltSvy) %in% 1:2,])

# names of ageing error definitions
# (from models\2019.001.001_base\2019_petrale.dat)
ageerr_names <- c(
  "no error", # 1
  "CAP BB", # 2
  "CAP Surface", # 3
  "CAP BB/Surface", # 4
  "WDFW Combo", # 5
  "WDFW Surface", # 6
  "WDFW BB", # 7
  "CAP Surface Pre-1990" # 8
)
# use negative fleet to exclude CAP Surface Pre-1990 and WDFW Surface from likelihood
files$dat$agecomp <-
  files$dat$agecomp %>%
  dplyr::mutate(FltSvy = ifelse(Ageerr %in% c(6, 8), yes = -abs(FltSvy), no = FltSvy))
dir <- "models/2023.a034.303"
SS_write(files, dir)




# all main recdevs
dir <- "models/2023.a034.603_all_main_devs"
r4ss::copy_SS_inputs(dir.old = mod.34.1$inputs$dir,
  dir.new = dir, copy_exe = TRUE, overwrite = TRUE)


# plot(0, type = 'n', xlim = c(0,1.05), ylim = c(0,1.05), xaxs = 'i', yaxs = 'i')
# points(mod.34.1$Length_Comp_Fit_Summary$Curr_Var_Adj,
#   mod.34.301$Dirichlet_Multinomial_pars$"Theta/(1+Theta)"[1:4])
# points(mod.34.1$Age_Comp_Fit_Summary$Curr_Var_Adj,
#   mod.34.301$Dirichlet_Multinomial_pars$"Theta/(1+Theta)"[5:7])


# command below is equivalent to mod.34.301 = SS_output("models/2023.a034.301")
get_mod(34,301)
get_mod(34,302)
get_mod(34,303)
mod_list <- list(mod.34.1, mod.34.301, mod.34.302, mod.34.303)
mod_names <- c("Base", "Dirichlet-multinomial weights", 
  "Include early age comps", "Exclude early Oregon 'special project' samples")
# make table
sens_make_table(num = 34, sens_mods = mod_list, plot = TRUE, 
  sens_type = "comp", sens_names = mod_names, uncertainty = 1, legendloc = "topright")


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
#get_mod(34,201)
get_mod(34,202)
get_mod(34,401)
get_mod(34,203) 
get_mod(34,204)
get_mod(34,205)
mod_list <- list(mod.34.1, 
  #mod.34.201, 
  mod.34.202,
  mod.34.202, mod.34.203, 
  mod.34.204, mod.34.205)
mod_names <- c("Base", 
#"Separate Q for late triennial", 
"Separate Q and selex for late triennial", 
"Allow triennial selex to be dome-shaped",
"No 2004 triennial index obs.", 
"Extra SD for WCGBTS estimated",
"Fisheries CPUE included"
)
# make table
sens_make_table(num = 34, sens_mods = mod_list, plot = TRUE, 
  sens_type = "index", sens_names = mod_names, 
  legendncol = 1) # petrale declines too far so bumps into label


# RECRUITMENT sensitivities
get_mod(34,602)
get_mod(34,603) 
get_mod(34,606)
mod_list <- list(mod.34.1, mod.34.606, mod.34.602, mod.34.603)
mod_names <- c("Base", 
"Environmental index", 
"Zero-centered recdevs", 
"All recdevs in 'main' period"
)
SSplotComparisons(SSsummarize(list(mod.34.1, mod.34.606)),
  print = TRUE, plotdir = "figures/compare/Comparison plots - recruitment",
  indexfleets = 4, legendlabels = c("Base model", "with env. index"))

rbind(
  mod.34.1$likelihoods_by_fleet %>% 
    dplyr::filter(Label == "Surv_like") %>%
    dplyr::select(Triennial, WCGBTS),
  mod.34.606$likelihoods_by_fleet %>% 
    dplyr::filter(Label == "Surv_like" ) %>%
    dplyr::select(Triennial, WCGBTS)
)
#   Triennial   WCGBTS
# 1  -4.98340 -31.8745
# 2  -4.64211 -29.8632

# make table
sens_make_table(num = 34, sens_mods = mod_list, plot = TRUE, 
  sens_type = "recruit", sens_names = mod_names, 
  uncertainty = 1, # only show uncertainty for the base model
  legendncol = 1) # petrale declines too far so bumps into label

# plot of recdevs with and without env. index
mod.temp <- mod.34.606
# shift year value so the points don't overlap
mod.temp$cpue$Yr <- mod.temp$cpue$Yr - 0.3
png("figures/diags_model34/env_index_fit.png",
  res = 300, width = 7, height = 6, units = "in", pointsize = 10)
SSplotComparisons(SSsummarize(list(mod.34.1, mod.34.606)), 
  subplots = 12, xlim = c(1984.5, 2023.5), legend = FALSE,
  col = c("blue", "green3"), new = FALSE)
legend("bottomleft", pch = c(21,1,2), lty = 1, pt.lwd = 2,
  col = c("black", "blue", "green3"),
  pt.bg = "white", bty = "n",
  legend = c("Env. index observations", "Base model recruit devs", "Recruit devs fit to index"))
SSplotIndices(mod.temp, subplots = 1, add = TRUE)
axis(1, at = seq(1985, 2015, by = 10))
dev.off()

# CANADIAN sensitivities
get_mod(34,701)
get_mod(34,702) 
get_mod(34,703)
mod_list <- list(mod.34.1, mod.34.701, mod.34.702, mod.34.703)
mod_names <- c("Base", 
"Canadian index added", 
"Canadian catches added", 
"Canadian catches and index added"
)
# make table
sens_make_table(num = 34, sens_mods = mod_list, plot = TRUE, 
  sens_type = "Canada", sens_names = mod_names, 
  uncertainty = 1, # only show uncertainty for the base model
  legendncol = 1) # petrale declines too far so bumps into label

SSplotComparisons(SSsummarize(mod_list), subplots = 11, indexfleets = 5)
SSplotIndices(mod.34.701, subplot = 9, 
  fleetcols = c("blue", "red", "orange", "green3", "purple"),
  print = TRUE, plotdir = 'figures')

# STAR sensitivities
get_mod(34, 506) # male MGparm offsets
get_mod(34, 608) # estimate h (MLE = 0.961403)

