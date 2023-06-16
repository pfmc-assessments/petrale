## this R script contains info on sensitivities
## conducted on the 2019 petrale model

# assumes the working directory is wherever the "petrale" repository
# has been cloned from github
library(magrittr)
library(r4ss)

# copy 2019 input files
inputs.2019.001.001 <- SS_read("models/2019.001.001_base")

# sens 2: remove fishery CPUE
inputs.2019.001.002 <- inputs.2019.001.001
# look at fleets for CPUE
table(inputs.2019.001.002$dat$CPUE$index)
#  1  3  5  6  7
# 23 23  5  4 16
inputs.2019.001.002$dat$fleetnames
# [1] "WinterN"  "SummerN"  "WinterS"  "SummerS"  "TriEarly" "TriLate"  "NWFSC"
inputs.2019.001.002$dat$CPUE %>%
  dplyr::mutate(index = ifelse(index %in% 1:4, yes = -index, no = index))

SS_write(inputs.2019.001.002, dir = "models/2019.001.002_no_cpue")
run("models/2019.001.002_no_cpue", exe = "ss_win")

########################################################################
### sens 2: remove fishery CPUE

inputs.2019.001.002 <- inputs.2019.001.001
# look at fleets for CPUE
table(inputs.2019.001.002$dat$CPUE$index)
#  1  3  5  6  7
# 23 23  5  4 16
inputs.2019.001.002$dat$fleetnames
# [1] "WinterN"  "SummerN"  "WinterS"  "SummerS"  "TriEarly" "TriLate"  "NWFSC"
inputs.2019.001.002$dat$CPUE <-
  inputs.2019.001.002$dat$CPUE %>%
  dplyr::mutate(index = ifelse(index %in% 1:4, yes = -index, no = index))

SS_write(inputs.2019.001.002, dir = "models/2019.001.002_no_cpue")
run("models/2019.001.002_no_cpue", exe = "ss_win")

########################################################################
### sens 3: remove early surface ages
inputs.2019.001.003 <- inputs.2019.001.001
inputs.2019.001.001 <- SS_read("models/2019.001.001_base")

inputs.2023.a024.004 <- SS_read("models/2023.a024.004_no_surface_ages/")
inputs.2023.a024.031 <- SS_read("models/2023.a024.031_min_sample_retuned_recdev2_h0.8_nosurface")
inputs <- inputs.2023.a024.031 # shortcut for summaries below
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

# which years have which types
table(
  inputs$dat$agecomp$Yr,
  inputs$dat$agecomp$Ageerr
)

# which years have CAP Surface Pre-1990 or WDFW Surface?
table(inputs$dat$agecomp$Yr[inputs$dat$agecomp$Ageerr %in% c(6, 8)])

# use negative fleet to exclude CAP Surface Pre-1990 and WDFW Surface from likelihood
inputs$dat$agecomp <-
  inputs$dat$agecomp %>%
  dplyr::mutate(FltSvy = ifelse(Ageerr %in% c(6, 8), yes = -abs(FltSvy), no = FltSvy))
# did change work?
table(
  inputs$dat$agecomp$FltSvy,
  inputs$dat$agecomp$Ageerr
)

# write modified files
SS_write(inputs, dir = inputs$dir, overwrite = TRUE)
run(inputs_dir, exe = "ss_win")

remove_ages <- function(dir1, dir2 = NULL, ageerror = c(6,8)) {
  inputs <- SS_read(dir1)
  
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

  # which years have which types
  table(
    inputs$dat$agecomp$Yr,
    inputs$dat$agecomp$Ageerr
  )
  
  # use negative fleet to exclude CAP Surface Pre-1990 and WDFW Surface from likelihood
  inputs$dat$agecomp <-
    inputs$dat$agecomp %>%
    dplyr::mutate(FltSvy = ifelse(Ageerr %in% c(6, 8), yes = -abs(FltSvy), no = FltSvy))
  
  # did change work?
  message("Ageerr by ")
  table(
    inputs$dat$agecomp$FltSvy,
    inputs$dat$agecomp$Ageerr
  )
  if (is.null(dir2)) {
    dir2 <- dir1
  }
  SS_write(inputs, dir = dir2)
}
########################################################################
### sens 4 & 5: sex ratio at birth

inputs.2019.001.004 <- inputs.2019.001.001
inputs.2019.001.005 <- inputs.2019.001.001

# change sex ratio parameter to be estimated for sens 4
inputs.2019.001.004$ctl$MG_parms["FracFemale_GP_1", "PHASE"] <- 5
# change sex ratio parameter to be fixed at 0.45 for sens 5
inputs.2019.001.005$ctl$MG_parms["FracFemale_GP_1", "INIT"] <- 0.45


SS_write(inputs.2019.001.004, dir = "models/2019.001.004_est_frac_female")
SS_write(inputs.2019.001.005, dir = "models/2019.001.005_frac_female0.45")
run("models/2019.001.004_est_frac_female", exe = "ss_win")
run("models/2019.001.005_frac_female0.45", exe = "ss_win")


# compare sensitivities
mod.2019.001.001 <- SS_output("models/2019.001.001_base", verbose = FALSE, printstats = FALSE)
mod.2019.001.002 <- SS_output("models/2019.001.002_no_cpue", verbose = FALSE, printstats = FALSE)
mod.2019.001.003 <- SS_output("models/2019.001.003_no_surface", verbose = FALSE, printstats = FALSE)
# base_again run has no changes, but additional output uncertainty for derived quants
mod.2019.001.099 <- SS_output("models/2019.001.099_base_again", verbose = FALSE, printstats = FALSE)

# plot comparison of spawning biomass with and without CPUE
summary_tmp <- SSsummarize(list(mod.2019.001.001, mod.2019.001.003))
SSplotComparisons(summary_tmp,
  legendlabels = c("2019 base model", "no surface ages"),
  subplots = 2, print = TRUE, plot = FALSE, plotdir = "figures",
  filenameprefix = "comparison_2019.001_vs_2019.003_"
)
SSplotComparisons(summary_tmp,
  legendlabels = c("2019 base model", "no surface ages"),
  filenameprefix = "comparison_2019.001_vs_2019.003_"
)

# confirm that survey likelihoods are zero for winter fisheries
summary_tmp$likelihoods_by_fleet %>% dplyr::filter(Label == "Surv_like")
#      model     Label      ALL  WinterN SummerN  WinterS SummerS TriEarly  TriLate    NWFSC
# 183      1 Surv_like -74.6638 -25.9897       0 -22.1759       0 -1.99662 -1.75590 -22.7456
# 1961     2 Surv_like -26.4443   0.0000       0   0.0000       0 -1.96481 -1.69893 -22.7805

# SSplotComparisons(SSsummarize(list(mod.2019.001.001, mod.2019.001.002, mod.2019.001.003)))

# look at plots for individual models
SS_plots(mod.2019.001.002)
SS_plots(mod.2019.001.003)

# estimated value is ~0.45, so two sensitivities are redundant
mod.2019.001.004 <- SS_output("models/2019.001.004_est_frac_female", verbose = FALSE, printstats = FALSE)
mod.2019.001.005 <- SS_output("models/2019.001.005_frac_female0.45", verbose = FALSE, printstats = FALSE)
SS_plots(mod.2019.001.004)

SSplotComparisons(SSsummarize(list(mod.2019.001.099, mod.2019.001.004, mod.2019.001.006)),
  legendlabels = c("2019 base", "est frac female", "est frac female + no selex offset")
)

########################################################################
### sens 6: remove sex-specific offset for WCGBTS selectivity
mod.2019.001.006 <- SS_output("models/2019.001.006_est_frac_female_no_WCGBTS_sex_selex")

########################################################################
### sens 7: remove priors on selectivity

inputs.2019.001.007 <- inputs.2019.001.001
# confirm that priors are only on M, h, and time-varying selectivity
test <- SS_parlines(ctlfile = file.path(inputs.2019.001.001$dir, "2019_petrale.ctl"))
test <- test %>% dplyr::filter(PR_type > 0 & PHASE > 0)
grepl("BLK", test$Label) %>% table()
# FALSE  TRUE
#     3    46
test$Label[!grepl("BLK", test$Label)]
# [1] "NatM_p_1_Fem_GP_1" "NatM_p_1_Mal_GP_1" "SR_BH_steep"

# set prior type to zero
inputs.2019.001.007$ctl$size_selex_parms_tv$PR_type <- 0
SS_write(inputs.2019.001.007,
  dir = "models/2019.001.007_no_selex_priors", overwrite = TRUE
)
run("models/2019.001.007_no_selex_priors", exe = "ss_win")

mod.2019.001.007 <- SS_output("models/2019.001.007_no_selex_priors")


# sens 13: add real seasons
inputs.2019.001.013 <- inputs.2019.001.001
dat <- inputs.2019.001.013$dat
dat$nseas <- 2
dat$months_per_seas <- c(4, 8)
# look at fleet names
dat$fleetnames
# [1] "WinterN"  "SummerN"  "WinterS"  "SummerS"  "TriEarly" "TriLate"  "NWFSC"

# assign catch to season 1 or 2
dat$catch <- dat$catch %>%
  dplyr::mutate(seas = ifelse(fleet %in% c(1, 3), yes = 1, no = 2))

# assign indices to month 1 or 7 (called season in the data.frame)
dat$CPUE <- dat$CPUE %>%
  dplyr::mutate(seas = ifelse(index %in% c(1, 3), yes = 1, no = 7))
# assign length comps to month 1 or 7 (called season in the data.frame)
dat$lencomp <- dat$lencomp %>%
  dplyr::mutate(Seas = ifelse(FltSvy %in% c(1, 3), yes = 1, no = 7))
dat$agecomp <- dat$agecomp %>%
  dplyr::mutate(Seas = ifelse(FltSvy %in% c(1, 3), yes = 1, no = 7))
dat$discard_data <- dat$discard_data %>%
  dplyr::mutate(Seas = ifelse(Flt %in% c(1, 3), yes = 1, no = 7))
dat$discard_data <- dat$discard_data %>%
  dplyr::mutate(Seas = ifelse(Flt %in% c(1, 3), yes = 1, no = 7))
dat$meanbodywt <- dat$meanbodywt %>%
  dplyr::mutate(Seas = ifelse(Fleet %in% c(1, 3), yes = 1, no = 7))

inputs.2019.001.013$dat <- dat
SS_write(inputs.2019.001.013,
  dir = "models/2019.001.013_real_seasons",
  overwrite = TRUE
)
run("models/2019.001.013_real_seasons", exe = "ss_win")
mod.2019.001.099 <- SS_output("models/2019.001.099_base_again", verbose = FALSE, printstats = FALSE)
mod.2019.001.013 <- SS_output("models/2019.001.013_real_seasons", verbose = FALSE, printstats = FALSE)

# plot comparison of spawning biomass with and without CPUE
summary_tmp <- SSsummarize(list(mod.2019.001.099, mod.2019.001.013))
SSplotComparisons(summary_tmp,
  legendlabels = c("2019 base model", "with true seasons"),
  subplots = 2, print = TRUE, plot = FALSE, plotdir = "figures",
  filenameprefix = "comparison_2019.099_vs_2019.013_"
)

# get ggplot colors used elsewhere for summer and winter
col.seas <- scales::hue_pal()(2) %>%
  scales::alpha(c(0.7, 1)) # add transparency for color on top

# fake the max age since SSplotAgeMatrix() doesn't include xlim argument
png("figures/seasonal_growth_2019.013.png",
  width = 6.5, height = 5, res = 300, units = "in"
)
SSplotAgeMatrix(mod.2019.001.013,
  option = 1,
  slices = 3, col.bars = col.seas[2], mainTitle = FALSE,
  labels = c(
    "Age", # 1
    "Length (cm)", # 2
    "True age", # 3
    "Observed age", # 4
    "for ageing error type", # 5
    "Distribution of", # 6
    "at"
  )
)
SSplotAgeMatrix(mod.2019.001.013,
  option = 1,
  slices = 7, col.bars = col.seas[1], col.grid = NA, add = TRUE
)
title(main = "Distribution of female length at age in mid-season")
legend("topleft", fill = col.seas, legend = c("Summer", "Winter"), bty = "n")
dev.off()



# # comparing models with all of Vlada's explorations
# 2023.009.001	2019 WCGBTS CAAL added
# 2023.009.002	Updated WA landings

# 2023.010.001	2023.009.001, plus Summer N selex is  mirrored Winter N
# 2023.010.002	Commercial landings lengths for  mirrored fleets summed up
# 2023.010.003	Discard lengths combined for Northern winter and Summer fleets - add weighted numners of fish from 2019 discard bio file
# 2023.010.004	No discard mean weight

# 2023.011.001	2023.010.002, plus Summer S selex is  mirrored to Winter S with N feets selex mirrored as 2023.010.002
# 2023.011.002	2023.010.003, Commercial length (landings and discard) summed up,Summer fleet is mirrorred
# 2023.011.003	New WA historical landings

modvec <- file.path(
  "models",
  c(
    "2023.009.001",
    "2023.009.002",
    "2023.010.001",
    "2023.010.002",
    "2023.010.003",
    "2023.010.004",
    "2023.011.001",
    "2023.011.002",
    "2023.011.003"
  )
)
mods <- SSgetoutput(dirvec = modvec)
sums <- SSsummarize(mods[c(1,3:9)])
SSplotComparisons(sums, legendlabels = modvec[-2])
SSplotComparisons(SSsummarize(mods[c(1,8)]), 
  legendlabels = c("separate selectivity",
                   "summer and winter mirrored"),
                   plotdir = "figures", filenameprefix = "comparison_009.001_vs_011.002_", 
                   plot = FALSE, print = TRUE, subplots = 1)

SSplotComparisons(SSsummarize(list(mod.2019.001.099, mod.2019.001.006)), 
  legendlabels = c("2019 base model",
                   "Estimated fraction female, no sex offset for WCGBTS selectivity"),
                   plotdir = "figures", filenameprefix = "comparison_2019.001.099_vs_2019.001.006_",
                   plot = FALSE, print = TRUE, subplots = 2)

# new commercial comps
mod.2023.007.001 <- SS_output("models/2023.007.001", verbose = FALSE, printstats = FALSE)
mod.2023.007.003 <- SS_output("models/2023.007.003", verbose = FALSE, printstats = FALSE)
SS_plots(mod.2023.007.003)

# testing impact of multiplying by 100 on DM sample sizes
# read input files
files.2023.a003.002 <- SS_read("models/2023.a003.002")
# copy to new list
files.2023.a003.003 <- files.2023.a003.002
# multiply length comp sample sizes by 10 
files.2023.a003.003$dat$lencomp <- files.2023.a003.002$dat$lencomp %>% 
  dplyr::mutate(Nsamp = 10*Nsamp)
# multiply age comp sample sizes by 10 
files.2023.a003.003$dat$agecomp <- files.2023.a003.002$dat$agecomp %>% 
  dplyr::mutate(Nsamp = ifelse(FltSvy != -7 | FltSvy != 7, 10*Nsamp, Nsamp))
# write files
SS_write(files.2023.a003.003, "models/2023.a003.005", overwrite = TRUE)
run("models/2023.a003.005", extras = "-nohess", exe = "ss_win")

# multiply age comp sample sizes by 10 with the exception of the CAAL data
# because the CAAL was hitting the upper bound and therefore won't match if the 
# sample size increases
files.2023.a003.004 <- files.2023.a003.003
files.2023.a003.004$dat$agecomp <- files.2023.a003.003$dat$agecomp %>% 
  dplyr::mutate(Nsamp = ifelse(FltSvy == -7 | FltSvy == 7, 10*Nsamp, Nsamp))
SS_write(files.2023.a003.004, "models/2023.a003.004", overwrite = TRUE)
run("models/2023.a003.004", extras = "-nohess", exe = "ss_win")

# selectivity blocks (DM)
copy_SS_inputs("models/2023.a003.002", "models/2023.a004.001_selex_blocks")
copy_SS_inputs("models/2023.c003.002", "models/2023.c004.001_selex_blocks")

# selectivity blocks (Francis)
copy_SS_inputs("models/2023.a002.002a", "models/2023.a002.004_selex_blocks", overwrite = TRUE)
copy_SS_inputs("models/2023.c002.002", "models/2023.c002.004_selex_blocks")
copy_SS_inputs("models/2023.a002.002a", "models/2023.a002.005_selex_blocks_males")
copy_SS_inputs("models/2023.c002.002", "models/2023.c002.005_selex_blocks_males")

files.a24 <- SS_read("models/2023.a002.004_selex_blocks")
files.c24 <- SS_read("models/2023.c002.004_selex_blocks")

ctl <- files.a24$ctl
ctl <- files.c24$ctl
# add time-varying parameters for ascending selex for fleets 1 and 3
ctl$size_selex_parms["SizeSel_P_3_WinterN(1)", "Block"] <- 1
ctl$size_selex_parms["SizeSel_P_3_WinterS(3)", "Block"] <- 1
ctl$size_selex_parms["SizeSel_P_3_WinterN(1)", "Block_Fxn"] <- 1
ctl$size_selex_parms["SizeSel_P_3_WinterS(3)", "Block_Fxn"] <- 1

#ctl$SzSel_Male_Peak

# get first set of each pair of years from the block design
block_years <- ctl$Block_Design[[1]] %>% .[1:length(.) %% 2 == 1]
new_tv_pars <- data.frame(LO = rep(-5, length(block_years)), 
  HI = 5, INIT = 0, PRIOR = 0, PR_SD = 99, PR_type = 0, PHASE = 6)

add_selex_pars <- function(ctl, new_tv_pars, preceding_tv_par_string) {
     # figure out which is the first part of the original table
     first_part <- 1:max(grep(preceding_tv_par_string, 
       rownames(ctl$size_selex_parms_tv), 
       fixed = TRUE))
    
    # bind everything together
    ctl$size_selex_parms_tv <- rbind(
     # first part of original table
     ctl$size_selex_parms_tv[first_part,], 
     # new stuff
     new_tv_pars, 
     # remainder of original table
     ctl$size_selex_parms_tv[-first_part, ] 
  )
  return(ctl)
}

# add blocks for fleet 1
rownames(new_tv_pars) <- paste0("SizeSel_P_3_fleet1_BLK1add_", selex_block_years)
ctl <- add_selex_pars(
  ctl = ctl, 
  new_tv_pars = new_tv_pars, 
  preceding_tv_par_string = "SizeSel_P_1_WinterN(1)"
)

# add blocks for fleet 3
rownames(new_tv_pars) <- paste0("SizeSel_P_3_fleet3_BLK1add_", selex_block_years)
ctl <- add_selex_pars(
  ctl = ctl, 
  new_tv_pars = new_tv_pars, 
  preceding_tv_par_string = "SizeSel_P_1_WinterS(3)"
)

# add modified control file back into list
files.a24$ctl <- ctl
files.c24$ctl <- ctl
# write all input files
SS_write(files.a24, dir = files.a24$dir, overwrite = TRUE)
SS_write(files.c24, dir = files.c24$dir, overwrite = TRUE)
# run model without estimation
run(files.a24$dir, extras = "-stopph 0 -nohess", exe = "ss_win", show_in_console = TRUE)


### M explorations for Chantel are in 2023.c002.003_M_shared

## pacfin explorations 24 May
p13.3 <- SS_output('models/2023.a013.003', printstats = FALSE, verbose = FALSE)
p13.4 <- SS_output('models/2023.a013.004', printstats = FALSE, verbose = FALSE)
p13.5 <- SS_output('models/2023.a013.005', printstats = FALSE, verbose = FALSE)
p13.6 <- SS_output('models/2023.a013.006', printstats = FALSE, verbose = FALSE)
for (mod in list(p13.4, p13.5, p13.6)) {
  SS_plots(mod)
}

copy_SS_inputs('models/2023.a013.004', 'models/2023.a013.007')
copy_SS_inputs('models/2023.a013.005', 'models/2023.a013.008')
copy_SS_inputs('models/2023.a013.006', 'models/2023.a013.009')

tune_comps(replist = p13.4, dir = 'models/2023.a013.007', niters_tuning = 1)
tune_comps(replist = p13.5, dir = 'models/2023.a013.008', niters_tuning = 1)
tune_comps(replist = p13.6, dir = 'models/2023.a013.009', niters_tuning = 1)

p13.7 <- SS_output('models/2023.a013.007', printstats = FALSE, verbose = FALSE)
p13.8 <- SS_output('models/2023.a013.008', printstats = FALSE, verbose = FALSE)
p13.9 <- SS_output('models/2023.a013.009', printstats = FALSE, verbose = FALSE)
for (mod in list(p13.7, p13.8, p13.9)) {
  SS_plots(mod)
}
p13.10 <- SS_output('models/2023.a013.010', printstats = FALSE, verbose = FALSE)
p13.11 <- SS_output('models/2023.a013.011', printstats = FALSE, verbose = FALSE)

# exploring sex ratios for small sizes
p22.7.files = r4ss::SS_read("models/2023.a022.007")

x1 <- p22.7.files$dat$agecomp %>% dplyr::filter(FltSvy == 1, Part == 2)
x2 <- p22.7.files$dat$agecomp %>% dplyr::filter(FltSvy == 2, Part == 2)
plot(x1$Yr, (x1$f4 + x1$f5) / (x1$f4 + x1$f5 + x1$m4 + x1$m5), pch = 16)
points(x2$Yr, (x2$f4 + x2$f5) / (x2$f4 + x2$f5 + x2$m4 + x2$m5), col = 2, pch = 16)
points(x1$Yr, x1$f5 / (x1$f5 + x1$m5), col = 4, pch = 16)
points(x2$Yr, x2$f5 / (x2$f5 + x2$m5), col = 3, pch = 16)
abline(h= 0.4)

p22.7 = r4ss::SS_output("models/2023.a022.007")
p22.8 = r4ss::SS_output("models/2023.a022.008_shared_L_Amin")
p22.10 = r4ss::SS_output("models/2023.a022.010_shared_L_Amin_plus_bins")
p22.11 = r4ss::SS_output("models/2023.a022.011_fix_h", printstats = FALSE, verbose = FALSE)
p22.12 = r4ss::SS_output("models/2023.a022.012_no_early_recdevs", printstats = FALSE, verbose = FALSE)

SStableComparisons(SSsummarize(list(p19,p22.7, p22.8, p22.10)), 
  names = c("Recr_Virgin", "R0",
    "steep", "NatM", "L_at_Amin", "L_at_Amax", "VonBert_K",
    "SSB_Virg", "Bratio_2023", "SPRratio_2022"))

# profile(dir = x, string = "steep", profilevec = c(0.7, 1.0, 0.1)))

p22.14 = SS_output('models/2023.a022.014_New_Francis/')
p23.1 = SS_output('models/2023.a023.001_env_index/')
p24.1 = SS_output('models/2023.a024.001_min_sample/')
p24.2 = SS_output('models/2023.a024.002_narrow_h_prior/')
p24.3 = SS_output('models/2023.a024.003_bias_adj/')
p24.4 = SS_output('models/2023.a024.004_no_surface_ages/')
p24.5 = SS_output('models/2023.a024.005_no_surface_ages_or_early_devs/')
p24.6 = SS_output('models/2023.a024.006_min_sample_sigmaR_0.3')
p24.7 = SS_output('models/2023.a024.007_min_sample_sigmaR_0.6')
p24.8 = SS_output('models/2023.a024.008_min_sample_autocorr')
p23.4 = SS_output("models/2023.a023.004_env_index_GLORYS_Q")
p24.9 = SS_output("models/2023.a024.009_recdev2/")
p24.10 = SS_output("models/2023.a024.010_recdev2_h0.8/")
p24.11 = SS_output("models/2023.a024.011_recdev2_h0.8_lambda0.5/")
p24.12 = SS_output("models/2023.a024.012_recdev2_h0.8_lambda0.01/")
p24.13 = SS_output("models/2023.a024.013_recdev2_h0.8_lambda0.2/")
p24.14 = SS_output("models/2023.a024.014_recdev2_h0.8_forecast/")
p24.15 = SS_output("models/2023.a024.015_recdev2_h0.8_lambda0.5_forecast/")
p24.16 = SS_output("models/2023.a024.016_min_sample_h0.8")
p24.17 = SS_output("models/2023.a024.017_min_sample_h0.9")
p24.18 = SS_output('models/2023.a024.018_min_sample_retuned/')

p24.19 = SS_output("models/2023.a024.019_min_sample_retuned_h0.8", verbose = FALSE)
p24.20 = SS_output("models/2023.a024.020_min_sample_retuned_recdev2", verbose = FALSE)
p24.21 = SS_output("models/2023.a024.021_min_sample_retuned_recdev2_h0.8", verbose = FALSE)
p24.22 = SS_output("models/2023.a024.022_min_sample_retuned_recdev2_h0.8_lambda0.5", verbose = FALSE)
p24.23 = SS_output("models/2023.a024.023_recdev2_h0.8_sigmaR0.56", verbose = FALSE)
p24.24 = SS_output("models/2023.a024.024_min_sample_retuned_recdev2_h0.8_sigmaR0.56", verbose = FALSE)
p24.25 = SS_output("models/2023.a024.025_min_sample_retuned_recdev2_narrow_prior", verbose = FALSE)
SSplotComparisons(SSsummarize(list(p24.18, p24.19, p24.20, p24.21, p24.22, p24.25)))
SStableComparisons(SSsummarize(list(p24.18, p24.19, p24.20, p24.21, p24.22, p24.25)))

p24.26 = SS_output("models/2023.a024.026_min_sample_retuned_recdev2_h0.8_sigmaR0.5", verbose = FALSE, printstats = FALSE)
p24.27 = SS_output("models/2023.a024.027_min_sample_retuned_recdev2_h0.8_sigmaR0.5_lambda0.5", verbose = FALSE, printstats = FALSE)
p24.28 = SS_output("models/2023.a024.028_min_sample_retuned_recdev2_h0.8_sigmaR_est", verbose = FALSE, printstats = FALSE)
p24.29 = SS_output("models/2023.a024.029_min_sample_retuned_recdev2_h0.8_sigmaR_est_lambda0.5", verbose = FALSE, printstats = FALSE)
p24.30 = SS_output("models/2023.a024.030_min_sample_retuned_recdev2_h0.8_sigmaR_est_noprior/", verbose = FALSE, printstats = FALSE)
p24.31 = SS_output("models/2023.a024.031_min_sample_retuned_recdev2_h0.8_nosurface/", verbose = FALSE, printstats = FALSE) 
p24.32 = SS_output("models/2023.a024.032_min_sample_retuned_recdev2_h0.8_sigmaR_0.5_nosurface/", verbose = FALSE, printstats = FALSE) 
p24.33 = SS_output("models/2023.a024.033_from_24.31_nosurface_tune/", verbose = FALSE, printstats = FALSE)
r4ss::tune_comps(replist = p24.33, dir = p24.33$inputs$dir, niters_tuning = 1)
p24.34 = SS_output("models/2023.a024.034_from_24.31_nosurface_tune_sigmaR_0.5/", verbose = FALSE, printstats = FALSE)
r4ss::tune_comps(replist = p24.34, dir = p24.34$inputs$dir, niters_tuning = 1)

xsum = SSsummarize(list(p24.20, p24.21, p24.22, p24.24, p24.26, p24.27, p24.28, p24.29))
xsum = SSsummarize(list(p24.20, p24.21, p24.22, p24.24, p24.26, p24.28))
xsum = SSsummarize(list(p24.21, p24.22, p24.25, p24.26))
xsum = SSsummarize(list(p24.21, p24.22, p24.25, p24.26, p24.30, p24.31))

p23.3 = SS_output('models/2023.a023.003_env_index_GLORYS')
p23.5 = SS_output('models/2023.a023.005_env_index_GLORYS_shifted_2022/')
p23.6 = SS_output('models/2023.a023.006_env_index_GLORYS_shifted_2023/')

tune_comps(dir = "models/2023.a024.018_min_sample_retuned", niters_tuning=2, extras = "-nohess")

sum1 = SSsummarize(list(p24.1, p24.16, p24.9, p24.10, p24.11))
SSplotComparisons(sum1, legendlabels = 
  c("recdev 1, h = ~1.0", 
    "recdev 1, fix h = 0.8",
    "recdev 2, h = ~1.0", 
    "recdev 2, fix h = 0.8", 
    "recdev 2, fix h = 0.8, lambda = 0.5"
    ), plotdir = 'figures', filenameprefix = "test_", print = TRUE) #, xlim = c(1960, 2020))

p24.1$maximum_gradient_component
p24.16$maximum_gradient_component
p24.9$maximum_gradient_component
p24.10$maximum_gradient_component
p24.11$maximum_gradient_component

SStableComparisons(sum1)
