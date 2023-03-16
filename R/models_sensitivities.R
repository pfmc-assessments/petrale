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
  inputs.2019.001.003$dat$agecomp$Yr,
  inputs.2019.001.003$dat$agecomp$Ageerr
)

# which years have CAP Surface Pre-1990 or WDFW Surface?
table(inputs.2019.001.003$dat$agecomp$Yr[inputs.2019.001.003$dat$agecomp$Ageerr %in% c(6, 8)])

# use negative fleet to exclude CAP Surface Pre-1990 and WDFW Surface from likelihood
inputs.2019.001.003$dat$agecomp <-
  inputs.2019.001.003$dat$agecomp %>%
  dplyr::mutate(FltSvy = ifelse(Ageerr %in% c(6, 8), yes = -abs(FltSvy), no = FltSvy))
# did change work?
table(
  inputs.2019.001.003$dat$agecomp$FltSvy,
  inputs.2019.001.003$dat$agecomp$Ageerr
)

SS_write(inputs.2019.001.003, dir = "models/2019.001.003_no_surface")
run("models/2019.001.003_no_surface", exe = "ss_win")

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
summary_tmp <- SSsummarize(list(mod.2019.001.001, mod.2019.001.002))
SSplotComparisons(summary_tmp,
  legendlabels = c("2019 base model", "without commercial CPUE"),
  subplots = 2, print = TRUE, plot = FALSE, plotdir = "figures",
  filenameprefix = "comparison_2019.001_vs_2019.002_"
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

