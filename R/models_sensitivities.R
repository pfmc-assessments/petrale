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
  "no error", #1
  "CAP BB", #2
  "CAP Surface", #3
  "CAP BB/Surface", #4
  "WDFW Combo", #5
  "WDFW Surface", #6
  "WDFW BB", #7
  "CAP Surface Pre-1990" #8
)

# which years have which types
table(inputs.2019.001.003$dat$agecomp$Yr,  
      inputs.2019.001.003$dat$agecomp$Ageerr)

# which years have CAP Surface Pre-1990 or WDFW Surface?
table(inputs.2019.001.003$dat$agecomp$Yr[inputs.2019.001.003$dat$agecomp$Ageerr %in% c(6,8)])

# use negative fleet to exclude CAP Surface Pre-1990 and WDFW Surface from likelihood
inputs.2019.001.003$dat$agecomp <- 
  inputs.2019.001.003$dat$agecomp %>%
  dplyr::mutate(FltSvy = ifelse(Ageerr %in% c(6,8), yes = -abs(FltSvy), no = FltSvy))
# did change work?
table(inputs.2019.001.003$dat$agecomp$FltSvy, 
      inputs.2019.001.003$dat$agecomp$Ageerr)

SS_write(inputs.2019.001.003, dir = "models/2019.001.003_no_surface")
run("models/2019.001.003_no_surface", exe = "ss_win")


# compare sensitivities
mod.2019.001.001 <- SS_output("models/2019.001.001_base", verbose = FALSE, printstats = FALSE)
mod.2019.001.002 <- SS_output("models/2019.001.002_no_cpue", verbose = FALSE, printstats = FALSE)
mod.2019.001.003 <- SS_output("models/2019.001.003_no_surface", verbose = FALSE, printstats = FALSE)

SSplotComparisons(SSsummarize(list(mod.2019.001.001, mod.2019.001.002, mod.2019.001.003)))

# look at plots for individual models
SS_plots(mod.2019.001.002)
SS_plots(mod.2019.001.003)