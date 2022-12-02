## this R script contains info on model changes from 2019 base to 2021 models

# assumes the working directory is wherever the "petrale" repository 
# has been cloned from github

# read 2019 SS3 input files into R
base2019 <- r4ss::SS_read("models/2019.001.001_base")
# copy 2019 model inputs to new list object
base2021 <- base2019
# update ending year
base2021$dat$endyr <- 2022
# add placeholder catch values
base2021$dat$catch <- rbind(base2021$dat$catch, 
  data.frame(year = 2019:2022, seas = 7, fleet = 4, catch = 10, catch_se = 0.01))
# rename data and control files
base2021$start$datfile <- "petrale_data.ss"
base2021$start$ctlfile <- "petrale_control.ss"

# write modified files for initial 2021 model
r4ss::SS_write(base2021, dir = "models/2021.001.001_new_endyr", overwrite = TRUE)
# run the model
r4ss::run("models/2021.001.001_new_endyr", exe = "ss_win", skipfinished = FALSE)

# read output from new base model
mod.2021.001.001 <- r4ss::SS_output("models/2021.001.001_new_endyr")
# create executive summary tables
r4ss::SSexecutivesummary(mod.2021.001.001)