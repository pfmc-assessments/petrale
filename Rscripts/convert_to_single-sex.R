## converting a petrale model from 2-sex to 1-sex
## code by Kiva Oken and Ian Taylor

# read model
mod <- SS_read('models/2023.a034.011_forecast_SR')
# remove male mortality and growth parameters
mod$ctl$MG_parms <- mod$ctl$MG_parms[-grep('Mal', rownames(mod$ctl$MG_parms)),]
# remove specification of male selectivity
mod$ctl$size_selex_types$Male <- 0
# remove male selectivity parameters
mod$ctl$size_selex_parms <- mod$ctl$size_selex_parms[-grep('MalOff', rownames(mod$ctl$size_selex_parms)),]
# change data file settings
mod$dat$Nsexes <- 1
# assign length comps to combined sex
mod$dat$lencomp$sex <- 0
# remove male columns from length comps
mod$dat$lencomp <- select(mod$dat$lencomp, -(m12:m62))
# remove male rows from CAAL comps and assign female rows to combined sex
mod$dat$agecomp <- filter(mod$dat$agecomp, sex != 2) |>
  mutate(sex = 0) |>
  select(-(m1:m17))

# turn of use of starter file which no longer matches the number of parameters
mod$start$init_values_src <- 0

# write modified files
SS_write(mod, 'models/2024.a001.001_single-sex', overwrite = TRUE)