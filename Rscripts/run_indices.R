pak::pkg_install("kellijohnson-NOAA/indexwc")
data <- nwfscSurvey::pull_catch(
    common = "petrale sole",
    survey = "NWFSC.Combo"
)
# run(data, family = sdmTMB::delta_gamma())

library(indexwc)
indexwc::run(data, 
  family = sdmTMB::delta_lognormal(), 
  formula = indexwc:::lookup_formula("WCGBTS"))
#  run(data, family = sdmTMB::delta_gamma_mix())
#  run(data, family = sdmTMB::delta_lognormal_mix())