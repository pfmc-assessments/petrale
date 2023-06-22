# Update Q-Q plots after changes to {indexwc}
# https://github.com/kellijohnson-NOAA/indexwc/issues/11

# update {indexwc} after change to QQ plot function
pak::pak("kellijohnson-NOAA/indexwc")

# load output from petrale index from WCGBTS
dir_index <- "data-raw/indices/wcgbts/delta_lognormal/index/"
load(file.path(dir_index, "sdmTMB_save.RData"))

# make new QQ plots
qq_wcgbts <- indexwc::plot_qq(fit, file.path(dir_index, "qq_new_wcgbts.png"))

# load output from petrale index from triennial
dir_index <- "data-raw/indices/triennial/delta_lognormal_no_depth/index/"
load(file.path(dir_index, "sdmTMB_save.RData"))

# make new QQ plots
indexwc::plot_qq(fit, file.path(dir_index, "qq_new_tri.png"))

