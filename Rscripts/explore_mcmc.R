# Script by Kiva Oken 
# https://raw.githubusercontent.com/okenk/Assessment_Class/main/code/mcmc_example.R

# Must put ss3 executable in "path" directory with other model files
# There is no way around this given how adnuts is currently written

# In addition to standard input files, you must run the model in MLE mode
# within the "path" directory and have a positive definite hessian

# I *think* the hessian is only necessary for the adnuts wrapper functions,
# since many of the diagnostic functions compare MLE and MCMC output.
# You *should* be able to run MCMC from the command line without a hessian.

# pak::pkg_install('Cole-Monnahan-NOAA/adnuts')

# model to explore
dir_model <- "models/2023.a024.001_min_sample"
dir_model <- "models/2023.a024.010_recdev2_h0.8"
dir_model <- "models/2019.001.001_base"
dir_model <- "models/2023.a024.018_min_sample_retuned"
dir_model <- "models/2023.a024.021_min_sample_retuned_recdev2_h0.8"
dir_adnuts <- file.path(dir_model, "adnuts_")
#dir_adnuts <- file.path(dir_model, "adnuts2e6")
# copy everything to a folder within the model directory
dir.create(dir_adnuts)
R.utils::copyDirectory(dir_model, dir_adnuts, recursive = FALSE)
# re-run to get covar
tictoc::tic()
run(dir_adnuts, skipfinished = FALSE)
# re-run with mcmc to revise stuff (not sure if this makes a difference)
run(dir_adnuts, extras = "-mcmc 100 -hbf", skipfinished = FALSE)
tictoc::toc()
tictoc::tic()
fit <- adnuts::sample_rwm(model = 'ss', # this is the name of the executable
                          path = dir_adnuts, # directory with executable, input file, MLE output files (including covariance)
                          iter = 3e5, # 2e5, # 2e6, # 200000,
                          thin = 100, # thin to save memory, could try not
                          chains = 3)
# By default the first 50% is burn in.
tictoc::toc()
save(fit, file = file.path(dir_adnuts, "fit_3e5_11June.Rdata"))

# get parameter names
model <- r4ss::SS_output(dir_model, printstats = FALSE, verbose = FALSE)
report_info <- 
  rbind(
    model$parameters %>% 
      dplyr::filter(!is.na(Active_Cnt)) %>%
      dplyr::select(Label, Value, Parm_StDev) %>%
      dplyr::rename(std.dev = Parm_StDev),
    
    model$derived_quants %>% 
      dplyr::select(Label, Value, StdDev) %>%
      dplyr::rename(std.dev = StdDev)
  )

# get informative parameter labels used by Stock Synthesis
# model <- r4ss::SS_output() # read model output
model <- r4ss::SS_output(dir_model, printstats = FALSE, verbose = FALSE)
est_par_labels <- model$parameters %>% 
  dplyr::filter(!is.na(Active_Cnt)) %>%
  dplyr::pull(Label)

# copy adnuts results to avoid messing things up
fit2 <- fit

# fit$samples is a 3D array where the third dimension is the parameter value
# The "lp__" column seems to be the log-posterior added by adnuts.
dimnames(fit2$samples)[[3]] <- c(est_par_labels, "lp__")
fit2$par_names = est_par_labels

# for my model samples_unbounded = NULL, so I'm not sure about the following step
if (!is.null(fit2$samples_unbounded)) {
  dimnames(fit2$samples_unbounded)[[3]] <- c(est_par_labels, "lp__")
}

adnuts::pairs_admb(fit2, pars=1:6, order='slow', label.cex = 0.6)

adnuts::plot_marginals(fit2, label.cex = 0.6)
summary(fit2)
adnuts::launch_shinyadmb(fit2)

adnuts::pairs_admb(fit2, pars=1:6, order='slow', diag='hist', label.cex = 0.6)
adnuts::plot_uncertainties(fit2)


post <- adnuts::extract_samples(fit)
