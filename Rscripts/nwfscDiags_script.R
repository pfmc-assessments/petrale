#run_diags_petrale <- function(runs = 1:5, 
basedir = "models/2023.a034.001" #) {

# devtools::load_all("c:/github/nwfscDiag")
# pak::pkg_install("pfmc-assessments/nwfscDiag@profile_control")
require(nwfscDiag)

#######################################################################################################
# Define the parameters to profile and the parameter ranges:
get <- get_settings_profile(
  parameters = c("NatM_uniform_Fem_GP_1", "SR_BH_steep", "SR_LN(R0)"),
  low = c(0.4, 0.6, -1.5),
  high = c(0.45, 1.0, 1.5),
  step_size = c(0.01, 0.05, 0.5),
  param_space = c("multiplier", "real", "relative"),
)
# subset to just steepness
# get <- get[2,]
# subset to M #and R0
get <- get[1,]

#######################################################################################################
# Create a list of settings to run the profiles, jitters, and retrospectives:

# define outer directory on a specific computer
if(Sys.info()["user"] == "Ian.Taylor"){
  mydir <- "C:/SS/Petrale/Petrale2023/petrale/"
}
if(Sys.info()["user"] == "Vladlena.Gertseva"){
  mydir <- "FILL IN PATH TO 'models' HERE"
}
mydir <- file.path(mydir, basedir)

if (FALSE) {
  model_settings <- get_settings(settings = list(
    oldctlfile = "petrale_control.ss",
    base_name = "diags2",
    run = c("jitter", "profile", "retro"),
    profile_details = get,
    prior_like = 1, 
    verbose = TRUE
    #extras = "-stopph 0 -nohess"
    #exe = "c:/SS/SSv3.30.21.00_Feb10/ss_win" # this doesn't work
  ))
}

# using par file
model_settings <- get_settings(settings = list(
  oldctlfile = "petrale_control.ss",
  #base_name = "diags_steep_est",
  base_name = "diags_par3",
  run = c("profile"),
  profile_details = get,
  #prior_like = 1, 
  verbose = TRUE,
  usepar = TRUE,
  globalpar = FALSE,
  init_values_src = 1,
  #extras = "-nohess -phase 10",
  extras = "-nohess",
  #parstring = "SR_parm[2]" # steepness
  #parstring = "SR_parm[1]" # log(R0)
  parstring = "MGparm[1]", # female M
  remove_files = FALSE
))

# "base_name" is the folder name that contains the base model
# "run" specifies which diagnostics to run. Can be all or a subset.  If all diagnostics should be run
# 	this does not need to be given to the get_settings function.
# "profile details" defines the parameters to do profiles for and the the range and step size to conduct
# 	the profile over.  The default setting for this function is shown above.

#######################################################################################################
# Run all diagnostics
if (FALSE) {
  run_diagnostics(mydir = mydir, model_settings = model_settings)
}

# only run profiles
if (TRUE) {
  profile_settings <- model_settings
  jitter_settings <- model_settings
  profile_settings$run <- "profile"
  jitter_settings$run <- "jitter"
  run_diagnostics(mydir = mydir, model_settings = profile_settings, skipruns = FALSE)
  #run_diagnostics(mydir = mydir, model_settings = jitter_settings)
}

# "mydir" is the working directory (parent folder with the base model)
# "model_settings" defined above using the get_settings function.  The results of this function is a list
# and can be viewed in the R terminal.

#}