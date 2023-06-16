#######################################################################################################
# Here are the required packages that should be loaded with the nwfscDiag
# library(HandyCode)
# library(plyr)
# devtools::load_all("C:/Users/Chantel.Wetzel/Documents/GitHub/r4ss")
# devtools::load_all("C:/Users/Chantel.Wetzel/Documents/GitHub/nwfscDiag")

devtools::load_all("c:/github/nwfscDiag")
library(nwfscDiag)

#######################################################################################################
# Define the parameters to profile and the parameter ranges:
#------------------------------------------------------------------------------------------------------
# Can use the get_settings_profile function to specify which parameters to run a profile for and
# the parameter ranges for each profile.  The low and high values can be specified in 3 ways:
# as a 'multiplier' where a percent where the low and high range will be specified as x% of the base
# parameter (i.e., (base parameter - base parameter* x) - (base parameter + base parameter * x)),
# in 'real' space where the low and high values are in the parameter space, and finally as
# 'relative' where the low and high is a specified amount relative to the base model parameter
# (i.e., (base parameter - x) - (base parameter + x).
# Here is an example call to the get_settings_profile function:

get <- get_settings_profile(
  parameters = c("NatM_uniform_Fem_GP_1", "SR_BH_steep", "SR_LN(R0)"),
  low = c(0.4, 0.25, -2),
  high = c(0.4, 1.0, 2),
  step_size = c(0.01, 0.05, 0.25),
  param_space = c("multiplier", "real", "relative")
)

#######################################################################################################
# Create a list of settings to run the profiles, jitters, and retrospectives:

# define outer directory on a specific computer
if(Sys.info()["user"] == "Ian.Taylor"){
  mydir <- "C:/SS/Petrale/Petrale2023/petrale/models/"
}
if(Sys.info()["user"] == "Vladlena.Gertseva"){
  mydir <- "FILL IN PATH TO 'models' HERE"
}
mydir <- file.path(mydir, "2023.a026.001_hess_step")

model_settings <- get_settings(settings = list(
  oldctlfile = "petrale_control.ss",
  base_name = "diags",
  run = c("jitter", "profile", "retro"),
  profile_details = get,
  prior_like = 1 #,
  #exe = "c:/SS/SSv3.30.21.00_Feb10/ss_win"
))

# "base_name" is the folder name that contains the base model
# "run" specifies which diagnostics to run. Can be all or a subset.  If all diagnostics should be run
# 	this does not need to be given to the get_settings function.
# "profile details" defines the parameters to do profiles for and the the range and step size to conduct
# 	the profile over.  The default setting for this function is shown above.

#######################################################################################################
# Run all diagnostics
run_diagnostics(mydir = mydir, model_settings = model_settings)

# only run profiles
profile_settings <- model_settings
profile_settings$run <- "profile"
run_diagnostics(mydir = mydir, model_settings = profile_settings)

# "mydir" is the working directory (parent folder with the base model)
# "model_settings" defined above using the get_settings function.  The results of this function is a list
# and can be viewed in the R terminal.
