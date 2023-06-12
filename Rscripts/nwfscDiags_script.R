#######################################################################################################
# Here are the required packages that should be loaded with the nwfscDiag
# library(HandyCode)
# library(plyr)
# devtools::load_all("C:/Users/Chantel.Wetzel/Documents/GitHub/r4ss")
# devtools::load_all("C:/Users/Chantel.Wetzel/Documents/GitHub/nwfscDiag")

library(nwfscDiag)

#######################################################################################################
# Define the working directory where the base model is and where all jitter, profile, and retrospective
# runs will be done:

mydir <- "C:/SS/Petrale/Petrale2023/petrale/models"

# The base model should be within a fold in the above directory.

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
  low = c(0.4, 0.5, -2),
  high = c(0.4, 1.0, 2),
  step_size = c(0.1, 0.05, 0.5),
  param_space = c("multiplier", "real", "relative")
)

#######################################################################################################
# Create a list of settings to run the profiles, jitters, and retrospectives:

model_settings <- get_settings(settings = list(
  base_name = "2023.a022.016_hess",
  run = c("jitter", "profile", "retro"),
  profile_details = get
))

model_settings <- get_settings(settings = list(
  base_name = "2023.a022.016_hess",
  run = c("jitter", "profile"), # "retro"),
  profile_details = get
))

# "base_name" is the folder name that contains the base model
# "run" specifies which diagnostics to run. Can be all or a subset.  If all diagnostics should be run
# 	this does not need to be given to the get_settings function.
# "profile details" defines the parameters to do profiles for and the the range and step size to conduct
# 	the profile over.  The default setting for this function is shown above.

#######################################################################################################
# Run all diagnostics

run_diagnostics(mydir = mydir, model_settings = model_settings)

# "mydir" is the working directory (parent folder with the base model)
# "model_settings" defined above using the get_settings function.  The results of this function is a list
# and can be viewed in the R terminal.



#####################################################################################################
#
# Example 2: Run only a profile over steepness
#
#####################################################################################################

get <- get_settings_profile(
  parameters = c("SR_BH_steep"),
  low = c(0.25),
  high = c(1.0),
  step_size = c(0.05),
  param_space = c("real"),
  use_prior_like = 1
)


model_settings <- get_settings(settings = list(
  base_name = "2023.a022.014_New_Francis",
  run = "profile",
  profile_details = get,
  verbose = TRUE
))

run_diagnostics(mydir = mydir, model_settings = model_settings)

profile(
  dir = "C:/SS/Petrale/Petrale2023/petrale/models/2023.a022.016_hess/h_profile",
  string = "steep",
  profilevec = c(.99, 0.95, 0.90, 0.85, 0.80, 0.75),
  newctlfile = "petrale_control.ss",
  exe = "ss_win",
  extras = "-nohess",
  verbose = TRUE,
  show_in_console = TRUE
)

h_profile_mods <- SSgetoutput(keyvec = 1:6, dirvec = "models/2023.a022.016_hess/h_profile")
h_profile_summary <- SSsummarize(h_profile_mods)
SSplotProfile(h_profile_summary,
  print = TRUE,
  plotdir = "models/2023.a022.016_hess/h_profile"
)
PinerPlot(h_profile_summary, component = "Length_like", profile.string = "steep")
PinerPlot(h_profile_summary, component = "Age_like", profile.string = "steep")
PinerPlot(h_profile_summary, component = "Surv_like", profile.string = "steep")

profile_plot_values <- function(summary, profile_string, value_string,
  add = FALSE) {
    # combine pars and quants into one data frame
  combined <- rbind(summary$pars %>% dplyr::select(-recdev),
                    summary$quants)
  # get labels for parameter or derived quantitiy
  profile_string <- grep(profile_string, combined$Label, value = TRUE)
  value_string <- grep(value_string, combined$Label, value = TRUE)
  message("plotting ", profile_string, " vs ", value_string)
  # get values
  x <- combined %>%
    dplyr::filter(Label == profile_string) %>%
    dplyr::select(dplyr::starts_with("replist")) %>%
    as.numeric()
  y <- combined %>%
    dplyr::filter(Label == value_string) %>%
    dplyr::select(dplyr::starts_with("replist")) %>%
    as.numeric()
  # make plot
  if (!add) {
	plot(x = x, y = y, xlab = profile_string, ylab = value_string, 
	  type = "n'")
  }
  lines(x = x, y = y, type = "o")
}
profile_plot_values(h_profile_summary, "steep", "Dead_Catch_MSY")
profile_plot_values(h_profile_summary, "SR_BH_steep", "NatM_uniform_Fem")
profile_plot_values(h_profile_summary, "SR_BH_steep", "NatM_uniform_Mal", add = TRUE)

#####################################################################################################
#
# Example 3: Run only jitters
#
#####################################################################################################

model_settings <- get_settings(settings = list(
  base_name = "example_model",
  run = "jitter",
  Njitter = 100,
  jitter_fraction = 0.10
))

run_diagnostics(mydir = mydir, model_settings = model_settings)


#####################################################################################################
#
# Example 4: Run only retrospectives
#
#####################################################################################################

model_settings <- get_settings(settings = list(
  base_name = "example_model",
  run = "retro",
  retro_yrs = -1:-5
))

run_diagnostics(mydir = mydir, model_settings = model_settings)
