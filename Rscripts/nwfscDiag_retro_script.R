#run_diags_petrale <- function(runs = 1:5, 
basedir = "models/2023.a034.001" #) {

# devtools::load_all("c:/github/nwfscDiag")
# pak::pkg_install("pfmc-assessments/nwfscDiag@profile_control")
require(nwfscDiag)

#######################################################################################################
# Create a list of settings to run the profiles, jitters, and retrospectives:

# define outer directory on a specific computer
if(Sys.info()["user"] == "Ian.Taylor"){
  mydir <- "C:/SS/Petrale/Petrale2023/petrale/"
}
mydir <- file.path(mydir, basedir)

model_settings <- get_settings(settings = list(
  oldctlfile = "petrale_control.ss",
  base_name = "diags",
  run = c("retro"),
  #profile_details = get,
  prior_like = 1, 
  verbose = TRUE
))

nwfscDiag::retro_wrapper( mydir = mydir, model_settings = model_settings,
  skipruns = TRUE)
