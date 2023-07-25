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

# additional explorations
retro_mods <- SSgetoutput(dirvec = paste0("models/2023.a034.001/diags_retro/retro/retro-", 1:5))
retro_mods2 <- retro_mods
retro_mods2[[6]] <- mod.34.1
retro_mods2 <- retro_mods2[c(6,1:5)]
retro_summary <- SSsummarize(retro_mods2)
png("figures/diags_model34/retro_recruits.png", 
  width = 6.5, height = 5, res = 300, units = 'in', pointsize = 10
)
SSplotRetroRecruits(retro_summary, cohorts = 2006:2022, 
  endyrvec = 2023 + 0:-5, main = "")
dev.off()
png("figures/diags_model34/retro_recruits_relative.png", 
  width = 6.5, height = 5, res = 300, units = 'in', pointsize = 10
)
SSplotRetroRecruits(retro_summary, cohorts = 2006:2022, 
  endyrvec = 2023 + 0:-5, relative = TRUE, main = "")
dev.off()
