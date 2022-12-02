# drafting the initial document 
# (only had to be run once, or after significant change to sa4ss package)
remotes::install_github("pfmc-assessments/sa4ss")
library(sa4ss)
setwd("doc")
sa4ss::draft(authors = c("Ian G. Taylor", "Vladlena Gertseva"), 
             create_dir = FALSE,
             species = "petrale sole",
             latin = "Eopsetta jordani",
)
setwd("..")

# specifying the base model
setwd("doc") # assumes working directory is already in "petrale"
base <- "../models/2021.001.001_new_endyr" # relative to "docs"
# creating the standard r4ss plots
mod_base <- SS_output(base)
SS_plots(mod_base)

# Create a model Rdata object and tex tables
sa4ss::read_model(
    mod_loc = base,
	create_plots = FALSE, 
    fecund_mult = 'mt',
	save_loc = "tables"
)

# load the Rdata file for the base model created by sa4ss::read_model() 
load("00mod.Rdata")

# clean out old version of file that gets compiled into the PDF
setwd("doc") # skip this if already in "doc"
if(file.exists("_main.Rmd")){
	file.remove("_main.Rmd")
}
# Render the pdf
bookdown::render_book("00a.Rmd", clean=FALSE, output_dir = getwd())
setwd("..")

### OPTIONAL
# Use to only render a specific section which can be quicker
bookdown::preview_chapter("01executive.Rmd", preview = TRUE, clean = FALSE)