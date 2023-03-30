# drafting the initial document 
# (only had to be run once, or after significant change to sa4ss package)
pak::pkg_install("pfmc-assessments/sa4ss")
pak::pkg_install("pfmc-assessments/sa4ss@table_functions_78")

library(sa4ss)
library(magrittr)
setwd("documents")
sa4ss::draft(authors = c("Ian G. Taylor", "Vladlena Gertseva"), 
             create_dir = FALSE,
             species = "petrale sole",
             latin = "Eopsetta jordani",
)
setwd("..")

# specifying the base model
setwd("documents") # assumes working directory is already in "petrale"
base <- "../models/2023.001.001_new_endyr" # relative to "docs"
# creating the standard r4ss plots
mod_base <- SS_output(base, printstats = FALSE, verbose = FALSE)
#SS_plots(mod_base)

# Create a model Rdata object and tex tables
sa4ss::read_model(
    mod_loc = base,
	create_plots = FALSE, 
    fecund_mult = 'mt',
	save_loc = "tex_tables"
)

# load the Rdata file for the base model created by sa4ss::read_model() 
load("00mod.Rdata")
mod_base <- model

# clean out old version of file that gets compiled into the PDF
setwd("documents") # skip this if already in "documents"
if(file.exists("_main.Rmd")){
	file.remove("_main.Rmd")
}
# Render the pdf
bookdown::render_book("00a.Rmd", clean=FALSE, output_dir = getwd())
setwd("..")

### OPTIONAL
# Use to only render a specific section which can be quicker
bookdown::preview_chapter(input = "22biology.Rmd")
