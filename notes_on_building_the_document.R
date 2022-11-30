# drafting the initial document (only had to be run once)
remotes::install_github("pfmc-assessments/sa4ss")
library(sa4ss)
setwd("doc")
sa4ss::draft(authors = c("Ian G. Taylor", "Vladlena Gertseva"), 
             create_dir = FALSE,
             species = "petrale sole",
             latin = "Eopsetta jordani",
)

# build the document after making changes
setwd("doc")
bookdown::render_book("00a.Rmd", clean = FALSE, output_dir = getwd())
setwd("..")
