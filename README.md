# petrale
Petrale Sole stock assessment for the U.S. West Coast

## compiling the document

```
# make sure you're in the right directory
# line below should return "Package: petrale"
readLines("DESCRIPTION")[1]

# load everything in the petrale folder
# first time: install.packages("devtools")
devtools::load_all()
# load other libraries
library(sa4ss)
library(magrittr)
# compile document for current base model
# (depends on model files in "models" directory)
compile_petrale()

# update r4ss to branch related to units of spawning output
pak::pkg_install("r4ss/r4ss@spawn_output_label_838")

## other helpful functions
# first run `devtool::load_all()` in the petrale folder
get_mod(26, 1)
# # reports the following
# id = 2023.a026.001
# dir = 2023.a026.001_hess_step
# reading model from: models/2023.a026.001_hess_step
# creating mod.26.1 in workspace
```
