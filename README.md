# petrale
Petrale Sole stock assessment for the U.S. West Coast

## compiling the document

```
# make sure you're in the right directory
# line below should return "Package: petrale"
readLines("DESCRIPTION")[1]

# load everything in the petrale folder
devtools::load_all()
# load other libraries
library(sa4ss)
library(magrittr)
# compile document for model 2023.a024.034
# (depends on model files in "models" directory)
compile_petrale(get_dir_petrale(24,34))


