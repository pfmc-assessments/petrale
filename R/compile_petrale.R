#' Compile the stock assessment document(s) for a given species
#'
#' @description
#' Adapted from
#' https://github.com/pfmc-assessments/lingcod/blob/main/R/compile_ling.R
#'
#' Compile the stock assessment document(s) for a given species where the number of
#' documents depends on the number of directories that you pass to dir.
#' Typically, this happens in the directory called `doc` that stores a single
#' directory for each stock.
#' [bookdown::render_book] will be called inside on each directory in `dir`.
#'
#' @details status:
#' This function is currently in beta testing, which means that it
#' * might be helpful,
#' * could contain bugs,
#' * will more than likely have non-descript error and warning messages.
#' Please post an [issue](https::/github.com/pfmc-assessments/sa4ss/issues)
#' or email the author of this function.
#' Thank you for helping test it out, and we hope the pain is worth the gain!
#'
#' @details index.Rmd
#' When specifying a directory to [bookdown::render_book], the
#' file `index.Rmd` is normally used as the master file for rendering the book
#' and all other files are brought in as chapters in alphabetical order.
#' Here, in the \pkg{sa4ss} package we use `00a.Rmd` as the indexing file and
#' all other .Rmd files are sourced in alphabetical order.
#' The renaming is done automatically for you in the call to [draft] and just
#' mentioned here for completeness.
#'
#' @param dir A single directory where the draft files are located.
#' @param wipe A logical values specifying if you want to remove all cached objects.
#' For lingcod, this will remove plots, custom_plots, and cached markdown files and
#' will lead to the document taking approximately an hour to build.
#' @param time The number of seconds you will have to close the pdf
#' if it is found to be open.
#' The integer value is passed to [base::Sys.sleep].
#' @param ... Arguments passed to [bookdown::render_book]
#'
#' @author Kelli F. Johnson and Ian G. Taylor
#' @export
#' @examples
#' \dontrun{
#' # An example for petrale in 2023.
#' compile_petrale()
#' }
#'
compile_petrale <- function(
    basemodelname = "2023.a034.001",
    dir = "documents",
    time = 10,
    wipe = FALSE,
    ...) {
  # load packages doesn't happen automatically with devtools::load_all()
  require(sa4ss)
  require(magrittr)

  # Sort out paths and directories

  # remove any extra directory info like "models" in front of the name
  basemodelname <- basename(basemodelname)
  # add "models"
  fullpathbasemodel <- file.path(
    # dirname(system.file(package = "petrale")),
    "models", basemodelname
  )
  message("base model: ", basemodelname)

  # If wipe, remove cache and plots
  if (wipe) {
    unlink(file.path(fullpathbasemodel, "plots"), recursive = TRUE)
    unlink(file.path(fullpathbasemodel, "custom_plots"), recursive = TRUE)
  }

  if (
    !file.exists(file.path(fullpathbasemodel, "plots")) |
      !file.exists(file.path(fullpathbasemodel, "00mod.Rdata"))
  ) {
    # prepare stuff in model directory, including 00mod.Rdata
    # and plots as created by wrapper function make_r4ss_plots_petrale()
    compile_precursor(
      basemodelname,
      plot = !file.exists(file.path(fullpathbasemodel, "custom_plots"))
    )
  }

  # load model output (object is "model")
  load(file.path(fullpathbasemodel, "00mod.Rdata"))
  # rename to make more clear which model it is
  mod_base <- model
  # path relative to "documents" directory
  mod_loc_relative <- file.path("..", fullpathbasemodel)

  # Set working directory back to getwd() upon exit
  stopifnot(length(dir) == 1)
  stopifnot(utils::file_test("-d", fullpathbasemodel))
  olddir <- getwd()
  message("olddir: ", olddir)
  # setwd(file.path(dirname(system.file(package = "petrale")), "documents"))
  setwd("documents")
  on.exit(setwd(olddir), add = TRUE)

  if (wipe) {
    unlink(file.path("_bookdown_files", paste0(dir, "_cache")), recursive = TRUE)
  }

  # Create the file name for the output based on in dir name
  hidden_book_filename <- "_main"

  # Check if the pdf file exists and if it is open
  test <- tryCatch(
    pdf(dir(
      pattern = paste0(hidden_book_filename, ".pdf"),
      full.names = TRUE
    )),
    error = function(e) {
      message(
        "You have ", time, " seconds to close the pdf named ",
        file.path(dir, hidden_book_filename)
      )
      flush.console()
      Sys.sleep(time)
      return(FALSE)
    },
    finally = TRUE
  )
  if (is.null(test)) dev.off()

  # Make the document
  bookdown::render_book("00a.Rmd", clean = FALSE, output_dir = getwd())

  return(invisible(TRUE))
}

# function to load model, create plots and tables if needed
# and save list of model output as 00mod.Rdata file
compile_precursor <- function(basemodel, plot = TRUE) {
  oldwd <- getwd()
  on.exit(setwd(oldwd))
  # setwd(dirname(system.file(package = "petrale")))

  # Read the model files
  sa4ss::read_model(
    mod_loc = file.path("models", basemodel),
    create_plots = plot,
    save_loc = file.path("models", basemodel, "tex_tables"),
    SpawnOutputLabel = "Spawning output (trillions of eggs)"
  )

  # Load in the resulting 00mod file
  # I think this avoids overlap with any existing objects in the workspace
  modenv <- new.env()

  load(file.path("00mod.Rdata"), envir = modenv)
  if (plot) {
    # add custom plotting functions here if needed
    make_r4ss_plots_petrale(modenv$model, verbose = TRUE)
  }

  # get exec summary tables
  r4ss::SSexecutivesummary(replist = modenv$model, format = FALSE)
  tex_tables_dir1 <- file.path("models", basemodel, "tex_tables")
  tex_tables_dir2 <- "documents/tex_tables"
  sa4ss::es_table_tex(
    dir = modenv$mod_loc,
    save_loc = tex_tables_dir1,
    csv_name = "table_labels.csv"
  )

  message("copying tables from ", tex_tables_dir1, " to ", tex_tables_dir2)
  R.utils::copyDirectory(tex_tables_dir1, tex_tables_dir2, overwrite = TRUE)

  save(
    list = ls(envir = modenv),
    file = file.path("models", basemodel, "00mod.Rdata"),
    envir = modenv
  )
  unlink("00mod.Rdata")
}
