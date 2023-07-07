#' get directory for a petrale model
#'
#' reads model output from a directory found by `get_dir_petrale()`
#' and invisibly returns the list created by `r4ss::SS_output()`.
#'
#' Adapted from Lingcod 2021
#' https://github.com/pfmc-assessments/lingcod/blob/main/R/get_mod.R
#'
#' @inheritParams get_dir_petrale
#' @param dir Directory where model files are located as an alternative
#' to the previous inputs
#' @param covar Read covar.sso file? Passed to r4ss::SS_output() and
#' required in some cases where there was a non-positive-definite Hessian.
#' @param assign Assign the result of r4ss::SS_output() to the
#' user's workspace with the name `mod.[id]` (e.g. `mod.2021.s.002.001`).
#' @param printstats Argument passed on r4ss::SS_output()
#' @template verbose
#' @return invisibly returns the list created by `r4ss::SS_output()`
#' while also optionally assigning that list to the workspace
#' (if `assign = TRUE`).
#' @author Ian G. Taylor
#' @export
#' @seealso [get_dir_petrale()]
#' @examples
#' \dontrun{
#' # read model output and assign to workspace as 'mod.2021.s.001.001"
#' get_mod(num = 24, sens = 18)
#' get_mod(id = "2023.a024.018")
#'
#' # read model output and make all default r4ss plots
#' get_mod(num = 2, plot = TRUE)
#'
#' # read model output and make only biology, selex, and time series plots
#' get_mod(area = "s", num = 2, plot = 1:3)
#' }
get_mod <- function(num = NULL,
                    sens = 1,
                    yr = 2023,
                    code = "a",
                    id = NULL,
                    dir = NULL,
                    covar = TRUE,
                    assign = TRUE,
                    SpawnOutputLabel = "Spawning output (trillions of eggs)",
                    printstats = FALSE,
                    plot = FALSE,
                    verbose = TRUE,
                    ...) {
  if (is.null(dir)) {
    dir <- get_dir_petrale(
      num = num,
      sens = sens,
      yr = yr,
      code = code,
      id = id,
      verbose = verbose
    )
  }
  if (verbose) {
    message("reading model from: ", dir)
  }
  mod <- r4ss::SS_output(
    dir = dir,
    covar = covar,
    SpawnOutputLabel = SpawnOutputLabel,
    printstats = printstats,
    verbose = FALSE
  )
  modname <- basename(dir)
  if (substring(modname, 1, 6) == "2023.a") {
    # for 2023 annual models, create an object name
    # like mod.24.18

    # convert num and sens from "001" to 1
    num <- as.numeric(substring(modname, 7, 9))
    sens <- as.numeric(substring(modname, 11, 13))
    modname <- paste0("mod.", num, ".", sens)
  } else {
    # for other years/code combinations, use the full name
    # "mod.2019.001.001" or "mod.2023.s001.001"
    modname <- paste0("mod.", modname)
  }

  # assign to workspace
  if (assign) {
    if (verbose) {
      message("creating ", modname, " in workspace")
    }
    assign(x = modname, value = mod, pos = 1)
  }

  invisible(mod)
}
