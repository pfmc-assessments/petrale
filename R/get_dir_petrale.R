#' get directory for a petrale model
#'
#' uses either a model id like "2023.a001.001" or the combination of
#' year (`yr`), code (`code`, e.g. "a" for annual),
#' run number (`num`), and sensitivity number
#' (`sens`) to assemble that id, then returns the associated directory
#' based on the table in the models/README.md
#'
#' Adapted from Lingcod 2021
#' https://github.com/pfmc-assessments/lingcod/blob/main/R/get_dir_ling.R
#'
#' @param code either "" or "a" (annual), "c" (coastwide), "s" (seasonal)
#' @param num base model number
#' @param sens sensitivity number (default 1)
#' @param yr year (default 2023)
#' @param id model id (like "2021.s.001.001") as an alternative to
#' the combination of inputs `num`, `sens`, `code`, and `year`
#' @param verbose A logical value specifying if output should be printed
#' to the console or not. The default is `FALSE`, which will suppress messages.
#' @author Ian G. Taylor, Kelli Faye Johnson
#' @export
#' @seealso [get_mod()]
#' @examples
#' \dontrun{
#' # both commands below return
#' # "models/2023.a024.018_min_sample_retuned"
#' get_dir_petrale(num = 24, sens = 18)
#' get_dir_petrale(id = "2023.a024.018")
#'
#' # get directory for the 2019 base model
#' get_dir_petrale(yr = 2019, num = 1, code = "")
#'
#' # get a seasonal model directory
#' get_dir_petrale(yr = 2023, num = 1, sens = 1, code = "s")
#' }
get_dir_petrale <- function(num = NULL,
                            sens = 1,
                            yr = 2023,
                            code = "a",
                            id = NULL,
                            verbose = FALSE) {
  if (is.null(id) & is.null(num)) {
    stop("Either 'id' or 'num' are required inputs")
  }

  # read table of models
  models <- dir("models")

  # get string for model id (as decided in issue #32)
  if (is.null(id)) {
    id <- paste(yr,
      paste0(
        code,
        sprintf("%03d", num)
      ), # changes 1 to "001"
      sprintf("%03d", sens), # changes 1 to "001"
      sep = "."
    )
  }

  # find matching model(s)
  dir <- NULL
  # loop over values to preserve the order of the values in id (or originating inputs)
  # where previous use of %in% resulted in the order they occur in README.md
  # there's probably a more elegant way to do this
  for (i in 1:length(id)) {
    dirs <- grep(pattern = paste0("^", id[i]), x = models, value = TRUE)
    if (length(dirs) > 1) {
      dir[i] <- dirs[which(nchar(dirs) == min(nchar(dirs)))[1]]
      message(
        "multiple matches:\n  ",
        paste(dirs, collapse = "\n  "),
        "\nchoosing the shortest one:\n  ",
        dirs[i]
      )
    } else {
      dir[i] <- dirs
    }
  }

  if (length(dir) == 0) {
    stop("no model in 'models' has id = ", paste(id, collapse = ", "))
  }

  if (verbose) {
    message(
      "id = ", paste(id, collapse = ", "),
      "\ndir = ", paste(dir, collapse = ", ")
    )
  }

  dir <- file.path("models", dir)
  return(dir)
}
