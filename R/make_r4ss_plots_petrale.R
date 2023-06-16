#' Make r4ss plots for Lingcod assessment
#'
#' Wrapper for `r4ss::SS_plots()` applying default settings that work
#' for the fleets, colors, plot dimensions, etc.
#'
#' Adapted from Lingcod 2021
#' https://github.com/pfmc-assessments/lingcod/blob/main/R/make_r4ss_plots_ling.R
#' 
#' @param mod List created by [get_mod()] or `r4ss::SS_output()`
#' @param plot Vector of plot groups passed to `r4ss::SS_plots()`
#' as well as cutom plots with numbers starting at 31
#' (see code for which plots match which numbers).
#' @param verbose print more info to the console
#' @param dots additional arguments passed to `r4ss::SS_plots()`
#' @author Ian G. Taylor
#' @export
#' @seealso [get_mod()]
#' 

make_r4ss_plots_petrale <- function(mod, plot = c(1:26, 31:50),
                                 verbose = TRUE, ...) {

  fleetcols <- c("blue", "red", "orange", "green3")
  # r4ss functionality for `fleetcolors` is incomplete and inconsistent,
  # so use depends on the function

  if (any(1:26 %in% plot)) {
    # make default plots for most things
    r4ss::SS_plots(mod,
                   plot = plot,
                   #plot = intersect(plot, c(1:23, 25:26)),
                   #fleetnames = fleetnames,
                   fleetcols = fleetcols,
                   #comp.yupper = 0.15,
                   maxsize = 2, # larger bubbles in data plot 2
                   maxrows2 = 4, # fit all WCGBTS CAAL plots on 2 pages
                   maxcols2 = 5, # fit all WCGBTS CAAL plots on 2 pages
                   html = TRUE, # don't open HTML view yet
                   verbose = verbose, ...)
  }

  # custom plots not created by r4ss
  if (any(31:50 %in% plot)) {
    dir <- file.path(mod$inputs$dir, "custom_plots")
    if (!dir.exists(dir)) {
      dir.create(dir)
    }
  }

  if (31 %in% plot) {
    plot_petrale_tv_selex(mod)
    plot_petrale_tv_selex(mod, sex = 2)
  }  
}
