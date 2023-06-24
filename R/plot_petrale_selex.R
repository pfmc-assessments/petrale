#' Plot time-varying selectivity or selectivity
#'
#' Adapted from Lingcod 2021
#' https://github.com/pfmc-assessments/lingcod/blob/main/R/plot_selex.R
#'
#' @param mod A model object created by `r4ss::SS_output()`
#' @param fleet a single fleet number
#' @param Factor a factor from mod$sizeselex$Factor
#' @param sex sex 1 for females, 2 for males
#' @export
#' @author Ian G. Taylor
plot_sel_ret <- function(mod,
                         fleet = 1,
                         Factor = "Lsel",
                         sex = 1) {
  years <- mod$startyr:mod$endyr

  # run selectivity function to get table of info on time blocks etc.
  # NOTE: this writes a png file to sel01_multiple_fleets_length1.png
  #       within the model directory
  infotable <- r4ss::SSplotSelex(mod,
    fleets = fleet,
    sexes = sex,
    sizefactors = Factor,
    years = years,
    subplots = 1,
    plot = FALSE,
    print = TRUE,
    plotdir = mod$inputs$dir
  )$infotable
  # remove extra file (would need to sort out the relative path stuff)
  file.remove(file.path(mod$inputs$dir, "sel01_multiple_fleets_length1.png"))

  # how many lines are in the plot
  nlines <- nrow(infotable)
  # update vector of colors
  infotable$col <- r4ss::rich.colors.short(max(6, nlines), alpha = 0.7) %>%
    rev() %>%
    tail(nlines)
  infotable$pch <- NA
  infotable$lty <- nrow(infotable):1
  infotable$lwd <- 3
  infotable$longname <- infotable$Yr_range
  # run plot function again, passing in the modified infotable
  r4ss::SSplotSelex(mod,
    fleets = fleet,
    # fleetnames = ,
    sexes = sex,
    sizefactors = Factor,
    labels = c(
      "Length (cm)",
      "Age (yr)",
      "Year",
      ifelse(Factor == "Lsel", "Selectivity", "Retention"),
      "Retention",
      "Discard mortality"
    ),
    legendloc = "bottomright",
    years = years,
    subplots = 1,
    plot = TRUE,
    print = FALSE,
    infotable = infotable,
    mainTitle = TRUE,
    mar = c(2, 2, 2, 1)
  )
}

#' Plot selectivity and retention for the commercial fleets
#'
#' @param mod A model object created by [get_mod()] or
#' `r4ss::SS_output()`
#' @param sex Either 1 (females) or 2 (males)
#' @export
#' @author Ian G. Taylor
plot_petrale_tv_selex <- function(mod, sex = 1, verbose = TRUE) {
  filename <- "selectivity_time-varying.png"
  if (length(sex) == 1 && sex == 2) {
    filename <- gsub(".png", "_mal.png", filename)
  }
  if (length(sex) == 1 && sex == 1) {
    filename <- gsub(".png", "_fem.png", filename)
  }

  # create directory if it doesn't exist
  if (!dir.exists(file.path(mod$inputs$dir, "custom_plots"))) {
    dir.create(file.path(mod$inputs$dir, "custom_plots"))
  }
  filepath <- file.path(mod$inputs$dir, "custom_plots", filename)
  # set up png file with 2x2 format
  if (verbose) {
    message("creating plot: ", filepath)
  }

  png(filepath, width = 6.5, height = 6.5, units = "in", res = 300, pointsize = 10)
  par(mfrow = c(2, 2), oma = c(2, 2, 0, 0), las = 1)

  # plot selectivity
  plot_sel_ret(mod, Factor = "Lsel", fleet = 1, sex = sex)
  mtext("Selectivity", side = 2, line = 3, las = 0)
  plot_sel_ret(mod, Factor = "Lsel", fleet = 2, sex = sex)

  # plot retention
  plot_sel_ret(mod, Factor = "Ret", fleet = 1, sex = sex)
  mtext("Retention", side = 2, line = 3, las = 0)
  plot_sel_ret(mod, Factor = "Ret", fleet = 2, sex = sex)
  mtext("Length (cm)", side = 1, line = 0.5, outer = TRUE, las = 0)
  dev.off()
  caption <- paste(
    "Time-varying selectivity (top) and retention (bottom) for the ",
    "North and South fishing fleets."
  )
  write_custom_plots_csv(mod = mod, filename = filename, caption = caption)
}
