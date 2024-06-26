#' Plot a map of the stock boundaries for the 2023 petrale assessment
#'
#' Based on function used for lingcod in 2021
#' It writes a PNG file to figures/map_of_fleet_areas_42.png.
#' This depends on the mapdata package but I got an
#' Error: 'worldHiresMapEnv' is not an exported object from 'namespace:maps'
#' so include require(mapdata) in the function as a quick solution.
#'
#' @param cols A vector of colors for north and south areas. 
#' @param names A vector of names for north and south areas
#' @param caption A character string providing the caption for the figure.
#' The caption should not contain any special LaTeX characters because escaping
#' is not performed within this function. As of now, please leave off the final
#' full stop because it is added by [sa4ss::add_figure].
#' @param alttext A character string providing the alternative text for the figure.
#' The caption should not contain any special LaTeX characters because escaping
#' is not performed within this function. As of now, please leave off the final
#' full stop because it is added by [sa4ss::add_figure].
#'
#' @import maps
#' @import mapdata
#' @export
#' @author Ian G. Taylor
#' @seealso [plot_map()]
#' @examples
#' # make map using colors used in data-raw/lingcod_catch.R
#' plot_stock_boundary_map()
plot_stock_boundary_map <- function(names = c("North\nfleet", "South\nfleet"),
                                    cols = c("blue", "red"),
                                    caption = "Map of the U.S. west coast Exclusive Economic Zone within which the assessment is focused. The dashed line and colors delineate the two fishing fleets represented in the model: North (blue) and South (red).",
                                    alttext = "Outline of U.S. west coast split at forty-two degrees north latitude"
) {
  require(mapdata)

  # get EEZ data
  if (!exists("eez")) {
    eez <- read.csv("figures/map/EEZ_polygon_lat_lon.csv")
  }

  # File structure
  filename <- "figures/map/map_of_fleet_areas_42.png"
  # Write the caption to the same location as the figure
  utils::write.csv(
    row.names = FALSE,
    data.frame(caption = caption, alt_caption = alttext, label = "map",
      filein = file.path("..", filename)),
    file = gsub("\\.[pngjpg]{3}$", ".csv", filename)
  )

  # open PNGfile
  png(filename,
    width = 6.5, height = 8, res = 350, units = "in"
  )
  par(mar = c(3, 3, .1, .1))

  # map with Canada and Mexico (not sure how to add states on this one)
  maps::map("worldHires",
    regions = c("Canada", "Mexico"),
    xlim = c(-130, -114), ylim = c(30, 51),
    col = "grey", fill = TRUE, interior = TRUE, lwd = 1
  )
  # add eez polygon covering both areas
  polygon(eez$lon, eez$lat, col = cols[2], border = FALSE)
  # get subset of eez which is just north and add polgon on top
  #boundary.N <- eez[eez$lat > 40 + 10 / 60, ]
  boundary.N <- eez[eez$lat >= 42, ]
  polygon(boundary.N$lon, boundary.N$lat, col = cols[1], border = FALSE)

  # add horizontal line at 40-10
  #abline(h = c(40 + 10 / 60), lty = 3)
  abline(h = 42, lty = 3)
  #text(-127, 40, expression("40"*degree*"10'"), pos = 3)
  text(-127, 42 - 0.1, expression("42"*degree*""), pos = 3)
  # add map with U.S. states boundaries on top
  maps::map("state",
    regions = c(
      "Wash", "Oreg", "Calif", "Idaho",
      "Montana", "Nevada", "Arizona", "Utah"
    ),
    add = TRUE,
    col = "grey", fill = TRUE, interior = TRUE, lwd = 1
  )
  axis(2, at = seq(30, 50, 2), lab = paste0(seq(30, 50, 2), "\U00B0N"), las = 1)
  axis(1, at = seq(-130, -114, 4), lab = paste0(abs(seq(-130, -114, 4)), "\U00B0W"))

  # add model names
  text(-127, 44, names[1], font = 2)
  text(-123, 35, names[2], font = 2)

  # label states
  text(-122, 50, "Canada")
  text(-120, 47.75, "Washington")
  text(-121, 44, "Oregon")
  text(-120, 37, "California")
  text(-115.5, 32.2, "Mexico")

  # close PNG file
  box()
  dev.off()
}
