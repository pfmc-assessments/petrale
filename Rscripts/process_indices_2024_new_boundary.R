# first load indexwc with boundaries argument added to run_sdmbtmb()
indexwc_dir <- "c:/github/indexwc"
devtools::load_all(indexwc_dir)

configuration <- tibble::as_tibble(read.csv(
  file.path(indexwc_dir, "data-raw", "configuration.csv")
))

# subset to just WCGBTS petrale row of configuration file
configuration <- configuration %>%
  dplyr::filter(species == "petrale sole" & source == "NWFSC.Combo")
  
str(configuration)
# tibble [1 x 15] (S3: tbl_df/tbl/data.frame)
#  $ species        : chr "petrale sole"
#  $ fxn            : chr "nwfscSurvey::pull_catch(common_name = species,survey=source)"
#  $ source         : chr "NWFSC.Combo"
#  $ family         : chr "sdmTMB::delta_lognormal()"
#  $ formula        : chr "catch_weight ~ 0 + fyear + pass_scaled"
#  $ min_depth      : int -55
#  $ max_depth      : num -675
#  $ min_latitude   : num -Inf
#  $ max_latitude   : num Inf
#  $ min_year       : num -Inf
#  $ max_year       : num Inf
#  $ anisotropy     : logi TRUE
#  $ knots          : int 500
#  $ spatiotemporal1: chr "iid"
#  $ spatiotemporal2: chr "iid"

data <- configuration %>%
  # Row by row ... do stuff then ungroup
  dplyr::rowwise() %>%
  # Pull the data based on the function found in fxn column
  dplyr::mutate(
    data_raw = list(format_data(eval(parse(text = fxn)))),
    data_filtered = list(data_raw %>%
      dplyr::filter(
        depth <= min_depth, depth >= max_depth,
        latitude >= min_latitude, latitude <= max_latitude,
        year >= min_year, year <= max_year
      ))
  ) %>%
  dplyr::ungroup()

# command below created by taking pieces of configuration above
run_sdmtmb(
    data = data$data_filtered[[1]],
    formula = "catch_weight ~ 0 + fyear + pass_scaled",
    family = sdmTMB::delta_lognormal(),
    anisotropy = TRUE,
    n_knots = 500,
    boundaries = boundaries_data[c("Coastwide", "North of 46deg 53.3min", "South of 46deg 53.3min")],
    spatiotemporal = list("iid", "iid")
)

# manually copy results from 
# "petrale_sole/wcgbts/delta_lognormal"
# to 
# "data-raw/wcgbts/delta_lognormal_with2023_and_UandA_boundary"

est_by_area <- read.csv("data-raw/wcgbts/delta_lognormal_with2023_and_UandA_boundary/index/est_by_area.csv")
results_coast <- est_by_area |> 
  dplyr::filter(area == "Coastwide") 
results_north <- est_by_area |> 
  dplyr::filter(area == "North of 46deg 53.3min") |> 
  dplyr::mutate(ratio = est / results_coast$est)


(mean_all <- results_north$ratio |> mean() |> round(3))
# [1] 0.157

(mean_recent5 <- results_north |> 
  dplyr::filter(year >= 2018) |> 
  dplyr::pull(ratio) |> 
  mean() |> 
  round(3))
# [1] 0.18

results_north |> 
  ggplot(aes(x = year, y = ratio)) + 
    geom_point() + 
    geom_line(aes(y = mean_all, color = "mean of all obs"), lwd = 1) +
    geom_segment(aes(x = x1, y = y1, xend = x2, yend = y2, color = "mean of recent 5 obs"), 
      lwd = 1,
      data = data.frame(x1 = 2018, x2 = 2023, y1 = mean_recent5, y2 = mean_recent5)) +
    # commented out regression
    # stat_summary(fun.data=mean_cl_normal) + 
    # geom_smooth(method='lm') + 
    expand_limits(y = c(0, 0.25)) + 
    scale_x_continuous(breaks = seq(2003, 2023, 2)) +
    ylab(expression("Ratio of estimated biomass North of 46\u00B053.3' to coastwide biomass")) +
    theme_bw()

ggsave("data-raw/wcgbts/delta_lognormal_with2023_and_UandA_boundary/ratio_plot.png", 
    width = 10, height = 7)

results_north |> dplyr::select(ratio, year) |> lm()
# Call:
# lm(formula = dplyr::select(results_north, ratio, year))

# Coefficients:
# (Intercept)         year
#   -4.883590     0.002504
results_north |> dplyr::pull(ratio) |> range() |> round(3)
# [1] 0.127 0.217

results_coast |> dplyr::select(year, est) |> 
  round() |> 
  dplyr::rename(Year = year, "Coastwide biomass (mt)" = est) |> 
  dplyr::mutate("Biomass North of 46\u00B053.3' (mt)" = round(results_north$est)) |> 
  dplyr::mutate(Ratio = round(results_north$ratio, 3)) |> 
  knitr::kable(format = "html") |> 
  writeLines("data-raw/wcgbts/delta_lognormal_with2023_and_UandA_boundary/results_table.html")

