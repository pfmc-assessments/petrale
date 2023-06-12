# messy code related to processing and running indices from surveys
#
# perform various tasks related to indices for Petrale 2023 assessment
# 1. read sdmTMB index results saved as .RData files
# 2. plot comparison of indices with different error distributions
# 3. save the index output as a CSV file
# 4. save a separate CSV file with index output formatted for SS3
# 5. run a new index

#dir <- "\\\\nwcfile/FRAM/Assessments/CurrentAssessments/petrale_2023/data/wcgbts"
dir <- "data-raw/indices/wcgbts"

# load(file.path(dir, "delta_gamma/index/sdmTMB_save.RData"))
# index_areas_gamma <- index_areas
load(file.path(dir, "delta_lognormal/index/sdmTMB_save.RData"))
index_areas_lognormal <- index_areas
# # based on discussion with Kelli Johnson, the "mix" distributions
# # are still experimental and didn't work well for Petrale (commented out below)
# load(file.path(dir, "delta_gamma_mix/index/sdmTMB_save.RData"))
# index_areas_gamma_mix <- index_areas
load(file.path(dir, "delta_lognormal_mix/index/sdmTMB_save.RData"))
index_areas_lognormal_mix <- index_areas
head(index_areas_lognormal, 3)
#        area year      est      lwr      upr   log_est         se
# 1 coastwide 2003 18720.65 15585.51 22486.44  9.837382 0.09351458
# 2 coastwide 2004 23434.26 19736.19 27825.27 10.061955 0.08762677
# 3 coastwide 2005 23986.55 20596.48 27934.61 10.085249 0.07774280
tail(index_areas_lognormal, 3)
#    area year      est      lwr      upr  log_est        se
# 74   CA 2019 19090.40 14786.74 24646.62 9.856941 0.1303363
# 75   CA 2021 21991.89 18130.70 26675.38 9.998429 0.0985055
# 76   CA 2022 21602.49 17629.90 26470.24 9.980564 0.1036816


#index_areas_gamma$dist <- "gamma"
index_areas_lognormal$dist <- "lognormal"
index_areas_lognormal_mix2$dist <- "lognormal_mix"

# get rescaled lognormal_mix
ratio <- index_areas_lognormal %>% 
  dplyr::filter(area == "coastwide") %>% 
  dplyr::summarize(mean(est)) / 
index_areas_lognormal_mix %>% 
  dplyr::filter(area == "coastwide") %>% 
  dplyr::summarize(mean(est)) 
ratio <- as.numeric(ratio)

index_areas_lognormal_mix2 <- index_areas_lognormal_mix
index_areas_lognormal_mix2$est <- ratio * index_areas_lognormal_mix$est 
index_areas_lognormal_mix2$lwr <- ratio * index_areas_lognormal_mix$lwr 
index_areas_lognormal_mix2$upr <- ratio * index_areas_lognormal_mix$upr 

index_areas_lognormal_mix2$dist <- "lognormal_mix * 4.84"

# make data frame containing all distributions
index_all_dist <- rbind(
  #index_areas_gamma,
  index_areas_lognormal,
  index_areas_lognormal_mix,
  index_areas_lognormal_mix2
)
# convert distribution to factor
index_all_dist <-
  index_all_dist %>% dplyr::mutate(dist = factor(dist))
# filter for coastwide index only (no state-specific groups)
index_all_dist <-
  index_all_dist %>% dplyr::filter(area == "coastwide")


# make an index plot using code copied out of 
# indexwc::plot_indices()
library(ggplot2)

index_all_dist %>%
  ggplot2::ggplot(
    ggplot2::aes(
      x = year,
      y = est,
      group = dist,
      colour = dist,
      fill = dist
    )
  ) +
  ggplot2::geom_point(
    position = ggplot2::position_dodge(0.3)
  ) +
  ggplot2::geom_line(lty = 2) +
  ggplot2::geom_errorbar(
    ggplot2::aes(ymin = lwr, ymax = upr),
    width = 0.2,
    position = position_dodge(0.2)
  ) +
  # ggplot2::theme_bw() +
  ggplot2::xlab("Year") +
  ggplot2::ylab("Index (mt)") +
  expand_limits(y = 0)

# save plot created above
ggsave("figures/index_comparisons_WCGBTS_11-May-2023.png")


# save index output as CSV
write.csv(index_areas_lognormal,
  file = "data-raw/indices/wcgbts/wcgbts_lognormal_11-May-2023.csv",
  row.names = FALSE
)
write.csv(index_areas_gamma,
  file = "data-raw/indices/wcgbts_gamma_30-March-2023.csv",
  row.names = FALSE
)

# subset to coastwide index only and write to format used by SS3
index_areas_lognormal %>%
  dplyr::filter(area == "coastwide") %>%
  dplyr::select(year, est, se) %>%
  dplyr::mutate(month = 7, fleet = 4, .after = 1) %>%
  write.csv(
    file = "data-raw/indices/wcgbts_for_SS_lognormal_11-May-2023.csv",
    row.names = FALSE
  )

index_areas_gamma %>%
  dplyr::filter(area == "coastwide") %>%
  dplyr::select(year, est, se) %>%
  dplyr::mutate(month = 7, fleet = 7, .after = 1) %>%
  write.csv(
    file = "data-raw/indices/wcgbts_for_SS_gamma_30-March-2023.csv",
    row.names = FALSE
  )

dir <- "data-raw/indices/triennial"
load(file.path(dir, "delta_lognormal/index/sdmTMB_save.RData"))
index_tri_lognormal <- index_areas

index_tri_lognormal %>%
  dplyr::filter(area == "coastwide") %>%
  dplyr::select(year, est, se) %>%
  dplyr::mutate(month = 7, fleet = 3, .after = 1) %>%
  write.csv(
    file = "data-raw/indices/triennial_for_SS_lognormal_with_depth_12-May-2023.csv",
    row.names = FALSE
  )

# load Triennial indices and write to CSV
dir <- "data-raw/indices/triennial"
load(file.path(dir, "delta_lognormal_no_depth/index/sdmTMB_save.RData"))
index_tri_lognormal <- index_areas
load(file.path(dir, "delta_lognormal_with_depth/index/sdmTMB_save.RData"))
index_tri_lognormal_depth <- index_areas

index_tri_lognormal %>%
  dplyr::filter(area == "coastwide") %>%
  dplyr::select(year, est, se) %>%
  dplyr::mutate(month = 7, fleet = 3, .after = 1) %>%
  write.csv(
    file = "data-raw/indices/triennial_for_SS_lognormal_no_depth_12-May-2023.csv",
    row.names = FALSE
  )
index_tri_lognormal_depth %>%
  dplyr::filter(area == "coastwide") %>%
  dplyr::select(year, est, se) %>%
  dplyr::mutate(month = 7, fleet = 3, .after = 1) %>%
  write.csv(
    file = "data-raw/indices/triennial_for_SS_lognormal_with_depth_12-May-2023.csv",
    row.names = FALSE
  )

index_tri_lognormal$dist <- "lognormal"
index_tri_lognormal_depth$dist <- "lognormal_with_depth"

# get rescaled lognormal_depth
ratio <- index_tri_lognormal %>% 
  dplyr::filter(area == "coastwide") %>% 
  dplyr::summarize(mean(est)) / 
index_tri_lognormal_depth %>% 
  dplyr::filter(area == "coastwide") %>% 
  dplyr::summarize(mean(est)) 
ratio <- as.numeric(ratio)

index_tri_lognormal_depth2 <- index_tri_lognormal_depth
index_tri_lognormal_depth2$est <- ratio * index_tri_lognormal_depth$est 
index_tri_lognormal_depth2$lwr <- ratio * index_tri_lognormal_depth$lwr 
index_tri_lognormal_depth2$upr <- ratio * index_tri_lognormal_depth$upr 

index_tri_lognormal_depth2$dist <- "lognormal_with_depth * 5.99"


index_all_tri <- rbind(
  index_tri_lognormal,
  index_tri_lognormal_depth,
  index_tri_lognormal_depth2
)
# convert distribution to factor
index_all_tri <-
  index_all_tri %>% dplyr::mutate(dist = factor(dist))
# filter for coastwide index only (no state-specific groups)
index_all_tri <-
  index_all_tri %>% dplyr::filter(area == "coastwide")


# make an index plot using code copied out of 
# indexwc::plot_indices()
library(ggplot2)

index_all_tri %>%
  ggplot2::ggplot(
    ggplot2::aes(
      x = year,
      y = est,
      group = dist,
      colour = dist,
      fill = dist
    )
  ) +
  ggplot2::geom_point(
    position = ggplot2::position_dodge(0.3)
  ) +
  ggplot2::geom_line(lty = 2) +
  ggplot2::geom_errorbar(
    ggplot2::aes(ymin = lwr, ymax = upr),
    width = 0.2,
    position = position_dodge(0.2)
  ) +
  # ggplot2::theme_bw() +
  ggplot2::xlab("Year") +
  ggplot2::ylab("Index (mt)") +
  expand_limits(y = 0)

# save plot created above
ggsave("figures/index_comparisons_Triennial_12-May-2023.png")


### running a new index
# code is copied from 
# https://github.com/kellijohnson-NOAA/indexwc/blob/main/data-raw/configuration.R

pak::pkg_install("kellijohnson-NOAA/indexwc")

configuration <- read.csv('https://raw.githubusercontent.com/kellijohnson-NOAA/indexwc/main/data-raw/configuration.csv')
configuration <- tibble::as_tibble(configuration)
# configuration <- tibble::as_tibble(read.csv(
#   file.path("data-raw", "configuration.csv")
# ))

# subset to just petrale rows of configuration file
configuration <- configuration %>%
  dplyr::filter(species == "petrale sole")
# # A tibble: 2 x 12
#   species      fxn                                  source family formula min_depth max_depth min_latitude max_latitude min_year max_year anisotropy
#   <chr>        <chr>                                <chr>  <chr>  <chr>       <int>     <dbl>        <dbl>        <dbl>    <dbl>    <dbl> <lgl>     
# 1 petrale sole nwfscSurvey::pull_catch(common_name~ NWFSC~ sdmTM~ catch_~       -55      -675         -Inf          Inf     -Inf      Inf TRUE      
# 2 petrale sole nwfscSurvey::pull_catch(common_name~ Trien~ sdmTM~ catch_~       -55      -366           37          Inf     -Inf      Inf TRUE    

# download data for the two surveys
data <- configuration %>%
  # Row by row ... do stuff then ungroup
  dplyr::rowwise() %>%
  # Pull the data based on the function found in fxn column
  dplyr::mutate(
    data_raw = list(indexwc::format_data(eval(parse(text = fxn)))),
    data_filtered = list(data_raw %>%
      dplyr::filter(
        depth <= min_depth, depth >= max_depth,
        latitude >= min_latitude, latitude <= max_latitude,
        year >= min_year, year <= max_year
      ))
  ) %>%
  dplyr::ungroup()

# run both indices
# to run just WCGBTS, use data[1,] instead of data in the first line
# to run just Triennial, use data[2,] instead of data in the first line
# the output goes to a "petrale_sole" folder in the working directory
best <- data %>%
  dplyr::mutate(
    # Evaluate the call in family
    family = purrr::map(family, .f = ~ eval(parse(text = .x))),
    # Run the model on each row in data
    results = purrr::pmap(
      .l = list(
        data = data_filtered,
        formula = formula,
        family = family,
        anisotropy = anisotropy
      ),
      .f = indexwc::run
    )
  )