# process indices
#
# perform various tasks related to indices for Petrale 2023 assessment
# 1. read sdmTMB index results saved as .RData files
# 2. plot comparison of indices with different error distributions
# 3. save the index output as a CSV file
# 4. save a separate CSV file with index output formatted for SS3

dir <- "\\\\nwcfile/FRAM/Assessments/CurrentAssessments/petrale_2023/data/wcgbts"

load(file.path(dir, "delta_gamma/index/sdmTMB_save.RData"))
index_areas_gamma <- index_areas
load(file.path(dir, "delta_lognormal/index/sdmTMB_save.RData"))
index_areas_lognormal <- index_areas
# # based on discussion with Kelli Johnson, the "mix" distributions
# # are still experimental and didn't work well for Petrale (commented out below)
# load(file.path(dir, "delta_gamma_mix/index/sdmTMB_save.RData"))
# index_areas_gamma_mix <- index_areas
# load(file.path(dir, "delta_lognormal_mix/index/sdmTMB_save.RData"))
# index_areas_lognormal_mix <- index_areas
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
# make data frame containing all distributions
index_areas_gamma$dist <- "gamma"
index_areas_lognormal$dist <- "lognormal"
index_all_dist <- rbind(
  index_areas_gamma,
  index_areas_lognormal
)
# convert distribution to factor
index_all_dist <-
  index_all_dist %>% dplyr::mutate(dist = factor(dist))
# filter for coastwide index only (no state-specific groups)
index_all_dist <-
  index_all_dist %>% dplyr::filter(area == "coastwide")

# make plot using code copied out of indexwc::plot_indices()
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
    position = position_dodge(0.3)
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
ggsave("figures/index_comparisons_WCGBTS_30-March-2023.png")

write.csv(index_areas_lognormal,
  file = "data-raw/nwfscSurvey/indices/wcgbts_lognormal_30-March-2023.csv",
  row.names = FALSE
)
write.csv(index_areas_gamma,
  file = "data-raw/nwfscSurvey/indices/wcgbts_gamma_30-March-2023.csv",
  row.names = FALSE
)

# write to format used by SS3
index_areas_lognormal %>%
  dplyr::filter(area == "coastwide") %>%
  dplyr::select(year, est, se) %>%
  dplyr::mutate(month = 7, fleet = 7, .after = 1) %>%
  write.csv(
    file = "data-raw/nwfscSurvey/indices/wcgbts_for_SS_lognormal_30-March-2023.csv",
    row.names = FALSE
  )
index_areas_gamma %>%
  dplyr::filter(area == "coastwide") %>%
  dplyr::select(year, est, se) %>%
  dplyr::mutate(month = 7, fleet = 7, .after = 1) %>%
  write.csv(
    file = "data-raw/nwfscSurvey/indices/wcgbts_for_SS_gamma_30-March-2023.csv",
    row.names = FALSE
  )
