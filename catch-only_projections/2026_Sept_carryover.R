# November 2025 revision to catch only projections for petrale sole
dir_old <- "2025_Nov20"
dir_new <- "2026_Sept_carryover"
r4ss::copy_SS_inputs(
  dir.old = file.path("catch-only_projections", dir_old),
  dir.new = file.path("catch-only_projections", dir_new),
  copy_par = TRUE,
  copy_exe = TRUE,
  overwrite = TRUE
)

output <- r4ss::SS_output(file.path("catch-only_projections", dir_old))
inputs <- r4ss::SS_read(file.path("catch-only_projections", dir_old))

# modify forecast to reduce the fixed catches in 2027 and 2028 to be the difference
# between the OFL and the ACL projected for 2029 and 2030 respectively

# get catch by fleet from the estimated timeseries output 
# in the format required by the forecast file
catch <- r4ss::SS_ForeCatch(output, yrs = 2023:2036)

# remove comment column from catch data which is hard to keep in sync with changes
catch <- catch[, -which(names(catch) == "comment")]
names(catch) <- c("Year", "Seas", "Fleet", "Catch")
limits <- data.frame(
  Year = 2027:2036,
  OFL = output$derived_quants[paste0("OFLCatch_", 2027:2036), "Value"],
  ACL = output$derived_quants[paste0("ForeCatch_", 2027:2036), "Value"]
) |>
  dplyr::mutate(
    Ratio = ACL / OFL,
    Diff = OFL - ACL
  )

# scale the fleet-level catches by the ACL/OFL ratio for the carryover years so both
# fleets move together and the combined-year adjustment is applied once, not once per fleet
catch_new_full <- catch |>
  dplyr::mutate(
    Catch = dplyr::case_when(
      Year == 2027 ~ Catch * limits$Ratio[limits$Year == 2029],
      Year == 2028 ~ Catch * limits$Ratio[limits$Year == 2030],
      Year == 2029 ~ Catch / limits$Ratio[limits$Year == 2029],
      Year == 2030 ~ Catch / limits$Ratio[limits$Year == 2030],
      Year == 2031 ~ Catch * limits$Ratio[limits$Year == 2033],
      Year == 2032 ~ Catch * limits$Ratio[limits$Year == 2034],
      Year == 2033 ~ Catch / limits$Ratio[limits$Year == 2033],
      Year == 2034 ~ Catch / limits$Ratio[limits$Year == 2034],
      TRUE ~ Catch
    )
  )

catch_new_full_v2 <- catch |>
  dplyr::mutate(
    Year = Year,
    Catch = dplyr::case_when(
      Year == 2029 ~ Catch * limits$Ratio[limits$Year == 2031],
      Year == 2030 ~ Catch * limits$Ratio[limits$Year == 2032],
      Year == 2031 ~ Catch / limits$Ratio[limits$Year == 2031],
      Year == 2032 ~ Catch / limits$Ratio[limits$Year == 2032],
      Year == 2033 ~ Catch * limits$Ratio[limits$Year == 2035],
      Year == 2034 ~ Catch * limits$Ratio[limits$Year == 2036],
      Year == 2035 ~ Catch / limits$Ratio[limits$Year == 2035],
      Year == 2036 ~ Catch / limits$Ratio[limits$Year == 2036],
      TRUE ~ Catch
    )
  )

# combine old and new annual tallies for plotting purposes
# to confirm that the adjustments have been applied correctly
catch_combined <- dplyr::bind_rows(
  catch |> dplyr::mutate(source = "2025 catch-only projection"),
  catch_new_full |> dplyr::mutate(source = "Carryover 100%"),
  catch_new_full_v2 |> dplyr::mutate(source = "Carryover 100% v2")
)

# add an additional line for the total catch across all fleets by year
catch_combined <- catch_combined |>
  dplyr::mutate(Fleet = as.character(Fleet)) |>
  dplyr::group_by(Year, source) |>
  dplyr::summarise(Catch = sum(Catch), .groups = "drop") |>
  dplyr::mutate(Fleet = "Total") |>
  dplyr::bind_rows(catch_combined |> dplyr::mutate(Fleet = as.character(Fleet)))

# add an additional line for the OFL using the forecast limits directly
catch_combined <- limits |>
  dplyr::select(Year, Catch = OFL) |>
  dplyr::mutate(Fleet = "OFL", source = "OFL") |>
  dplyr::bind_rows(catch_combined |> dplyr::mutate(Fleet = as.character(Fleet)))

# plot total catch by year for both the original and adjusted catch data
# use colors for fleet and lines for source
library(ggplot2)
catch_combined |>
  dplyr::filter(Year >= 2027) |>
  dplyr::mutate(Fleet = factor(Fleet)) |> # avoids color gradient for legend
  ggplot(aes(
    x = Year,
    y = Catch,
    color = source,
    linetype = Fleet, # only make linetype 
    group = interaction(Fleet, source)
  )) +
  geom_line(linewidth = 0.8) +
  geom_point() +
  labs(title = "Total Catch by Year", x = "Year", y = "Total catch (mt)") +
  # make sure all years from 2027 to 2036 are shown on the x-axis with no mid-year ticks
  scale_x_continuous(
    breaks = 2027:2036,
    labels = 2027:2036,
    minor_breaks = NULL
  ) +
  expand_limits(y = 0) +
  # add line at y = 0
  geom_hline(yintercept = 0) +
  theme_minimal()

ggsave(
  filename = file.path(
    "catch-only_projections",
    dir_new,
    "total_catch_by_year.png"
  ),
  width = 8,
  height = 6
)

# update buffer now all forecast catches are fixed, so the fraction can be set to 1
inputs$fore$Flimitfraction_m <- inputs$fore$Flimitfraction_m |>
  dplyr::mutate(fraction = 1)

# make two new sets of inputs
inputs_carryover_full <- inputs
inputs_carryover_full_v2 <- inputs
# inputs_carryover_M_adjusted <- inputs

# update fixed catches in forecast file with the new catch values
inputs_carryover_full$fore$ForeCatch <- catch_new_full
inputs_carryover_full_v2$fore$ForeCatch <- catch_new_full_v2
# inputs_carryover_M_adjusted$fore$ForeCatch <- catch_new_M_adjusted


# write updated files
r4ss::SS_write(
  inputs_carryover_full,
  dir = file.path("catch-only_projections", dir_new, "carryover_full"),
  overwrite = TRUE
)
r4ss::SS_write(
  inputs_carryover_full_v2,
  dir = file.path("catch-only_projections", dir_new, "carryover_full_v2"),
  overwrite = TRUE
)
# r4ss::SS_write(
#   inputs_carryover_M_adjusted,
#   dir = file.path("catch-only_projections", dir_new, "carryover_M_adjusted"),
#   overwrite = TRUE
# )

# run models
r4ss::run(
  file.path("catch-only_projections", dir_new, "carryover_full"),
  skipfinished = FALSE,
  extras = "-nohess -phase 10",
  show_in_console = TRUE
)
r4ss::run(
  file.path("catch-only_projections", dir_new, "carryover_full_v2"),
  skipfinished = FALSE,
  extras = "-nohess -phase 10",
  show_in_console = TRUE
)
# r4ss::run(
#   file.path("catch-only_projections", dir_new, "carryover_M_adjusted"),
#   skipfinished = FALSE,
#   extras = "-nohess -phase 10",
#   show_in_console = TRUE
# )

# read in new model outputs
newoutput_full <- r4ss::SS_output(
  file.path("catch-only_projections", dir_new, "carryover_full"),
  printstats = FALSE,
  verbose = FALSE
)
newoutput_full_v2 <- r4ss::SS_output(
  file.path("catch-only_projections", dir_new, "carryover_full_v2"),
  printstats = FALSE,
  verbose = FALSE
)
# newoutput_M_adjusted <- r4ss::SS_output(
#   file.path("catch-only_projections", dir_new, "carryover_M_adjusted"),
#   printstats = FALSE,
#   verbose = FALSE
# )

# colors from ggplot figure above
cols <- c("#F8766D", "#7CAE00", "#00BFC4", "#C77CFF")

r4ss::SSplotComparisons(
  r4ss::SSsummarize(list(output, newoutput_full, newoutput_full_v2)),
  legendlabels = c("Original", "Carryover 100%", "Carryover 100% v2"),
  xlim = c(2000, 2036),
  subplots = 1,
  endyrvec = 2036,
  print = TRUE,
  plot = FALSE,
  plotdir = file.path("catch-only_projections", dir_new),
  uncertainty = FALSE,
  col = cols[1:3] # 4th color is OFL in plot above
)

output_catch_original <- r4ss::SS_ForeCatch(output)
output_catch_full <- r4ss::SS_ForeCatch(newoutput_full)
output_catch_full_v2 <- r4ss::SS_ForeCatch(newoutput_full_v2)
# output_catch_M_adjusted <- r4ss::SS_ForeCatch(newoutput_M_adjusted)


# TODO: check on small differences in total dead catch across scenarios
# could it be from discard mortality differences? 
# but fixed forecast catches are supposed to include all mortality
output_catch_original$`dead(B)` |> sum()
# [1] 29152.73
output_catch_full$`dead(B)` |> sum()
# [1] 29241.05
output_catch_full_v2$`dead(B)` |> sum()
# [1] 29276.71