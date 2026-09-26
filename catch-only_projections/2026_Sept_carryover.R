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
names(catch)
# [1] "#Year"   "Seas"    "Fleet"   "dead(B)" "comment"
# remove comment column from catch data which is hard to keep in sync with changes
catch <- catch[, -which(names(catch) == "comment")]
# improve names for clarity using dplyr::rename()
catch <- dplyr::rename(catch, Year = "#Year", Catch = "dead(B)")

# get the OFL and ACL limits for the years 2027 to 2036
limits <- data.frame(
  Year = 2027:2036,
  OFL = output$derived_quants[paste0("OFLCatch_", 2027:2036), "Value"],
  ACL = output$derived_quants[paste0("ForeCatch_", 2027:2036), "Value"]
) |>
  dplyr::mutate(
    Ratio = ACL / OFL,
    Diff = OFL - ACL
  )

# make a function to calculate the difference between OFL and ACL for a given year
# in absolute terms, and then calculate the ratios to scale the fleet-level catches accordingly
# both up from the ACL to the OFL in some years or down by the same amount (not the same ratio)
# in other years
calc_diff_ratio <- function(catch_table, year) {
  # combined-fleet ratio
  ratio <- limits |> dplyr::filter(Year == year) |> dplyr::pull(Ratio)
  # fleet-specific differences associated with that ratio
  OFL <- limits |> dplyr::filter(Year == year) |> dplyr::pull(OFL)
  ACL <- limits |> dplyr::filter(Year == year) |> dplyr::pull(ACL)
  catch_table |>
    dplyr::filter(Year == year) |>
    dplyr::mutate(
      Increased_Catch = Catch / ratio,
      Catch_diff = Increased_Catch - Catch
    ) |>
    dplyr::pull(Catch_diff)
}

# scale the fleet-level catches so the catch with carryover equals the full OFL
catch_new_full <- catch |>
  dplyr::mutate(
    Catch = dplyr::case_when(
      Year == 2027 ~ Catch - calc_diff_ratio(catch, 2029),
      Year == 2028 ~ Catch - calc_diff_ratio(catch, 2030),
      Year == 2029 ~ Catch + calc_diff_ratio(catch, 2029),
      Year == 2030 ~ Catch + calc_diff_ratio(catch, 2030),
      Year == 2031 ~ Catch - calc_diff_ratio(catch, 2033),
      Year == 2032 ~ Catch - calc_diff_ratio(catch, 2034),
      Year == 2033 ~ Catch + calc_diff_ratio(catch, 2033),
      Year == 2034 ~ Catch + calc_diff_ratio(catch, 2034),
      TRUE ~ Catch
    )
  )

# get M as the mean of the natural mortality for females and males
# TODO: if there is Lorenzen or other age-specific M, calculate accordingly
# such as from age-specific values in output$endgrowth$M
# also would need to change for single-sex models
M <- mean(
  c(
    output$parameters["NatM_uniform_Fem_GP_1", "Value"],
    output$parameters["NatM_uniform_Mal_GP_1", "Value"]
  )
)
# discount is 0.7433405 for petrale
exp(-2 * M)

# option 1 for M adjustments has the same underutilization as
# the "full" case so after discount by M, the catch + carryover
# is less than the full OFL
catch_new_M_adjusted1 <- catch |>
  dplyr::mutate(
    Year = Year,
    Catch = dplyr::case_when(
      Year == 2027 ~ Catch - calc_diff_ratio(catch, 2029),
      Year == 2028 ~ Catch - calc_diff_ratio(catch, 2030),
      Year == 2029 ~ Catch + calc_diff_ratio(catch, 2029) * exp(-2 * M),
      Year == 2030 ~ Catch + calc_diff_ratio(catch, 2030) * exp(-2 * M),
      Year == 2031 ~ Catch - calc_diff_ratio(catch, 2033),
      Year == 2032 ~ Catch - calc_diff_ratio(catch, 2034),
      Year == 2033 ~ Catch + calc_diff_ratio(catch, 2033) * exp(-2 * M),
      Year == 2034 ~ Catch + calc_diff_ratio(catch, 2034) * exp(-2 * M),
      TRUE ~ Catch
    )
  )

# option 2 has larger underutilization so catch plus carryover equals
# the full OFL even after discount by exp(-2*M)
catch_new_M_adjusted2 <- catch |>
  dplyr::mutate(
    Year = Year,
    Catch = dplyr::case_when(
      Year == 2027 ~ Catch - calc_diff_ratio(catch, 2029) / exp(-2 * M),
      Year == 2028 ~ Catch - calc_diff_ratio(catch, 2030) / exp(-2 * M),
      Year == 2029 ~ Catch + calc_diff_ratio(catch, 2029),
      Year == 2030 ~ Catch + calc_diff_ratio(catch, 2030),
      Year == 2031 ~ Catch - calc_diff_ratio(catch, 2033) / exp(-2 * M),
      Year == 2032 ~ Catch - calc_diff_ratio(catch, 2034) / exp(-2 * M),
      Year == 2033 ~ Catch + calc_diff_ratio(catch, 2033),
      Year == 2034 ~ Catch + calc_diff_ratio(catch, 2034),
      TRUE ~ Catch
    )
  )

# option 3 for M adjustments, like option 1, has the same underutilization as
# the "full" case so after discount by M, the catch + carryover
# is less than the full OFL, but in this case the discount is half as large: exp(-M)
catch_new_M_adjusted3 <- catch |>
  dplyr::mutate(
    Year = Year,
    Catch = dplyr::case_when(
      Year == 2027 ~ Catch - calc_diff_ratio(catch, 2029),
      Year == 2028 ~ Catch - calc_diff_ratio(catch, 2030),
      Year == 2029 ~ Catch + calc_diff_ratio(catch, 2029) * exp(-M),
      Year == 2030 ~ Catch + calc_diff_ratio(catch, 2030) * exp(-M),
      Year == 2031 ~ Catch - calc_diff_ratio(catch, 2033),
      Year == 2032 ~ Catch - calc_diff_ratio(catch, 2034),
      Year == 2033 ~ Catch + calc_diff_ratio(catch, 2033) * exp(-M),
      Year == 2034 ~ Catch + calc_diff_ratio(catch, 2034) * exp(-M),
      TRUE ~ Catch
    )
  )

# combine old and new annual tallies for plotting purposes
# to confirm that the adjustments have been applied correctly
catch_combined <- dplyr::bind_rows(
  catch |> dplyr::mutate(source = "2025 catch-only projection"),
  catch_new_full |> dplyr::mutate(source = "Carryover 100%"),
  catch_new_M_adjusted1 |> dplyr::mutate(source = "Carryover M-adjusted"),
  catch_new_M_adjusted2 |>
    dplyr::mutate(source = "Carryover M-adjusted (larger)"),
  catch_new_M_adjusted3 |>
    dplyr::mutate(source = "Carryover M-adjusted (half M)"),
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
inputs_carryover_M_adjusted1 <- inputs
inputs_carryover_M_adjusted2 <- inputs
inputs_carryover_M_adjusted3 <- inputs

# update fixed catches in forecast file with the new catch values
inputs_carryover_full$fore$ForeCatch <- catch_new_full
inputs_carryover_M_adjusted1$fore$ForeCatch <- catch_new_M_adjusted1
inputs_carryover_M_adjusted2$fore$ForeCatch <- catch_new_M_adjusted2
inputs_carryover_M_adjusted3$fore$ForeCatch <- catch_new_M_adjusted3

# write updated files
r4ss::SS_write(
  inputs_carryover_full,
  dir = file.path("catch-only_projections", dir_new, "carryover_full"),
  overwrite = TRUE
)
r4ss::SS_write(
  inputs_carryover_M_adjusted1,
  dir = file.path("catch-only_projections", dir_new, "carryover_M_adjusted1"),
  overwrite = TRUE
)
r4ss::SS_write(
  inputs_carryover_M_adjusted2,
  dir = file.path("catch-only_projections", dir_new, "carryover_M_adjusted2"),
  overwrite = TRUE
)
r4ss::SS_write(
  inputs_carryover_M_adjusted3,
  dir = file.path("catch-only_projections", dir_new, "carryover_M_adjusted3"),
  overwrite = TRUE
)

# run models
for (dir in c(
  "carryover_full",
  "carryover_M_adjusted1",
  "carryover_M_adjusted2",
  "carryover_M_adjusted3"
)) {
  r4ss::run(
    file.path("catch-only_projections", dir_new, dir),
    skipfinished = FALSE,
    extras = "-nohess -phase 10",
    show_in_console = TRUE
  )
}

# read in new model outputs
newoutput_full <- r4ss::SS_output(
  file.path("catch-only_projections", dir_new, "carryover_full"),
  printstats = FALSE,
  verbose = FALSE
)
newoutput_M_adjusted1 <- r4ss::SS_output(
  file.path("catch-only_projections", dir_new, "carryover_M_adjusted1"),
  printstats = FALSE,
  verbose = FALSE
)
newoutput_M_adjusted2 <- r4ss::SS_output(
  file.path("catch-only_projections", dir_new, "carryover_M_adjusted2"),
  printstats = FALSE,
  verbose = FALSE
)
newoutput_M_adjusted3 <- r4ss::SS_output(
  file.path("catch-only_projections", dir_new, "carryover_M_adjusted3"),
  printstats = FALSE,
  verbose = FALSE
)

# colors from ggplot figure above
default_hex <- scales::hue_pal()(6)

model_summary <- r4ss::SSsummarize(list(
  output,
  newoutput_full,
  newoutput_M_adjusted1,
  newoutput_M_adjusted3, # note: order change to match alphabetical adjustments in ggplot above
  newoutput_M_adjusted2
))
model_summary$SpawnOutputLabels <- rep(
  "Spawning output (trillions of eggs)",
  1
  #model_summary$n
)
r4ss::SSplotComparisons(
  model_summary,
  legendlabels = c(
    "Original",
    "Carryover 100%",
    "Carryover M adjusted",
    "Carryover M adjusted (half M)",
    "Carryover M adjusted (larger)"
  ),
  xlim = c(2020, 2037),
  subplots = c(1, 3),
  endyrvec = 2037,
  print = TRUE,
  plot = FALSE,
  plotdir = file.path("catch-only_projections", dir_new),
  uncertainty = FALSE,
  col = default_hex[1:5] # 6th color is OFL in plot above
)

output_catch_original <- r4ss::SS_ForeCatch(output, yrs = 2027:2036)
output_catch_full <- r4ss::SS_ForeCatch(newoutput_full, yrs = 2027:2036)
output_catch_M_adjusted1 <- r4ss::SS_ForeCatch(
  newoutput_M_adjusted1,
  yrs = 2027:2036
)
output_catch_M_adjusted2 <- r4ss::SS_ForeCatch(
  newoutput_M_adjusted2,
  yrs = 2027:2036
)
output_catch_M_adjusted3 <- r4ss::SS_ForeCatch(
  newoutput_M_adjusted3,
  yrs = 2027:2036
)

# add cli message if these three values aren't all equal
if (
  !all.equal(
    output_catch_original$`dead(B)` |> sum() |> round(1),
    output_catch_full$`dead(B)` |> sum() |> round(1)
  )
) {
  cli::cli_alert_danger(
    "Total dead catch for full scenario differs from original"
  )
} else {
  cli::cli_alert_success(
    "Total dead catch is consistent between original and full"
  )
}

output_catch_original
#    #Year Seas Fleet dead(B)                comment
# 1   2027    1     1 1825.32    #sum_for_2027: 2489
# 2   2027    1     2  663.68
# 3   2028    1     1 1825.32    #sum_for_2028: 2489
# 4   2028    1     2  663.68
# 5   2029    1     1 1758.15 #sum_for_2029: 2408.66
# 6   2029    1     2  650.51

# function gets the year, fleet, and dead(B) columns, then adds
# year-specific fleet totals and grand totals averaged across years
add_fleet_totals <- function(catch_table, scenario = "original") {
  fleet_catch <- catch_table |>
    dplyr::select(Year = `#Year`, Fleet, Catch = `dead(B)`) |>
    dplyr::mutate(Year = as.character(Year), Fleet = as.character(Fleet))

  total_by_year <- fleet_catch |>
    dplyr::group_by(Year) |>
    dplyr::summarise(
      Fleet = "Total",
      Catch = sum(Catch),
      .groups = "drop"
    )

  grand_mean_by_fleet <- fleet_catch |>
    dplyr::group_by(Fleet) |>
    dplyr::summarise(
      Catch = mean(Catch),
      .groups = "drop"
    ) |>
    dplyr::mutate(Year = "Mean") |>
    dplyr::select(Year, Fleet, Catch)

  grand_mean <- grand_mean_by_fleet |>
    dplyr::summarise(
      Year = "Mean",
      Fleet = "Total",
      Catch = mean(total_by_year$Catch)
    )

  dplyr::bind_rows(
    fleet_catch,
    total_by_year,
    grand_mean_by_fleet,
    grand_mean
  ) |>
    dplyr::arrange(Year, Fleet) |>
    dplyr::mutate(Catch = round(Catch, 1)) |>
    dplyr::mutate(Scenario = scenario)
}

# create table of catch by year for each scenario
catch_by_year <- dplyr::bind_rows(
  add_fleet_totals(
    output_catch_original,
    scenario = "Original"
  ),
  add_fleet_totals(
    output_catch_full,
    scenario = "Carryover 100%"
  ),
  add_fleet_totals(
    output_catch_M_adjusted1,
    scenario = "Carryover M adjusted"
  ),
  add_fleet_totals(
    output_catch_M_adjusted2,
    scenario = "Carryover M adjusted (larger)"
  ),
  add_fleet_totals(
    output_catch_M_adjusted3,
    scenario = "Carryover M adjusted (half M)"
  )
) |>
  dplyr::group_by(Year, Fleet, Scenario) |>
  dplyr::summarise(Catch = sum(Catch), .groups = "drop") |>
  tidyr::pivot_wider(names_from = Scenario, values_from = Catch) |>
  # put the Original scenario column first among the catch tables
  dplyr::select(Year, Fleet, Original, dplyr::everything())

# convert to HTML (opens in browser for Ian)
catch_by_year |> gt::gt()

write.csv(
  catch_by_year,
  file.path("catch-only_projections", dir_new, "catch_by_fleet_and_year.csv"),
  row.names = FALSE
)

# filter out the fleet-specific rows
catch_totals_by_year <- catch_by_year |>
  dplyr::filter(Fleet == "Total") |>
  dplyr::select(-Fleet)

catch_totals_by_year <-
  dplyr::bind_rows(
    catch_totals_by_year,
    tibble::tibble(
      Year = "OFL in 2036",
      model_summary$quants |>
        dplyr::filter(Label == "OFLCatch_2036") |>
        dplyr::select(1:(ncol(catch_totals_by_year) - 1)) |>
        round(1) |>
        dplyr::rename_with(~ names(catch_totals_by_year)[-1])
    ),
    tibble::tibble(
      Year = "ACL in 2036",
      model_summary$quants |>
        dplyr::filter(Label == "ForeCatch_2036") |>
        dplyr::select(1:(ncol(catch_totals_by_year) - 1)) |>
        round(1) |>
        dplyr::rename_with(~ names(catch_totals_by_year)[-1])
    ),
    # tibble::tibble(
    #   Year = "Frac. unfished in 2036",
    #   model_summary$Bratio |>
    #     dplyr::filter(Yr == 2036) |>
    #     dplyr::select(1:(ncol(catch_totals_by_year) - 1)) |>
    #     round(3) |>
    #     dplyr::rename_with(~ names(catch_totals_by_year)[-1])
    # )
  )

catch_totals_by_year |> gt::gt()


write.csv(
  catch_totals_by_year,
  file.path("catch-only_projections", dir_new, "catch_totals_by_year.csv"),
  row.names = FALSE
)
