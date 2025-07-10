# read 2023 petrale model
inputs_original <- r4ss::SS_read("models/2023.a034.011_forecast_SR")
#inputs_original <- r4ss::SS_read("models/2023.a034.920_base_Pstar45")
inputs <- inputs_original
inputs$fore$Flimitfraction_m
#                      year fraction
# #_Flimitfraction_m1  2023    1.000
# #_Flimitfraction_m2  2024    1.000
# #_Flimitfraction_m3  2025    0.935
# #_Flimitfraction_m4  2026    0.930
# #_Flimitfraction_m5  2027    0.926
# #_Flimitfraction_m6  2028    0.922
# #_Flimitfraction_m7  2029    0.917
# #_Flimitfraction_m8  2030    0.913
# #_Flimitfraction_m9  2031    0.909
# #_Flimitfraction_m10 2032    0.904
# #_Flimitfraction_m11 2033    0.900
# #_Flimitfraction_m12 2034    0.896

inputs$fore$Flimitfraction_m <- PEPtools::get_buffer(
    years = 2023:2036,
    sigma = 0.5,
    pstar = 0.45
)
# change the buffer column to 1.0 for the years with fixed forecast catches
inputs$fore$Flimitfraction_m[inputs$fore$Flimitfraction_m$year <= 2026, "buffer"] <- 1.0

# GMT catches emailed by Brian on Jun 10, 2025, also found in
# https://docs.google.com/spreadsheets/d/1UtxsXxbwQTWMgn1TwYZaCyptq-S0BZtO3wGH0LJ7M1w/edit?gid=537713331#gid=537713331
# fmt: skip
GMT_catch <- tibble::tribble(
    ~Year, ~Com_north_WA_OR, ~Com_south_CA,
    2023, 2018.1, 884.2,
    2024, 2246.0, 681.4,
    2025, 1588.6, 568.6,
    2026, 1515.1, 536.4
)

# convert GMT_catches to format required by SS3
GMT_catch <- GMT_catch |>
    tidyr::pivot_longer(
        cols = -Year,
        names_to = "fleet",
        values_to = "catch_or_F"
    ) |>
    # uncapitalize year
    dplyr::rename(year = Year) |>
    # add season and convert names to fleet numbers
    dplyr::mutate(
        seas = 1,
        fleet = dplyr::case_when(
            fleet == "Com_north_WA_OR" ~ 1,
            fleet == "Com_south_CA" ~ 2
        )
    ) |>
    # order columns
    dplyr::select(year, seas, fleet, catch_or_F)


inputs$fore$ForeCatch <- GMT_catch

# increase the number of years in the forecast
old_Nforecastyrs <- inputs$fore$Nforecastyrs
inputs$fore$Nforecastyrs <- 2036 - inputs$dat$endyr
# calculate new years to add to the forecast
additional_years <- inputs$dat$endyr +
    old_Nforecastyrs +
    1:(inputs$fore$Nforecastyrs - old_Nforecastyrs)

# modify .par file to add forecast years
inputs$par$recdev_forecast <- rbind(
    inputs$par$recdev_forecast,
    data.frame(year = additional_years, recdev = 0)
)

# check that the .par file is being used
if (inputs$start$init_values_src != 1) {
    stop(
        "The .par file is not being used. Change things around to adapt to that."
    )
}

# write new model inputs
r4ss::SS_write(
    inputs,
    dir = "catch-only_projections/2025_standard",
    overwrite = TRUE
)

r4ss::run(
    dir = "catch-only_projections/2025_standard",
    extras = "-nohess -phase 10", # start in late phase if using .par as input
    show_in_console = TRUE,
    skipfinished = FALSE
)

quarto::quarto_render(
    file.path(
        "catch-only_projections",
        "catch_only_projection_petrale_2025_1table.qmd"
    )
)

### alternative 1: apply 2025 attainment to 2026 ACL to get 2026 catches

# read output from standard catch-only projection above
run1 <- r4ss::SS_output("catch-only_projections/2025_standard", verbose = FALSE, printstats = FALSE)
# read input from standard catch-only projection above to use for alterative approach
inputs2 <- r4ss::SS_read("catch-only_projections/2025_standard")

inputs2$fore$Flimitfraction_m <- PEPtools::get_buffer(
    years = 2023:2036,
    sigma = 0.5,
    pstar = 0.45
)
# change the buffer column to 1.0 for the years with fixed forecast catches
# differs from run 1 in only fixing catches up through 2025
inputs2$fore$Flimitfraction_m[inputs$fore$Flimitfraction_m$year <= 2025, "buffer"] <- 1.0
# buffer for 2026 is 92% of the HCR buffer
inputs2$fore$Flimitfraction_m[inputs$fore$Flimitfraction_m$year == 2026, "buffer"] <- 
  0.92 * inputs2$fore$Flimitfraction_m[inputs$fore$Flimitfraction_m$year == 2026, "buffer"]

# remove 2026 fixed catches
inputs2$fore$ForeCatch <- inputs2$fore$ForeCatch |>
    dplyr::filter(year <= 2025)

# write run2 inputs
r4ss::SS_write(
    inputs2,
    dir = "catch-only_projections/2025_alternative1",
    overwrite = TRUE
)

r4ss::run(
    dir = "catch-only_projections/2025_alternative1",
    extras = "-nohess -phase 10", # start in late phase if using .par as input
    show_in_console = TRUE,
    skipfinished = FALSE
)
run2 <- r4ss::SS_output("catch-only_projections/2025_alternative1", verbose = FALSE, printstats = FALSE)