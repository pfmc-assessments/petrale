# November 2025 revision to catch only projections for petrale sole
dir_new2 <- "2025_alternative1_3.30.24_july15"
dir_new3 <- "2025_Nov20"
r4ss::copy_SS_inputs(
    dir.old = file.path("catch-only_projections", dir_new2),
    dir.new = file.path("catch-only_projections", dir_new3),
    copy_par = TRUE,
    copy_exe = TRUE,
    overwrite = TRUE
)

output <- r4ss::SS_output(file.path("catch-only_projections", dir_new2))
fore <- r4ss::SS_readforecast(file.path(
    "catch-only_projections",
    dir_new2,
    "forecast.ss"
))

# modify forecast to have fixed catch inputs through 2028
catch <- r4ss::SS_ForeCatch(output)
catch2027 <- catch |> dplyr::filter(`#Year` == 2027)
newcatch <- catch2027 |>
    dplyr::mutate(`dead(B)` = `dead(B)` * 2489 / sum(`dead(B)`))
newcatch <- rbind(newcatch, newcatch |> dplyr::mutate(`#Year` = `#Year` + 1)) |>
    dplyr::select(-comment)
names(newcatch) <- names(fore$ForeCatch)
fore$ForeCatch <- rbind(
    fore$ForeCatch,
    newcatch
)
# update buffer (note that the 2026 buffer is set to 0.8556 = 0.93 * 0.92 to reflect an attainment of 0.92 in 2026)
fore$Flimitfraction_m <- fore$Flimitfraction_m |>
    dplyr::mutate(fraction = ifelse(year %in% 2027:2028, 1, fraction))

# write updated forecast file
r4ss::SS_writeforecast(
    fore,
    dir = file.path("catch-only_projections", dir_new3),
    overwrite = TRUE,
    verbose = TRUE
)
r4ss::run(
    file.path("catch-only_projections", dir_new3),
    skipfinished = FALSE,
    extras = "-nohess -phase 10",
    show_in_console = TRUE
)

newoutput <- r4ss::SS_output(
    file.path("catch-only_projections", dir_new3),
    printstats = FALSE,
    verbose = FALSE
)
# confirm that the 2027 and 2028 catches are as expected
r4ss::SS_ForeCatch(newoutput) |>
    dplyr::filter(`#Year` %in% c(2026:2028))    

