today_date <- "26.May.2023"
out_date <- "08.May.2023"
load(paste0(
    "data-raw/pacfin/len_comps_all_", today_date,
    "_data_from_", out_date, ".Rdata"
  )
)

samps1 <- len_comps_annual2 %>% 
  dplyr::filter(fleet == 1) %>%
  dplyr::select(year, Ntows, Nsamps, InputN) %>%
  dplyr::rename(Trips_North = Ntows, Fish_North = Nsamps, InputN_North = InputN)

samps2 <- len_comps_annual2 %>% 
  dplyr::filter(fleet == 2) %>%
  dplyr::select(year, Ntows, Nsamps, InputN) %>%
  dplyr::rename(Trips_South = Ntows, Fish_South = Nsamps, InputN_South = InputN)

samps0 <- data.frame(year = min(len_comps_annual2$year):max(len_comps_annual2$year))

len_samps <- dplyr::full_join(samps1, samps2, by = "year")
# fill in missing years
len_samps <- dplyr::full_join(samps0, len_samps, by = "year")
len_samps[is.na(len_samps)] <- 0
len_samps <- len_samps %>% 
  dplyr::arrange(year) %>%
  dplyr::filter(year < 2023)


write.csv(len_samps, 
  file = "tables/length_samps_comm_by_fleet.csv",
  row.names = FALSE)

