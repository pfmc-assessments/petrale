# PacFIN BDS sample sizes
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

# PacFIN age samples
ages <- read.csv("data-raw/pacfin/forSS_annual/Age_for_SS3_ALL_COLUMNS_30.Jun.2023_data_from_08.May.2023.csv") %>% 
  dplyr::select(year, fleet, Ntows, Nsamps, InputN, ageErr) %>%
  dplyr::rename(AgeMat = ageErr, Ntrips = Ntows, Nfish = Nsamps)

ages_north <- ages %>% dplyr::filter(fleet == 1) %>% dplyr::select(!fleet)
ages_south <- ages %>% dplyr::filter(fleet == 2) %>% dplyr::select(!fleet)

names(ages_north)[-1] <- paste("North", names(ages_north)[-1])
names(ages_south)[-1] <- paste("South", names(ages_south)[-1])

# get unique year code (e.g. "X1999"   "X1999.1" "X1999.2") for duplicate years prior to merge
ages_north <- data.frame(code = make.names(ages_north$year, unique = TRUE), ages_north, check.names = FALSE)
ages_south <- data.frame(code = make.names(ages_south$year, unique = TRUE), ages_south, check.names = FALSE)

ages_table <- dplyr::full_join(ages_north, ages_south) %>% 
  dplyr::arrange(code) %>% 
  dplyr::select(-code)

ages_table[is.na(ages_table)] <- 0
ages_table$"North AgeMat"[ages_table$"North AgeMat" == 0] <- NA
ages_table$"South AgeMat"[ages_table$"South AgeMat" == 0] <- NA
ageerr_codes  <-  c(
        "none",
        "C-BB",
        "C-S2",
        "C-C",
        "W-C",
        "W-S",
        "W-BB",
        "C-S1"
        )
# replace numbers with codes
ages_table$"North AgeMat" <- ageerr_codes[ages_table$"North AgeMat"]
ages_table$"South AgeMat" <- ageerr_codes[ages_table$"South AgeMat"]

write.csv(ages_table, "tables/age_samps_comm_by_fleet.csv", row.names = FALSE)
# Survey sample sizes ()

# get text for caption
ageerr_names2 = c(
      "no error", # 1
      "CAP Break & Burn", # 2
      "CAP Surface", # 3
      "CAP Combo", # 4
      "WDFW Combo", # 5
      "WDFW Surface", # 6
      "WDFW Break & Burn", # 7
      "CAP Surface Pre-1990" # 8
    )
    ageerr_codes = c(
        "none",
        "C-BB",
        "C-S2",
        "C-C",
        "W-C",
        "W-S",
        "W-BB",
        "C-S1")
    
    paste(paste(ageerr_codes, ageerr_names2, sep = ": ")[c(8,3,4,2,6,5,7)], collapse = ", ")
# [1] "C-S1: CAP Surface Pre-1990, C-S2: CAP Surface, C-C: CAP Combo, C-BB: CAP Break & Burn, W-S: WDFW Surface, W-C: WDFW Combo, W-BB: WDFW Break & Burn"