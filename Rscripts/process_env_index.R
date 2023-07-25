# The current version of SS3 (3.30.21.00) can estimate a catchability
# parameter to rescale the estimated recdevs to match the observed 
# variability of a normally distributed index, but it can't shift the
# observed or expected values. Rescaling the index to have mean = 0 
# reduces that mismatch.

# read index supplied by Nick Tolimieri by email on 6 June 2023
env_index <- read.csv("data-raw/env_index/Petrale_glorys_index.csv")

head(env_index)
#   year         fit         se     recdev      stdv
# 1 1993 -0.01434852 0.08691477  0.0585736 0.2036850
# 2 1994 -0.26940561 0.06556901  0.2400430 0.1735190
# 3 1995 -0.23715130 0.06721877 -0.3434000 0.2052190
# 4 1996 -0.26419213 0.06590863 -0.2280370 0.1666110
# 5 1997 -0.12396710 0.07249955 -0.2220600 0.1652430
# 6 1998  0.47977526 0.14354019  0.6543340 0.0887485

env_index_raw <- env_index %>%
  dplyr::select(year, fit, se) %>% # select required columns
  dplyr::mutate(month = 1, fleet = 5, .after = year) # add columns
write.csv(env_index_raw, 
  "data-raw/env_index/Petrale_glorys_index_with_2023_for_SS3_27-June-2023.csv", 
  row.names = FALSE)

mean(env_index$fit)
# [1] 0.04512026

env_index_rescaled <- env_index %>%
  dplyr::select(year, fit, se) %>% # select required columns
  dplyr::mutate(month = 1, fleet = 5, .after = year) %>% # add columns
  dplyr::mutate(fit = fit - mean(fit)) # rescale by the mean across all years
env_index_rescaled2 <- env_index %>%
  dplyr::select(year, fit, se) %>% # select required columns
  dplyr::mutate(month = 1, fleet = 5, .after = year) %>% # add columns
  dplyr::mutate(fit = fit + 0.08) # rescale by an estimate of the devs in index fit to previous model

mean(env_index_rescaled$fit)
mean(env_index_rescaled2$fit)
# write index with 2023 included
write.csv(env_index_rescaled, "data-raw/env_index/Petrale_glorys_index_with_2023_for_SS3_7-June-2023.csv")
write.csv(env_index_rescaled2, 
  "data-raw/env_index/Petrale_glorys_index_with_2023_for_SS3_26-June-2023.csv", 
  row.names = FALSE)

# remove 2023
env_index_rescaled <- env_index_rescaled %>%
  dplyr::filter(year < 2023) %>%
  dplyr::mutate(fit = fit - mean(fit)) # rescale again without 2023
mean(env_index_rescaled$fit)
write.csv(env_index_rescaled, "data-raw/env_index/Petrale_glorys_index_for_SS3_7-June-2023.csv")

# check that mean recruitment in a recent model is close to zero
if ('2023.a024.001_min_sample' %in% dir('models')) {
  p24.1 <- SS_output('models/2023.a024.001_min_sample/') 
  p24.1$recruit %>% 
    dplyr::filter(Yr %in% 1993:2022) %>% 
    dplyr::pull("dev") %>%
    mean()

  # [1] 0.004292205 ## close enough
}