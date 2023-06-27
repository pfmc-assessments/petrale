# get count of parameters in base and 2019 models
parcounts_base <- table_parcounts(mod_base) %>% dplyr::rename(Base = Count)
parcounts_2019 <- table_parcounts(mod.2019.001.001_base)

# combine the two tables
tab <- data.frame(
  parcounts_base,
  model2019 = parcounts_2019$Count,
  check.names = FALSE
) %>%
  dplyr::filter(Base > 0 | model2019 > 0) %>% # remove zero-parameter rows
  dplyr::rename("2019 model" = model2019)


# add notes
tab$Note <- ""
tab$Note[tab$Type == "Growth mean"]  <- "Females and males share the L_at_Amin parameter in 2023"
tab$Note[tab$Type == "Stock-recruit"]  <- "Steepness now fixed at 0.8"
tab$Note[tab$Type == "Rec. dev. time series"]  <- "Extended by 4 years (2019-2022)"
tab$Note[tab$Type == "Index"]  <- paste("2019 model had 4 catchability parameters related to fishery CPUE",
  "(base and power for 2 fleets) and 2 extra SD pars for early and late Triennial vs. 1 in 2023 base")
tab$Note[tab$Type == "Index time-variation"]  <- paste("2019 model had a change in fishery CPUE catchability in 2004")
tab$Note[tab$Type == "Size selectivity"]  <- paste("2019 model had separate Winter and Summer fishery fleets")
tab$Note[tab$Type == "Size selectivity time-variation"]  <- paste("Fewer fishery fleets in 2023 but more parameters are time-varying")
tab$Note[tab$Type == "Retention"]  <- paste("Fewer fishery fleets in 2023")
tab$Note[tab$Type == "Retention time-variation"]  <- paste("Fewer fishery fleets in 2023")

write.csv(tab, "tables/par_summary.csv", row.names = FALSE)


outputconfig_base <- table_outputconfig(mod_base) %>% 
  dplyr::rename(Base = Parameterization)
outputconfig_2019 <- table_outputconfig(mod.2019.001.001_base)

# combine the two tables
tab <- data.frame(
  outputconfig_base,
  model2019 = outputconfig_2019$Parameterization,
  check.names = FALSE
) %>%
  dplyr::rename("2019 model" = model2019)

write.csv(tab, "tables/config_summary.csv", row.names = FALSE)
