gemm <- nwfscSurvey::pull_gemm() %>% dplyr::filter(species == "Petrale Sole")

write.csv(gemm, 
  file = "data-raw/catch/gemm_6-July-2023.csv", 
  row.names = FALSE)

gemm2017 <- gemm %>% dplyr::filter(year == 2017)

# only Oregon Rec has an impact from mortality rates
table(gemm2017$total_discard_and_landings_mt / gemm2017$total_discard_with_mort_rates_applied_and_landings_mt)
    #    1 1.09729486200871
    #   25                1
gemm2017 %>% dplyr::filter(total_discard_and_landings_mt != 
  total_discard_with_mort_rates_applied_and_landings_mt)
#   i_cv     grouping              sector      species total_discard_and_landings_mt total_discard_mt total_discard_with_mort_rates_applied_and_landings_mt total_discard_with_mort_rates_applied_mt total_landings_mt       type year
# 1   NA Petrale sole Oregon Recreational Petrale Sole                      4.133311        0.3919136                                              3.766819                               0.02542142          3.741398 groundfish 2017

gemm2017 <- gemm2017 %>% dplyr::select(sector, total_discard_and_landings_mt) %>% dplyr::as_tibble()
sum(gemm2017$total_discard_and_landings_mt)
gemm2017 %>% dplyr::filter(total_discard_and_landings_mt > 5)
