# code copied from https://github.com/pfmc-assessments/canary_2023/blob/main/code/canary_pacfin_comps.R

pacfin <- bds.pacfin
  #---------------------------------------------------------------------
  #Pull out WDFW aged fish that have multiple reads of B&B to use for ageing error
  #Cleaned data exclude reader info, so join based on SAMPLE_NUMBER and FISH_SEQUENCE_NUMBER
  #from original data and SAMPLE_NO and FISH_NO in cleaned data. These are not unique for CA
  #but are within WA and OR data
  wa_dReads <- Pdata %>% 
    dplyr::filter(state=="WA" & !(is.na(age1) & is.na(age2) & is.na(age3))) %>% 
    dplyr::mutate(mult = dplyr::case_when(
      (!is.na(age1) & !is.na(age2) & !is.na(age3)) == TRUE ~ 3,
      (!is.na(age1) & !is.na(age2)) == TRUE ~ 2,
      (!is.na(age2) & !is.na(age3)) == TRUE ~ 2,
      (!is.na(age1) & !is.na(age3)) == TRUE ~ 2,
      TRUE ~ 1)) %>%
    dplyr::filter(mult > 1 & !(AGE_METHOD1 %in% c("S") & mult == 2)) %>%
    dplyr::left_join(.,pacfin[pacfin$AGENCY_CODE=="W",
                              c("SAMPLE_NUMBER","FISH_SEQUENCE_NUMBER","agedby1","agedby2","agedby3")],
                     by = dplyr::join_by("SAMPLE_NO" == "SAMPLE_NUMBER", "FISH_NO" == "FISH_SEQUENCE_NUMBER"))
  #---------------------------------------------------------------------

table(paste(wa_dReads$agedby1, wa_dReads$agedby2, wa_dReads$ageby3, sep = ","))

