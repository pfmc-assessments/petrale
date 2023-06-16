# get U.S.-landed catch from area "3S"
# depends on confidential file only available to Vlada and Ian
# which is described as "SOUTHERN PORTION OF AREA 3C (UNITED STATES ONLY)"
load("data-raw/PacFIN.PTRL.CompFT.27.Jan.2023.RData")
catch_agg <- aggregate(LANDED_WEIGHT_MTONS ~ LANDING_YEAR + PACFIN_CATCH_AREA_CODE, data = catch.pacfin, FUN = sum)
catch_agg_3S <- catch_agg[catch_agg$PACFIN_CATCH_AREA_CODE == "3S",]
plot(catch_agg_3S$LANDING_YEAR, catch_agg_3S$LANDED_WEIGHT_MTONS, type = 'h', lwd = 10, lend = 3)
write.csv(catch_agg_3S, file = "data-raw/pacfin/catch_US_portion_of_3C.csv",
          row.names = FALSE)

# load different set of data
load('data-raw/foreign/PacFIN.PTRL.CompFT.27.Feb.2023.RData')
catch_agg_CAN <- aggregate(LANDED_WEIGHT_MTONS ~ LANDING_YEAR + PACFIN_CATCH_AREA_CODE, data = catch.pacfin, FUN = sum)
plot(catch_agg_CAN$LANDING_YEAR, catch_agg_CAN$LANDED_WEIGHT_MTONS, type = 'h', lwd = 10, lend = 3)

library(tidyverse)
# which years have too few vessels
dplyr::group_by(catch.pacfin,LANDING_YEAR) %>% 
  dplyr::summarize(count=dplyr::n_distinct(VESSEL_ID)) %>% 
  dplyr::filter(count<3)
# # A tibble: 8 x 2
#   LANDING_YEAR count
#          <int> <int>
# 1         2008     1
# 2         2009     2
# 3         2010     1
# 4         2011     1
# 5         2013     1
# 6         2014     1
# 7         2021     2
# 8         2022     1

# which areas have too few vessels
dplyr::group_by(catch.pacfin, PACFIN_CATCH_AREA_NAME) %>%
  dplyr::summarize(count=dplyr::n_distinct(VESSEL_ID)) %>% 
  dplyr::filter(count<3)
# # A tibble: 2 x 2
#   PACFIN_CATCH_AREA_NAME count
#   <chr>                  <int>
# 1 9Z                         1
# 2 YAKUTAT                    2

table(catch.pacfin$LANDING_YEAR, catch.pacfin$PACFIN_CATCH_AREA_CODE)

  #       3D  3N  4A  6A  6B  9Z  CT  GS  SE  VC  YK
  # 1981  67   0  15   0   0   0   0   0   0   3   0
  # 1982   0   0   8   0   0   0   0   0   0   0   0
  # 1983   0   0  10   0   0   0   0   0   0   1   0
  # 1984   0   0  26   0   0   0   1   0   0  19   0
  # 1985   0   0   8  17   0   0   6   6   2  22   0
  # 1986   0   0  11   0   0   0  22   7   0  29   0
  # 1987   0   0   2   0   0   0  27   0   0  20   0
  # 1988   0   0   8   0   0   0  21   0   0  23   0
  # 1989   0  81   6   0   0   0  20   1   0  25   0

table(catch.pacfin$PACFIN_CATCH_AREA_NAME)
  #     3C-N         3D         4A         6A         6B         9Z  CHARLOTTE GRGIA STRT S. EASTERN 
  #      900         67       1265         17         15          1        837         22          7 
  # VNCVR-BC    YAKUTAT
  #      958          2

# summarize total catch by area
catch.pacfin %>% dplyr::group_by(PACFIN_CATCH_AREA_NAME) %>% 
  dplyr::summarize(sum = sum(LANDED_WEIGHT_MTONS))

# # A tibble: 11 x 2
#    PACFIN_CATCH_AREA_NAME        sum
#    <chr>                       <dbl>
#  1 3C-N                      1.78   
#  2 3D                        2.43   
#  3 4A                       75.0    
#  4 6A                        0.151
#  5 6B                        0.00490
#  6 9Z                        0.00136
#  7 CHARLOTTE               473.
#  8 GRGIA STRT                4.24
#  9 S. EASTERN                0.216
# 10 VNCVR-BC               1031.
# 11 YAKUTAT                   0.00816

# only three areas have catches > 5 mt and 4A is Puget Sound

# fewer than 3 vessels by year / area
catch.pacfin %>% 
  dplyr::filter(PACFIN_CATCH_AREA_NAME %in% c("CHARLOTTE", "VNCVR-BC")) %>%
  dplyr::group_by(PACFIN_CATCH_AREA_NAME, LANDING_YEAR) %>% 
  dplyr::summarize(count=dplyr::n_distinct(VESSEL_ID)) %>% 
  dplyr::filter(count<3)

# # A tibble: 9 x 3
# # Groups:   PACFIN_CATCH_AREA_NAME [2]
#   PACFIN_CATCH_AREA_NAME LANDING_YEAR count
#   <chr>                         <int> <int>
# 1 CHARLOTTE                      1984     1
# 2 CHARLOTTE                      2000     2
# 3 CHARLOTTE                      2001     1
# 4 CHARLOTTE                      2002     1
# 5 CHARLOTTE                      2003     1
# 6 CHARLOTTE                      2004     2
# 7 VNCVR-BC                       1981     2
# 8 VNCVR-BC                       1983     1
# 9 VNCVR-BC                       2002     2

# fewer than 3 vessels by year (grouped across areas)
catch.pacfin %>% 
  dplyr::filter(PACFIN_CATCH_AREA_NAME %in% c("CHARLOTTE", "VNCVR-BC")) %>%
  dplyr::group_by(LANDING_YEAR) %>% 
  dplyr::summarize(count=dplyr::n_distinct(VESSEL_ID)) %>% 
  dplyr::filter(count<3)

dat <- catch.pacfin %>% 
  dplyr::filter(PACFIN_CATCH_AREA_NAME %in% c("CHARLOTTE", "VNCVR-BC")) %>%
  dplyr::group_by(LANDING_YEAR, PACFIN_CATCH_AREA_NAME) %>% 
  dplyr::summarize(sum = sum(LANDED_WEIGHT_MTONS)) 
# dat %>%
#   ggplot(aes(PACFIN_CATCH_AREA_NAME, LANDING_YEAR)) + geom_col()

catch.pacfin %>% 
  dplyr::filter(PACFIN_CATCH_AREA_NAME %in% c("CHARLOTTE", "VNCVR-BC")) %>%
  ggplot(aes(factor(LANDING_YEAR))) + 
  geom_bar(aes(weight = LANDED_WEIGHT_MTONS, fill = PACFIN_CATCH_AREA_NAME), position = "dodge")


# Use modified version of {nwfscSurvey} package to get data from the 
# NMFS Triennial survey from stations in Canada (WCVI)

# # install the "Triennial.Canada" branch of the package
# install.packages("pak")
# pak::pkg_install("pfmc-assessments/nwfscSurvey@Triennial.Canada")

# # create a local directory to save the files
# mydir <- "data-raw/Triennial_Canada"
# dir.create(mydir)

# extract catch
catch <- PullCatch.fn(Name = "petrale sole", 
                      SurveyName = "Triennial.Canada",
                      SaveFile = TRUE, Dir = mydir)
table(catch$Year)
# 1980 1983 1986 1989 1992 1995 1998 2001
#   48   42    1   65   61   71   60   40

# extract biological samples
bio <- PullBio.fn(Name = "petrale sole", 
                  SurveyName = "Triennial.Canada",
                  SaveFile = TRUE, Dir = mydir)
table(bio$Year)
# 1989 1992 1998 
#   60   35    2 