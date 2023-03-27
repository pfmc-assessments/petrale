################################################################################################
#
# 	PacFIN Data Expansion for PETRALE sole 2023
#
# 		Written by Chantel Wetzel, Vlada Gertseva, & Ian Taylor
#
################################################################################################

library(ggplot2)
library(tidyverse)
# install.packages("pak") # newer/better tool for installing R packages
# pak::pkg_install('pfmc-assessments/PacFIN.Utilities')
# pak::pkg_install('pfmc-assessments/nwfscSurvey')

# 15 March 2023: install branch of PacFIN.Utilities that contains
#                restored version of combineCalCOM() function
# pak::pkg_install('pfmc-assessments/PacFIN.Utilities@CALCOM')

library(PacFIN.Utilities)
library(nwfscSurvey)

# working directory needs to match local location of
# git repository at https://github.com/pfmc-assessments/petrale

# Load in the PacFIN bds data
# requested via https://github.com/pfmc-assessments/PacFIN.Utilities/issues/87
bds_file <- "PacFIN.PTRL.bds.27.Jan.2023.RData"
dir <- "data-raw"
load(file.path(dir, bds_file))

# CALCOM data for petrale sole - from Brenda Erwin 2011
CALCOM <- read.csv("data-raw/calcom/PetraleCALCOM_Query2011.csv")

# load estimated weight-length parameters (fa, fb, ma, mb, ua, ub)
load("data/weight-length_pars.rda")

# Process the CalCOM data
# Check weight at length for CalCOM data for comparison with total wgt
CALCOM$tot.wgt <- CALCOM$rel.error <- CALCOM$wgt.est <- CALCOM$num <- NA
CALCOM[CALCOM$SEX == 2, "wgt.est"] <- (fa * (CALCOM[CALCOM$SEX == 2, "TLENGTH"] / 10)^fb) * 2.20462
CALCOM[CALCOM$SEX == 1, "wgt.est"] <- (ma * (CALCOM[CALCOM$SEX == 1, "TLENGTH"] / 10)^mb) * 2.20462

samp.no <- unique(CALCOM$SAMPLE_NO)
for (a in 1:length(samp.no)) {
  find <- which(CALCOM[, "SAMPLE_NO"] == samp.no[a])
  CALCOM$tot.wgt[find] <- sum(CALCOM$wgt.est[find])
  CALCOM$rel.error[find] <- (CALCOM$tot.wgt[find] - CALCOM$SumOfWEIGHT[find]) / CALCOM$tot.wgt[find]
  CALCOM$num[find] <- length(find)
}

# plot(CALCOM[, "SumOfWEIGHT"], CALCOM[, "tot.wgt"])
# lines(1:200, 1:200)

# There are a number (614) of records where the estimated weight of the sampled fish in the two differ
# significantly from the SumOfWEIGHT in the CalCOM records.
find <- which(CALCOM$rel.error > 0.5 | CALCOM$rel.error < -0.5)
# write.csv(CALCOM[find,], "CalCOM_bad_weights.csv")
# write.csv(CALCOM, "CalCOM_all_data_check.csv")

# These records have > 50% difference between SumOfWEIGHT and estimated weights
badCArecords <- as.character(unique(CALCOM$SAMPLE_NO[find]))
# replace the SumOfWEIGHT column with the sum of the estimated weights
replace <- which(CALCOM$SAMPLE_NO %in% badCArecords)
CALCOM$SumOfWEIGHT[replace] <- round(CALCOM$tot.wgt[replace], 2)

# also fill in estimated weights when SumOfWEIGHT == NA
replace <- which(is.na(CALCOM$SumOfWEIGHT))
CALCOM$SumOfWEIGHT[replace] <- round(CALCOM$tot.wgt[replace], 2)

# add length units (which must be MM based on the range)
range(CALCOM$TLENGTH, na.rm = TRUE)
# [1] 198 879
CALCOM$FISH_LENGTH_UNITS <- "MM"

# check to make sure there is not overlap:
table(bds.pacfin$SAMPLE_YEAR[bds.pacfin$AGENCY_CODE == "C"])
# 1979 1990 1991 1992 1994 2001 2002 2003 2004 2005 2006 2007 2008 2009 2010 2011 2012 2013 2014 2015 2016 2017 2018 2019 2020 2021 2022
#    1  202  418   88    1  594  461  729  693 1183 2100 4627 4466 2346 2011 1571 2224 3683 3016 3144 3617 3778 3486 4496 4274 2865 1444
range(CALCOM$SAMPLE_DATE)
# [1] "1/10/1949" "9/9/1988"
# NOTE: the 1 PacFIN sample from 1979 was on 9/27/1979 and that dat isn't found in the CALCOM data
#
# NOTE: all CalCOM data are assigned to AGE_METHOD1 = "S",
#       but the 2019 assessment used
#       "CAP Surface Pre-1990 - 8" for 1966-1984
#       "CAP BB/Surface - 4" for 1985-1989
# ## checking ageing error in 2019 model
# mod.2019.001.001 <- SS_output
# mod.2019.001.001$agedbase %>% dplyr::filter(Fleet %in% 3:4 & Yr < 2005) %>% dplyr::select(Ageerr, Yr) %>% table()
#       Yr
# Ageerr 1966 1967 1968 1969 1970 1971 1972 1973 1974 1975 1976 1977 1978 1979 1980 1981 1982 1983 1984 1985 1986 1987 1988 1989 1990 1991 1992 2003 2004
#      2    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0   33   66   66
#      4    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0   66   66   66   66   33   66   33    0    0    0
#      8   66   66   33   66   66   66   66   66   66   66   66   66   66   66   66   66   66   66   66    0    0    0    0    0    0    0    0    0    0

# The cleanPacFIN function retains records that are
# randomly collected based on sampling protocols, removes
# any age reads that don't align with the keep_age_methods
# (e.g., sets the ages to NA), retains only records in the
# U.S., and other various data factors

# run cleanPacFIN() once before combining PacFIN and CALCOM
Pdata <- cleanPacFIN(
  Pdata = bds.pacfin,
  keep_age_method = c("B", "S"),
  CLEAN = TRUE,
  verbose = TRUE
)

# Use PacFIN.Utilities to combine the PacFIN and CALCOM data
CombinedDat <- combineCalCOM(Pdata = Pdata, CalCOM = CALCOM)

# Combining CalCOM and PacFIN data
# PacFIN records: 187611
# CalCOM records: 53794
# Combined dataset: 241405

### codes mentioned in 2019 script, not clear why
# CHR = Charlotte, Canada
# VCN = Vancouver, Canada

# command below causes error: :Error in plot.window(...) : need finite 'ylim' values"
# plotRawData(Pdata)

# run cleanPacFIN() again now that CALCOM has been added
Pdata <- cleanPacFIN(
  Pdata = CombinedDat,
  keep_age_method = c("B", "S"),
  CLEAN = FALSE,
  verbose = TRUE
)

# confirm that CalCOM ages got retained after cleaning
table(Pdata$Age, Pdata$SOURCE_AGID) %>% head()
#      C CalCOM    O    W
# 1    0      3    1    1
# 2    0     58    3   38
# 3   26    759  114  577
# 4  287   2912 1056 3023
# 5  564   4186 2865 7475
# 6  454   3495 3383 9041

# plotRawData(Pdata) # this created an error

##########################################################################################
# Let do some data checking
##########################################################################################

# Check lengths by sex to find outliers

# These are just starting values - estimates will be based on sex
k <- 0.2
Linf <- 50
L0 <- 18
CV1 <- 0.1
CV2 <- 0.1
# rename columns for what's expected by est_growth()
Pdata$Length_cm <- Pdata$lengthcm
Pdata$Sex <- Pdata$SEX

# estimate growth (adds columns "Lhat_low","Lhat_pred", "Lhat_high" to Pdat)
Pdata <- nwfscSurvey::est_growth(
  dat = Pdata,
  Par = data.frame(K = k, Linf = Linf, L0 = L0, CV0 = CV1, CV1 = CV2),
  sdFactor = 4, # four standard deviations
  dir = file.path(dir, "pacfin")
) # where to write Rdata file with estimates

# flag as outliers
Pdata$outlier <- Pdata[, "lengthcm"] > Pdata[, "Lhat_high"] |
  Pdata[, "lengthcm"] < Pdata[, "Lhat_low"]
# samples with no age have NA values for outlier, set them to FALSE
Pdata$outlier[is.na(Pdata$outlier)] <- FALSE
# table of outliers by age (columns) and length (rows)
table(Pdata[Pdata$outlier, "lengthcm"], Pdata[Pdata$outlier, "Age"])

# Save the filtered data
save(Pdata, file = file.path(dir, "Cleaned_PacFIN.PTRL.bds.16.Mar.2023.Rda"))
# # Load the filtered data
# load("data-raw/Cleaned_PacFIN.PTRL.bds.16.Mar.2023.Rda")

# plot of age vs length with outliers shown as larger points
ggplot(Pdata, aes(x = Age, y = lengthcm)) +
  scale_colour_viridis_d() +
  geom_jitter(aes(col = SEX, size = factor(outlier), alpha = 0.2)) +
  guides(alpha = FALSE)
# save plot
ggsave("figures/data/pacfin_outliers.png",
  width = 6.5, height = 5, units = "in", scale = 1.5
)

# summary of how many outliers were identified
table(Pdata$outlier[!is.na(Pdata$Age)])
# FALSE  TRUE
# 75933    43

# fraction of fish that will be removed as outliers
mean(Pdata$outlier[!is.na(Pdata$Age)])
# [1] 0.0005659682
# Set these outlier records to NA for length and age
Pdata[Pdata$outlier, c("lengthcm", "Age")] <- NA

# confirm reasonable range for values
quantile(Pdata$lengthcm, na.rm = TRUE)
# 0%  25%  50%  75% 100%
# 10   33   37   42   87
quantile(Pdata$Age, na.rm = TRUE)
# 0%  25%  50%  75% 100%
#  1    5    6    8   27


# Deal with Petrale's complex age error assumptions
table(Pdata$AGE_METHOD1, useNA = "always")
#      1      2      9      B     BB      N      S   <NA>
#  11300   5945   1245   7160   2127     43  31258 128533
# codes explained at
# https://pacfin.psmfc.org/pacfin_pub/table_cols.php
Pdata$agemethod <- Pdata$AGE_METHOD1
Pdata$agemethod[Pdata$agemethod %in% c("1", "B", "BB")] <- "B"
Pdata$agemethod[Pdata$agemethod %in% c("2", "S")] <- "S"
# Group failed ages with no ages
# 9 = "unable"
# N = "not aged"
Pdata$agemethod[Pdata$agemethod %in% c("9")] <- NA
Pdata$agemethod[Pdata$agemethod %in% c("N")] <- NA

# There are years with blank agemethod - do this to be easily trackable
Pdata$agemethod[!Pdata$agemethod %in% c("B", "S")] <- "U"
table(Pdata$agemethod, is.na(Pdata$Age), useNA = "always")
#       FALSE   TRUE   <NA>
# B     20579      8      0
# S     55155  35842      0
# U       199 129622      0
# <NA>      0      0      0

# code below maps various ageing codes to a shorter list, copied from
# https://github.com/pfmc-assessments/PacFIN.Utilities/blob/main/R/getAge.R#L113-L125
# which uses code from
# https://pacfin.psmfc.org/pacfin_pub/table_cols.php
# For Petrale, these are all "B", "S", or NA
Pdata <- Pdata %>%
  dplyr::mutate(dplyr::across(
    # dplyr::matches("AGE_CODE|AGE_M"),
    # todo: uncomment above when FINAL_FISH_AGE_CODE has correct codes rather than
    # duplicates of the final age
    dplyr::matches("AGE_M"),
    ~ dplyr::case_when(
      .x %in% c("B", "BB", 1) ~ "B",
      .x %in% c("S", "SR", 2) ~ "S",
      .x %in% c("T", "TS", "X", 4) ~ "T",
      .x %in% c("O", 5) ~ "O",
      .x %in% c("L", 6) ~ "L",
      .x %in% c("N", 9) ~ NA, # WDFW "N-not aged" ODFW: "9-unable"
      TRUE ~ as.character(.x)
    )
  ))

# Assign an ageing method of "B" if any of the 3 methods listed is "B"
# Assign an ageing method of "S" if any of the 3 methods listed is "S" and there is no "B" among them
# Assign to NA if there are none at all
Pdata <- Pdata %>%
  dplyr::mutate(
    agemethod =
      dplyr::case_when(
        AGE_METHOD1 %in% "B" | AGE_METHOD2 %in% "B" | AGE_METHOD3 %in% "B" ~ "B", # if any B
        AGE_METHOD1 %in% "S" | AGE_METHOD2 %in% "S" | AGE_METHOD3 %in% "S" ~ "S", # if any S but no B
        TRUE ~ NA
      ) # neither B nor S in any column
  )

# Define ageing error vector based on Petrale reading methods
Pdata$ageerr <- NA
Pdata$ageerr[!is.na(Pdata$Age)] <- -99
# 2 = CAP BB; 3 = CAP surface; 4 = CAP Combo; 5 = WDFW Combo; 6 = WDFW Surface; 7 = WDFW BB; 8 = CAP pre-1990 surface

# CAP Lab reads the California ages
# California
Pdata$ageerr[!is.na(Pdata$Age) & Pdata$SOURCE_AGID %in% c("C", "CalCOM") & Pdata$agemethod == "S" & Pdata$fishyr < 1990] <- 8 # CAP pre-1990 surface

# There are middle years where there are U BB and S reads
Pdata$ageerr[!is.na(Pdata$Age) & Pdata$SOURCE_AGID %in% c("C", "CalCOM") & Pdata$fishyr >= 1985 & Pdata$fishyr <= 1991] <- 4 # CAP Combo
Pdata$ageerr[!is.na(Pdata$Age) & Pdata$SOURCE_AGID %in% c("C", "CalCOM") & Pdata$agemethod == "B" & Pdata$fishyr >= 1992] <- 2 # CAP BB

# Oregon
Pdata$ageerr[!is.na(Pdata$Age) & Pdata$SOURCE_AGID == "O" & Pdata$agemethod == "S" & Pdata$fishyr <= 1980] <- 8 # CAP pre-1990 surface
Pdata$ageerr[!is.na(Pdata$Age) & Pdata$SOURCE_AGID == "O" & Pdata$fishyr >= 1980 & Pdata$fishyr < 1999] <- 4 # CAP Combo
Pdata$ageerr[!is.na(Pdata$Age) & Pdata$SOURCE_AGID == "O" & Pdata$agemethod == "B" & Pdata$fishyr >= 1999] <- 2 # CAP BB
Pdata$ageerr[!is.na(Pdata$Age) & Pdata$SOURCE_AGID == "O" & Pdata$agemethod == "S" & Pdata$fishyr >= 1999] <- 3 # CAP Surface
Pdata$ageerr[!is.na(Pdata$Age) & Pdata$SOURCE_AGID == "O" & Pdata$fishyr >= 2007] <- 2 # CAP BB

# Washington
Pdata$ageerr[!is.na(Pdata$Age) & Pdata$SOURCE_AGID == "W" & Pdata$agemethod %in% c("S")] <- 6 # WDFW Surface
Pdata$ageerr[!is.na(Pdata$Age) & Pdata$SOURCE_AGID == "W" & Pdata$agemethod %in% c("U") & Pdata$fishyr < 2008] <- 6 # WDFW Surface
Pdata$ageerr[!is.na(Pdata$Age) & Pdata$SOURCE_AGID == "W" & Pdata$agemethod == "B"] <- 7 # WDFW BB
Pdata$ageerr[!is.na(Pdata$Age) & Pdata$SOURCE_AGID == "W" & Pdata$fishyr %in% c(2009, 2010)] <- 5 # WDFW Combo

table(Pdata$ageerr, Pdata$agemethod, useNA = "always")
#           B      S   <NA>
# 2     11217      0      0
# 3         0   1400      0
# 4      2401   4795      0
# 5       932      4      0
# 6         0  30652      0
# 7      6811      0      0
# 8         0  17721      0
# <NA>      8  35842 129622

table(Pdata$ageerr)
#     2     3     4     5     6     7     8
# 11217  1400  7196   936 30652  6811 17721

# NOTE from 2019:
# There is an Oregon tow where 29 female and 1 unsexed fish were sampled
# Only the female fish were subsampled for ages
# This causes the getExpansion_1 code to error out due the UNK_NUM being 1
# Overwriting the UNK_NUM for now but this is wrong
# find = which(Pdata$SAMPLE_NO == "OR110384")
# Pdata$UNK_NUM[find] = Pdata$UNK_WT = NA

# Create separate data frame with seasons and adjusted years
# In getSeason(), the season_type = 1 is for Petrale
# it assigns months 1,2,11,12 to season 1 and the rest to seas 2
# yearUp=c(11,12) assigns months 11 and 12 to the following year
# the "fishyr" column contains the modified values
# "SAMPLE_YEAR" and "year" columns are not changed
Pdata_seas <- getSeason(Pdata, season_type = 1, yearUp = c(11, 12))

# look at distribution of gears
table(Pdata$geargroup)
# CalCOM    HKL    NET    POT    TWL    TWS
#  53794    602     56     37 186838     78
# Set up the expected fleet structure for the annual version

# Fleets for seasonal model
Pdata_seas$fleet[Pdata_seas$state != "CA"] <- "WA_OR"
Pdata_seas$fleet[Pdata_seas$state == "CA"] <- "CA"

# Fleets for annual model
Pdata$geargroup <- "ALL"
Pdata$fleet[Pdata$state != "CA"] <- "WA_OR_ALL" # where "ALL" is the geargroup
Pdata$fleet[Pdata$state == "CA"] <- "CA_ALL"

# Load in the catches by state for expansion
# annual catch
catch_annual <- read.csv(file.path(dir, "catch", "commercial_catch_by_state.csv"))
colnames(catch_annual) <- c("Year", "CA_ALL", "OR_ALL", "WA_ALL")

# seasonal catch
catch_seas <- read.csv(file.path(dir, "catch", "catch_file_season fleets v2.csv"))
# rename catch file headers
catch_seas <- catch_seas %>%
  dplyr::rename_with(~ gsub("Winter", 1, .x)) %>%
  dplyr::rename_with(~ gsub("Summer", 2, .x)) %>%
  dplyr::rename_with(~ gsub("South", "CA", .x)) %>%
  dplyr::rename_with(~ gsub("North", "WA_OR", .x))

tail(catch_seas)
#     Year     CA_2     CA_1      OR_2      OR_1      WA_2      WA_1
# 142 2017 605.5175 201.0418 1063.2037 1033.1083 212.37512 109.19556
# 143 2018 411.3346 239.6965  980.7012  814.6673 299.37760 142.65389
# 144 2019 442.1013 140.9231  954.1388  850.9189 256.01552 185.83679
# 145 2020 346.0112 101.0924  811.1161  676.2677  38.29677  17.83117
# 146 2021 460.4991 292.8038  963.9382  685.0887 184.15487  44.94156
# 147 2022 514.0657 382.5875 1147.8599 1033.0380  16.29313   7.64666

# create new column in PacFIN data that matches the seasonal catch file headers
Pdata_seas$stratification <- paste(Pdata_seas$state, Pdata_seas$season, sep = "_")
table(Pdata_seas$stratification)
#  CA_1  CA_2  OR_1  OR_2  WA_1  WA_2
# 37262 74050 22961 29521 16432 61179

dir.create(file.path(dir, "pacfin", "plots_seas"), recursive = TRUE)
dir.create(file.path(dir, "pacfin", "plots_annual"), recursive = TRUE)

# First stage expansion: expand comps to the trip level
# seasonal
Pdata_exp1_seas <- getExpansion_1(
  Pdata = Pdata_seas,
  plot = file.path(dir, "pacfin", "plots_seas"),
  fa = fa, fb = fb, ma = ma, mb = mb, ua = ua, ub = ub
)
# annual
Pdata_exp1_annual <- getExpansion_1(
  Pdata = Pdata,
  plot = file.path(dir, "pacfin", "plots_annual"),
  fa = fa, fb = fb, ma = ma, mb = mb, ua = ua, ub = ub
)

# Second stage expansion: expand comps up to the state and fleet
# The stratification.col input below needs to be the same
# as in the catch csv file
Pdata_exp2_seas <- getExpansion_2(
  Pdata = Pdata_exp1_seas,
  Catch = catch_seas,
  Units = "MT",
  stratification.cols = c("state", "geargroup"),
  # stratification.cols = "stratification",
  savedir = file.path(dir, "pacfin", "plots_seas")
)
# No Catch was found for these rows in Pdata, where
# N is the number of rows with missing Catch info:
#   fishyr stratification N
# 1   2023           CA_1 3

Pdata_exp2_annual <- getExpansion_2(
  Pdata = Pdata_exp1_annual,
  Catch = catch_annual,
  Units = "MT",
  stratification.cols = c("state", "geargroup"),
  savedir = file.path(dir, "pacfin", "plots_annual")
)

# No Catch was found for these rows in Pdata, where
# N is the number of rows with missing Catch info:
#    fishyr stratification  N
# 1    1956         WA_ALL  2
# 2    1958         WA_ALL  3
# 3    1961         WA_ALL  1
# 4    1964         WA_ALL  1
# ...

# Calculate the final expansion size
# seasonal
Pdata_exp2_seas$Final_Sample_Size <-
  capValues(Pdata_exp2_seas$Expansion_Factor_1_L *
    Pdata_exp2_seas$Expansion_Factor_2)
# annual
Pdata_exp2_annual$Final_Sample_Size <-
  capValues(Pdata_exp2_annual$Expansion_Factor_1_L *
    Pdata_exp2_annual$Expansion_Factor_2)


# get length comps
length_comps_seas <- getComps(
  Pdata = Pdata_exp2_seas[!is.na(Pdata_exp2_seas$lengthcm), ],
  Comps = "LEN"
)
length_comps_annual <- getComps(
  Pdata = Pdata_exp2_annual[!is.na(Pdata_exp2_annual$lengthcm), ],
  Comps = "LEN"
)
table(length_comps_annual$fleet, length_comps_annual$season)
#              1
# CA_ALL    1816
# WA_OR_ALL 1875
table(length_comps_seas$fleet, length_comps_seas$season)
#          1    2
# CA    1509 1666
# WA_OR 1434 1725

table(Pdata$SOURCE_AGID, Pdata$SEX)
#            F     M     U
# C      14832 34414  8272
# CalCOM 31032 22762     0
# O      27352 24952   178
# W      37724 38510  1377

# stacked bar plot showing length by sex
Pdata$count <- 1
Pdata %>% ggplot(aes(x = lengthcm, y = count, fill = SEX)) +
  geom_histogram(aes(y = count),
    position = "stack", stat = "identity",
    binwidth = 2
  ) +
  scale_fill_viridis_d()

# distributions as lines showing length by sex
ggplot(Pdata, aes(lengthcm, color = SEX)) +
  geom_freqpoly(binwidth = 1) +
  scale_fill_viridis_d()
# # save plot
# ggsave("figures/data/pacfin_comps_all.png",
#   width = 6.5, height = 5, units = "in", scale = 1.5
# )

####################################################################################################
# Create the length composition data
####################################################################################################

# Commenting out for now because I don't want to assign unsexed
# due to the dimorphic growth
# There area a fair number of U in CA and in the early years of WA
# length_compSR <- doSexRatio(
# 	CompData = length_comps,
# 	ratioU = 0.5,
# 	maxsizeU = 25,
# 	savedir = file.path(dir, "commercial_comps"))

len_bins <- seq(12, 62, 2)
# remove .Rdata extension from bds file name to include in CSV file names
# results in something like "PacFIN.PTRL.bds.27.Jan.2023"
out_name <- sub(pattern = "(.*)\\..*$", replacement = "\\1", bds_file)
# remove the first part "PacFIN.PTRL.bds."
out_date <- gsub(pattern = "PacFIN.PTRL.bds.", replacement = "", x = out_name, fixed = TRUE)
# TODO: modify the writeComps() commands below to clean up the columns to match the SS3 input
#       as has been done for the age comps further down

##########################################################
# Create the length compositions in the SS3 format
##########################################################

# seasonal length comps for SS3 as separate tables of sexed and unsexed
len_comps_seas <- writeComps(
  inComps = length_comps_seas,
  fname = file.path(dir, "pacfin", "forSS_seas", paste0("Lengths_", out_name, ".csv")),
  lbins = len_bins,
  sum1 = TRUE,
  month = c(1, 7), # new input added 14 Feb 2023: https://github.com/pfmc-assessments/PacFIN.Utilities/pull/90
  partition = 2,
  digits = 4,
  dummybins = FALSE
)

# annual length comps for SS3 as separate tables of sexed and unsexed
len_comps_annual <- writeComps(
  inComps = length_comps_annual,
  fname = file.path(dir, "pacfin", "forSS_annual", paste0("Lengths_", out_name, ".csv")),
  lbins = len_bins,
  sum1 = TRUE,
  partition = 2,
  digits = 4,
  dummybins = FALSE
)

# # combine the "FthenM" table with the "Uout" table as separate vectors
# # within the same table
# # TODO: filter the unsexed comps for years with very small sample sizes
# names(len_comps_seas$Uout) <- names(len_comps_seas$FthenM)
# len_comps_seas2 <- rbind(len_comps_seas$Uout, len_comps_seas$FthenM)
# names(len_comps_annual$Uout) <- names(len_comps_annual$FthenM)
# len_comps_annual2 <- rbind(len_comps_annual$Uout, len_comps_annual$FthenM)

# select only the sexed fish
len_comps_seas2 <- len_comps_seas$FthenM
len_comps_annual2 <- len_comps_annual$FthenM

# assign fleets for annual model:
# Fleets:
# 1 = WinterN
# 2 = SummerN
# 3 = WinterS
# 4 = SummerS
len_comps_seas2 <- len_comps_seas2 %>%
  dplyr::mutate(
    fleet =
      dplyr::case_when(
        month == 1 & fleet == "WA_OR" ~ 1,
        month == 7 & fleet == "WA_OR" ~ 2,
        month == 1 & fleet == "CA" ~ 3,
        month == 7 & fleet == "CA" ~ 4
      )
  )

len_comps_annual2 <- len_comps_annual2 %>%
  dplyr::mutate(
    fleet =
      dplyr::case_when(
        fleet == "CA_ALL" ~ 1,
        fleet == "WA_OR_ALL" ~ 2
      )
  )

# remove Ntows and Nsamps columns
# TODO: retain Nsamps instead of InputN
len_comps_seas2 <- len_comps_seas2 %>%
  dplyr::select(!c(Ntows, Nsamps))
len_comps_annual2 <- len_comps_annual2 %>%
  dplyr::select(!c(Ntows, Nsamps))

# sort by fleet, year, and then sex error type
len_comps_seas2 <- len_comps_seas2 %>%
  dplyr::arrange(fleet, year, sex)
len_comps_annual2 <- len_comps_annual2 %>%
  dplyr::arrange(fleet, year, sex)

# write to CSV file
today_date <- format(as.Date(Sys.time()), "%d.%b.%Y")
write.csv(len_comps_seas2,
  file = file.path(
    dir, "pacfin", "forSS_seas",
    paste0("Len_for_SS3_", today_date, "_data_from_", out_date, ".csv")
  ),
  row.names = FALSE
)
write.csv(len_comps_annual2,
  file = file.path(
    dir, "pacfin", "forSS_annual",
    paste0("Len_for_SS3_", today_date, "_data_from_", out_date, ".csv")
  ),
  row.names = FALSE
)

##########################################################
# Calculate the expansion for age data
##########################################################

Pdata_exp2_seas$Final_Sample_Size <- capValues(Pdata_exp2_seas$Expansion_Factor_1_A * Pdata_exp2_seas$Expansion_Factor_2)
Pdata_exp2_annual$Final_Sample_Size <- capValues(Pdata_exp2_annual$Expansion_Factor_1_A * Pdata_exp2_annual$Expansion_Factor_2)

age_comps_seas <- getComps(
  Pdata_exp2_seas[!is.na(Pdata_exp2_seas$Age), ],
  defaults = c("fleet", "fishyr", "season", "ageerr"),
  Comps = "Age"
)
age_comps_annual <- getComps(
  Pdata_exp2_annual[!is.na(Pdata_exp2_annual$Age), ],
  defaults = c("fleet", "fishyr", "season", "ageerr"),
  Comps = "Age"
)

##########################################################
# Create the age compositions in the SS3 format
##########################################################

age_bins <- 1:17

# process age comps for each ageing error type SEASONAL
age_comps_seas2 <- NULL
# loop across ageing error methods to get comps for that method
for (ageerr in unique(age_comps_seas$ageerr)) {
  age_comps_seas2 <- rbind(
    age_comps_seas2,
    age_comps_seas2 <- writeComps(
      inComps = age_comps_seas[age_comps_seas$ageerr == ageerr, ], # filter for subset that matches
      fname = file.path(dir, "pacfin", "forSS_seas", paste0("Age_", out_name, ".csv")),
      abins = age_bins,
      sum1 = TRUE,
      month = c(1, 7), # new input added 14 Feb 2023: https://github.com/pfmc-assessments/PacFIN.Utilities/pull/90
      partition = 2,
      ageErr = ageerr, # value being looped across
      digits = 4,
      dummybins = FALSE
    )$FthenM # only keep FthenM table, ignoring 49 petrale ages that are unsexed
  )
}

# process age comps for each ageing error type ANNUAL
age_comps_annual2 <- NULL
# loop across ageing error methods to get comps for that method
for (ageerr in unique(age_comps_annual$ageerr)) {
  age_comps_annual2 <- rbind(
    age_comps_annual2,
    age_comps_annual2 <- writeComps(
      inComps = age_comps_annual[age_comps_annual$ageerr == ageerr, ], # filter for subset that matches
      fname = file.path(dir, "pacfin", "forSS_annual", paste0("Age_", out_name, ".csv")),
      abins = age_bins,
      sum1 = TRUE,
      partition = 2,
      ageErr = ageerr, # value being looped across
      digits = 4,
      dummybins = FALSE
    )$FthenM # only keep FthenM table, ignoring 49 petrale ages that are unsexed
  )
}

# assign fleets for seasonal model:
# Fleets:
# 1 = WinterN
# 2 = SummerN
# 3 = WinterS
# 4 = SummerS
age_comps_seas2 <- age_comps_seas2 %>%
  dplyr::mutate(
    fleet =
      dplyr::case_when(
        month == 1 & fleet == "WA_OR" ~ 1,
        month == 7 & fleet == "WA_OR" ~ 2,
        month == 1 & fleet == "CA" ~ 3,
        month == 7 & fleet == "CA" ~ 4
      )
  )

age_comps_annual2 <- age_comps_annual2 %>%
  dplyr::mutate(
    fleet =
      dplyr::case_when(
        fleet == "CA_ALL" ~ 1,
        fleet == "WA_OR_ALL" ~ 2
      )
  )

# remove Ntows and Nsamps columns
age_comps_seas2 <- age_comps_seas2 %>%
  dplyr::select(!c(Ntows, Nsamps))
age_comps_annual2 <- age_comps_annual2 %>%
  dplyr::select(!c(Ntows, Nsamps))

# sort by fleet, year, and then ageing error type
age_comps_seas2 <- age_comps_seas2 %>%
  dplyr::arrange(fleet, year, ageErr)
age_comps_annual2 <- age_comps_annual2 %>%
  dplyr::arrange(fleet, year, ageErr)

# write to CSV file
today_date <- format(as.Date(Sys.time()), "%d.%b.%Y")
write.csv(age_comps_seas2,
  file = file.path(
    dir, "pacfin", "forSS_seas",
    paste0("Age_for_SS3_", today_date, "_data_from_", out_date, ".csv")
  ),
  row.names = FALSE
)
write.csv(age_comps_annual2,
  file = file.path(
    dir, "pacfin", "forSS_annual",
    paste0("Age_for_SS3_", today_date, "_data_from_", out_date, ".csv")
  ),
  row.names = FALSE
)

# read seasonal data file from Vlada's 15 March 2023 email
datfile <- r4ss::SS_readdat("models/2023_March16/petrale_data.ss")
# copy data file list to new name before modifying values
datfile2 <- datfile

# match names in order to bind data frames
# rbind(names(len_comps_seas2), names(datfile$lencomp)) # comparing names
names(len_comps_seas2) <- names(datfile$lencomp)
names(age_comps_seas2) <- names(datfile$agecomp)
# names(len_comps_annual2) <- names(datfile$lencomp)
# names(age_comps_annual2) <- names(datfile$agecomp)


# bind together with samples from other fleets (fleet != 1:4)
datfile2$lencomp <- rbind(len_comps_seas2, datfile$lencomp[!datfile$lencomp$FltSvy %in% 1:4, ])

# bind together with samples from other fleets (fleet != 1:4)
datfile2$agecomp <- rbind(age_comps_seas2, datfile$agecomp[!datfile$agecomp$FltSvy %in% 1:4, ])

# # save stuff created above
# save(Pdata, Pdata_seas, Pdata_exp2_seas, Pdata_exp2_annual, file = file.path(dir, "Cleaned_PacFIN.PTRL.bds.3.Mar.2023.Rda"))

r4ss::SS_writedat(datfile, "models/2023_March16/petrale_data_16March2023.ss",
  overwrite = TRUE
)
