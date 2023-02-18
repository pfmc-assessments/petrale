################################################################################################
#
# 	PacFIN Data Expansion for PETRALE sole 2023
#
# 		Written by Chantel Wetzel, Vlada Gertseva, & Ian Taylor
#
################################################################################################

library(ggplot2)
library(tidyverse)
# remotes::install_github('pfmc-assessments/PacFIN.Utilities')
# remotes::install_github('pfmc-assessments/nwfscSurvey')
library(PacFIN.Utilities)
library(nwfscSurvey)

# working directory needs to match local location of
# git repository at https://github.com/pfmc-assessments/petrale

# Load in the PacFIN bds data
# requested via https://github.com/pfmc-assessments/PacFIN.Utilities/issues/87
bds_file <- "PacFIN.PTRL.bds.27.Jan.2023.RData"
dir <- "data-raw"
load(file.path(dir, bds_file))

# The cleanPacFIN function retains records that are
# randomly collected based on sampling protocols, removes
# any age reads that don't align with the keep_age_methods
# (e.g., sets the ages to NA), retains only records in the
# U.S., and other various data factors
Pdata <- cleanPacFIN(
  Pdata = bds.pacfin,
  keep_age_method = c("B", "S"),
  CLEAN = TRUE,
  verbose = TRUE
)

# # CALCOM data for petrale sole - from Brenda Erwin 2011
# datfileCA <- "PetraleCALCOM_Query2011.csv" #
# CALCOM <- read.csv(datfileCA, header = TRUE,sep = ",")

# get weight-length relationship
# TODO: update this to use survey data instead of (or in addition to) PacFIN data
if (FALSE) {
  WLpars <- getWLpars(Pdata)
  # These estimates are without the most recent survey ages because they are not done yet
  fa <- WLpars %>%
    dplyr::filter(group == "female") %>%
    dplyr::select(A)
  fb <- WLpars %>%
    dplyr::filter(group == "female") %>%
    dplyr::select(B)
  ma <- WLpars %>%
    dplyr::filter(group == "male") %>%
    dplyr::select(A)
  mb <- WLpars %>%
    dplyr::filter(group == "male") %>%
    dplyr::select(B)
  ua <- WLpars %>%
    dplyr::filter(group == "all") %>%
    dplyr::select(A)
  ub <- WLpars %>%
    dplyr::filter(group == "all") %>%
    dplyr::select(B)
}

# fixed values (copied from process_pacfin_bds_NO_seasons.R)
fa <- 1.99e-06
fb <- 3.484
ma <- 2.98e-06
mb <- 3.363
ua <- (fa + ma) / 2
ub <- (fb + mb) / 2

# # Process the CalCOM data
# # Check weight at length for CalCOM data for comparison with total wgt
# CALCOM$tot.wgt = CALCOM$rel.error = CALCOM$wgt.est = CALCOM$num = NA
# CALCOM[CALCOM$SEX == 2, "wgt.est"] = (femalea*(CALCOM[CALCOM$SEX == 2, "TLENGTH"]/10)^femaleb) * 2.20462
# CALCOM[CALCOM$SEX == 1, "wgt.est"] = (malea  *(CALCOM[CALCOM$SEX == 1, "TLENGTH"]/10)^maleb) * 2.20462

# samp.no = unique(CALCOM$SAMPLE_NO)
# for (a in 1:length(samp.no)){
# 	find = which(CALCOM[,"SAMPLE_NO"] == samp.no[a])
# 	CALCOM$tot.wgt[find] = sum(CALCOM$wgt.est[find])
# 	CALCOM$rel.error[find] = (CALCOM$tot.wgt[find] - CALCOM$SumOfWEIGHT[find]) / CALCOM$tot.wgt[find]
# 	CALCOM$num[find] = length(find)
# }

# plot(CALCOM[, "SumOfWEIGHT"], CALCOM[, "tot.wgt"])
# lines(1:200, 1:200)

# There are a number (614) of records where the estimated weight of the sampled fish in the two differ
# significantly from the SumOfWEIGHT in the CalCOM records.
# find = which(CALCOM$rel.error > 0.5 | CALCOM$rel.error < -0.5)
# write.csv(CALCOM[find,], "CalCOM_bad_weights.csv")
# write.csv(CALCOM, "CalCOM_all_data_check.csv")

# #These records have > 50% difference between SumOfWEIGHT and estimated weights
# badCArecords = as.character(unique(CALCOM$SAMPLE_NO[find]))
# # replace the SumOfWEIGHT column with the sum of the estimated weights
# replace = which(CALCOM$SAMPLE_NO %in% badCArecords)
# CALCOM$SumOfWEIGHT[replace] = round(CALCOM$tot.wgt[replace], 2)

# # also fill in estimated weights when SumOfWEIGHT == NA
# replace = which(is.na(CALCOM$SumOfWEIGHT))
# CALCOM$SumOfWEIGHT[replace] = round(CALCOM$tot.wgt[replace], 2)


# # Use PacFIN.Utilities to combine the PacFIN and CALCOM data
# MasterDat = combineCalCOM( Pdata = TheData, CalCOM = CALCOM )

# CHR = Charlotte, Canada
# VCN = Vancouver, Canada

# plotRawData(Pdata)

# Pdata = cleanPacFIN(Pdata = MasterDat,
# 					keep_length_type = c("", "A", "F", "U", "T", NA),
# 					keep_missing_lengths = FALSE,
# 					keep_INPFC = c("VUS","CL","VN","COL","NC","SC","EU","CP","EK","MT","PS"))
# # Removal Report

# # Records in input:                  256005
# # Records not in USINPFC             0
# # Records not in INPFC_AREA:         44732
# # Records in bad INPFC_AREA:         0
# # Records in badRecords list:        0
# # Records with bad SAMPLE_TYPE       2520
# # Records with bad SAMPLE_METHOD     669
# # Records with no SAMPLE_NO          0
# # Records with no usable length      217
# # Records remaining:                 207867

#### FEMALES_NUM no longer a column in Pdata
# # Fix Oregon sample where there is no length, weight, or age for a female fish (OR141974) from 2014
# Pdata[Pdata$SAMPLE_NO == "OR141974", "FEMALES_NUM"] = 22

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
# 57962    27
# fraction of fish that will be removed as outliers
mean(Pdata$outlier[!is.na(Pdata$Age)])
# [1] 0.0004656216
# Set these outlier records to NA for length and age
Pdata[Pdata$outlier, c("lengthcm", "Age")] <- NA

# confirm reasonable range for values
quantile(Pdata$lengthcm, na.rm = TRUE)
#   0%  25%  50%  75% 100%
#   10   34   38   42   70
quantile(Pdata$Age, na.rm = TRUE)
#   0%  25%  50%  75% 100%
#    2    5    7    8   27

# Save the filtered data
save(Pdata, file = file.path(dir, "Cleaned_PacFIN.PTRL.bds.10.Feb.2023.Rda"))
# load("data-raw/Cleaned_PacFIN.PTRL.bds.10.Feb.2023.Rda")

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
table(Pdata$agemethod, useNA = "always")
#      B      S      U   <NA>
#  20587  37203 129821      0

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
# Early CA ages need to wait for CalCOM data
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
# 2     11218      0      0
# 3         0   1400      0
# 4      2401   4538      0
# 5       932      4      0
# 6         0  30658      0
# 7      6811      0      0
# <NA>      7     20 129622

table(Pdata$ageerr)
#     2     3     4     5     6     7
# 11218  1400  6939   936 30658  6811

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
#   HKL    NET    POT    TWL    TWS
#   602     56     37 186838     78
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
catch_annual <- read.csv(file.path(dir, "pacfin", "commercial_catch_by_state.csv"))
colnames(catch_annual) <- c("Year", "CA_ALL", "OR_ALL", "WA_ALL")

# seasonal catch
catch_seas <- read.csv(file.path(dir, "pacfin", "catch_file_season fleets.csv"))
# rename catch file headers to CA_2 CA_1 WA_OR_2 WA_OR_1
catch_seas <- catch_seas %>%
  dplyr::rename_with(~ gsub("Winter", 1, .x)) %>%
  dplyr::rename_with(~ gsub("Summer", 2, .x)) %>%
  dplyr::rename_with(~ gsub("South", "CA", .x)) %>%
  dplyr::rename_with(~ gsub("North", "WA_OR", .x))

# create new column in PacFIN data that matches the seasonal catch file headers
Pdata_seas$stratification <- paste(Pdata_seas$fleet, Pdata_seas$season, sep = "_")
table(Pdata_seas$stratification)
#    CA_1    CA_2 WA_OR_1 WA_OR_2
#   20629   36889   39393   90700

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
  # stratification.cols = "stratification",
  savedir = file.path(dir, "pacfin", "plots_seas")
)

Pdata_exp2_annual <- getExpansion_2(
  Pdata = Pdata_exp1_annual,
  Catch = catch_annual,
  Units = "MT",
  stratification.cols = c("state", "geargroup"),
  savedir = file.path(dir, "pacfin", "plots_annual")
)


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
#                1
#   CA_ALL     832
#   WA_OR_ALL 1875
table(length_comps_seas$fleet, length_comps_seas$season)
#            1    2
#   CA     699  737
#   WA_OR 1434 1725

table(Pdata$SOURCE_AGID, Pdata$SEX)
#         F     M     U
#   C 14832 34414  8272
#   O 27352 24952   178
#   W 37724 38510  1377

# stacked bar plot showing length by sex
Pdata$count <- 1
ggplot(Pdata, aes(x = lengthcm, y = count, fill = SEX)) +
  geom_histogram(aes(y = count), position = "stack", stat = "identity") +
  scale_fill_viridis_d()

# distributions as lines showing length by sex
ggplot(Pdata, aes(lengthcm, color = SEX)) +
  geom_freqpoly(binwidth = 1) +
  scale_fill_viridis_d()

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

# TODO: modify the writeComps() commands below to clean up the columns to match the SS3 input
#       as has been done for the age comps further down

# seasonal length comps for SS3
writeComps(
  inComps = length_comps_seas,
  fname = file.path(dir, "pacfin", "forSS_seas", paste0("Lengths_", out_name, ".csv")),
  lbins = len_bins,
  sum1 = TRUE,
  month = c(1, 7), # new input added 14 Feb 2023: https://github.com/pfmc-assessments/PacFIN.Utilities/pull/90
  partition = 2,
  digits = 4,
  dummybins = FALSE
)
# annual length comps for SS3
writeComps(
  inComps = length_comps_annual,
  fname = file.path(dir, "pacfin", "forSS_annual", paste0("Lengths_", out_name, ".csv")),
  lbins = len_bins,
  sum1 = TRUE,
  partition = 2,
  digits = 4,
  dummybins = FALSE
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
  dplyr::mutate(fleet = 
    dplyr::case_when(
      month == 1 & fleet == "WA_OR" ~ 1,
      month == 7 & fleet == "WA_OR" ~ 2,
      month == 1 & fleet == "CA" ~ 3,
      month == 7 & fleet == "CA" ~ 4))

age_comps_annual2 <- age_comps_annual2 %>%
  dplyr::mutate(fleet = 
    dplyr::case_when(
      fleet == "CA_ALL" ~ 1,
      fleet == "WA_OR_ALL" ~ 2))

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
write.csv(age_comps_seas2, 
  file = file.path(dir, "pacfin", "forSS_seas", paste0("Age_for_SS3_", out_name, ".csv")),
  row.names = FALSE
)
write.csv(age_comps_annual2, 
  file = file.path(dir, "pacfin", "forSS_annual", paste0("Age_for_SS3_", out_name, ".csv")),
  row.names = FALSE
)