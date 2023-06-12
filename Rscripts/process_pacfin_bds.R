################################################################################################
#
# 	PacFIN Data Expansion for PETRALE sole 2023
#
# 		Written by Chantel Wetzel, Vlada Gertseva, & Ian Taylor
#
################################################################################################

#### source this script (may take 4-5 minutes)
if (FALSE) {
  tictoc::tic()
  source("R/process_pacfin_bds.R")
  tictoc::toc()
  # 280.06 sec elapsed
}

library(ggplot2)
library(tidyverse)
# install.packages("pak") # newer/better tool for installing R packages
# pak::pkg_install('pfmc-assessments/PacFIN.Utilities')
# pak::pkg_install('pfmc-assessments/nwfscSurvey')

# update PacFIN.Utilities after change on 16 May 2023
# pak::pkg_install('pfmc-assessments/PacFIN.Utilities')
library(PacFIN.Utilities)
library(nwfscSurvey)

# NOTE: working directory needs to match local location of
# git repository at https://github.com/pfmc-assessments/petrale
dir <- "data-raw"

# Load in the PacFIN bds data
# requested via https://github.com/pfmc-assessments/PacFIN.Utilities/issues/87
bds_file_Jan <- "pacfin/PacFIN.PTRL.bds.27.Jan.2023.RData"
bds_file_Apr <- "pacfin/PacFIN.PTRL.bds.05.Apr.2023.RData"
bds_file <- "pacfin/PacFIN.PTRL.bds.08.May.2023.RData"

# explore differences between BDS extractions
# seems to
if (FALSE) {
  load(file.path(dir, bds_file_Jan))
  bds_Jan <- bds.pacfin
  load(file.path(dir, bds_file_Apr))
  bds_Apr <- bds.pacfin
  load(file.path(dir, bds_file))
  bds_May <- bds.pacfin


  table(bds_May$SAMPLE_YEAR[!bds_May$FISH_ID %in% bds_Jan$FISH_ID])
  # 1990 2022 2023
  #   50  141   43

  bds_Jan <- bds_Jan %>%
    dplyr::arrange(FISH_ID) %>%
    dplyr::select(-PACFIN_LOAD_DATE)

  bds_May_compare <- bds_May %>%
    dplyr::select(-FISH_WEIGHT_GUTTED) %>%
    dplyr::filter(FISH_ID %in% bds_Jan$FISH_ID) %>%
    dplyr::arrange(FISH_ID) %>%
    dplyr::select(-PACFIN_LOAD_DATE)

  bds_diff <- setdiff(bds_Jan, bds_May_compare)
  bds_diff2 <- setdiff(bds_May_compare, bds_Jan)
  # changes are only in recent years
  table(bds_diff$SAMPLE_YEAR)
  # 2006 2018 2019 2020 2021 2022
  #  308  316  471  782 1062 1330

  # lots of ages changed from NA to new estimate
  table(bds_diff$FINAL_FISH_AGE_IN_YEARS, useNA = "always")
  #  4    5    6    7    8    9   10   11   12   13   14   15   16   17   18   19   20   21 <NA>
  #  2    7   20   30   15   25   21   17   15   11   17   16   10    7    3    2    1    1 4049
  table(bds_diff2$FINAL_FISH_AGE_IN_YEARS, useNA = "always")
  #  3    4    5    6    7    8    9   10   11   12   13   14   15   16   17   18   19   20   21 <NA>
  #  5   79  257  394  382  427  389  346  283  252  261  198  108   69   34   24   17    5    3  736

  # no changes in lengths for the samples that are in both data sets
  table(bds_Jan$FISH_LENGTH - bds_May_compare$FISH_LENGTH)
  #      0
  # 261656
} # end exploration of differences among extractions

load(file.path(dir, bds_file))


# looking at Oregon "special project" samples
# some of these get filtered at a later step
table(bds.pacfin$SAMPLE_TYPE == "S", bds.pacfin$SAMPLE_YEAR)
samples <- bds.pacfin %>%
  dplyr::filter(AGENCY_CODE == "O" & SAMPLE_TYPE == "S")
table(is.na(samples$EXPANDED_SAMPLE_WEIGHT), samples$SAMPLE_YEAR)
table(is.na(samples$FISH_WEIGHT), samples$SAMPLE_YEAR)
samples <- bds.pacfin %>%
  dplyr::filter(SAMPLE_TYPE == "S")
table(samples$AGENCY_CODE, samples$SAMPLE_YEAR)
table(samples$AGENCY_CODE == "O" & samples$SAMPLE_YEAR <= 1986)


# CALCOM data for petrale sole - from Brenda Erwin 2011
CALCOM <- read.csv("data-raw/calcom/PetraleCALCOM_Query2011.csv")
# alternative file from EJ to compare sample sizes
# CALCOM2 <- read.csv("data-raw/calcom/petrale_flatfish_bin_ages_for_Ian.csv")

# load estimated weight-length parameters (fa, fb, ma, mb, ua, ub)
load("data/weight-length_pars.rda")

# Process the CalCOM data
# assign ageing method (later refined with additional details)
CALCOM$AGE_METHOD <- "S"

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
# CALCOM$FISH_LENGTH_UNITS <- "MM" # no longer needed

# check to make sure there is not overlap between PacFIN and CALCOM samples:
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


# remove temporary columns used for fixing outliers in CALCOM data
# (all columns from "num" to "tot.wgt")
CALCOM <- CALCOM %>% dplyr::select(!num:tot.wgt)

# CALCOM$Age  <- CALCOM$AGE # temporary fix for issue with combineCalCOM()

# Use PacFIN.Utilities to combine the PacFIN and CALCOM data
CombinedDat <- combineCalCOM(Pdata = bds.pacfin, CalCOM = CALCOM)

### codes mentioned in 2019 script, not clear why
# CHR = Charlotte, Canada
# VCN = Vancouver, Canada

# The cleanPacFIN function retains records that are
# randomly collected based on sampling protocols, removes
# any age reads that don't align with the keep_age_methods
# (e.g., sets the ages to NA), retains only records in the
# U.S., and other various data factors

# run cleanPacFIN() once before combining PacFIN and CALCOM
Pdata <- cleanPacFIN(
  Pdata = CombinedDat,
  keep_sample_type = c("M", "S"),
  keep_age_method = c("B", "S"),
  CLEAN = TRUE,
  verbose = TRUE
)

# Gear groupings reflect those in the table at
# https://pacfin.psmfc.org/pacfin_pub/data_rpts_pub/code_lists/gr.txt
# GRID was assigned to geargroup with the following names:

#    HKL    NET    POT    TWL    TWS
#    615     56     37 316114     78

# There are 0 records for which the state (i.e., 'CA', 'OR', 'WA')
# could not be assigned and were labeled as 'UNK'.

#     CA     OR     WA
# 111362  94482 111056
# The following unmatched values were found n times in `codify_sex()`:
# 'NA' (n = 26)
# i The following length types were kept in the data: 'U'The following length types were kept in the data: 'F'The following length types were kept in the data: 'T'
# i Lengths ranged from 101--879 (mm)
# i 0 fish had lengths of 0 (mm) and were changed to NAs
# i 111051 lengths (cm) and were converted to mm
#   `getAge()` summary information -
# v 0 rows were missing a final age
# i The distribution (in numbers) for fish aged 0--32 years is 0, 14, 115, 1782, 9703, 21728, 25785, 22201, 15118, 9386, 6112,
#   3810, 2703, 1732, 1159, 730, 418, 238, 181, 88, 67, 46, 21, 12, 17, 6, 5, 5, 1, 3, 1, 0, 2
# i Age methods 'B', 'S' and 'NA' were present
# i Age methods 'B' and 'S' were desired
# i 100 ages used undesired age methods
# v Number of ages by age (years) changed to `NA` is age-3 (n = 1), age-4 (n = 11), age-5 (n = 19), age-6 (n = 28), age-7 (n =
#   22), age-8 (n = 10), age-9 (n = 7), age-10 (n = 1), age-12 (n = 1)
#   `getAgeMethod()` summary information -
# i Age methods were originally coded to 'NA', 'S', '2', '9', '1', 'BB', 'N', 'B' or 'UNK'
# i Age methods are now coded to B (n = 26770), B--S (n = 528), S (n = 95891) and NA (n = 193711)
# i Number of samples (n) per combinations of ageing methods
#    AGE_METHOD1 AGE_METHOD2 AGE_METHOD3 Age method for best age      n
# 1            B           B           B                       B     28
# 2            B           B        <NA>                       B   1376
# 3            B           S        <NA>                       B    118
# 4            B           S        <NA>                    B--S    181
# 5            B        <NA>           B                       B     13
# 6            B        <NA>        <NA>                       B  24801
# 7            S           B        <NA>                       B    235
# 8            S           B        <NA>                    B--S    347
# 9            S           B        <NA>                       S      1
# 10           S           S        <NA>                       S    573
# 11           S        <NA>        <NA>                       S  95317
# 12           S        <NA>        <NA>                    <NA>  35807
# 13        <NA>           B           B                       B     40
# 14        <NA>           B        <NA>                       B    159
# 15        <NA>        <NA>        <NA>                    <NA> 157904

#  The table below summarizes the number of records that are outside
#  the area included in U.S. West Coast population assessments.
#  Columns with information about area of landing were pasted together
#  and searched for specific strings indicative of an excluded region.
#   strings            region     n
# 1      3D       CAN (VNCVR) 21396
# 2      4A Sound and Straits   336
# 3      5A               CAN  6590
# 4      5B               CAN  2018
# 5      5D               CAN   505


# N SAMPLE_TYPEs changed from M to S for special samples from OR: 0
# N not in keep_sample_type (SAMPLE_TYPE): 273
# N with SAMPLE_TYPE of NA: 156
# N not in keep_sample_method (SAMPLE_METHOD): 425
# N with SAMPLE_NO of NA: 0
# N without length: 1229
# N without Age: 193711
# N without length and Age: 194710
# N sample weights not available for OR: 19464
# N records: 316900
# N remaining if CLEAN: 266175
# N removed if CLEAN: 50725
# v Data are from a flatfish and CalCOM data are present

test <- Pdata$SAMPLE_TYPE != "S" | (Pdata$SAMPLE_TYPE <= 1986 & Pdata$SOURCE_AGID == "O")
# remove samples that are special projects after 1986 and from other agencies
Pdata2 <- Pdata %>%
  dplyr::filter(SAMPLE_TYPE != "S" | (SAMPLE_TYPE <= 1986 & SOURCE_AGID == "O"))
nrow(Pdata) - nrow(Pdata2)
# [1] 24536

# confirm that CalCOM ages got retained after cleaning
table(Pdata$Age, Pdata$SOURCE_AGID) %>% head()
#      C CalCOM    O    W
# 1    0      3   10    1
# 2    0     58   18   38
# 3   31    759  328  577
# 4  351   2912 2504 3023
# 5  743   4186 5993 7475
# 6  708   3495 7041 9041

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
today_date <- format(as.Date(Sys.time()), "%d.%b.%Y")
save(Pdata, file = file.path(dir, paste0("Cleaned_PacFIN.PTRL.bds.", today_date, ".Rda")))

# # Load the filtered data saved above to skip the cleaning steps (need to change the date)
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
# 95796    99

# fraction of fish that will be removed as outliers
mean(Pdata$outlier[!is.na(Pdata$Age)])
# [1] 0.001032379
# Set these outlier records to NA for length and age
Pdata[Pdata$outlier, c("lengthcm", "Age")] <- NA

# confirm reasonable range for values
quantile(Pdata$lengthcm, na.rm = TRUE)
# 0%  25%  50%  75% 100%
# 10   33   37   42   87
quantile(Pdata$Age, na.rm = TRUE)
# 0%  25%  50%  75% 100%
#  1    5    6    8   32


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
# B     26447     14      0
# S     69150  35892      0
# U       199 134473      0
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
# 2     14846      0      0
# 3         0   1608      0
# 4      4640  13095      0
# 5       932      4      0
# 6         0  30653      0
# 7      6811      0      0
# 8         0  23207      0
# <NA>     14  35892 134473

table(Pdata$ageerr)
#     2     3     4     5     6     7     8
# 14846  1608 17735   936 30653  6811 23207

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
#   Assigning seasons for Petrale; winter == 1, summer == 2.
#   Incremented fishyr for months 11, 12to the next year.

# look at distribution of gears
table(Pdata$geargroup)
#  HKL    NET    POT    TWL    TWS
#  606     56     37 265398     78

# Set up the expected fleet structure for the annual version
# Fleets for annual model
Pdata$geargroup <- "ALL"
Pdata$fleet[Pdata$state != "CA"] <- "WA_OR_ALL" # where "ALL" is the geargroup
Pdata$fleet[Pdata$state == "CA"] <- "CA_ALL"

# Fleets for seasonal model
Pdata_seas$fleet[Pdata_seas$state != "CA"] <- "WA_OR"
Pdata_seas$fleet[Pdata_seas$state == "CA"] <- "CA"

# Alternative coastwide model with a single fleet
Pdata_coast <- Pdata
Pdata_coast$fleet <- "ALL"

# Load in the catches by state for expansion
# annual catch
catch_annual <- read.csv(file.path(dir, "catch", "commercial_catch_by_state_v2.csv"))
colnames(catch_annual) <- c("Year", "CA_ALL", "OR_ALL", "WA_ALL")

# seasonal catch
catch_seas <- read.csv(file.path(dir, "catch", "catch_file_season fleets v3.csv"))

tail(catch_seas)
#     Year     CA.S     CA.W      OR.S      OR.W      WA.S      WA.W
# 142 2017 468.8005 155.6495 1063.2037 1033.1083 212.37512 109.19556
# 143 2018 411.3346 239.6965  980.7012  814.6673 299.37760 142.65389
# 144 2019 442.1013 140.9231  954.1388  850.9189 256.01552 185.83679
# 145 2020 346.0112 101.0924  811.1161  676.2677  38.29677  17.83117
# 146 2021 460.4991 292.8038  963.9382  685.0887 184.15487  44.94156
# 147 2022 514.0657 382.5875 1147.8599 1033.0380  16.29313   7.64666

# create new column in PacFIN data that matches the seasonal catch file headers
Pdata_seas$stratification <-
  paste(Pdata_seas$state, ifelse(Pdata_seas$season == 1, "W", "S"), sep = ".")
table(Pdata_seas$stratification)
#  CA.S  CA.W  OR.S  OR.W  WA.S  WA.W
# 74050 37312 46851 28167 63323 16472

dir.create(file.path(dir, "pacfin", "plots_annual"), recursive = TRUE)
dir.create(file.path(dir, "pacfin", "plots_seas"), recursive = TRUE)
dir.create(file.path(dir, "pacfin", "plots_coast"), recursive = TRUE)
dir.create(file.path(dir, "pacfin", "plots_annual_Age"), recursive = TRUE)
dir.create(file.path(dir, "pacfin", "plots_seas_Age"), recursive = TRUE)
dir.create(file.path(dir, "pacfin", "plots_coast_Age"), recursive = TRUE)

# First stage expansion: expand comps to the trip level
# seasonal
# annual
Pdata_exp1_annual <- getExpansion_1(
  Pdata = Pdata,
  plot = file.path(dir, "pacfin", "plots_annual"),
  fa = fa, fb = fb, ma = ma, mb = mb, ua = ua, ub = ub
)
# seasonal
Pdata_exp1_seas <- getExpansion_1(
  Pdata = Pdata_seas,
  plot = file.path(dir, "pacfin", "plots_seas"),
  fa = fa, fb = fb, ma = ma, mb = mb, ua = ua, ub = ub
)
# coastwide
Pdata_exp1_coast <- getExpansion_1(
  Pdata = Pdata_coast,
  plot = file.path(dir, "pacfin", "plots_coast"),
  fa = fa, fb = fb, ma = ma, mb = mb, ua = ua, ub = ub
)

# Second stage expansion: expand comps up to the state and fleet
# The stratification.col input below needs to be the same
# as in the catch csv file
Pdata_exp2_annual <- getExpansion_2(
  Pdata = Pdata_exp1_annual,
  Catch = catch_annual,
  Units = "MT",
  stratification.cols = c("state", "geargroup"),
  savedir = file.path(dir, "pacfin", "plots_annual")
)
# No Catch was found for these rows in Pdata, where
# n is the number of rows with missing Catch info:
# # A tibble: 1 x 4
# # Groups:   fishyr, stratification [1]
#   fishyr stratification Sum_Sampled_Lbs     n
#    <dbl> <chr>                    <int> <int>
# 1   2023 WA_ALL                    1708     2

test <- Pdata_exp1_annual %>% 
  dplyr::filter(SOURCE_AGID == "CalCOM")
test_exp2_annual <- getExpansion_2(
  Pdata = test,
  Catch = catch_annual,
  Units = "MT",
  stratification.cols = c("state", "geargroup")
)


Pdata_exp2_seas <- getExpansion_2(
  Pdata = Pdata_exp1_seas,
  Catch = catch_seas,
  Units = "MT",
  stratification.cols = c("state", "geargroup"),
  # stratification.cols = "stratification",
  savedir = file.path(dir, "pacfin", "plots_seas")
)


# No Catch was found for these rows in Pdata, where
# n is the number of rows with missing Catch info:
# # A tibble: 4 x 4
# # Groups:   fishyr, stratification [4]
#   fishyr stratification Sum_Sampled_Lbs     n
#    <dbl> <chr>                    <int> <int>
# 1   2023 CA.W                     33072     3
# 2   2023 OR.W                    249004    15
# 3   2023 WA.S                        12     1
# 4   2023 WA.W                     59138     3

Pdata_exp2_coast <- getExpansion_2(
  Pdata = Pdata_exp1_coast,
  Catch = catch_annual,
  Units = "MT",
  stratification.cols = c("state", "geargroup"),
  savedir = file.path(dir, "pacfin", "plots_coast")
)

# Calculate the final expansion size
# annual
Pdata_exp2_annual$Final_Sample_Size <-
  capValues(Pdata_exp2_annual$Expansion_Factor_1_L *
    Pdata_exp2_annual$Expansion_Factor_2)
# Maximum expansion capped at 0.95 quantile: 1610.615

# seasonal
Pdata_exp2_seas$Final_Sample_Size <-
  capValues(Pdata_exp2_seas$Expansion_Factor_1_L *
    Pdata_exp2_seas$Expansion_Factor_2)
# Maximum expansion capped at 0.95 quantile: 1650.809

# coastwide
Pdata_exp2_coast$Final_Sample_Size <-
  capValues(Pdata_exp2_coast$Expansion_Factor_1_L *
    Pdata_exp2_coast$Expansion_Factor_2)
# Maximum expansion capped at 0.95 quantile: 1610.615

# look at expansions
Pdata_exp2_annual %>%
  ggplot(aes(x = Expansion_Factor_1_L, fill = SOURCE_AGID, colour = SOURCE_AGID)) +
  geom_histogram(alpha = 0.5, position = "stack")
ggsave("figures/data/Expansion_Factor_1_L_26May.png",
  width = 6.5, height = 5, units = "in", scale = 1.5
)

Pdata_exp2_annual %>%
  ggplot(aes(x = Expansion_Factor_2, fill = SOURCE_AGID, colour = SOURCE_AGID)) +
  geom_histogram(alpha = 0.5, position = "stack")
ggsave("figures/data/Expansion_Factor_2_26May.png",
  width = 6.5, height = 5, units = "in", scale = 1.5
)

Pdata_exp2_annual %>%
  ggplot(aes(x = Final_Sample_Size, fill = SOURCE_AGID, colour = SOURCE_AGID)) +
  geom_histogram(alpha = 0.5, position = "stack")
ggsave("figures/data/Final_Sample_Size_L_26May.png",
  width = 6.5, height = 5, units = "in", scale = 1.5
)

# get unexpanded version
Pdata_unexpanded <- Pdata_exp2_annual
Pdata_unexpanded$Final_Sample_Size_A <- 1
Pdata_unexpanded$Final_Sample_Size_L <- 1
Pdata_unexpanded$Final_Sample_Size <- 1

# get length comps
length_comps_annual <- getComps(
  Pdata = Pdata_exp2_annual[!is.na(Pdata_exp2_annual$lengthcm), ],
  Comps = "LEN"
)
length_comps_unexpanded_annual <- getComps(
  Pdata = Pdata_unexpanded[!is.na(Pdata_unexpanded$lengthcm), ],
  Comps = "LEN"
)
length_comps_seas <- getComps(
  Pdata = Pdata_exp2_seas[!is.na(Pdata_exp2_seas$lengthcm), ],
  Comps = "LEN"
)
length_comps_coast <- getComps(
  Pdata = Pdata_exp2_coast[!is.na(Pdata_exp2_coast$lengthcm), ],
  Comps = "LEN"
)
table(length_comps_annual$fleet, length_comps_annual$season)
#              1
# CA_ALL    1816
# WA_OR_ALL 2073
table(length_comps_seas$fleet, length_comps_seas$season)
#          1    2
# CA    1509 1666
# WA_OR 1584 1903
table(length_comps_coast$fleet, length_comps_coast$season)
#        1
# ALL 2368

table(Pdata$SOURCE_AGID, Pdata$SEX)
#            F     M     U
# C      14880 34416  8272
# CalCOM 31032 22762     0
# O      39955 34808   255
# W      37991 40424  1380

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

### COMMENT FROM 2019
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
out_name <- sub(pattern = "pacfin/", replacement = "", bds_file, fixed = TRUE)
out_name <- sub(pattern = "(.*)\\..*$", replacement = "\\1", out_name)
# remove the first part "PacFIN.PTRL.bds."
out_date <- gsub(pattern = "PacFIN.PTRL.bds.", replacement = "", x = out_name, fixed = TRUE)
# TODO: modify the writeComps() commands below to clean up the columns to match the SS3 input
#       as has been done for the age comps further down

##########################################################
# Create the length compositions in the SS3 format
##########################################################

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

len_comps_unexpanded_annual <- writeComps(
  inComps = length_comps_unexpanded_annual,
  fname = file.path(dir, "pacfin", "forSS_annual_unexpanded", paste0("Lengths_", out_name, ".csv")),
  lbins = len_bins,
  sum1 = TRUE,
  partition = 2,
  digits = 4,
  dummybins = FALSE
)

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

# coastwide length comps for SS3 as separate tables of sexed and unsexed
len_comps_coast <- writeComps(
  inComps = length_comps_coast,
  fname = file.path(dir, "pacfin", "forSS_coast", paste0("Lengths_", out_name, ".csv")),
  lbins = len_bins,
  sum1 = TRUE,
  partition = 2,
  digits = 4,
  dummybins = FALSE
)

#### ignoring unsexed fish as they are such a small fraction
# # combine the "FthenM" table with the "Uout" table as separate vectors
# # within the same table
# names(len_comps_seas$Uout) <- names(len_comps_seas$FthenM)
# len_comps_seas2 <- rbind(len_comps_seas$Uout, len_comps_seas$FthenM)
# names(len_comps_annual$Uout) <- names(len_comps_annual$FthenM)
# len_comps_annual2 <- rbind(len_comps_annual$Uout, len_comps_annual$FthenM)

# select only the sexed fish
len_comps_annual2 <- len_comps_annual$FthenM
len_comps_unexpanded_annual2 <- len_comps_unexpanded_annual$FthenM
len_comps_seas2 <- len_comps_seas$FthenM
len_comps_coast2 <- len_comps_coast$FthenM


# assign fleets for annual model
len_comps_annual2 <- len_comps_annual2 %>%
  dplyr::mutate(
    fleet =
      dplyr::case_when(
        fleet == "WA_OR_ALL" ~ 1,
        fleet == "CA_ALL" ~ 2
      )
  )

len_comps_unexpanded_annual2 <- len_comps_unexpanded_annual2 %>%
  dplyr::mutate(
    fleet =
      dplyr::case_when(
        fleet == "WA_OR_ALL" ~ 1,
        fleet == "CA_ALL" ~ 2
      )
  )

# assign fleets for seasonal model:
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

# assign fleet for coastwide model
len_comps_coast2$fleet <- 1

# save Rdata file with stuff
today_date <- format(as.Date(Sys.time()), "%d.%b.%Y")
save(len_comps_seas2, len_comps_annual2, len_comps_coast2,
  file = paste0(
    "data-raw/pacfin/len_comps_all_", today_date,
    "_data_from_", out_date, ".Rdata"
  )
)

## exploring Nsamps vs InputN
# plot(len_comps_annual2$year, len_comps_annual2$Nsamps/len_comps_annual2$InputN, ylim = c(0, 100))
test <- len_comps_annual2
test$fish_per_tow <- test$Nsamps / test$Ntows
# comps with very large numbers of fish per tow
test[order(test$fish_per_tow, decreasing = TRUE), 1:8] %>% head()
#    year month fleet sex partition Ntows Nsamps InputN
# 59 1958     7     1   3         2     3   2140     21
# 56 1955     7     1   3         2     1    507      7
# 57 1956     7     1   3         2     2    689     14
# 72 1974     7     1   3         2    33   9485    233
# 74 1976     7     1   3         2     7   1971     49
# 64 1966     7     1   3         2     4   1125     28

# input_option <- "InputN"
# remove extra sample size columns
# remove_cols <- c("Ntows", "InputN")  # just keep "Nsamps"
remove_cols <- c("Ntows", "Nsamps") # just keep "InputN"

len_comps_annual2 <- len_comps_annual2 %>%
  dplyr::select(!remove_cols)
len_comps_unexpanded_annual2 <- len_comps_unexpanded_annual2 %>%
  dplyr::select(!remove_cols)
len_comps_seas2 <- len_comps_seas2 %>%
  dplyr::select(!remove_cols)
len_comps_coast2 <- len_comps_coast2 %>%
  dplyr::select(!remove_cols)

# sort by fleet, year, and then sex error type
len_comps_annual2 <- len_comps_annual2 %>%
  dplyr::arrange(fleet, year, sex)
len_comps_unexpanded_annual2 <- len_comps_unexpanded_annual2 %>%
  dplyr::arrange(fleet, year, sex)
len_comps_seas2 <- len_comps_seas2 %>%
  dplyr::arrange(fleet, year, sex)
len_comps_coast2 <- len_comps_coast2 %>%
  dplyr::arrange(fleet, year, sex)

# write SS3 format comps to CSV files
write.csv(len_comps_annual2,
  file = file.path(
    dir, "pacfin", "forSS_annual",
    paste0("Len_for_SS3_", today_date, "_data_from_", out_date, ".csv")
  ),
  row.names = FALSE
)
write.csv(len_comps_unexpanded_annual2,
  file = file.path(
    dir, "pacfin", "forSS_annual_unexpanded",
    paste0("Len_for_SS3_", today_date, "_data_from_", out_date, ".csv")
  ),
  row.names = FALSE
)
write.csv(len_comps_seas2,
  file = file.path(
    dir, "pacfin", "forSS_seas",
    paste0("Len_for_SS3_", today_date, "_data_from_", out_date, ".csv")
  ),
  row.names = FALSE
)
write.csv(len_comps_coast2,
  file = file.path(
    dir, "pacfin", "forSS_coast",
    paste0("Len_for_SS3_", today_date, "_data_from_", out_date, ".csv")
  ),
  row.names = FALSE
)

##########################################################
# Calculate the expansion for age data
##########################################################

# filter out samples with no age
# because there were Inf values in first stage expansion
# for the unaged fish, causing issues with the quantiles
# calculated in capValues()
Adata <- Pdata %>% dplyr::filter(!is.na(Age))
Adata_seas <- Pdata_seas %>% dplyr::filter(!is.na(Age))
Adata_coast <- Pdata_coast %>% dplyr::filter(!is.na(Age))

# First stage expansion: expand comps to the trip level
# annual
Adata_exp1_annual <-
  getExpansion_1(
    Pdata = Adata, plot = file.path(dir, "pacfin", "plots_annual_Age"),
    fa = fa, fb = fb, ma = ma, mb = mb, ua = ua, ub = ub
  )
# Maximum expansion capped at 0.95 quantile: 279.0518

# seasonal
Adata_exp1_seas <-
  getExpansion_1(
    Pdata = Adata_seas, plot = file.path(dir, "pacfin", "plots_seas_Age"),
    fa = fa, fb = fb, ma = ma, mb = mb, ua = ua, ub = ub
  )
# coastwide
Adata_exp1_coast <-
  getExpansion_1(
    Pdata = Adata_coast, plot = file.path(dir, "pacfin", "plots_coast_Age"),
    fa = fa, fb = fb, ma = ma, mb = mb, ua = ua, ub = ub
  )

# Second stage expansion: expand comps up to the state and fleet
# The stratification.col input below needs to be the same
# as in the catch csv file
Adata_exp2_annual <- getExpansion_2(
  Pdata = Adata_exp1_annual,
  Catch = catch_annual,
  Units = "MT",
  stratification.cols = c("state", "geargroup"),
  savedir = file.path(dir, "pacfin", "plots_annual_Age")
)

# Maximum expansion capped at 0.95 quantile: 97.75194
# Maximum expansion capped at 0.95 quantile: 3523.095
# Maximum expansion capped at 0.95 quantile: 3523.095

Adata_exp2_seas <- getExpansion_2(
  Pdata = Adata_exp1_seas,
  Catch = catch_seas,
  Units = "MT",
  stratification.cols = c("state", "geargroup"),
  savedir = file.path(dir, "pacfin", "plots_seas_Age")
)
Adata_exp2_coast <- getExpansion_2(
  Pdata = Adata_exp1_coast,
  Catch = catch_annual,
  Units = "MT",
  stratification.cols = c("state", "geargroup"),
  savedir = file.path(dir, "pacfin", "plots_coast_Age")
)

# replace final sample size with lower cap
# (0.80 quantile vs 0.95 default matches 2019 update assessment)
Adata_exp2_annual$Final_Sample_Size <- capValues(Adata_exp2_annual$Expansion_Factor_1_A * Adata_exp2_annual$Expansion_Factor_2, maxVal = 0.8)
# Maximum expansion capped at 0.8 quantile: 1104.523
Adata_exp2_seas$Final_Sample_Size <- capValues(Adata_exp2_seas$Expansion_Factor_1_A * Adata_exp2_seas$Expansion_Factor_2, maxVal = 0.8)
Adata_exp2_coast$Final_Sample_Size <- capValues(Adata_exp2_coast$Expansion_Factor_1_A * Adata_exp2_coast$Expansion_Factor_2, maxVal = 0.8)

# plots of expansion factors
Adata_exp2_annual %>%
  ggplot(aes(x = Expansion_Factor_1_A, fill = SOURCE_AGID, colour = SOURCE_AGID)) +
  geom_histogram(alpha = 0.5, position = "stack")
ggsave("figures/data/Expansion_Factor_1_A_26May.png",
  width = 6.5, height = 5, units = "in", scale = 1.5
)
Adata_exp2_annual %>%
  ggplot(aes(x = Expansion_Factor_2, fill = SOURCE_AGID, colour = SOURCE_AGID)) +
  geom_histogram(alpha = 0.5, position = "stack")
ggsave("figures/data/Expansion_Factor_2_A_26May.png",
  width = 6.5, height = 5, units = "in", scale = 1.5
)
Adata_exp2_annual %>%
  ggplot(aes(x = Final_Sample_Size, fill = SOURCE_AGID, colour = SOURCE_AGID)) +
  geom_histogram(alpha = 0.5, position = "stack")
ggsave("figures/data/Final_Sample_Size_A_26May.png",
  width = 6.5, height = 5, units = "in", scale = 1.5
)

age_comps_annual <- getComps(
  Adata_exp2_annual,
  defaults = c("fleet", "fishyr", "season", "ageerr"),
  Comps = "AGE"
)
age_comps_seas <- getComps(
  Adata_exp2_seas,
  defaults = c("fleet", "fishyr", "season", "ageerr"),
  Comps = "AGE"
)
age_comps_coast <- getComps(
  Adata_exp2_coast,
  defaults = c("fleet", "fishyr", "season", "ageerr"),
  Comps = "AGE"
)

# unexpanded age comps as sensitivity and also for
# conditional age-at-length comps
# annual model only (for now)
Adata_unexpanded <- Adata_exp2_annual
Adata_unexpanded$Final_Sample_Size_A <- 1
Adata_unexpanded$Final_Sample_Size_L <- 1
Adata_unexpanded$Final_Sample_Size <- 1

age_comps_unexpanded_annual <- getComps(
  Adata_unexpanded,
  defaults = c("fleet", "fishyr", "season", "ageerr"),
  Comps = "AGE"
)

CAAL_comps_annual <- getComps(
  Adata_unexpanded[!is.na(Adata_unexpanded$Age), ],
  defaults = c("fleet", "fishyr", "season", "ageerr"),
  Comps = "AAL"
)

##########################################################
# Create the age compositions in the SS3 format
##########################################################

age_bins <- 1:17

# process age comps for each ageing error type SEASONAL
age_comps_seas2 <- NULL

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

# process age comps for each ageing error type COASTWIDE
age_comps_coast2 <- NULL
# loop across ageing error methods to get comps for that method
for (ageerr in unique(age_comps_coast$ageerr)) {
  age_comps_coast2 <- rbind(
    age_comps_coast2,
    age_comps_coast2 <- writeComps(
      inComps = age_comps_coast[age_comps_coast$ageerr == ageerr, ], # filter for subset that matches
      fname = file.path(dir, "pacfin", "forSS_coast", paste0("Age_", out_name, ".csv")),
      abins = age_bins,
      sum1 = TRUE,
      partition = 2,
      ageErr = ageerr, # value being looped across
      digits = 4,
      dummybins = FALSE
    )$FthenM # only keep FthenM table, ignoring 49 petrale ages that are unsexed
  )
}

# process ANNUAL UNEXPANDED age comps
# process age comps for each ageing error type ANNUAL
age_comps_unexpanded_annual2 <- NULL
# loop across ageing error methods to get comps for that method
for (ageerr in unique(age_comps_annual$ageerr)) {
  age_comps_unexpanded_annual2 <- rbind(
    age_comps_unexpanded_annual2,
    age_comps_unexpanded_annual2 <- writeComps(
      inComps = age_comps_unexpanded_annual[age_comps_unexpanded_annual$ageerr == ageerr, ], # filter for subset that matches
      fname = file.path(dir, "pacfin", "forSS_annual_unexpanded", paste0("Age_", out_name, ".csv")),
      abins = age_bins,
      sum1 = FALSE,
      partition = 2,
      ageErr = ageerr, # value being looped across
      digits = 4,
      dummybins = FALSE
    )$FthenM # only keep FthenM table, ignoring 49 petrale ages that are unsexed
  )
}

# process CAAL comps for each ageing error type ANNUAL
CAAL_comps_annual2 <- NULL
# loop across ageing error methods to get comps for that method
for (ageerr in unique(CAAL_comps_annual$ageerr)) {
  temp <- writeComps(
    inComps = CAAL_comps_annual[CAAL_comps_annual$ageerr == ageerr, ], # filter for subset that matches
    fname = file.path(dir, "pacfin", "forSS_annual", paste0("CAAL_", out_name, ".csv")),
    abins = age_bins,
    lbins = len_bins,
    sum1 = FALSE,
    partition = 2,
    ageErr = ageerr, # value being looped across
    digits = 4,
    dummybins = FALSE
  )
  # gather together female comps then male comps
  CAAL_comps_annual2 <- rbind(
    CAAL_comps_annual2,
    temp$Fout,
    temp$Mout
  )
}

# assign fleets for annual model
age_comps_annual2 <- age_comps_annual2 %>%
  dplyr::mutate(
    fleet =
      dplyr::case_when(
        fleet == "WA_OR_ALL" ~ 1,
        fleet == "CA_ALL" ~ 2
      )
  )
age_comps_unexpanded_annual2 <-
  age_comps_unexpanded_annual2 %>%
  dplyr::mutate(
    fleet =
      dplyr::case_when(
        fleet == "WA_OR_ALL" ~ 1,
        fleet == "CA_ALL" ~ 2
      )
  )
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

# need to rename columns to avoid duplicate column names
names(CAAL_comps_annual2)[names(CAAL_comps_annual2) %in% paste0("A", age_bins)] <-
  c(paste0("F", age_bins), paste0("M", age_bins))
CAAL_comps_annual2 <- CAAL_comps_annual2 %>%
  dplyr::mutate(
    fleet =
      dplyr::case_when(
        fleet == "WA_OR_ALL" ~ 1,
        fleet == "CA_ALL" ~ 2
      )
  )
# fleet assignment for coastwide age comps
age_comps_coast2$fleet <- 1

# remove extra sample size columns (not present for CAAL data)
# the "remove_cols" vector is set earlier (prior to processing length comps)
age_comps_annual2 <- age_comps_annual2 %>%
  dplyr::select(!remove_cols)
age_comps_unexpanded_annual2 <- age_comps_unexpanded_annual2 %>%
  dplyr::select(!remove_cols)
age_comps_seas2 <- age_comps_seas2 %>%
  dplyr::select(!remove_cols)
age_comps_coast2 <- age_comps_coast2 %>%
  dplyr::select(!remove_cols)

# sort by fleet, year, and then ageing error type
age_comps_seas2 <- age_comps_seas2 %>%
  dplyr::arrange(fleet, year, ageErr)
age_comps_annual2 <- age_comps_annual2 %>%
  dplyr::arrange(fleet, year, ageErr)
age_comps_coast2 <- age_comps_coast2 %>%
  dplyr::arrange(fleet, year, ageErr)
# CAAL comps also need sorting by sex and length bin
CAAL_comps_annual2 <- CAAL_comps_annual2 %>%
  dplyr::arrange(fleet, year, sex, LbinLo, ageErr)

# write to CSV file
today_date <- format(as.Date(Sys.time()), "%d.%b.%Y")
write.csv(age_comps_annual2,
  file = file.path(
    dir, "pacfin", "forSS_annual",
    paste0("Age_for_SS3_", today_date, "_data_from_", out_date, ".csv")
  ),
  row.names = FALSE
)
write.csv(age_comps_unexpanded_annual2,
  file = file.path(
    dir, "pacfin", "forSS_annual_unexpanded",
    paste0("Age_for_SS3_", today_date, "_data_from_", out_date, ".csv")
  ),
  row.names = FALSE
)
write.csv(age_comps_seas2,
  file = file.path(
    dir, "pacfin", "forSS_seas",
    paste0("Age_for_SS3_", today_date, "_data_from_", out_date, ".csv")
  ),
  row.names = FALSE
)
write.csv(age_comps_coast2,
  file = file.path(
    dir, "pacfin", "forSS_coast",
    paste0("Age_for_SS3_", today_date, "_data_from_", out_date, ".csv")
  ),
  row.names = FALSE
)
write.csv(CAAL_comps_annual2,
  file = file.path(
    dir, "pacfin", "forSS_annual",
    paste0("CAAL_for_SS3_", today_date, "_data_from_", out_date, ".csv")
  ),
  row.names = FALSE
)

##########################################################
# Calculate empirical weight-at-age matrix
##########################################################
WAAdata <- Pdata %>% dplyr::filter(!is.na(weightkg) & !is.na(Age))
LAAdata <- Pdata %>% dplyr::filter(!is.na(lengthcm) & !is.na(Age))
# estimated weight based on length
LAAdata$weightkg_est[LAAdata$Sex == "F"] <- fa * LAAdata$lengthcm[LAAdata$Sex == "F"]^fb
LAAdata$weightkg_est[LAAdata$Sex == "M"] <- ma * LAAdata$lengthcm[LAAdata$Sex == "M"]^mb

# create empty matrix to store empirical weight-at-age values
years <- sort(unique(Pdata$SAMPLE_YEAR))
years <- years[years < 2023]
WAAmatrix_F <- matrix(
  data = NA,
  nrow = length(years),
  ncol = length(age_bins),
  dimnames = list(years, paste0("A", age_bins))
) %>%
  data.frame()


# copy matrix for males
WAAmatrix_M <- WAAmatrix_F

for (y in years) {
  for (a in age_bins) {
    # fill in female mean weights
    WAAmatrix_F[paste(y), paste0("A", a)] <-
      LAAdata %>% # WAAdata %>%
      dplyr::filter(
        SAMPLE_YEAR == y,
        Age %in% ifelse(
          test = a < max(age_bins),
          yes = a,
          no = a:(a + 50) # plus group
        ) &
          Sex == "F"
      ) %>%
      dplyr::summarize(mean(weightkg_est)) # estimated weights from length
    # dplyr::summarize(mean(weightkg)) # true weights from WAAdata
    # fill in male mean weights
    WAAmatrix_M[paste(y), paste0("A", a)] <-
      LAAdata %>% # WAAdata %>%
      dplyr::filter(
        SAMPLE_YEAR == y,
        Age %in% ifelse(
          test = a < max(age_bins),
          yes = a,
          no = a:(a + 50) # plus group
        ) &
          Sex == "M"
      ) %>%
      dplyr::summarize(mean(weightkg_est)) # estimated weights from length
    # dplyr::summarize(mean(weightkg)) # true weights from WAAdata
  }
}

par(mfrow = c(2, 1))
image(years, age_bins, as.matrix(WAAmatrix_F), main = "Females")
image(years, age_bins, as.matrix(WAAmatrix_M), main = "Males")

#### code below was related to adding commercial comps to an SS3 data file

# # read seasonal data file from Vlada's 15 March 2023 email
# datfile <- r4ss::SS_readdat("models/2023_March16/petrale_data.ss")
# # copy data file list to new name before modifying values
# datfile2 <- datfile

# # match names in order to bind data frames
# # rbind(names(len_comps_seas2), names(datfile$lencomp)) # comparing names
# names(len_comps_seas2) <- names(datfile$lencomp)
# names(age_comps_seas2) <- names(datfile$agecomp)
# # names(len_comps_annual2) <- names(datfile$lencomp)
# # names(age_comps_annual2) <- names(datfile$agecomp)


# # bind together with samples from other fleets (fleet != 1:4)
# datfile2$lencomp <- rbind(len_comps_seas2, datfile$lencomp[!datfile$lencomp$FltSvy %in% 1:4, ])

# # bind together with samples from other fleets (fleet != 1:4)
# datfile2$agecomp <- rbind(age_comps_seas2, datfile$agecomp[!datfile$agecomp$FltSvy %in% 1:4, ])

# # # save stuff created above
# # save(Pdata, Pdata_seas, Pdata_exp2_seas, Pdata_exp2_annual, file = file.path(dir, "Cleaned_PacFIN.PTRL.bds.3.Mar.2023.Rda"))

# r4ss::SS_writedat(datfile, "models/2023_March16/petrale_data_16March2023.ss",
#   overwrite = TRUE
# )
