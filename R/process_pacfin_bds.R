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
load(file.path("data-raw", bds_file)

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
Pdata$Sex <- Pdata$SEX # presumably needed for est_growth()

# estimate growth (adds columns "Lhat_low","Lhat_pred", "Lhat_high" to Pdat)
Pdata <- nwfscSurvey::est_growth(
  dat = Pdata,
  Par = data.frame(K = k, Linf = Linf, L0 = L0, CV0 = CV1, CV1 = CV2),
  sdFactor = 4, # four standard deviations
  dir = "data-raw/PacFIN.Utilities"
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
save(Pdata, file = "data-raw/Cleaned_PacFIN.PTRL.bds.10.Feb.2023.Rda")
# load("data-raw/Cleaned_PacFIN.PTRL.bds.10.Feb.2023.Rda")

# Create separate data frame with seasons and adjusted years
# In getSeason(), the season_type = 1 is for Petrale
# it assigns months 1,2,11,12 to season 1 and the rest to seas 2
# yearUp=c(11,12) assigns months 11 and 12 to the following year
# the "fishyr" column contains the modified values
# "SAMPLE_YEAR" and "year" columns are not changed
Pdata_seas <- getSeason(Pdata, season_type = 1, yearUp = c(11, 12))

table(Pdata$geargroup)
#   HKL    NET    POT    TWL    TWS 
#   602     56     37 186838     78 
# Set up the expected fleet structure for the annual version

# Fleets for seasonal model
Pdata_seas$fleet[Pdata_seas$state != "CA"] <- "WA_OR"
Pdata_seas$fleet[Pdata_seas$state == "CA"] <- "CA"

# Fleets for annual model
Pdata$geargroup = "ALL"
Pdata$fleet[Pdata$state != "CA"] = paste0("WA_OR", "_", Pdata$geargroup)
Pdata$fleet[Pdata$state == "CA"] = paste0("CA", "_", Pdata$geargroup)

# Load in the catches by state for expansion
# annual catch
catch_annual <- read.csv("data-raw/pacfin/commercial_catch_by_state.csv")
colnames(catch_annual) <- c("Year", "CA_1", "OR_1", "WA_1")

# seasonal catch
catch_seas <- read.csv("data-raw/pacfin/catch_file_season fleets.csv")
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

dir.create("data-raw/pacfin/plots_seas", recursive = TRUE)
dir.create("data-raw/pacfin/plots_annual", recursive = TRUE)

# First stage expansion: expand comps to the trip level
# seasonal
Pdata_exp1_seas <- getExpansion_1(
  Pdata = Pdata_seas,
  plot = "data-raw/pacfin/plots_seas",
  fa = fa, fb = fb, ma = ma, mb = mb, ua = ua, ub = ub
)
# annual
Pdata_exp1_annual <- getExpansion_1(
  Pdata = Pdata,
  plot = "data-raw/pacfin/plots_annual",
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
  savedir = "data-raw/pacfin/plots_seas"
)

Pdata_exp2_annual <- getExpansion_2(
  Pdata = Pdata_exp1_annual,
  Catch = catch_annual,
  Units = "MT",
  stratification.cols = c("state", "geargroup"),
  savedir = "data-raw/pacfin/plots_annual"
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

table(Pdata$SOURCE_AGID, Pdata$SEX)

Pdata$count <- 1
ggplot(Pdata, aes(x = lengthcm, y = count, fill = SEX)) +
  geom_histogram(aes(y = count), position = "stack", stat = "identity") +
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
out_name <- sub(pattern = "(.*)\\..*$", replacement = "\\1", bds_file)

writeComps(
  inComps = length_comps_seas,
  fname = file.path("data-raw/pacfin/forSS_seas/", paste0("Lengths_", out_name, ".csv")),
  lbins = len_bins,
  sum1 = TRUE,
  partition = 2,
  digits = 4,
  dummybins = FALSE
)
writeComps(
  inComps = length_comps_annual,
  fname = file.path("data-raw/pacfin/forSS_annual/", paste0("Lengths_", out_name, ".csv")),
  lbins = len_bins,
  sum1 = TRUE,
  partition = 2,
  digits = 4,
  dummybins = FALSE
)

# TODO: Ian stopped here on 10 Feb, continue here

##########################################################
# Calculate the expansion for age data
##########################################################

Pdata_exp$Final_Sample_Size <- capValues(Pdata_exp$Expansion_Factor_1_A * Pdata_exp$Expansion_Factor_2)

age_comps <- getComps(
  Pdata_exp[!is.na(Pdata_exp$Age), ],
  Comps = "Age"
)

##########################################################
# Create the age compositions
##########################################################

age_bins <- 1:17

writeComps(
  inComps = age_comps,
  fname = file.path(dir, "pacfin", "forSS", paste0("Age_", out_name, ".csv")),
  abins = age_bins,
  sum1 = TRUE,
  partition = 2,
  digits = 4,
  dummybins = FALSE
)

# Create condition-age-at-length compositions just in case
# you want to explore them
caal_comps <- getComps(
  Pdata = Pdata_exp[!is.na(Pdata_exp$age), ],
  Comps = "AAL"
)

writeComps(
  inComps = caal_comps,
  fname = file.path(dir, "pacfin", "forSS", paste0("CAAL_", out_name, ".csv")),
  lbins = len_bins,
  abins = age_bins,
  sum1 = TRUE,
  partition = 2,
  dummybins = FALSE
)

###############################################################################################
# Let's format the csv files for direct use in SS3
#####################################################################################

out <- read.csv(
  file.path(dir, "pacfin", "forSS", paste0("Lengths_", out_name, ".csv")),
  skip = 3,
  header = TRUE
)

start <- 1
end <- which(as.character(out[, 1]) %in% c(" Females only ")) - 1
cut_out <- out[start:end, ]

# format the length comps
cut_out$fleet[cut_out$fleet == "CA_ALL"] <- 1
cut_out$fleet[cut_out$fleet == "WA_OR_ALL"] <- 2

ind <- which(colnames(cut_out) %in% paste0("F", min(len_bins))):
which(colnames(cut_out) %in% paste0("M", max(len_bins)))
format <- cbind(cut_out$year, cut_out$month, cut_out$fleet, cut_out$sex, cut_out$partition, cut_out$InputN, cut_out[, ind])
colnames(format) <- c("year", "month", "fleet", "sex", "part", "InputN", colnames(cut_out[ind]))
format <- format[format$year != 2021, ]
write.csv(
  format,
  file = file.path(dir, "pacfin", "forSS", paste0("Lcomps_for_SS3_", out_name, ".csv")),
  row.names = FALSE
)


# Let's create the sample table
temp <- Pdata[!is.na(Pdata$lengthcm) & Pdata$year < 2021, ]
Nfish <- table(temp$year, temp$state)
colnames(Nfish) <- sort(unique(temp$state))

# Deal with age error
keep_age_methods <- c("B", "S", "")
Adata$agemethod <- Adata$AGE_METHOD
if (!1 %in% keep_age_methods) {
  Adata$agemethod[Adata$agemethod == "1"] <- "B"
}
if (!2 %in% keep_age_methods) {
  Adata$agemethod[Adata$agemethod == "2"] <- "S"
}
Adata$agemethod[is.na(Adata$agemethod)] <- -1
# There are years with blank agemethod - do this to be easily trackable
Adata$agemethod[!Adata$agemethod %in% c("B", "S")] <- "U"

# Define ageing error vector based on Petrale reading methods
# 2 = CAP BB; 3 = CAP surface; 4 = CAP Combo; 5 = WDFW Combo; 6 = WDFW Surface; 7 = WDFW BB; 8 = CAP pre-1990 surface

# CAP Lab reads the California ages
# California
Adata$ageerr <- -99
Adata$ageerr[Adata$SOURCE_AGID %in% c("C", "CalCOM") & Adata$agemethod == "S" & Adata$fishyr < 1990] <- 8
Adata$ageerr[Adata$SOURCE_AGID %in% c("C", "CalCOM") & Adata$agemethod == "U"] <- 4

# There are middle years where there are U BB and S reads
Adata$ageerr[Adata$SOURCE_AGID %in% c("C", "CalCOM") & Adata$fishyr >= 1985 & Adata$fishyr <= 1991] <- 4
Adata$ageerr[Adata$SOURCE_AGID %in% c("C", "CalCOM") & Adata$agemethod == "B" & Adata$fishyr >= 1992] <- 2

# Oregon
Adata$ageerr[Adata$SOURCE_AGID == "O" & Adata$agemethod == "S" & Adata$fishyr <= 1980] <- 8
Adata$ageerr[Adata$SOURCE_AGID == "O" & Adata$fishyr >= 1980 & Adata$fishyr < 1999] <- 4
Adata$ageerr[Adata$SOURCE_AGID == "O" & Adata$agemethod == "B" & Adata$fishyr >= 1999] <- 2
Adata$ageerr[Adata$SOURCE_AGID == "O" & Adata$agemethod == "S" & Adata$fishyr >= 1999] <- 3
Adata$ageerr[Adata$SOURCE_AGID == "O" & Adata$fishyr >= 2007] <- 2

# Washington
Adata$ageerr[Adata$SOURCE_AGID == "W" & Adata$agemethod %in% c("S")] <- 6
Adata$ageerr[Adata$SOURCE_AGID == "W" & Adata$agemethod %in% c("U") & Adata$fishyr < 2008] <- 6
Adata$ageerr[Adata$SOURCE_AGID == "W" & Adata$agemethod == "B"] <- 7
Adata$ageerr[Adata$SOURCE_AGID == "W" & Adata$fishyr %in% c(2009, 2010)] <- 5

Adata$fleet[Adata$stratification %in% c("WA.TRAWL.1", "OR.TRAWL.1")] <- 1
Adata$fleet[Adata$stratification %in% c("WA.TRAWL.2", "OR.TRAWL.2")] <- 2
Adata$fleet[Adata$stratification %in% c("CA.TRAWL.1")] <- 3
Adata$fleet[Adata$stratification %in% c("CA.TRAWL.2")] <- 4

# There is an Oregon tow where 29 female and 1 unsexed fish were sampled
# Only the female fish were subsampled for ages
# This causes the getExpansion_1 code to error out due the UNK_NUM being 1
# Overwriting the UNK_NUM for now but this is wrong
# find = which(Adata$SAMPLE_NO == "OR110384")
# Adata$UNK_NUM[find] = Adata$UNK_WT = NA


Acomps <- getComps(Adata, defaults = c("fleet", "fishyr", "season", "ageerr"), Comps = "AGE")
Acomps <- doSexRatio(Acomps)
writeComps(
  inComps = Acomps, abins = seq(1, 17, 1), partition = 2,
  fname = "forSS/AGE.PTRL.2019.26.June.csv"
)

out <- read.csv(paste0(getwd(), "/forSS/AGE.PTRL.2019.26.June.csv"), skip = 3, header = TRUE)

start <- which(as.character(out[, 1]) %in% c(" Females then males ")) + 2
end <- nrow(out)
cut_out <- out[start:end, ]


ind <- which(colnames(cut_out) %in% "A1"):which(colnames(cut_out) %in% "A17.1")
lbinlow <- lbinhi <- -1
format <- cbind(cut_out$fishyr, 7, cut_out$fleet, cut_out$gender, cut_out$part, cut_out$ageerr, lbinlow, lbinhi, cut_out$Nsamps, cut_out$Ntows, cut_out[, ind])
colnames(format) <- c("fishyr", "month", "fleet", "sex", "part", "ageerr", "low", "high", "Nsamps", "Ntows", colnames(cut_out[ind]))
format <- format[format$fishyr != 2019, ]
write.csv(format, file = paste0(getwd(), "/forSS/Acomps_females_males_formatted.csv"), row.names = FALSE)

write.csv(format[, !(colnames(format) %in% c("Nsamps"))], file = paste0(getwd(), "/forSS/Acomps_females_males_formatted.csv"), row.names = FALSE)


temp <- Adata[!is.na(Adata$age) & Adata$SAMPLE_YEAR < 2019, ]
Nfish <- table(temp$SAMPLE_YEAR, temp$stratification)
Nfish <- cbind(
  Nfish[, "WA.TRAWL.1"] + Nfish[, "OR.TRAWL.1"], Nfish[, "WA.TRAWL.2"] + Nfish[, "OR.TRAWL.2"],
  Nfish[, "CA.TRAWL.1"], Nfish[, "CA.TRAWL.2"]
)
colnames(Nfish) <- c("Winter_N", "Summer_N", "Winter_S", "Summer_S")

aa <- unique(temp$stratification)
yy <- sort(unique(temp$SAMPLE_YEAR))
Ntows <- matrix(0, length(yy), length(aa))
for (y in 1:length(yy)) {
  for (a in 1:length(aa)) {
    ind <- which(temp$SAMPLE_YEAR == yy[y] & temp$stratification == aa[a])
    if (length(ind) > 0) {
      Ntows[y, a] <- length(unique(temp$SAMPLE_NO[ind]))
    }
  }
}
colnames(Ntows) <- aa
rownames(Ntows) <- yy

samples <- cbind(
  Ntows[, "WA.TRAWL.1"] + Ntows[, "OR.TRAWL.1"], Nfish[, "Winter_N"],
  Ntows[, "WA.TRAWL.2"] + Ntows[, "OR.TRAWL.2"], Nfish[, "Summer_N"],
  Ntows[, "CA.TRAWL.1"], Nfish[, "Winter_S"], Ntows[, "CA.TRAWL.2"], Nfish[, "Summer_S"]
)

colnames(samples) <- c(
  "Winter_N_NTows", "Winter_N_Nfish", "Summer_N_NTows", "Summer_N_Nfish",
  "Winter_S_NTows", "Winter_S_Nfish", "Summer_S_NTows", "Summer_S_Nfish"
)
write.csv(samples, file = paste0(getwd(), "/forSS/Fishery_Age_Samples.csv"), row.names = TRUE)
