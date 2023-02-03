################################################################################################
#			PacFIN Comps for the Petrale Sole 2019 Update Assessment	
#		
#		 Modifying the 2015 file: Run.PTRL.Assessment.Comps 01 April 2015.r
#				Orig written: Petrale Update, 31 Mar 2015
#					Modified by: Chantel Wetzel (2/12/19)
#
#		This code is only used to generate the length and age comps for petrale
################################################################################################			


#devtools::install_github("nwfsc-assess/PacFIN.Utilities")
#library(PacFIN.Utilities)
devtools::load_all("C:/Users/Chantel.Wetzel/Documents/GitHub/PacFIN.Utilities")


setwd("C:/Assessments/2019/petrale_2019/Data/Commercial_Comps")

# PacFIN BDS file
#load("2015 comparisons/PacFIN.PTRL.bds.17.Mar.2015.dmp") 
#TheData = PacFIN.PTRL.bds.17.Mar.2015   
#load("PacFIN.PTRL.bds.11.Mar.2019.dmp") 
#MarchData = PacFIN.PTRL.bds.11.Mar.2019 
#MayData = PacFIN.PTRL.bds.16.May.2019.dmp
#load("PacFIN.PTRL.bds.26.Jun.2019.dmp") 
#TheData = PacFIN.PTRL.bds.26.Jun.2019  

# Between the march and may data pull Oregon removed a bunch of samples that should have 
# been marked as special samples where they did not collected outside the standard sampling design.
# This removed all samples from 1978, and 1980-1986.

# Additionally, Ali said that all records where SAMPLE_QUALITY == 63 should be removed from PacFIN.
# They will not have this complete for this assessment cycle but are working on this issue.
# This column was not included in the June 26 data pull.
# According to Ali at ODFW all samples before 1981 were special projects and should not 
# be considered random.
badORsamples = which(TheData$SOURCE_AGID == "O" & TheData$SAMPLE_QUALITY == 63)
TheData = TheData[-badORsamples, ]
badORsamples = which(TheData$SOURCE_AGID == "O" & TheData$SAMPLE_YEAR < 1981) # all OR before 1981 are not random
TheData = TheData[-badORsamples, ]

# CALCOM data for petrale sole - from Brenda Erwin 2011
datfileCA <- "PetraleCALCOM_Query2011.csv" #
CALCOM <- read.csv(datfileCA, header = TRUE,sep = ",")

# Load in the catches by state for expansion
C.wd = "C:/Assessments/2019/petrale_2019/Data/Landings/"
C.file = paste0(C.wd, "Unformatted_Catch_All_Yrs.csv")
catch.file <- read.table(C.file, header = TRUE,sep = ",")


# Load in the current weight-at-length estimates by sex
load("C:/Assessments/2019/petrale_2019/Data/Biology/alpha_beta_ests.rda") 
# named the alpha and beta backward when I made the bio.ests file
# These estimates are without the most recent survey ages because they are not done yet
femalea = bio.ests$females["nwfsc","b"]; femaleb = bio.ests$females["nwfsc", "a"] 
malea   = bio.ests$males["nwfsc","b"];     maleb = bio.ests$males["nwfsc", "a"]    
unsexa  = bio.ests$unsexed["nwfsc","b"];  unsexb = bio.ests$unsexed["nwfsc", "a"]        


# Process the CalCOM data
# Check weight at length for CalCOM data for comparison with total wgt
CALCOM$tot.wgt = CALCOM$rel.error = CALCOM$wgt.est = CALCOM$num = NA
CALCOM[CALCOM$SEX == 2, "wgt.est"] = (femalea*(CALCOM[CALCOM$SEX == 2, "TLENGTH"]/10)^femaleb) * 2.20462
CALCOM[CALCOM$SEX == 1, "wgt.est"] = (malea  *(CALCOM[CALCOM$SEX == 1, "TLENGTH"]/10)^maleb) * 2.20462

samp.no = unique(CALCOM$SAMPLE_NO)
for (a in 1:length(samp.no)){
	find = which(CALCOM[,"SAMPLE_NO"] == samp.no[a])
	CALCOM$tot.wgt[find] = sum(CALCOM$wgt.est[find])
	CALCOM$rel.error[find] = (CALCOM$tot.wgt[find] - CALCOM$SumOfWEIGHT[find]) / CALCOM$tot.wgt[find]
	CALCOM$num[find] = length(find)
}

# plot(CALCOM[, "SumOfWEIGHT"], CALCOM[, "tot.wgt"])
# lines(1:200, 1:200)

# There are a number (614) of records where the estimated weight of the sampled fish in the two differ
# significantly from the SumOfWEIGHT in the CalCOM records. 
find = which(CALCOM$rel.error > 0.5 | CALCOM$rel.error < -0.5)
write.csv(CALCOM[find,], "CalCOM_bad_weights.csv")
write.csv(CALCOM, "CalCOM_all_data_check.csv")

#These records have > 50% difference between SumOfWEIGHT and estimated weights
badCArecords = as.character(unique(CALCOM$SAMPLE_NO[find]))
# replace the SumOfWEIGHT column with the sum of the estimated weights
replace = which(CALCOM$SAMPLE_NO %in% badCArecords)
CALCOM$SumOfWEIGHT[replace] = round(CALCOM$tot.wgt[replace], 2)

# also fill in estimated weights when SumOfWEIGHT == NA
replace = which(is.na(CALCOM$SumOfWEIGHT))
CALCOM$SumOfWEIGHT[replace] = round(CALCOM$tot.wgt[replace], 2)


# Use PacFIN.Utilities to combine the PacFIN and CALCOM data
MasterDat = combineCalCOM( Pdata = TheData, CalCOM = CALCOM ) 


# CHR = Charlotte, Canada
# VCN = Vancouver, Canada

plotRawData(MasterDat)

Pdata = cleanPacFIN(Pdata = MasterDat, 
					keep_length_type = c("", "A", "F", "U", "T", NA),
					keep_missing_lengths = FALSE,
					keep_INPFC = c("VUS","CL","VN","COL","NC","SC","EU","CP","EK","MT","PS"))
# Removal Report

# Records in input:                  256005 
# Records not in USINPFC             0 
# Records not in INPFC_AREA:         44732 
# Records in bad INPFC_AREA:         0 
# Records in badRecords list:        0 
# Records with bad SAMPLE_TYPE       2520 
# Records with bad SAMPLE_METHOD     669 
# Records with no SAMPLE_NO          0 
# Records with no usable length      217 
# Records remaining:                 207867

# Fix Oregon sample where there is no length, weight, or age for a female fish (OR141974) from 2014
Pdata[Pdata$SAMPLE_NO == "OR141974", "FEMALES_NUM"] = 22


# Check grade function written by Kelli
checkGrade(Pdata)

MasterPdata = Pdata

# Check lengths by sex
# Females first
k = c(0.135, 0.20, (0.135+0.20)/2)
Linf = c(54, 43, (54+43)/2)
L0 = c(16, 16, 16)
CV1 = c(0.19, 0.14, 0.165)
CV2 = c(0.03, 0.04, 0.04)
Par = list(k[1], Linf[1], L0[1], CV1[1], CV2[1])

Pdata = checkLenAge(Pdata = Pdata, Par = Par, keepAll = TRUE, Optim = TRUE)
remove = which(Pdata[,'length'] > Pdata[,'Lhat_high'] | Pdata[,'FISH_LENGTH'] < Pdata[,'Lhat_low'])
badRecords = Pdata[remove,]

# Look at the flagged records relative to length-age relationship
# par(mfrow = c(1,1))
# plot(badRecords[badRecords$SEX == "F",   "age"], badRecords[badRecords$SEX == "F", "length"], type = 'p', col = 'red', ylim = c(0, 900))
# points(badRecords[badRecords$SEX == "F", "age"], badRecords[badRecords$SEX == "F", "Lhat_pred"], pch = 16, col = 'red')
# points(badRecords[badRecords$SEX == "M", "age"], badRecords[badRecords$SEX == "M", "length"], col = 'blue')
# points(badRecords[badRecords$SEX == "M", "age"], badRecords[badRecords$SEX == "M", "Lhat_pred"], pch = 16, col = 'blue')
# points(badRecords[badRecords$SEX == "U", "age"], badRecords[badRecords$SEX == "U", "length"], col = 'darkgrey')
# points(badRecords[badRecords$SEX == "U", "age"], badRecords[badRecords$SEX == "U", "Lhat_pred"], pch = 16, col = 'darkgrey')
# 
# # Look at the bad records relative to all the other data
# par(mfrow=c(3,1))
# plot(Pdata[Pdata$SEX == "F", "age"], Pdata[Pdata$SEX == "F", "length"], type = 'p', col = 'red', ylim = c(0, 900))
# points(Pdata[Pdata$SEX == "F", "age"], Pdata[Pdata$SEX == "F", "Lhat_pred"], pch = 16, col = 1)
# points(badRecords[badRecords$SEX == "F", "age"], badRecords[badRecords$SEX == "F", "length"], pch = 17, col = 1)
# 
# plot(Pdata[Pdata$SEX == "M", "age"], Pdata[Pdata$SEX == "M", "length"], type = 'p', col = 'blue',  ylim = c(0, 900))
# points(Pdata[Pdata$SEX == "M", "age"], Pdata[Pdata$SEX == "M", "Lhat_pred"], pch = 16, col = 1)
# points(badRecords[badRecords$SEX == "M", "age"], badRecords[badRecords$SEX == "M", "length"], pch = 17, col = 1)
# 
# plot(Pdata[Pdata$SEX == "U", "age"], Pdata[Pdata$SEX == "U", "length"], type = 'p', col = 'grey',  ylim = c(0, 900))
# points(Pdata[Pdata$SEX == "U", "age"], Pdata[Pdata$SEX == "U", "Lhat_pred"], pch = 16, col = 1)
# points(badRecords[badRecords$SEX == "U", "age"], badRecords[badRecords$SEX == "U", "length"], pch = 17, col = 1)
# 
# # Look at the lengths  where there are not ages 
# par(mfrow =c (1,2))
# check = Pdata$SEX == "F" & Pdata$age == -1
# plot(Pdata[check, "length"], type = 'p', col = 'red', pch = 21, ylim = c(0, max(Pdata$length, na.rm = T)))
# check =  Pdata$SEX == "M" & Pdata$age == -1
# plot(Pdata[check, "length"], type = 'p', col = 'blue', pch = 21, ylim = c(0, max(Pdata$length, na.rm = T)))
# There are 3 freakishly large female fish, all from CalCOM...

# Remove records where length is not consistent with the age & fish that are greater than ever observed
bad_len = which(Pdata$length > 700)
bad_sex = which(Pdata$SEX == "H")
toss = c(remove, bad_len, bad_sex)
Bad = Pdata[toss,]
write.csv(Bad, file = paste0(getwd(), "/Bad_PacFIN_Records.csv"))
Pdata$length[toss] = NA
Pdata$age[toss] = -1

# Save the filtered data
save(Pdata, file = "Cleaned_PacFIN.PTRL.bds.26.Jun.2019.Rda")


# Further filtering and preparation for expansions
Pdata = getSeason(Pdata, season_type=1, yearUp=c(11,12))
Pdata = getGearGroup(Pdata)

# Plot the length data
# plotCleaned(Pdata)

Pdata$usegear = paste('TRAWL',Pdata$season,sep=".")

# CATCH HEADERS ARE: WA.TRAWL.1	WA.TRAWL.2	OR.TRAWL.1	OR.TRAWL.2 CA.TRAWL.1 CA.TRAWL.2
# target is the set of the common columns of Pdata and the Catch to use for stratification, e.g.,
# Specify fleets
Pdata$fleet[Pdata$state != "CA"] = "WA_OR"
Pdata$fleet[Pdata$state == "CA"] = "CA"

Pdata$mygear = "TRAWL"
Pdata$stratification = paste(Pdata$state, Pdata$mygear, Pdata$season, sep=".")


#################################################################################
# Length comp expansions
#################################################################################

Pdata =  getExpansion_1(Pdata, 
						maxExp = 0.95,
						Exp_WA = TRUE, 
						Indiv_Wgts = TRUE,
						plot = FALSE,
						fa=femalea, fb=femaleb, ma=malea, mb=maleb, ua=unsexa, ub=unsexb)

# The convert input will change the catch from external file into pounds
Pdata = getExpansion_2(Pdata, 
					   Catch = catch.file, 
					   Convert=TRUE, 
					   maxExp=0.80)

# set the final sample size, evaluate it and cap it at the 90th percentile value.
# COMPS WITH ONLY EXPANSION FACTOR 1, EXPANDED TO PORT SAMPLE
# Pdata$Final_Sample_Size = Pdata$Expansion_Factor_1
# COMPS WITH EXPANSION FACTORS 1 & 2, EXPANDED TO PORT SAMPLE AND STATE CATCH
#Pdata$Final_Sample_Size = Pdata$Expansion_Factor_1 * Pdata$Expansion_Factor_2
#Pdata$Final_Sample_Size = capValues(Pdata$Final_Sample_Size, maxVal = 0.80)

Pdata$Final_Sample_Size <- capValues(Pdata$Expansion_Factor_1_L * Pdata$Expansion_Factor_2, maxVal = 0.80)

# Look for consistency between lengths and ages of sampled fish
myLbins = c(seq(12,62,2))
# plotStrat(data = Pdata, dir = getwd(), ylimperc = 0.65, npages = 11, dolbins = myLbins)

# Length COMPS WITH EXPANSIONS 1 AND 2, PORT SAMPLE AND AREA (STATE) LANDED
# The default stratification is by fleet, fishyr, and season, and the lengthcm, age or both are appended to the depending on the "Comps" argument.
Lcomps = getComps(Pdata, strat = c("fleet","usegear"), Comps = "LEN")
Lcomps = doSexRatio(Lcomps, findRatio=TRUE)
masterLcomps = Lcomps

writeComps(inComps = Lcomps, 
		   returns = "FthenM",
		   fname = "forSS/Lcomps.PTRL2019.26.June.csv", 
		   lbins = myLbins, 
		   partition = 2, 
		   dummybins = FALSE)


out = read.csv(paste0(getwd(), "/forSS/Lcomps.PTRL2019.26.June.csv"), skip = 3, header = TRUE)
start = which(as.character(out[,1]) %in% c(" Females then males ")) + 2
end   = nrow(out)
cut_out = out[start:end,]

# format the length comps
cut_out$fleetnum = NA
cut_out$fleetnum[cut_out$fleet =="WA_OR" & cut_out$usegear == "TRAWL.1"] = 1
cut_out$fleetnum[cut_out$fleet =="CA"    & cut_out$usegear == "TRAWL.1"] = 3
cut_out$fleetnum[cut_out$fleet =="WA_OR" & cut_out$usegear == "TRAWL.2"] = 2
cut_out$fleetnum[cut_out$fleet =="CA"    & cut_out$usegear == "TRAWL.2"] = 4

ind = which(colnames(cut_out) %in% "L12"):which(colnames(cut_out) %in% "L62.1")
format = cbind(cut_out$fishyr, cut_out$season, cut_out$fleetnum, cut_out$gender, cut_out$part, cut_out$Nsamps, cut_out$Ntows, cut_out[,ind])
colnames(format) = c("fishyr", "month", "fleet", "sex", "part", "Nsamps", "Ntows", colnames(cut_out[ind]))
format = format[format$fishyr != 2019, ]
write.csv(format[,!(colnames(format) %in% c("Nsamps"))], file = paste0(getwd(), "/forSS/Lcomps_females_males_formatted.csv"), row.names = FALSE)

temp = Pdata[!is.na(Pdata$FISH_LENGTH) & Pdata$SAMPLE_YEAR < 2019,]
Nfish = table(temp$SAMPLE_YEAR, temp$stratification)
Nfish = cbind(Nfish[,"WA.TRAWL.1"] + Nfish[,"OR.TRAWL.1"], Nfish[,"WA.TRAWL.2"] + Nfish[,"OR.TRAWL.2"],
			  Nfish[,"CA.TRAWL.1"], Nfish[,"CA.TRAWL.2"])
colnames(Nfish) = c("Winter_N", "Summer_N", "Winter_S", "Summer_S")

aa = unique(temp$stratification)
yy = sort(unique(temp$SAMPLE_YEAR))
Ntows = matrix(0, length(yy), length(aa))
for(y in 1:length(yy)){
	for(a in 1:length(aa)){
		ind = which(temp$SAMPLE_YEAR == yy[y] & temp$stratification == aa[a])
		if(length(ind) > 0) {Ntows[y, a] = length(unique(temp$SAMPLE_NO[ind])) }
	}
}
colnames(Ntows) = aa
rownames(Ntows) = yy

samples = cbind(Ntows[,"WA.TRAWL.1"] + Ntows[,"OR.TRAWL.1"], Nfish[,"Winter_N"], 
			    Ntows[,"WA.TRAWL.2"] + Ntows[,"OR.TRAWL.2"], Nfish[,"Summer_N"],
				Ntows[,"CA.TRAWL.1"],  Nfish[,"Winter_S"], Ntows[,"CA.TRAWL.2"], Nfish[,"Summer_S"])

colnames(samples) = c("Winter_N_NTows", "Winter_N_Nfish", "Summer_N_NTows", "Summer_N_Nfish",
	 				  "Winter_S_NTows", "Winter_S_Nfish", "Summer_S_NTows", "Summer_S_Nfish")
write.csv(samples, file = paste0(getwd(),"/forSS/Fishery_Length_Samples.csv"), row.names = TRUE)


##############################################################################################
##### AGE COMPS, must be run after lengths ##############
##############################################################################################

# Adata = cleanAges(Pdata, minAge = 1)
# 
# Removal report
# 
# Records in input:                   207867 
# Records with age less than min:     141716 
# Records with bad agemethods:        82 
# Records remaining:                  66069 

Adata = Pdata

#Expand the age comp data
Adata = getExpansion_1(Pdata = Adata, 
					   Exp_WA = TRUE, 
					   Indiv_Wgts = TRUE,
					   fa = femalea, fb = femaleb, ma = malea, mb = maleb, ua = unsexa, ub = unsexb)

Adata = getExpansion_2(Pdata = Adata, 
					   Catch = catch.file, 
					   Convert = TRUE, 
					   maxExp = 0.80)

Adata$Final_Sample_Size = capValues(Adata$Expansion_Factor_1_A * Adata$Expansion_Factor_2, maxVal = 0.80)

# Try filetering out only ages at this point
Adata  = Adata[Adata$age >= 1,]

# Deal with age error
keep_age_methods=c("B","S","")
Adata$agemethod = Adata$AGE_METHOD
if (!1 %in% keep_age_methods) {
  Adata$agemethod[Adata$agemethod == "1"] = "B"
}
if (!2 %in% keep_age_methods){
  Adata$agemethod[Adata$agemethod == "2"] = "S"
}
Adata$agemethod[is.na(Adata$agemethod)] = -1
# There are years with blank agemethod - do this to be easily trackable
Adata$agemethod[!Adata$agemethod %in% c("B", "S")] = "U"

#Define ageing error vector based on Petrale reading methods
# 2 = CAP BB; 3 = CAP surface; 4 = CAP Combo; 5 = WDFW Combo; 6 = WDFW Surface; 7 = WDFW BB; 8 = CAP pre-1990 surface

# CAP Lab reads the California ages
# California 
Adata$ageerr = -99
Adata$ageerr[Adata$SOURCE_AGID %in% c("C", "CalCOM") & Adata$agemethod == "S" & Adata$fishyr < 1990] = 8
Adata$ageerr[Adata$SOURCE_AGID %in% c("C", "CalCOM") & Adata$agemethod == "U"] = 4

#There are middle years where there are U BB and S reads 
Adata$ageerr[Adata$SOURCE_AGID %in% c("C", "CalCOM") & Adata$fishyr >= 1985 & Adata$fishyr <= 1991] = 4
Adata$ageerr[Adata$SOURCE_AGID %in% c("C", "CalCOM") & Adata$agemethod == "B" & Adata$fishyr >= 1992 ] = 2

#Oregon
Adata$ageerr[Adata$SOURCE_AGID == "O" & Adata$agemethod == "S" & Adata$fishyr <= 1980] = 8
Adata$ageerr[Adata$SOURCE_AGID == "O" & Adata$fishyr >= 1980 & Adata$fishyr < 1999] = 4
Adata$ageerr[Adata$SOURCE_AGID == "O" & Adata$agemethod == "B" & Adata$fishyr >= 1999] = 2
Adata$ageerr[Adata$SOURCE_AGID == "O" & Adata$agemethod == "S" & Adata$fishyr >= 1999] = 3
Adata$ageerr[Adata$SOURCE_AGID == "O" & Adata$fishyr >= 2007] = 2

#Washington
Adata$ageerr[Adata$SOURCE_AGID == "W" & Adata$agemethod %in% c("S") ] = 6 
Adata$ageerr[Adata$SOURCE_AGID == "W" & Adata$agemethod %in% c("U") & Adata$fishyr < 2008] = 6 
Adata$ageerr[Adata$SOURCE_AGID == "W" & Adata$agemethod == "B"] = 7
Adata$ageerr[Adata$SOURCE_AGID == "W" & Adata$fishyr %in% c(2009, 2010)] = 5

Adata$fleet[Adata$stratification %in% c("WA.TRAWL.1", "OR.TRAWL.1")] = 1
Adata$fleet[Adata$stratification %in% c("WA.TRAWL.2", "OR.TRAWL.2")] = 2
Adata$fleet[Adata$stratification %in% c("CA.TRAWL.1")] = 3
Adata$fleet[Adata$stratification %in% c("CA.TRAWL.2")] = 4

# There is an Oregon tow where 29 female and 1 unsexed fish were sampled
# Only the female fish were subsampled for ages
# This causes the getExpansion_1 code to error out due the UNK_NUM being 1
# Overwriting the UNK_NUM for now but this is wrong
# find = which(Adata$SAMPLE_NO == "OR110384")
# Adata$UNK_NUM[find] = Adata$UNK_WT = NA


Acomps = getComps(Adata, defaults = c("fleet", "fishyr", "season", "ageerr"), Comps="AGE")
Acomps = doSexRatio(Acomps)
writeComps(inComps = Acomps, abins=seq(1,17,1), partition = 2, 
			fname="forSS/AGE.PTRL.2019.26.June.csv")

out = read.csv(paste0(getwd(), "/forSS/AGE.PTRL.2019.26.June.csv"), skip = 3, header = TRUE)

start = which(as.character(out[,1]) %in% c(" Females then males ")) + 2
end   = nrow(out)
cut_out = out[start:end,]


ind = which(colnames(cut_out) %in% "A1"):which(colnames(cut_out) %in% "A17.1")
lbinlow = lbinhi = -1
format = cbind(cut_out$fishyr, 7, cut_out$fleet, cut_out$gender, cut_out$part, cut_out$ageerr, lbinlow, lbinhi, cut_out$Nsamps, cut_out$Ntows, cut_out[,ind])
colnames(format) = c("fishyr", "month", "fleet", "sex", "part", "ageerr", "low", "high", "Nsamps", "Ntows", colnames(cut_out[ind]))
format = format[format$fishyr != 2019, ]
write.csv(format, file = paste0(getwd(), "/forSS/Acomps_females_males_formatted.csv"), row.names = FALSE)

write.csv(format[,!(colnames(format) %in% c("Nsamps"))], file = paste0(getwd(), "/forSS/Acomps_females_males_formatted.csv"), row.names = FALSE)


temp = Adata[!is.na(Adata$age) & Adata$SAMPLE_YEAR < 2019,]
Nfish = table(temp$SAMPLE_YEAR, temp$stratification)
Nfish = cbind(Nfish[,"WA.TRAWL.1"] + Nfish[,"OR.TRAWL.1"], Nfish[,"WA.TRAWL.2"] + Nfish[,"OR.TRAWL.2"],
			  Nfish[,"CA.TRAWL.1"], Nfish[,"CA.TRAWL.2"])
colnames(Nfish) = c("Winter_N", "Summer_N", "Winter_S", "Summer_S")

aa = unique(temp$stratification)
yy = sort(unique(temp$SAMPLE_YEAR))
Ntows = matrix(0, length(yy), length(aa))
for(y in 1:length(yy)){
	for(a in 1:length(aa)){
		ind = which(temp$SAMPLE_YEAR == yy[y] & temp$stratification == aa[a])
		if(length(ind) > 0) {Ntows[y, a] = length(unique(temp$SAMPLE_NO[ind])) }
	}
}
colnames(Ntows) = aa
rownames(Ntows) = yy

samples = cbind(Ntows[,"WA.TRAWL.1"] + Ntows[,"OR.TRAWL.1"], Nfish[,"Winter_N"], 
			    Ntows[,"WA.TRAWL.2"] + Ntows[,"OR.TRAWL.2"], Nfish[,"Summer_N"],
				Ntows[,"CA.TRAWL.1"],  Nfish[,"Winter_S"], Ntows[,"CA.TRAWL.2"], Nfish[,"Summer_S"])

colnames(samples) = c("Winter_N_NTows", "Winter_N_Nfish", "Summer_N_NTows", "Summer_N_Nfish",
	 				  "Winter_S_NTows", "Winter_S_Nfish", "Summer_S_NTows", "Summer_S_Nfish")
write.csv(samples, file = paste0(getwd(),"/forSS/Fishery_Age_Samples.csv"), row.names = TRUE)