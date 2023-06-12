# testing how to create unexpanded comps

# load pacfin data
load(file.path(
  "\\\\nwcfile/FRAM/Assessments/Assessment Data/2023 Assessment Cycle/petrale sole/",
  "PacFIN.PTRL.bds.08.May.2023.RData"
))

Pdata <- cleanPacFIN(
  Pdata = bds.pacfin,
  keep_age_method = c("B", "BB", 1),
  CLEAN = TRUE,
  verbose = TRUE
)

Pdata$geargroup <- "ALL"
Pdata$fleet[Pdata$state != "CA"] <- "WA_OR_ALL"
Pdata$fleet[Pdata$state == "CA"] <- "CA_ALL"

# create dummy catch file with some contrast
years <- 1876:2022
catch <- data.frame(
  Year = years,
  CA_ALL = seq(10, 2000, length = length(years)),
  OR_ALL = seq(1000, 10, length = length(years)),
  WA_ALL = rep(200, length = length(years))
)

# expansion 1
Pdata_exp <- getExpansion_1(
  Pdata = Pdata,
  fa = 2e-6, fb = 3, ma = 2e-6, mb = 3, ua = 2e-6, ub = 3
)

# expansion 2
Pdata_exp <- getExpansion_2(
  Pdata = Pdata_exp,
  Catch = catch,
  Units = "MT",
  stratification.cols = c("state", "geargroup")
)
# savedir = file.path(dir, "pacfin_bds", "plots"))

# Calculate the final expansion size
Pdata_exp$Final_Sample_Size <- capValues(Pdata_exp$Expansion_Factor_1_L * Pdata_exp$Expansion_Factor_2)

# create unexpanded versions
Pdata_unexp1 <- Pdata_exp
Pdata_unexp2 <- Pdata_exp
Pdata_unexp3 <- Pdata_exp
# set Final_Sample_Size in 1 to in different places
Pdata_unexp1$Final_Sample_Size <- 1
Pdata_unexp2$Final_Sample_Size_L <- 1
Pdata_unexp3$Final_Sample_Size_A <- 1

# calculate comps
len_bins <- seq(12, 62, 2)
age_bins <- 1:17

# length comps 3 ways
length_comps <- getComps(
  Pdata = Pdata_exp[!is.na(Pdata_exp$lengthcm), ],
  Comps = "LEN"
)
length_comps_unexp1 <- getComps(
  Pdata = Pdata_unexp1[!is.na(Pdata_unexp1$lengthcm), ],
  Comps = "LEN"
)
length_comps_unexp2 <- getComps(
  Pdata = Pdata_unexp2[!is.na(Pdata_unexp2$lengthcm), ],
  Comps = "LEN"
)
# age comps 3 ways
age_comps <- getComps(
  Pdata = Pdata_exp[!is.na(Pdata_exp$Age), ],
  Comps = "AGE"
)
age_comps_unexp1 <- getComps(
  Pdata = Pdata_unexp1[!is.na(Pdata_unexp1$Age), ],
  Comps = "AGE"
)
age_comps_unexp2 <- getComps(
  Pdata = Pdata_unexp2[!is.na(Pdata_unexp2$Age), ],
  Comps = "AGE"
)
age_comps_unexp3 <- getComps(
  Pdata = Pdata_unexp3[!is.na(Pdata_unexp3$Age), ],
  Comps = "AGE"
)

# check for differences
dplyr::all_equal(length_comps, length_comps_unexp1) # same
dplyr::all_equal(length_comps, length_comps_unexp2) # different

dplyr::all_equal(age_comps, age_comps_unexp1) # same
dplyr::all_equal(age_comps, age_comps_unexp2) # different
dplyr::all_equal(age_comps, age_comps_unexp3) # same
