# get U.S.-landed catch from area "3S"
# which is described as "SOUTHERN PORTION OF AREA 3C (UNITED STATES ONLY)"
load("data-raw/PacFIN.PTRL.CompFT.27.Jan.2023.RData")
catch_agg <- aggregate(LANDED_WEIGHT_MTONS ~ LANDING_YEAR + PACFIN_CATCH_AREA_CODE, data = catch.pacfin, FUN = sum)
catch_agg_3S <- catch_agg[catch_agg$PACFIN_CATCH_AREA_CODE == "3S",]
plot(catch_agg_3S$LANDING_YEAR, catch_agg_3S$LANDED_WEIGHT_MTONS, type = 'h', lwd = 10, lend = 3)

load("data-raw/foreign/PacFIN.PTRL.CompFT.27.Feb.2023.RData")

catch.pacfin %>% dplyr::group_by(LANDING_YEAR, VESSEL_ID, PACFIN_CATCH_AREA_NAME) %>% dplyr::n_distinct()
catch.pacfin %>% dplyr::group_by(LANDING_YEAR, PACFIN_CATCH_AREA_NAME) %>% dplyr::count()

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