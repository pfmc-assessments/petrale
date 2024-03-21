# load data from PacFIN which includes foreign catches, 
# extracted by Kelli Johnson on 27 Feb 2023
# file is confidential and only available to Vlada and Ian
load('data-raw/foreign/PacFIN.PTRL.CompFT.27.Feb.2023.RData')
catch_agg_CAN <- aggregate(LANDED_WEIGHT_MTONS ~ LANDING_YEAR + PACFIN_CATCH_AREA_CODE, data = catch.pacfin, FUN = sum)
plot(catch_agg_CAN$LANDING_YEAR, catch_agg_CAN$LANDED_WEIGHT_MTONS, type = 'h', lwd = 10, lend = 3)

# which years have fewer than 3 vessels so total catch can't be shared publicly
dplyr::group_by(catch.pacfin,LANDING_YEAR) |> 
  dplyr::summarize(count=dplyr::n_distinct(VESSEL_ID)) |> 
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
dplyr::group_by(catch.pacfin, PACFIN_CATCH_AREA_NAME) |>
  dplyr::summarize(count=dplyr::n_distinct(VESSEL_ID)) |> 
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

# list of Canadian areas from https://pacfin.psmfc.org/pacfin_pub/data_rpts_pub/code_lists/ar_tree.txt
# CANADIAN    \_CAN                INPFC-grp      -       All Canadian INPFC areas combined                                           
# CHARLOTTE      \_CT              INPFC          -       50 30' N TO 54 40' N                                                        
# 5A                \_5A           PSMFC          -       50 30' N TO 51 15' N                                                        
# 5B                \_5B           PSMFC          -       51 15' N TO 52 00' N WEST OF C. ST.JAMES & TO 52 10' EAST OF C. ST.JAMES    
# 5C                \_5C           PSMFC          -       52 10' N TO ~53 N & EAST OF QUEEN CHARLOTTE ISLANDS                         
# 5D                \_5D           PSMFC          -       ~53 N TO 54 40' N & EAST OF QUEEN CHARLOTTE ISLANDS                         
# 5E                \_5E           PSMFC          -       52 00' N TO 54 40' N & WEST OF QUEEN CHARLOTTE ISLANDS                      
# 5A-5D             \_5F           PSMFC          -       AREAS 5A THRU 5D COMBINED                                                   
# UNKN-DFO          \_5U           PSMFC          -       UNKNOWN CANADIAN AREA                                                       
# 62                \_62           Shrimp         -       52 10' N (EAST OF CHARLOTTE) & 52 00' N (WEST OF CHARLOTTE) TO 54 40' N     
# 64                \_64           Shrimp         -       50 30' N TO 52 00' N (W. OF C. ST.JAMES) & TO 52 10' N (E. OF C. ST.JAMES)  
# GRGIA STRT     \_GS              INPFC          -       GEORGIA STRAIT                                                              
# 4B                \_4B           PSMFC          -       GEORGIA STRAIT                                                              
# 68                \_68           Shrimp         -       GEORGIA STRAIT                                                              
# VNCVR-BC       \_VC              INPFC          -       47 30' N TO 50 30' N; CANADIAN CATCH ONLY                                   
# 3D                \_3D           PSMFC          -       49 00' N TO 50 30' N EXCLUDING 49 00' TO 200(T.)                            
# 3C-N              \_3N           PSMFC          -       NORTHERN PORTION OF AREA 3C (CANADIAN ONLY)   

CAN_areas <- c(
  "CHARLOTTE", "5A", "5B", "5C", "5D", "5E", "5A-5D", "UNKN-DFO", 
  "62", "64", "GRGIA STRT", "4B", "68", "VNCVR-BC", "3D", "3C-N"
)


# summarize total catch by area
catch.pacfin |> dplyr::group_by(PACFIN_CATCH_AREA_NAME) |> 
  dplyr::summarize(sum = sum(LANDED_WEIGHT_MTONS))

# only three areas (CHARLOTTE, VNCVR-BC, and 4A) have catches > 5 mt and 4A is Puget Sound

# fewer than 3 vessels by year / area
catch.pacfin |> 
  dplyr::filter(PACFIN_CATCH_AREA_NAME %in% c("CHARLOTTE", "VNCVR-BC")) |>
  dplyr::group_by(PACFIN_CATCH_AREA_NAME, LANDING_YEAR) |> 
  dplyr::summarize(count=dplyr::n_distinct(VESSEL_ID)) |> 
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
catch.pacfin |> 
  dplyr::filter(PACFIN_CATCH_AREA_NAME %in% c("CHARLOTTE", "VNCVR-BC")) |>
  dplyr::group_by(LANDING_YEAR) |> 
  dplyr::summarize(count=dplyr::n_distinct(VESSEL_ID)) |> 
  dplyr::filter(count<3)
# # A tibble: 3 x 2
#   LANDING_YEAR count
#          <int> <int>
# 1         1981     2
# 2         1983     1
# 3         2002     2

library(ggplot2)
catch.pacfin |> 
  dplyr::filter(PACFIN_CATCH_AREA_NAME %in% c("CHARLOTTE", "VNCVR-BC")) |>
  ggplot(aes(factor(LANDING_YEAR))) + 
  geom_bar(aes(weight = LANDED_WEIGHT_MTONS, fill = PACFIN_CATCH_AREA_NAME), position = "dodge")

dat <- catch.pacfin |> 
  dplyr::filter(PACFIN_CATCH_AREA_NAME %in% CAN_areas) |> # include only Canadian areas
  dplyr::filter(!LANDING_YEAR %in% c(1981, 1983, 2002)) |> # exclude years with too few vessels
  dplyr::group_by(LANDING_YEAR) |> 
  dplyr::summarize(sum = round(sum(LANDED_WEIGHT_MTONS), digits = 1)) |>
  dplyr::rename(year = LANDING_YEAR, catch_mt = sum)

writeLines(con = "data-raw/foreign/Canadian_catch_landed_in_Washington.csv", 
  text = c(
    "# Canadian catch of Petrale Sole from PacFIN",
    "# https://pacfin.psmfc.org/",
    "# Data have been aggregated across areas within Canada",
    "# and data from 1981 1983 and 2002 have been excluded due to fewer than 3 vessels represented.",
    "# Compiled by Ian Taylor <ian.taylor@noaa.gov> using the R script",
    "# https://github.com/pfmc-assessments/petrale/blob/main/Rscripts/process_data_for_Canada.R",
    "# raw data extracted by Kelli Johnson on 27 Feb 2023 contains confidential information.",
    ""
  )
)
write.table(
  dat, 
  "data-raw/foreign/Canadian_catch_landed_in_Washington.csv", 
  row.names = FALSE, 
  sep = ",",
  append = TRUE
)


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



# looking deeper at vessel info
x <- catch.pacfin |> 
  dplyr::filter(PACFIN_CATCH_AREA_NAME %in% c("CHARLOTTE", "VNCVR-BC"))
