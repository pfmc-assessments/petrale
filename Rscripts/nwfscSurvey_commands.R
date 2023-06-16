mydir <- "data-raw/nwfscSurvey"
# dir.create(mydir)

library(nwfscSurvey)
catch <- PullCatch.fn(Name = "petrale sole", 
                      SurveyName = "NWFSC.Combo",
                      SaveFile = TRUE, Dir = mydir)
catch_wcgbts <- nwfscSurvey::pull_catch(
  common = "petrale sole",
  survey = "NWFSC.Combo",
  dir = "data-raw/nwfscSurvey"
)
catch_wcgbts_canary <- nwfscSurvey::pull_catch(
  common = "canary rockfish",
  survey = "NWFSC.Combo",
  dir = "data-raw/nwfscSurvey"
)
catch_tri <- nwfscSurvey::pull_catch(
  common = "petrale sole",
  survey = "Triennial",
  dir = "data-raw/nwfscSurvey"
)
catch_tri_canary <- nwfscSurvey::pull_catch(
  common = "canary rockfish",
  survey = "Triennial",
  dir = "data-raw/nwfscSurvey"
)


bio <- PullBio.fn(Name = "petrale sole", 
                  SurveyName = "NWFSC.Combo",
                  SaveFile = TRUE, Dir = data-raw/nwfscSurvey)
bio <- pull_bio(common_name = "petrale sole", 
                survey = "NWFSC.Combo",
                dir = "data-raw/nwfscSurvey")

bio <- PullBio.fn(Name = "petrale sole", 
                  SurveyName = "Triennial",
                  SaveFile = TRUE, Dir = mydir)
bio <- pull_bio(common_name = "petrale sole", 
                survey = "Triennial",
                dir = "data-raw/nwfscSurvey")

load("data-raw/nwfscSurvey/Catch__NWFSC.Combo_2023-03-06.rda")
load("data-raw/nwfscSurvey/Bio_All_NWFSC.Combo_2023-03-20.rda")
bio <- Data
load("data-raw/nwfscSurvey/bio_petrale sole_NWFSC.Combo_2023-03-27.rdata")
bio2 <- x

# get weight-length relationship
WLpars <- PacFIN.Utilities::getWLpars(bio)

# install.packages("pak")
# pak::pkg_install("pfmc-assessments/nwfscSurvey@Triennial.Canada")
mydir <- "data-raw/Triennial_Canada"
dir.create(mydir)
catch <- PullCatch.fn(Name = "petrale sole", 
                      SurveyName = "Triennial.Canada",
                      SaveFile = TRUE, Dir = mydir)
table(catch$Year)
# 1980 1983 1986 1989 1992 1995 1998 2001
#   48   42    1   65   61   71   60   40

bio <- PullBio.fn(Name = "petrale sole", 
                  SurveyName = "Triennial.Canada",
                  SaveFile = TRUE, Dir = mydir)
table(bio$Year)
# 1989 1992 1998 
#   60   35    2 

plot_cpue(
  dir = mydir, 
  catch = catch)

plot_bio_patterns(
  dir = mydir, 
  bio = bio, 
  col_name = "Length_cm")

wh_plot_proportion(
  data_catch = catch,
  data_bio = bio
)

# maps
PlotMap.fn(
  dat = catch)

nwfscSurvey::PlotMap.fn(data_Tri, 
  dir = "data-raw/nwfscSurvey/triennial/")

load("data-raw/nwfscSurvey/Catch__NWFSC.Combo_2023-03-06.rda")
load("data-raw/nwfscSurvey/Bio_All_NWFSC.Combo_2023-03-20.rda")
catch <- Out
bio <- Data

strata = CreateStrataDF.fn(
  names = c("shallow_s", "mid_s", "shallow_n", "mid_n"), 
  depths.shallow = c( 55,   200,  55, 200),
  depths.deep    = c(200,   400, 200, 400),
  lats.south     = c( 32,    32,  42,  42),
  lats.north     = c( 42,    42,  49,  49))

# Calculate the design based index of abundance:

biomass = Biomass.fn(dir = "data-raw/nwfscSurvey", 
                     dat = catch,  
                     strat.df = strata)

PlotBio.fn(
  dir = "data-raw/nwfscSurvey", 
  dat = biomass)

# Plot the coastwide design-based index of abundance for each strata:
PlotBioStrata.fn(
  dir = "data-raw/nwfscSurvey/strata", 
  dat = biomass)

# tri
strata_tri = CreateStrataDF.fn(
  names = c("shallow_s", "mid_s", "shallow_n", "mid_n"), 
  depths.shallow = c( 55,   200,  55, 200),
  depths.deep    = c(200,   400, 200, 400),
  lats.south     = c( 32,    32,  37,  37),
  lats.north     = c( 37,    37,  49,  49))
biomass = Biomass.fn(dir = "data-raw/nwfscSurvey", 
                     dat = catch_tri,  
                     strat.df = strata_tri)

# exploring location of small fish
plot(-bio$Depth_m, bio$Latitude_dd, xlim = c(-200,0))
points(-bio$Depth_m[bio$Length_cm < 20], 
       bio$Latitude_dd[bio$Length_cm < 20], 
       col = ifelse(bio$Sex[bio$Length_cm < 20] == "F", "red", "blue"))

# expanded comps without rescaling
Length_Freq <- SurveyLFs.fn(dir = getwd(), 
                        datL =  bio, 
                        datTows = catch,
                        strat.df = strata,
                        lgthBins = seq(12,62,2))
Length_Freq2 <- SurveyLFs.fn(dir = getwd(), 
                        datL =  bio, 
                        datTows = catch,
                        strat.df = strata,
                        lgthBins = seq(12,62,2),
                        sum100 = FALSE)
                      
#colvec <- viridis::viridis(26, alpha = 0.7)
source(url("https://gist.githubusercontent.com/jlmelville/be981e2f36485d8ef9616aef60fd52ab/raw/466a6564a86066a9600860cf8058fab5d23e1da5/turbo_colormap.R"))
colvec <- rev(turbo(26))

matplot(Length_Freq2$year, Length_Freq2 %>% dplyr::select(F12:F62), 
  type = 'l', ylim = c(0, 8e6), col = colvec, lty = 1, lwd =3,
  ylab = "Expanded numbers of females by length bin",
  xlab = "Year",
  main = "Petrale sizes observed in WCGBT Survey")
matplot(Length_Freq2$year, Length_Freq2 %>% dplyr::select(F20:F24), 
  type = 'l', col = 1, lty = 3, lwd =1, add = TRUE)
legend('topleft', col = colvec, lwd = 3, legend = seq(12,62,2), ncol = 2)

# exploring sample sizes per tow
par(mfrow = c(2,1))
hist(as.numeric(table(bio$Trawl_id[bio$Year == 2010])), breaks = seq(0,120,5))
hist(as.numeric(table(bio$Trawl_id[bio$Year == 2022])), breaks = seq(0,120,5))

# looking at sex ratio per haul

catch_tri_canary %>% 
  ggplot(aes(x=-Depth_m, y=Latitude_dd, 
    size = cpue_kg_per_ha_der, color = cpue_kg_per_ha_der > 0)) +
    geom_point(alpha=0.3) + 
    scale_size(range = c(.1, 15), name="CPUE (kg/ha)") +
    labs(color = "Species observed",
      title = "Canary in the Triennial") + 
    geom_hline(yintercept = 36+48/60, 
      linetype=3) + 
    geom_vline(xintercept = -366, 
      linetype=3)
ggsave("figures/canary_triennial_depth_vs_lat.png")

catch_tri %>% 
  ggplot(aes(x=-Depth_m, y=Latitude_dd, 
    size = cpue_kg_per_ha_der, color = cpue_kg_per_ha_der > 0)) +
    geom_point(alpha=0.3) + 
    scale_size(range = c(.1, 15), name="CPUE (kg/ha)") +
    labs(color = "Species observed",
      title = "Canary in the Triennial") + 
    geom_hline(yintercept = 36+48/60, 
      linetype=3) + 
    geom_vline(xintercept = -366, 
      linetype=3)
ggsave("figures/triennial_depth_vs_lat.png")

# looking at distribution of triennial
plot(-catch_tri$Depth, catch_tri$Latitude_dd, pch = 21, 
  bg = rgb(1,0,0,.1), 
  col = rgb(1,0,0,.1), 
  cex = 0.2*sqrt(catch_tri$cpue_kg_km2))
abline(v = -366, h = 36+48/60)

plot(-catch_tri_canary$Depth, catch_tri_canary$Latitude_dd, pch = 21, 
  bg = rgb(1,0,0,.1), 
  col = rgb(1,0,0,.1), 
  cex = 0.05*sqrt(catch_tri_canary$cpue_kg_km2))
abline(v = -366, h = 36+48/60)


