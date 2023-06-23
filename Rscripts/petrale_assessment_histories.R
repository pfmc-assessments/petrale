# assessment history for Petrale Sole
# adapted from 2017 Yellowtail Rockfish assessment:
# https://github.com/iantaylor-NOAA/YTRK_doc/blob/master/Rcode/historical_assessment_timeseries.R

# get time series from old models in archive (back to 2005)
if (FALSE) { # only need to load archived models once
  # petrale folder in assessment archive
  archive_dir <- "\\\\nwcfile/FRAM/Assessments/Archives/PetraleSole/"
  
  # get 2005 summary biomass
  repfile_2005 <- file.path(archive_dir, 
    "PetraleSole_2005/PetraleSole_ModelFiles/STAR_SIMPLE3A2NorthFinal.rep")
  lines2005 <- readLines(repfile_2005)
  ts2005 <- read.table(repfile_2005,
    skip = grep("TIME_SERIES", lines2005),
    nrows = grep("SPR_series", lines2005) - grep("TIME_SERIES", lines2005) - 3,
    header = TRUE,
    fill = TRUE)
  
  # get 2009 time series
  repfile_2009 <- file.path(archive_dir, 
    "PetraleSole_2009/ModelFiles_PetraleSole_2009/Assessment/baseMay08/Report.SSO")
  lines2009 <- readLines(repfile_2009)
  ts2009 <- read.table(repfile_2009,
    skip = max(grep("TIME_SERIES", lines2009)),
    nrows = max(grep("SPR_series", lines2009)) - max(grep("TIME_SERIES", lines2009)) - 3,
    header = TRUE,
    fill = TRUE)
  
  # get 2011 time series
  mod2011 <- SS_output(file.path(archive_dir,         
        "PetraleSole_2011/Petrale_model_Files_2011/"))
  ts2011 <- mod2011$timeseries
  
  # get 2013 time series
  mod2013 <- SS_output(file.path(archive_dir,         
        "PetraleSole_2013/PetraleBaseModelFiles_2013/"))
  ts2013 <- mod2013$timeseries
  
  mod2015 <- SS_output(file.path(archive_dir,         
        "PetraleSole_2015_Update/Petrale2015_REALbase"))
  ts2015 <- mod2015$timeseries
  
  get_mod(id = "2019.001.001_base")
  ts2019 <- mod.2019.001.001_base$timeseries
  
  # filter time series
  ts2005 <- ts2005 %>% dplyr::select(year, season, bio.smry, recruit.0) %>%
    dplyr::rename(Yr = year, Seas = season, Bio_smry = bio.smry, Recruit_0 = recruit.0)
  ts2009 <- ts2009 %>% dplyr::select(Yr, Seas, Bio_smry, Recruit_0)
  ts2011 <- ts2011 %>% dplyr::select(Yr, Seas, Bio_smry, Recruit_0)
  ts2013 <- ts2013 %>% dplyr::select(Yr, Seas, Bio_smry, Recruit_0)
  ts2015 <- ts2015 %>% dplyr::select(Yr, Seas, Bio_smry, Recruit_0)
  ts2019 <- ts2019 %>% dplyr::select(Yr, Seas, Bio_smry, Recruit_0)
  
  names(ts2005)[3:4] <- paste(names(ts2005)[3:4], 2005, sep = "_")
  names(ts2009)[3:4] <- paste(names(ts2009)[3:4], 2009, sep = "_")
  names(ts2011)[3:4] <- paste(names(ts2011)[3:4], 2011, sep = "_")
  names(ts2013)[3:4] <- paste(names(ts2013)[3:4], 2013, sep = "_")
  names(ts2015)[3:4] <- paste(names(ts2015)[3:4], 2015, sep = "_")
  names(ts2019)[3:4] <- paste(names(ts2019)[3:4], 2019, sep = "_")
  
  ts2005$Seas[ts2005$Seas == "E"] <- 1
  ts2005$Seas <- as.numeric(ts2005$Seas)

  x = dplyr::full_join(x = ts2005, y = ts2009, by = c("Yr", "Seas")) %>%
    dplyr::full_join(y = ts2011, by = c("Yr", "Seas")) %>%
    dplyr::full_join(y = ts2013, by = c("Yr", "Seas")) %>%
    dplyr::full_join(y = ts2015, by = c("Yr", "Seas")) %>%
    dplyr::full_join(y = ts2019, by = c("Yr", "Seas")) %>%
    dplyr::arrange(Yr, Seas)

    write.csv(x, "tables/petrale_historical_assessment_timeseries.csv", 
      row.names = FALSE)
}

totcatch <- aggregate(mod_base$catch$kill_bio, by=list(mod_base$catch$Yr), FUN=sum)
names(totcatch) <- c("Yr","kill_bio")
png(filename="figures/historical_assessment_timeseries.png",
   width=7, height=5.5, res=300, units='in')
par(mar=c(4,4,1,1))
# compare summary biomass across previous stock assessments
stocks <- read.csv("tables/petrale_historical_assessment_timeseries.csv")
# subset to just columns with summary bio and season 1
stocks2 <- stocks[stocks$Seas == 1, grepl("smry", names(stocks)) | names(stocks) %in% "Yr"]
assess_yrs <- as.numeric(substring(names(stocks2)[-1], 10, 13))
assess_colors <- r4ss::rich.colors.short(length(assess_yrs) + 1)

# empty plot
plot(0, type='n', xlim=c(1905, 2024), ylim=c(0, 55e3),
     axes=FALSE, xaxs='i', yaxs='i', xlab="Year", ylab="Age 3+ biomass (x1000 mt)")
axis(1)
#axis(1, at=2022, label="2022")
axis(2, at=pretty(c(0,50e3)), lab=pretty(c(0,50000))/1000, las=1)
# add lines for the older assessments
for(istock in seq_along(assess_yrs)){
  assess_yr <- sort(unique(assess_yrs))[istock]
  sub <- stocks2$Yr <= assess_yr + 1
  lines(x=stocks2$Yr[sub], y=stocks2[sub, 1+which(assess_yrs==assess_yr)],
          col=assess_colors[1+istock], type='l', lty=1,
          lwd=2)
}

# add current model estimate
lines(mod_base$timeseries[mod_base$timeseries$Yr <= 2023, c("Yr","Bio_smry")],
      col=assess_colors[1], lwd=3)
abline(h=mod_base$timeseries$Bio_smry[1], col=assess_colors[1], lwd=1, lty=3)
text(x=2007, y=mod_base$timeseries$Bio_smry[1], col=1,
     labels="unfished equilibrium\nin base model")
legendnames <- c("Base model",
                 paste(sort(unique(assess_yrs), decreasing=TRUE), "assmt"))
points(x=totcatch$Yr, y=totcatch$kill_bio, type='h', lwd=6, lend=3)
text(x=1950, y=4e3, col=1,
     labels="total catch\nin base model", pos=3)
legend('bottomleft', legend=legendnames,
       col=c(1, rev(assess_colors)), lwd=c(3, rep(2, 7)), bty='n')
box()
dev.off()
