load("data-raw/pacfin/PacFIN.PTRL.CompFT.27.Jan.2023.RData")
load("data-raw/pacfin/PacFIN.PTRL.bds.27.Jan.2023.RData")

# looking at differences with new extraction
catch.pacfin_old = catch.pacfin
rm(catch.pacfin)
load("data-raw/pacfin/PacFIN.PTRL.CompFT.12.Jun.2023.RData")
table(catch.pacfin$LANDING_YEAR, 
  catch.pacfin$FISH_TICKET_ID %in% catch.pacfin_old$FISH_TICKET_ID)

x1 = aggregate(catch.pacfin$LANDED_WEIGHT_MTONS, 
  by = list(catch.pacfin$LANDING_YEAR, catch.pacfin$AGENCY_CODE), 
  FUN = sum)
x2 = aggregate(catch.pacfin_old$LANDED_WEIGHT_MTONS, 
  by = list(catch.pacfin_old$LANDING_YEAR, catch.pacfin_old$AGENCY_CODE), 
  FUN = sum)
xdiff <- data.frame(x1, sum_landings_Jan_mt = x2[,3], diff_Jun_minus_Jan = round(x1[,3] - x2[,3], 3))
xdiff %>% 
  dplyr::rename(year = Group.1, agency = Group.2, sum_landings_Jun_mt = x) %>% 
  dplyr::filter(diff_Jun_minus_Jan != 0, year < 2023) %>%
  dplyr::arrange(year)
#   year agency sum_landings_Jun_mt sum_landings_Jan_mt diff_Jun_minus_Jan
# 1 2014      C           622.68543           622.67996              0.005
# 2 2017      C           616.66437           841.16591           -224.502
# 3 2022      C           966.36331           964.23250              2.131
# 4 2022      W            79.89495            80.10133             -0.206

# more detail on mismatched catches from WA
z1 <- catch.pacfin     %>% dplyr::filter(AGENCY_CODE == "W" & LANDING_YEAR == 2022)
z2 <- catch.pacfin_old %>% dplyr::filter(AGENCY_CODE == "W" & LANDING_YEAR == 2022)

for(id in sort(unique(z1$FTID))) {
  vals1 <- z1 %>% 
    dplyr::filter(FTID == id) %>%
    dplyr::select(FTID, LANDED_WEIGHT_MTONS)
  vals2 <- z2 %>% 
    dplyr::filter(FTID == id) %>%
    dplyr::select(FTID, LANDED_WEIGHT_MTONS)
  if(dplyr::all_equal(vals1, vals2) != TRUE) {
    cat("June extraction records with id = ", id, "\n")
    print(vals1)
    cat("January extraction records with id = ", id, "\n")
    print(vals2)
  }
}

# more detail on mismatched catches from CA in 2017
z1 <- catch.pacfin     %>% dplyr::filter(AGENCY_CODE == "C" & LANDING_YEAR == 2017)
z2 <- catch.pacfin_old %>% dplyr::filter(AGENCY_CODE == "C" & LANDING_YEAR == 2017)

for(id in sort(unique(z1$FTID))) {
  vals1 <- z1 %>% 
    dplyr::filter(FTID == id) %>%
    dplyr::select(FTID, LANDED_WEIGHT_MTONS)
  vals2 <- z2 %>% 
    dplyr::filter(FTID == id) %>%
    dplyr::select(FTID, LANDED_WEIGHT_MTONS)
  if(dplyr::all_equal(vals1, vals2) != TRUE) {
    cat("June extraction records with id = ", id, "\n")
    print(vals1)
    cat("January extraction records with id = ", id, "\n")
    print(vals2)
  }
}


library(magrittr)
library(ggplot2)

plot(catch.pacfin$LANDING_DATE, catch.pacfin$LANDED_WEIGHT_MTONS, ylim = c(0, 50))


# catch by month or season
# probably more elegant way to group by month
catch.pacfin$yr.month <- catch.pacfin$LANDING_YEAR + (catch.pacfin$LANDING_MONTH - 1) / 12
test <- dplyr::group_by(catch.pacfin, yr.month) %>%
  dplyr::summarize(sum = sum(LANDED_WEIGHT_MTONS))
# define month and season
test$month <- round(12 * (test$yr.month - floor(test$yr.month))) + 1
test$seas <- ifelse(test$month %in% c(1, 2, 11, 12), "Winter", "Summer")
# make plot
test %>%
  dplyr::filter(yr.month >= 2000 & yr.month < 2023) %>%
  ggplot(aes(x = yr.month, y = sum, fill = seas)) +
  geom_bar(stat = "identity") +
  labs(fill = "Season", x = "Year", y = "Landings (t)") +
  theme(legend.position = c(0.1, 0.9))
# save plot
ggsave("figures/data/pacfin_catch_by_season.png",
  width = 6.5, height = 5, units = "in", scale = 1.0
)

# by agency as well (too messy)
test2 <- catch.pacfin %>%
  dplyr::filter(LANDING_YEAR >= 2000) %>%
  dplyr::group_by(yr.month, AGENCY_CODE) %>%
  dplyr::summarize(sum = sum(LANDED_WEIGHT_MTONS))
test2 %>%
  ggplot(aes(x = yr.month, y = sum, group = AGENCY_CODE, color = AGENCY_CODE)) +
  geom_line()
ggsave("figures/data/pacfin_catch_by_month_and_agency.png",
  width = 6.5, height = 5, units = "in", scale = 1.0
)


# plots of depth using BDS data

# load objects saved by R/process_pacfin_bds.R
load("data-raw/Cleaned_PacFIN.PTRL.bds.3.Mar.2023.Rda")

# violin plot of depth by year is messy (depends on years)
table(Pdata_seas$season)
p <- Pdata_seas %>%
  dplyr::filter(SAMPLE_YEAR %in% 2000:2015, !is.na(DEPTH_AVG)) %>%
  ggplot(aes(factor(SAMPLE_YEAR), DEPTH_AVG))
# p + geom_violin()
p + geom_violin(aes(fill = c("Winter", "Summer")[season]))

# group by decade to get a better plot
Pdata_seas$decade <- 10 * floor(Pdata_seas$SAMPLE_YEAR / 10)
p <- Pdata_seas %>%
  dplyr::filter(SAMPLE_YEAR >= 1970 & SAMPLE_YEAR < 2020, !is.na(DEPTH_AVG)) %>%
  ggplot(aes(factor(decade), DEPTH_AVG))

p + xlab("Decade") + ylab("Depth (m)") +
  geom_violin(aes(fill = c("Winter", "Summer")[season])) +
  labs(fill = "Season") + theme(legend.position = c(0.1, 0.9))
# save plot
ggsave("figures/data/pacfin_bds_depth.png",
  width = 6.5, height = 5, units = "in", scale = 1.0
)


# IFQs for 2022
ifq <- read.csv("data-raw/Shorebased IFQ Sector Balances as of 2023-03-07.csv",
  skip = 1, check.names = FALSE
)
ifq$attainment <- ifq$"Catch To Date (C)" /
  (ifq$"Sector Quota Pounds (A)" +
    ifq$"Carryover Quota Pounds (B)") %>%
    round(3)
ifq <- ifq[order(ifq$attainment, decreasing = TRUE), ]
ifq$catch_mt <- 0.000453592 * ifq$"Catch To Date (C)"
ifq$group <- ifq$"IFQ Species"
# ifq$group <- gsub("Â° ", "°", ifq$group)
bad <- substring(ifq$group[1], 23, 24)
ifq$group <- gsub(bad, substring(bad, 2, 2), ifq$group)
# ifq$highlight <- ifelse(ifq$group == " Petrale sole", 1, 2)
ifq$highlight <- ifq$group != " Petrale sole"

ifq %>%
  filter(group != " Pacific whiting") %>%
  ggplot(aes(
    x = attainment,
    y = fct_rev(fct_infreq(group)),
    # y = group,
    fill = highlight
  )) +
  geom_col(show.legend = FALSE) +
  labs(y = "", x = "Attainment (fraction of quota caught)")
ggsave("figures/attainment_2022.png")
ifq %>%
  filter(group != " Pacific whiting") %>%
  ggplot(aes(
    x = catch_mt,
    y = fct_rev(fct_infreq(group)),
    # y = group,
    fill = highlight
  )) +
  geom_col(show.legend = FALSE) +
  labs(y = "", x = "Catch (t)")
ggsave("figures/catch_2022.png")


calcom <- read.csv(file.path(
  "C:/SS/Petrale/Petrale2019/archive_files/PetraleSole_2019_Update/",
  "6_data/fishery_comps/PetraleCALCOM_Query2011.csv"
))
calcom <- calcom %>%
  dplyr::mutate(date = lubridate::as_date(SAMPLE_DATE, format = "%m/%d/%Y")) %>%
  dplyr::mutate(year = lubridate::year(date))

# new pull from 1990+ period by EJ on 9 March 2023
calcom2 <- read.csv("data-raw/calcom/CA_Petrale_lengths_from_CALCOM_MASTER_FISH_TABLE.csv")

tab <- data.frame(Year = 1948:2022, calcom_2011 = 0, pacfin_2019 = 0, pacfin_2023 = 0, calcom_2023 = 0)
for (irow in 1:nrow(tab)) {
  y <- tab$Year[irow]
  tab$calcom_2011[irow] <- sum(calcom$year == y)
  tab$pacfin_2019[irow] <- PacFIN.PTRL.bds.26.Jun.2019 %>%
    dplyr::filter(SAMPLE_YEAR == y, SAMPLE_AGENCY == "CA") %>%
    nrow()
  tab$pacfin_2023[irow] <- bds.pacfin %>%
    dplyr::filter(SAMPLE_YEAR == y, AGENCY_CODE == "C") %>%
    nrow()
  tab$calcom_2023[irow] <- sum(calcom2$YEAR == y)
}

bds.pacfin %>%
  dplyr::filter(SAMPLE_YEAR >= 2020, AGENCY_CODE == "O", !is.na(FINAL_FISH_AGE_IN_YEARS)) %>%
  select(SAMPLE_YEAR, SAMPLE_MONTH) %>%
  table()
