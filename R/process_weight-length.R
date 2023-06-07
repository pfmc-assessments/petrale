# get Petrale Sole weight-length relationship from WCGBTS

# # pull data from data warehouse
# bio <- PullBio.fn(Name = "petrale sole",
#                   SurveyName = "NWFSC.Combo",
#                   SaveFile = TRUE, Dir = mydir)

# load previously saved WCGBTS data
load("data-raw/nwfscSurvey/bio_petrale sole_NWFSC.Combo_2023-03-27.rdata")
bio <- x # rename saved object
counts <- bio %>% 
  dplyr::filter(!is.na(Weight_kg) & !is.na(Length_cm)) %>% 
  dplyr::group_by(Sex) %>%
  dplyr::count()
#   Sex       n      
#   <chr> <int>
# 1 F     12504
# 2 M      9173
# 3 U        27

# get percentages
round(100 * counts$n/sum(counts$n), 1)
# [1] 57.6 42.3  0.1

# estimate parameters
WLpars <- PacFIN.Utilities::getWLpars(bio, verbose = TRUE)
# Calculating the weight-length relationship from 21704
# fish because 54061 fish did not have empirical weights and lengths.

print(WLpars)
#    group median_intercept        SD            A        B
# 1 female     2.023350e-06 0.1067301 2.034908e-06 3.478401
# 2   male     3.025817e-06 0.1076196 3.043390e-06 3.359385
# 3    all     2.237759e-06 0.1079522 2.250836e-06 3.449051

# separate parameters into individual values
fa <- WLpars %>%
  dplyr::filter(group == "female") %>%
  dplyr::select(A) %>%
  as.numeric()
fb <- WLpars %>%
  dplyr::filter(group == "female") %>%
  dplyr::select(B) %>%
  as.numeric()
ma <- WLpars %>%
  dplyr::filter(group == "male") %>%
  dplyr::select(A) %>%
  as.numeric()
mb <- WLpars %>%
  dplyr::filter(group == "male") %>%
  dplyr::select(B) %>%
  as.numeric()
ua <- WLpars %>%
  dplyr::filter(group == "all") %>%
  dplyr::select(A) %>%
  as.numeric()
ub <- WLpars %>%
  dplyr::filter(group == "all") %>%
  dplyr::select(B) %>%
  as.numeric()

# values used in 2019 (and maybe previously)
fa_2019 <- 1.99e-06
fb_2019 <- 3.484
ma_2019 <- 2.98e-06
mb_2019 <- 3.363
ua_2019 <- (fa + ma) / 2
ub_2019 <- (fb + mb) / 2

# compare values across assessments 
# (plot not saved but results are almost identical)
x <- 12:62
plot(x, fa_2019 * x^fb_2019,
  type = "l", col = 2, , lwd = 1,
  xlab = "Length", ylab = "Weight"
)
lines(x, ma_2019 * x^mb_2019, type = "l", col = 4, lwd = 1)
lines(x, ua_2019 * x^ub_2019, type = "l", col = 3, lwd = 1)
lines(x, fa * x^fb, type = "l", col = 2, lwd = 3, lty = 2)
lines(x, ma * x^mb, type = "l", col = 4, lwd = 3, lty = 2)
lines(x, ua * x^ub, type = "l", col = 3, lwd = 3, lty = 2)

# save values for use in other scripts
save(WLpars, fa, fb, ma, mb, ua, ub, file = "data/weight-length_pars.rda")

