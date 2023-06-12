tows <- data.frame(
  fishyr = rep(2001:2005, 2), 
  stratification = "ALL",
  Trip_Sampled_Lbs = 1:10)
tows$Trip_Sampled_Lbs[1:3] <- NA

tows[, "Sum_Sampled_Lbs"] <- stats::ave(
    x = tows$Trip_Sampled_Lbs,
    # ... are levels to aggregate over
    tows[, "fishyr"], tows[, "stratification"],
    FUN = sum, na.rm = TRUE)

#    fishyr stratification Trip_Sampled_Lbs Sum_Sampled_Lbs
# 1    2001            ALL               NA              NA
# 2    2002            ALL               NA              NA
# 3    2003            ALL               NA              NA
# 4    2004            ALL                4              13
# 5    2005            ALL                5              15
# 6    2001            ALL                6              NA
# 7    2002            ALL                7              NA
# 8    2003            ALL                8              NA
# 9    2004            ALL                9              13
# 10   2005            ALL               10              15

# alternative use of na.rm that works
tows[, "Sum_Sampled_Lbs"] <- stats::ave(
    x = tows$Trip_Sampled_Lbs,
    # ... are levels to aggregate over
    tows[, "fishyr"], tows[, "stratification"],
    FUN = function(x) {sum(x, na.rm = TRUE)} )