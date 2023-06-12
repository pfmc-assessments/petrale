# read model with and without 2004 triennial survey
mod1 <- r4ss::SS_output("models/2019.001.001_base/", printstats = FALSE, verbose = FALSE)
mod2 <- r4ss::SS_output("models/2019.001.015_no_2004_tri/", printstats = FALSE, verbose = FALSE)

# info on survey fit
mod1$cpue %>%
  dplyr::filter(Yr == 2004, Fleet_name == "TriLate") %>%
  dplyr::select(Obs:Like)
#       Obs     Exp   Calc_Q    Eff_Q       SE  SE_input      Dev    Like
# 1 10521.2 5650.65 0.653522 0.653522 0.393267 0.0800975 0.621623 1.24925

# info without observation showing change in Q
mod2$cpue %>%
  dplyr::filter(Yr == 2004, Fleet_name == "TriLate") %>%
  dplyr::select(Obs:Like)
#       Obs     Exp   Calc_Q    Eff_Q       SE Dev Like
# 1 10521.2 4635.22 0.536088 0.536088 0.194036  NA   NA

# difference in current depletion
mod1$current_depletion
# [1] 0.3874229
mod2$current_depletion
# [1] 0.3868508
mod2$current_depletion - mod1$current_depletion
# [1] -0.0005721009


# plot comparison of index fits
SSplotComparisons(
  SSsummarize(list(mod1, mod2)),
  subplots = 13,
  indexPlotEach = TRUE,
  print = TRUE,
  plotdir = mod2$inputs$dir
)
