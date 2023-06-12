# notes on plotting time series of mean length at age
p2019 <- r4ss::SS_output("models/2019.001.001_base", printstats = FALSE, verbose = FALSE) 

a22a <- r4ss::SS_output("models/2023.a002.002a", printstats = FALSE, verbose = FALSE)
a24 <- r4ss::SS_output("models/2023.a002.004_selex_blocks", printstats = FALSE, verbose = FALSE)

# Annual model with Francis tuning
# north vs south comparison of mean length in retained
png("figures/mean_length_timeseries_2023.a002.002a.png", 
  width = 6.5, height = 3.5, units = "in", res = 250, pointsize = 10)
par(mfrow = c(1, 2), mar = c(2,2,2,1))
SSMethod.TA1.8(a22a, type = "len", fleet = 1, part = 2, set.pars = FALSE)
SSMethod.TA1.8(a22a, type = "len", fleet = 3, part = 2, set.pars = FALSE)
dev.off()

png("figures/mean_length_timeseries_2023.a002.004.png", 
  width = 6.5, height = 3.5, units = "in", res = 250, pointsize = 10)
par(mfrow = c(1, 2), mar = c(2,2,2,1))
SSMethod.TA1.8(a24, type = "len", fleet = 1, part = 2, set.pars = FALSE)
SSMethod.TA1.8(a24, type = "len", fleet = 3, part = 2, set.pars = FALSE)
dev.off()

# Coastwide model with Francis tuning (missing data from north in 1980s messes up fit)
png("figures/mean_length_timeseries_2023.c002.004.png", 
  width = 6.5, height = 3.5, units = "in", res = 250, pointsize = 10)
par(mfrow = c(1, 2), mar = c(2,2,2,1))
SSMethod.TA1.8(c24, type = "len", fleet = 1, part = 2, set.pars = FALSE)
SSMethod.TA1.8(c24, type = "len", fleet = 3, part = 2, set.pars = FALSE)
dev.off()

# 2019 base model (seasonal with Francis)
# north and south comparison of mean length in retained for 
png("figures/mean_length_timeseries_2019base.png", 
  width = 6.5, height = 6.5, units = "in", res = 250, pointsize = 10)
par(mfrow = c(2, 2), mar = c(2,2,2,1))
SSMethod.TA1.8(p2019, type = "len", fleet = 1, part = 2, set.pars = FALSE)
SSMethod.TA1.8(p2019, type = "len", fleet = 2, part = 2, set.pars = FALSE)
SSMethod.TA1.8(p2019, type = "len", fleet = 3, part = 2, set.pars = FALSE)
SSMethod.TA1.8(p2019, type = "len", fleet = 4, part = 2, set.pars = FALSE)
dev.off()
