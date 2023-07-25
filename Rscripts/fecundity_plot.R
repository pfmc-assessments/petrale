png("figures/fecundity.png", width = 6.5, height = 5, res = 300, pointsize = 10, units = 'in')
plot(mod_base$biology$Len_mean, mod_base$biology$Wt_F, type = 'l', col = 2, lwd = 3, 
xlim = c(10,70),
  ylim = c(0,4), yaxs = 'i', xaxs = 'i',
xlab = "Length (cm)", ylab = "weight (kg), fecundity (millions), or numbers (millions)")
lines(mod_base$biology$Len_mean, 1000*mod_base$biology$Fec, col = 4, lwd = 3)
natlen_equ <- mod_base$natlen[1, paste(seq(2,78,2))]
natlen2023 <- mod_base$natlen %>% dplyr::filter(Time == 2023.0 & Sex == 1) %>% dplyr::select(paste(seq(2,78,2)))
lines(mod_base$biology$Len_mean, 0.001*natlen_equ*mod_base$biology$Mat[1:41], col = 3, lwd = 3)
lines(mod_base$biology$Len_mean, 0.001*natlen2023*mod_base$biology$Mat[1:41], col = 3, lwd = 3, lty = 3)

legend(x = 10, y = 4, col = c(2,4), legend = c("Weight (kg)", "Fecundity (millions of eggs)"), 
lwd = 3, lty = 1, bty = 'n')
text(10, 3.2, pos = 4, "Estimated numbers (millions) of \nmature females by length bin:")
legend(x = 10, y = 3.1, col = c(3, 3), 
  legend = c(
  "in unfished equilibrium", 
  "at the start of 2023"), 
lwd = 3, lty = c(1,3), bty = 'n')
dev.off()

#x = mod_base$endgrowth$"Mat*Fecund"[1:41]
#y = as.numeric(mod_base$natage[1, paste(0:40)])

# product is in trillions
sum(x*y)
# [1] 22.90647

sum(x*y*1000) # fecundity * numbers in thousands
# 22906 # billions


