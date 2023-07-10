png("figures/fecundity.png", width = 6.5, height = 5, res = 300, pointsize = 10, units = 'in')
plot(mod_base$biology$Len_mean, mod_base$biology$Wt_F, type = 'l', col = 2, lwd = 3, 
xlim = c(10,70),
  ylim = c(0,4), yaxs = 'i', xaxs = 'i',
xlab = "Length (cm)", ylab = "Fecundity (millions) or weight (kg)")
lines(mod_base$biology$Len_mean, 1000*mod_base$biology$Fec, col = 4, lwd = 3)
lines(mod_base$biology$Len_mean, 0.001*mod_base$natlen[1,paste(seq(2,78,2))]*mod_base$biology$Mat[1:41], col = 3, lwd = 3)
lines(mod_base$biology$Len_mean, 0.001*mod_base$natlen[mod_base$natlen$Seas == 1 & mod_base$natlen$Yr == 2023 & mod_base$natlen$Sex == 1,paste(seq(2,78,2))]*mod_base$biology$Mat[1:41], col = 3, lwd = 3)

legend('topleft', col = c(2,4), legend = c("Weight (kg)", "Fecundity (millions of eggs)"), 
lwd = 3, bty = 'n')
dev.off()

x = mod_base$endgrowth$"Mat*Fecund"[1:41]
y = as.numeric(mod_base$natage[1, paste(0:40)])

# product is in trillions
sum(x*y)
# [1] 22.90647

sum(x*y*1000) # fecundity * numbers in thousands
# 22906 # billions


