if(FALSE) {
  get_mod(34, 1)
  get_mod(id = "2019.000.000")
}  

png('figures/discard_comparison.png',
res=300, units='in', pointsize = 10,
width=7, height = 7)

par(mfrow = c(2,2), mar = c(4,4,1,1), oma = c(1,1,2,1))
### discard plots
SSplotCatch(mod.2019.000.000_base_3.30.13, subplots = 7, ymax = 600)
mtext(side = 3, line = 1, "2019 assessment")
box()
SSplotCatch(mod.34.1, subplots = 7, ymax = 600)
mtext(side = 3, line = 1, "Base model")
box()
axis(2)
SSplotCatch(mod.2019.000.000_base_3.30.13, subplots = 8, ymax = 0.4)
SSplotCatch(mod.34.1, subplots = 8, ymax = 0.4)
axis(1)
dev.off()