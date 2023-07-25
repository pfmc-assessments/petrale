# make plot of catch with more columns in the legend
png("figures/catch_with_better_legend.png",
  res = 300, 
  units = "in",
  width = 6.5, 
  height = 5, # change this to make it taller
  pointsize = 8 # change this to make the text even smaller
  # r4ss default is pointsize = 10
)

fleets_with_catch <- sort(unique(model$catch$Fleet))
legend_labels <- model$FleetNames[fleets_with_catch]
# colors matching what's done in SSplotCatch()
# (removes first color from vector)
legend_colors <- r4ss::rich.colors.short(length(legend_labels) + 1)[-1]
SSplotCatch(model, showlegend = FALSE, subplots = 2)
legend("topleft",
  fill = legend_colors, 
  legend = legend_labels, 
  bty = "n",
  ncol = 2 # number of columns: could change this to 3 if you want
)
dev.off()
