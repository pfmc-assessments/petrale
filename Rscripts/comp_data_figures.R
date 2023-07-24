# read any model that contains the updated WCGBTS comp data
get_mod(34, 1)

npages <- 1 # work-around for bug in r4ss
# plot length comps from WCGBTS using cexZ1 to make bubbles bigger
SSplotComps(mod.34.1, subplots = 2, fleets = 3:4, 
  datonly = TRUE, bub = TRUE, cexZ1 = 8, 
  print = TRUE, plotdir = file.path(mod.34.1$inputs$dir, "custom_plots"), plot = FALSE)
# plot age comps
SSplotComps(mod.34.1, subplots = 2, kind = "GSTAGE", fleets = 3:4, 
  datonly = TRUE, bub = TRUE, cexZ1 = 8,
  print = TRUE, plotdir = file.path(mod.34.1$inputs$dir, "custom_plots"), plot = FALSE)



# plot length comps from WCGBTS using cexZ1 to make bubbles bigger
SSplotComps(mod.34.1, subplots = 2, fleets = 3, 
  datonly = TRUE, bub = TRUE, cexZ1 = 8, 
  print = TRUE, plotdir = "figures", plot = FALSE)
