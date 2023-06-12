# read any model that contains the updated WCGBTS comp data
mod.2023.007.004 <- SS_output("models/2023.007.004", 
  verbose = FALSE, printstats = FALSE)

# plot length comps from WCGBTS using cexZ1 to make bubbles bigger
SSplotComps(mod.2023.007.004, subplots = 2, fleets = 7, 
  datonly = TRUE, bub = TRUE, cexZ1 = 8, 
  print = TRUE, plotdir = "figures", plot = FALSE)
# plot age comps
SSplotComps(mod.2023.007.004, subplots = 2, kind = "GSTAGE", fleets = 7, 
  datonly = TRUE, bub = TRUE, cexZ1 = 8,
  print = TRUE, plotdir = "figures", plot = FALSE)



# plot length comps from WCGBTS using cexZ1 to make bubbles bigger
SSplotComps(mod.2023.a001.001, subplots = 2, fleets = 3, 
  datonly = TRUE, bub = TRUE, cexZ1 = 8, 
  print = TRUE, plotdir = "figures", plot = FALSE)
