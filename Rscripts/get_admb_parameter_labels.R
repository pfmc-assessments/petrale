# read model output
if (FALSE) {
  model <- r4ss::SS_output(...)
}

# read std file created by ADMB
stdfile <- read.table(file.path(model$inputs$dir, "ss.std"), header = TRUE)

# combine parameters and derived quantities from Report.sso
report_info <- 
  rbind(
    model$parameters %>% 
      dplyr::filter(!is.na(Active_Cnt)) %>%
      dplyr::select(Label, Value, Parm_StDev) %>%
      dplyr::rename(std.dev = Parm_StDev),
    
    model$derived_quants %>% 
      dplyr::select(Label, Value, StdDev) %>%
      dplyr::rename(std.dev = StdDev)
  )

# confirm that the values match
# absolute relative error works for most values but not forecast recruitments which are 0
value_match1 <- abs(stdfile$value - report_info$Value) / stdfile$value < 0.0001
std_match1 <- abs(stdfile$std.dev - report_info$std.dev) / stdfile$std.dev < 0.0001
# exact matches works for the zero values
value_match2 <- stdfile$value == report_info$Value

# confirm that all values and std estimates match (should both be TRUE)
all(value_match1 | value_match2)
all(std_match1)

data.frame(admb_label = stdfile$name, SS3label = report_info$Label)
