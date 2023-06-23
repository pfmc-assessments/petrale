# 2019 vs 2023 assessments
if (FALSE) {
  get_mod(34, 1)
  get_mod(id = "2019.001.001")
}

old_vs_new_table <- sens_make_table_old_vs_new(
    sens_mods = list(mod.34.1, mod.2019.001.001_base), 
    plot = FALSE, write = FALSE, sens_type = "old_vs_new",
    sens_names = c("Base model", "2019 assessment")
  )

# map catchability parameters
old_vs_new_table[old_vs_new_table$Label == "WCGBTS catchability", 3] <- 
  old_vs_new_table[old_vs_new_table$Label == "LnQ base NWFSC(7)", 3]
old_vs_new_table[old_vs_new_table$Label == "Triennial catchability", 3] <- 
  old_vs_new_table[old_vs_new_table$Label == "LnQ base TriEarly(5)", 3]
old_vs_new_table[old_vs_new_table$Label == "LnQ base TriLate(6)", 2] <- 
  old_vs_new_table[old_vs_new_table$Label == "Triennial catchability", 2]

old_vs_new_table <- old_vs_new_table %>% 
  dplyr::filter(!grepl("eggs", Label) & 
  !grepl("Winter", Label) & 
  !grepl("Summer", Label) &
  !grepl("Early", Label) &
  !grepl("NWFSC", Label)
  )

old_vs_new_table$Label <- gsub("Triennial catchability", "Triennial catchability - early", 
  old_vs_new_table$Label)
old_vs_new_table$Label <- gsub("LnQ base TriLate(6)", "Triennial catchability - late", 
  old_vs_new_table$Label, fixed = TRUE)
old_vs_new_table <- rbind(
    old_vs_new_table %>% dplyr::filter(!grepl("Tri", Label)),
    old_vs_new_table %>% dplyr::filter(grepl("Tri", Label)))

write.csv(old_vs_new_table, file = "tables/sens_table_old_vs_new.csv", row.names = FALSE)

