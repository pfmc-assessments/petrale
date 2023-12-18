# Data in "GMT016-final specifications.csv"
# extracted from https://reports.psmfc.org/pacfin/f?p=501:5302:16067970531079:::::,,,,,,
gmt_table <- read.csv("tables/GMT016-final specifications.csv", skip = 1)
head(gmt_table)

table_vi <- data.frame(Year = 2013:2022, OFL = NA, ABC = NA, ACL = NA, 
  Landings = NA, Total_dead = NA)

for (irow in 1:nrow(table_vi)) {
    y <- table_vi$Year[irow]
    table_vi$OFL[irow] <- gmt_table %>% 
      dplyr::filter(SPECIFICATION_TYPE == "OFL" & YEAR == y) %>%
      dplyr::pull(VAL) %>%
      round()
    table_vi$ABC[irow] <- gmt_table %>% 
      dplyr::filter(SPECIFICATION_TYPE == "ABC" & YEAR == y) %>%
      dplyr::pull(VAL) %>%
      round()
    table_vi$ACL[irow] <- gmt_table %>% 
      dplyr::filter(SPECIFICATION_TYPE == "ACL" & YEAR == y) %>%
      dplyr::pull(VAL) %>%
      round()
    table_vi$Landings[irow] <- round(sum(mod_base$catch$ret_bio[mod_base$catch$Yr == y]))
    table_vi$Total_dead[irow] <- round(sum(mod_base$catch$kill_bio[mod_base$catch$Yr == y]))
}
table_vi$"Dead/ACL" <- round(table_vi$Total_dead / table_vi$ACL, 2)
names(table_vi) <- gsub("_", " ", names(table_vi))

write.csv(table_vi, file = "tables/table_vi.csv", row.names = FALSE)
