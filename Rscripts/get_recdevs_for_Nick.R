# extract recdevs from base model to send to Nick for updating environmental index

model <- r4ss::SS_output("models//2023.a034.001/")

# subset parameters for initial age structure and recruitment deviations
recdevs <- model$parameters |>
  dplyr::filter(grepl("InitAge", Label) | grepl("RecrDev", Label)) |>
  # select only required columns
  dplyr::select(Label, Value, Parm_StDev)

# add year column
recdevs <- recdevs |>
  dplyr::mutate(
    Year = stringr::str_extract(Label, "[0-9]+"),
    .before = Label
  )
# convert initial age (1 to 31) to year of birth (1876 - 1 = 1875) to (1876 - 31 = 1845)
recdevs$Year[grepl("InitAge", recdevs$Label)] <-
  model$startyr - as.numeric(recdevs$Year[grepl("InitAge", recdevs$Label)])

readr::write_csv(recdevs, file = "ignored/recdevs_2023.csv")
