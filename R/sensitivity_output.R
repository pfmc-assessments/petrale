# functions related to sensitivity output processing adapted from
# https://github.com/pfmc-assessments/lingcod/blob/main/R/sensitivity_output.R
# which in turn was adapted from
# https://github.com/iantaylor-NOAA/BigSkate_Doc/blob/master/R/BigSkate_sensitivity_results.R

#' create a nicer label for model output tables
#'
#' adds a new column of english language labels
#' for parameters and quantities of interest
#' @param tab a table with a `Label` column containing parameter labels
#' @author Ian G. Taylor
#' @export
sens_clean_labels <- function(tab){
  newlabel <- tab$Label
  newlabel <- gsub("like", "likelihood (diff from base)", newlabel)
  newlabel <- gsub("wt", "weight", newlabel)
  newlabel <- gsub("Ret", "Retained", newlabel)
  newlabel <- gsub("p_1_", "", newlabel)
  newlabel <- gsub("GP_1", "", newlabel)
  newlabel <- gsub("SR_LN", "log", newlabel)
  newlabel <- gsub("SR_BH_steep", "Stock-recruit steepness (h)", newlabel)
  newlabel <- gsub("LnQ_base_WCGBTS(4)", "WCGBTS catchability", newlabel, 
    fixed = TRUE)
  newlabel <- gsub("LnQ_base_Triennial(3)", "Triennial catchability", newlabel, 
    fixed = TRUE)  
  newlabel <- gsub("NatM_uniform", "M", newlabel)
  newlabel <- gsub("Fem", "Female", newlabel)
  newlabel <- gsub("Mal", "Male", newlabel)
  newlabel <- gsub("Recr_Virgin", "Recruitment unfished", newlabel)
  newlabel <- gsub("SSB_Virgin", "B0", newlabel)
  newlabel <- gsub("SSB_2023", "B2023", newlabel)
  newlabel <- gsub("Bratio", "Fraction unfished", newlabel)
  newlabel <- gsub("OFLCatch", "OFL mt", newlabel)
  newlabel <- gsub("MSY", "MSY mt", newlabel)
  newlabel <- gsub("SPRratio", "Fishing intensity", newlabel)
  newlabel <- gsub("_", " ", newlabel)
  # not generalized, depends on fecundity parameters and summary biomass age
  newlabel <- gsub("SmryBio unfished", "Unfished age 3+ bio 1000 mt", newlabel) 
  #newlabel <- gsub("thousand", "1000", newlabel)
  newlabel <- gsub("B0", "B0 trillions of eggs", newlabel)
  newlabel <- gsub("B2023", "B2023 trillions of eggs", newlabel)
  tab$Label <- newlabel
  return(tab)
}

#' Convert offset parameters
#'
#' Convert log Q to standard space
#' and summary biomass into 1000s of mt
#' @param tab a table with a `Label` column containing parameter labels
sens_convert_vals <- function(tab){
  tab[grep("LnQ", tab$Label), -1] <-
    exp(tab[grep("LnQ", tab$Label), -1])
  tab[grep("SmryBio_unfished", tab$Label), -1] <-
    tab[grep("SmryBio_unfished", tab$Label), -1]/1e3
#   tab[grep("SSB", tab$Label), -1] <-
#     tab[grep("SSB", tab$Label), -1]*1e3

  # round values in mt to nearest ton
  tab[grep("Catch", tab$Label), -1] <-
    round(tab[grep("Catch", tab$Label), -1])
  # round values in mt to nearest ton
  tab[grep("OFL", tab$Label), -1] <-
    round(tab[grep("OFL", tab$Label), -1])

  return(tab)
  
}  

#' convert parameter offsets to standard space
#'
#' (not needed for lingcod since our male growth is modeled
#' as independent parameters)
#' @param tab a table with a `Label` column containing parameter labels
sens_convert_offsets <- function(tab){
  # male M offset
  tab[grep("NatM_p_1_Mal", tab$Label), -1] <-
    tab[grep("NatM_p_1_Fem", tab$Label), -1] *
    exp(tab[grep("NatM_p_1_Mal", tab$Label), -1])
  # male Linf offset
  # subset models for those that use male offset
  mods <- which(tab[grep("Linf_Mal", tab$Label), -1] < 0)
  tab[grep("Linf_Mal", tab$Label), 1 + mods] <-
    tab[grep("Linf_Fem", tab$Label), 1 + mods] *
    exp(tab[grep("Linf_Mal", tab$Label), 1 + mods])
  return(tab)
}  

#' Make a table of sensitivity results
#'
#' Works with [run_sensitivities()] and the info in
#' /inst/extdata/sensitivities.csv to make a table of results
#' 
#' @param area Which area "n" or "s"
#' @template num
#' @param sens_base Sensitivity number associated with the base model
#' or source for the other sensitivities.
#' @param sens_nums A vector of values from /inst/extdata/sensitivities.csv
#' @param sens_mods Optional list containing output from multiple models. If present
#' this will skip the reading of the models specified by the inputs above.
#' @param sens_names Optional vector of names for each model to use as headers in the table
#' and legend in the figure (needs to also include the base model)
#' @param sens_type A string indicating the type which is appended to the csv
#' file containing the sensitivity results
#' @param plot Logical for whether to make a two-panel time series plot
#' @param plot_dir Directory for the plot
#' @param legendloc Legend location in plot (as used by `r4ss::SSplotComparisons()`)
#' @param table_dir Directory for the table
#' @param write Logical to write csv file to doc or not
#' @param dots Additional arguments passed to plot_twopanel_comparison()
#'
#' @example 
#' @author Ian G. Taylor
#' @export

sens_make_table <- function(#area,
                            num = NULL,
                            sens_base = 1,
                            yr = 2023,
                            #sens_nums = NULL,
                            sens_mods = NULL,
                            sens_names = NULL,
                            sens_type = NULL,
                            plot = TRUE,
                            plot_dir = NULL,
                            legendloc = "bottomleft",
                            table_dir = "tables",
                            write = FALSE,
                            ...) {

  # which things to read from the model output
  thingnames <- c("Recr_Virgin", "steep", "NatM", "Linf",
                  "SmryBio_unfished",
                  "SSB_Virg", "SSB_2023",
                  "Bratio_2023", "SPRratio_2022", "Ret_Catch_MSY", "Dead_Catch_MSY",
                  "OFLCatch_2023",
                  "LnQ_base_WCGBTS")
  # likelihoods to include
  likenames = c("TOTAL", "Survey", "Length_comp", "Age_comp", 
                "Discard", "Mean_body_wt", "Recruitment", "priors")

  # read models (if needed)
  if (!is.null(sens_mods)) {
    # get names for list elements beyond first entry, which is the base
    # sens_dirs <- purrr::modify_depth(
    #   sens_mods[-1],
    #   1,
    #   ~ .[["inputs"]][["dir"]]
    # ) %>%
    #   purrr::as_vector()
    basedir <- basename(sens_mods[[1]]$inputs$dir)
  } else {
    stop("Need to provide the list: 'sens_mods'")
  }
#   } else {
#     # get base model directory (may not always match info_basemodels)
#     basedir <- basename(get_dir_ling(area, num, sens = sens_base, yr = yr))

#     # sens dirs doesn't include the base
#     sens_dirs <- basedir %>%
#       get_id_ling() %>%
#       stringr::str_sub(end = nchar("2023.a001.")) %>%
#       paste0(., sprintf("%03d", sens_nums)) %>%
#       get_dir_ling(id = .)

#     # filter for directories which are present
#     message("the following directories not found for this model:\n",
#             paste(sens_dirs[!dir.exists(sens_dirs)], collapse = "\n"))
#     sens_dirs <- sens_dirs[dir.exists(sens_dirs)]
    
#     # read the model output
#     # sens_mods includes the base (re-read here because that's easy)
#     sens_mods <-
#       r4ss::SSgetoutput(dirvec = c(file.path("models",basedir),
#                                    sens_dirs),
#                         getcovar = FALSE)
#   }
  
  # summarize the results
  sens_summary <- r4ss::SSsummarize(sens_mods, verbose = FALSE)

  
  # names for the columns in the table
  if (is.null(sens_names)) {
    sens_names <- c("Base model",
                    stringr::str_sub(sens_dirs,
                                     start = 1 + nchar("models/2023.a001.")
                                     )
                    )
  }
  
  # TODO: convert short sens_names to long names by adding a long name
  #       column to sensitivities.csv

  # make plot
  if (plot) {
    plot_filename1 <- paste0("sens_timeseries_", sens_type, "1.png")
    plot_filename2 <- paste0("sens_timeseries_", sens_type, "2.png")
    if (is.null(plot_dir)) {
      plot_dir <- file.path("models",
                            basedir,
                            "custom_plots"
                            )
    }
    plot_twopanel_comparison(mods = sens_mods,
                             legendlabels = sens_names,
                             legendloc = legendloc,
                             legendncol = ifelse(length(sens_mods) < 5, 1, 2),
                             file = plot_filename1,
                             dir = plot_dir,
                             ...
                             )
    plot_twopanel_comparison(mods = sens_mods,
                             legendlabels = sens_names,
                             legendloc = legendloc,
                             legendncol = ifelse(length(sens_mods) < 5, 1, 2),
                             file = plot_filename2,
                             dir = plot_dir,
                             subplot_top = 9, subplot_bottom = 11, 
                             xlim = c(1920, 2023),
                             ...
                             )

    # get an explanation of the type for use in the caption
    sens_type_long <- ""
    if (sens_type == "bio_rec") {
      sens_type_long <- "biology and recruitment."
    }
    if (sens_type == "index") {
      sens_type_long <- "indices of abundance."
    }
    if (sens_type == "comp") {
      sens_type_long <- "composition data."
    }
    
    caption <-
      paste("Time series of spawning biomass (top) and fraction of unfished",
            "(bottom) for the sensitivity analyses related to",
            sens_type_long)
            
    write_custom_plots_csv(mod = sens_mods[[1]],
                           filename = plot_filename1,
                           caption = caption)
    caption <-
      paste("Time series of recruitment (top) and recruitment deviations",
            "(bottom) for the sensitivity analyses related to",
            sens_type_long)
            
    write_custom_plots_csv(mod = sens_mods[[1]],
                           filename = plot_filename2,
                           caption = caption)
  }
  
  # make table of model results
  sens_table <-
    r4ss::SStableComparisons(sens_summary,
                             modelnames = sens_names,
                             names = thingnames,
                             likenames = likenames,
                             csv = FALSE
                             )
  # remove forecast recruitment
  sens_table <- sens_table %>% dplyr::filter(Label != "Forecast_Recruitment_like")

  # convert some things to new units (non-log or non-offset)
  sens_table <- sens_table %>%
    sens_convert_vals() %>%
    #sens_convert_offsets() %>% # not needed here
    sens_clean_labels()

  # convert likelihoods to difference from base (assumed to be in column 2)
  like_rows <- grep("likelihood", sens_table$Label)
  for (icol in ncol(sens_table):2) {
    sens_table[like_rows, icol] <- sens_table[like_rows, icol] - sens_table[like_rows, 2]
  }

#   # convert male offset for the shareM case
#   if ("shareM" %in% names(sens_table)) {
#     sens_table[grep("M Male", sens_table$Label), "shareM"] <-
#       sens_table[grep("M Female", sens_table$Label), "shareM"]
#   }

  # write to file
  if (write) {
    csvfile <- file.path(table_dir,
                         paste0("sens_table_", sens_type, ".csv"))
    message("writing ", csvfile)
    write.csv(sens_table, file = csvfile, row.names = FALSE)
  }

  sens_table
}

# compare 2019 model to 2023
sens_make_table_old_vs_new <- function(
                            sens_mods = NULL,
                            sens_names = NULL,
                            sens_type = "old_vs_new",
                            table_dir = "tables",
                            write = FALSE,
                            ...) {

  # which things to read from the model output
  thingnames <- c("Recr_Virgin", "steep", "NatM", "Linf",
                  "SmryBio_unfished",
                  "SSB_Virg", "SSB_2019", "SSB_2023",
                  "Bratio_2019", "Bratio_2023", 
                  "SPRratio_2018", "SPRratio_2022", 
                  "Ret_Catch_MSY", "Dead_Catch_MSY",
                  "LnQ_base")
  # likelihoods to include
  likenames = NULL

  # summarize the results
  sens_summary <- r4ss::SSsummarize(sens_mods, verbose = FALSE)
  
  # make table of model results
  sens_table <-
    r4ss::SStableComparisons(sens_summary,
                             modelnames = sens_names,
                             names = thingnames,
                             likenames = likenames,
                             csv = FALSE
                             )
  # remove forecast recruitment
  sens_table <- sens_table %>% dplyr::filter(Label != "Forecast_Recruitment_like")

  # convert some things to new units (non-log or non-offset)
  sens_table <- sens_table %>%
    sens_convert_vals() %>%
    #sens_convert_offsets() %>% # not needed here
    sens_clean_labels()

  # write to file
  if (write) {
    csvfile <- file.path(table_dir,
                         paste0("sens_table_", sens_type, ".csv"))
    message("writing ", csvfile)
    write.csv(sens_table, file = csvfile, row.names = FALSE)
  }

  sens_table
}
