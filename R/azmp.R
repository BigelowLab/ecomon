#' Read AZMP correction data example
#' 
#' @export
#' @param filename str the name of the file to read
#' @return table of example data
read_azmp_example = function(filename = "dattax_Cfin_IV-VI.csv.gz"){
  filename = system.file(file.path("model", filename[1]),
                         package = "ecomon")
  stopifnot(file.exists(filename))
  readr::read_csv(filename, show_col_types = FALSE)
}


#' Read an AZMP correction model
#' 
#' @export
#' @param filename str, one of 'Cfin_CIV_CVI' or 'Cfin_CV_CVI'
#' @return [mgcv::gamm()] model
read_azmp_model = function(filename = c('Cfin_CIV_CVI', 'Cfin_CV_CVI')[1]){
  filename = paste0(filename[1], 'gamms3_4_remove01.rds')
  filename = system.file(file.path("model", filename),
                         package = "ecomon")
                         
  stopifnot(file.exists(filename))
  readRDS(filename)
}

#' Given staged ecomon data correct to allow merging with AZMP data
#' 
#' @export
#' @param x data frame of staged data
#' @param model gam model from the mgcv package
#' @param stages str, the stages to combine.  "all" to combine all
#' @param max_depth numbers, depths above this must be manages as per [handle_deep]
#' @param handle_deep str, one of 'drop', 'clip' or 'skip' (to skip managing deeper stations)
#'   `drop` will drop stations above `max_depth`, `clip` will clip stations to `max_depth` 
#'   and any other value, like skip, will skip this drop/clip step.
#' @param sorochan logical, if TRUE then perform filtering that includes
#'   stations where the tow depth is less that `max_percent_z` of station depth,
#'   AND excludes stations that are within `min_above_bottom`
#'   AND have a minimum station depth of at least `min_station_depth`
#' @param max_percent_z num maximum percent of tow depth relative to station depth
#' @param min_above_bottom num, meters of tow depth above station depth
#' @param min_station_depth num, meters of minimum station depth
#' @return data frame input including computed abundance estimates 
correct_azmp = function(x = read_staged(species = "calfin", form = "tibble"),
                        model = read_azmp_model(filename = 'Cfin_CIV_CVI'),
                        stages = c("c4_m2", "c5_m2", "c6_m2"),
                        max_depth = 500,
                        handle_deep = c("drop", "clip")[1],
                        sorochan = TRUE,
                        max_percent_z = 95,
                        min_above_bottom = 15,
                        min_station_depth = 40){
  
  if(FALSE){
    x = read_staged(species = "calfin", form = "tibble")
    x = scale_ecomon()
    model = read_azmp_model(filename = 'Cfin_CIV_CVI')
    stages = c("c4_m2", "c5_m2", "c6_m2")
    max_depth = 500
    handle_deep = c("drop", "clip")[1]
    sorochan = TRUE
    max_percent_z = 95
    min_above_bottom = 15
    min_station_depth = 40
  }
  if (!requireNamespace("mgcv", quietly = TRUE)){
    stop("please install the mgcv R package before using this function")
  }
  
  is_stages = all(stages %in% colnames(x))
  if (!all(stages %in% colnames(x))){
    
  } else {
  
  x = x |>
    dplyr::mutate(Month = as.numeric(format(date, format="%m")),
                  fMonth = as.factor(Month), 
                  percZ_stn = (tow_depth/sta_depth))
  }

  # handle deep locations: drop, clip or continue with deep stations
  if(tolower(handle_deep) == "drop"){
    x = x |>
      dplyr::filter(.data$sta_depth < max_depth)
  } else if(tolower(handle_deep) == "clip") {
    x = x |>
      dplyr::mutate(sta_depth = ifelse(.data$sta_depth > max_depth, max_depth, .data$sta_depth)) 
  }
  

  
  # sochoran flags - which are to be corrected
  if (sorochan){
    # here Caroline multiplies abudnances per m2 by 10.  Must find out why
    # but [Omi doesn't](https://github.com/BigelowLab/predicting_NARW/blob/5edf1f1b0d825518cd094f3663c3055de53f1d54/datasets/vertical_correction/correct_ecomon_data.R#L39C3-L42C77)
    MULTIPLIER = 1
    x = x |>
      dplyr::mutate(.corrected = percZ_stn < max_percent_z/100 & 
                          (sta_depth - tow_depth) > min_above_bottom & 
                          sta_depth > min_station_depth,
                    .ind_m2 = rowSums(dplyr::pick(dplyr::contains(stages)), na.rm = TRUE) * MULTIPLIER)
    
  }
  
  # for clarity, these are the records to correct, trimmed to the columns required
  # by the model.  `ID` is excluded by `predict` but it has to be there anyway
  # so we make one up
  y = x |>
    dplyr::filter(.data$.corrected) |>
    dplyr::mutate(ID = paste(.data$cruise_name, .data$station, sep = "_")) |>
    dplyr::select(dplyr::all_of(c("percZ_stn", "sta_depth", "ID", "fMonth"))) |>
    dplyr::rename(Zstation = "sta_depth") 
  
  # here we apply the correction
  x = x |>  
    mutate(.predicted_pcum = ifelse(.data$.corrected, 
                                    suppressWarnings(predict(model, y, 
                                            type="response", 
                                            exclude ='s(ID)', 
                                            newdata.guaranteed = TRUE)), 
                                    1),
           .corrected_ind_m2 = .ind_m2/.predicted_pcum)
  
  return(x)
  
}