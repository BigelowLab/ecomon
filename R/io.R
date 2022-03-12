#' Retrieve a named vector of column names and types for ecomon
#'
#' @export
#' @param var_names character, identifies variable names to keep, all other marked to skip
#' @return listing of column type definitions
ecomon_cols <- function(var_names = character()){
    x = c(cruise_name = "c", station = "n", zoo_gear = "c", ich_gear = "c", 
          lat = "n", lon = "n", date = "c", time = "t", depth = "n", sfc_temp = "n", 
          sfc_salt = "n", btm_temp = "n", btm_salt = "n", volume_1m2 = "n", 
          ctyp_10m2 = "n", calfin_10m2 = "n", pseudo_10m2 = "n", penilia_10m2 = "n", 
          tlong_10m2 = "n", cham_10m2 = "n", echino_10m2 = "n", larvaceans_10m2 = "n", 
          para_10m2 = "n", gas_10m2 = "n", acarspp_10m2 = "n", mlucens_10m2 = "n", 
          evadnespp_10m2 = "n", salps_10m2 = "n", oithspp_10m2 = "n", cirr_10m2 = "n", 
          chaeto_10m2 = "n", hyper_10m2 = "n", gam_10m2 = "n", evadnord_10m2 = "n", 
          calminor_10m2 = "n", copepoda_10m2 = "n", clauso_10m2 = "n", 
          dec_10m2 = "n", euph_10m2 = "n", prot_10m2 = "n", acarlong_10m2 = "n", 
          euc_10m2 = "n", pel_10m2 = "n", poly_10m2 = "n", podon_10m2 = "n", 
          fish_10m2 = "n", bry_10m2 = "n", fur_10m2 = "n", calspp_10m2 = "n", 
          oncaea_10m2 = "n", cory_10m2 = "n", ost_10m2 = "n", tstyl_10m2 = "n", 
          oithspin_10m2 = "n", mysids_10m2 = "n", temspp_10m2 = "n", tort_10m2 = "n", 
          paraspp_10m2 = "n", scyphz_10m2 = "n", anthz_10m2 = "n", siph_10m2 = "n", 
          hydrom_10m2 = "n", coel_10m2 = "n", ctenop_10m2 = "n", euph1_10m2 = "n", 
          thysin_10m2 = "n", megan_10m2 = "n", thysra_10m2 = "n", thyslo_10m2 = "n", 
          eupham_10m2 = "n", euphkr_10m2 = "n", euphspp_10m2 = "n", thysgr_10m2 = "n", 
          nemaspp_10m2 = "n", stylspp_10m2 = "n", stylel_10m2 = "n", nemame_10m2 = "n", 
          thysspp_10m2 = "n", shysac_10m2 = "n", thypsp_10m2 = "n", nemabo_10m2 = "n", 
          thecos_10m2 = "n", spirre_10m2 = "n", spirhe_10m2 = "n", spirin_10m2 = "n", 
          spirtr_10m2 = "n", spirspp_10m2 = "n", clispp_10m2 = "n", crevir_10m2 = "n", 
          diatri_10m2 = "n", clicus_10m2 = "n", clipyr_10m2 = "n", cavunc_10m2 = "n", 
          cavinf_10m2 = "n", cavlon_10m2 = "n", stysub_10m2 = "n", spirbu_10m2 = "n", 
          crespp_10m2 = "n", cavspp_10m2 = "n", cavoli_10m2x = "n", gymnos_10m2 = "n", 
          pnespp_10m2 = "n", paedol_10m2 = "n", clilim_10m2 = "n", pnepau_10m2 = "n", 
          volume_100m3 = "n", ctyp_100m3 = "n", calfin_100m3 = "n", pseudo_100m3 = "n", 
          penilia_100m3 = "n", tlong_100m3 = "n", cham_100m3 = "n", echino_100m3 = "n", 
          larvaceans_100m3 = "n", para_100m3 = "n", gas_100m3 = "n", acarspp_100m3 = "n", 
          mlucens_100m3 = "n", evadnespp_100m3 = "n", salps_100m3 = "n", 
          oithspp_100m3 = "n", cirr_100m3 = "n", chaeto_100m3 = "n", hyper_100m3 = "n", 
          gam_100m3 = "n", evadnord_100m3 = "n", calminor_100m3 = "n", 
          copepoda_100m3 = "n", clauso = "n", dec_100m3 = "n", euph_100m3 = "n", 
          prot_100m3 = "n", acarlong_100m3 = "n", euc_100m3 = "n", pel_100m3 = "n", 
          poly_100m3 = "n", podon_100m3 = "n", fish_100m3 = "n", bry_100m3 = "n", 
          fur_100m3 = "n", calspp_100m3 = "n", oncaea_100m3 = "n", cory_100m3 = "n", 
          ost_100m3 = "n", tstyl_100m3 = "n", oithspin_100m3 = "n", mysids_100m3 = "n", 
          temspp_100m3 = "n", tort_100m3 = "n", paraspp_100m3 = "n", scyphz_100m3 = "n", 
          anthz_100m3 = "n", siph_100m3 = "n", hydrom_100m3 = "n", coel_100m3 = "n", 
          ctenop_100m3 = "n", euph1_100m3 = "n", thysin_100m3 = "n", megan_100m3 = "n", 
          thysra_100m3 = "n", thyslo_100m3 = "n", eupham_100m3 = "n", euphkr_100m3 = "n", 
          euphspp_100m3 = "n", thysgr_100m3 = "n", nemaspp_100m3 = "n", 
          stylspp_100m3 = "n", stylel_100m3 = "n", nemame_100m3 = "n", 
          thysspp_100m3 = "n", shysac_100m3 = "n", thypsp_100m3 = "n", 
          nemabo_100m3 = "n", thecos_100m3 = "n", spirre_100m3 = "n", spirhe_100m3 = "n", 
          spirin_100m3 = "n", spirtr_100m3 = "n", spirspp_100m3 = "n", 
          clispp_100m3 = "n", crevir_100m3 = "n", diatri_100m3 = "n", clicus_100m3 = "n", 
          clipyr_100m3 = "n", cavunc_100m3 = "n", cavinf_100m3 = "n", cavlon_100m3 = "n", 
          stysub_100m3 = "n", spirbu_100m3 = "n", crespp_100m3 = "n", cavspp_100m3 = "n", 
          cavoli_100m3x = "n", gymnos_100m3 = "n", pnespp_100m3 = "n", 
          paedol_100m3 = "n", clilim_100m3 = "n", pnepau_100m3 = "n", nofish_10m2 = "n", 
          bretyr_10m2 = "n", cluhar_10m2 = "n", cycspp_10m2 = "n", diaspp_10m2 = "n", 
          cermad_10m2 = "n", benspp_10m2 = "n", urospp_10m2 = "n", enccim_10m2 = "n", 
          gadmor_10m2 = "n", melaeg_10m2 = "n", polvir_10m2 = "n", meralb_10m2 = "n", 
          merbil_10m2 = "n", centstr_10m2 = "n", pomsal_10m2 = "n", cynreg_10m2 = "n", 
          leixan_10m2 = "n", menspp_10m2 = "n", micund_10m2 = "n", tauads_10m2 = "n", 
          tauoni_10m2 = "n", auxspp_10m2 = "n", scosco_10m2 = "n", pepspp_10m2 = "n", 
          sebspp_10m2 = "n", prispp_10m2 = "n", myoaen_10m2 = "n", myooct_10m2 = "n", 
          ammspp_10m2 = "n", phogun_10m2 = "n", ulvsub_10m2 = "n", anaspp_10m2 = "n", 
          citarc_10m2 = "n", etrspp_10m2 = "n", syaspp_10m2 = "n", botspp_10m2 = "n", 
          hipobl_10m2 = "n", parden_10m2 = "n", pseame_10m2 = "n", hippla_10m2 = "n", 
          limfer_10m2 = "n", glycyn_10m2 = "n", scoaqu_10m2 = "n", sypspp_10m2 = "n", 
          lopame_10m2 = "n", nofish_100m3 = "n", bretyr_100m3 = "n", cluhar_100m3 = "n", 
          cycspp_100m3 = "n", diaspp_100m3 = "n", cermad_100m3 = "n", benspp_100m3 = "n", 
          urospp_100m3 = "n", enccim_100m3 = "n", gadmor_100m3 = "n", melaeg_100m3 = "n", 
          polvir_100m3 = "n", meralb_100m3 = "n", merbil_100m3 = "n", centstr_100m3 = "n", 
          pomsal_100m3 = "n", cynreg_100m3 = "n", leixan_100m3 = "n", menspp_100m3 = "n", 
          micund_100m3 = "n", tauads_100m3 = "n", tauoni_100m3 = "n", auxspp_100m3 = "n", 
          scosco_100m3 = "n", pepspp_100m3 = "n", sebspp_100m3 = "n", prispp_100m3 = "n", 
          myoaen_100m3 = "n", myooct_100m3 = "n", ammspp_100m3 = "n", phogun_100m3 = "n", 
          ulvsub_100m3 = "n", anaspp_100m3 = "n", citarc_100m3 = "n", etrspp_100m3 = "n", 
          syaspp_100m3 = "n", botspp_100m3 = "n", hipobl_100m3 = "n", parden_100m3 = "n", 
          pseame_100m3 = "n", hippla_100m3 = "n", limfer_100m3 = "n", glycyn_100m3 = "n", 
          scoaqu_100m3 = "n", sypspp_100m3 = "n", lopame_100m3 = "n")
    if (length(var_names) > 0){
      ix <- names(x) %in% var_names
      x[names(x)[!ix]] <- "-"
    }
    
    x
}

#' Read a a single ecomon data file
#'
#' @param filename the name of the file
#' @param skip the number of header lines to skip
#' @param col_types a vector or compact character string used to declare input column types
#' @return tibble
read_ecomon_one <- function(filename, 
                            skip  = 0, 
                            col_types = paste(ecomon_cols(), collapse = "")){
  suppressWarnings(readr::read_csv(filename, 
                                   skip = skip,
                                   col_types = col_types,
                                   show_col_types = FALSE)) 
}

#' Read a ecomon data - trim to bare necessities
#'
#' @export
#' @param filename one or more filenames.  If multiples, the
#'   individual tables are row bound into one.
#' @param simplify logical if TRUE trim down to a simple dataset
#' @param select_vars character, the columns to select if simplyfing.  Ignored  
#'   unless \code{simplify = TRUE}. 
#' @param form character either 'tibble' or 'sf'
#' @return tibble or sf Points object
read_ecomon <- function(filename = list_data(id = "0187513"),
                         simplify = TRUE,
                         select_vars = c("cruise_name",
                                         "station",
                                         "zoo_gear",
                                         "ich_gear",
                                         "lat",
                                         "lon",
                                         "date",
                                         "time",
                                         "depth",
                                         "sfc_temp",
                                         "sfc_salt",
                                         "btm_temp",
                                         "btm_salt",
                                         "volume_1m2"),
                         form = c("tibble", "sf")[1]){
  
  if (simplify){
    cols <- ecomon_cols(select_vars)
  } else {
    cols <- ecomon_cols()
  }
  
  x <- lapply(filename, read_ecomon_one, 
              col_types = paste(cols, collapse = "")) |>
    dplyr::bind_rows() |>
    dplyr::mutate(date = as.Date(.data$date, "%d-%b-%y"))
  

  if (tolower(form[1]) == 'sf'){
    x <- sf::st_as_sf(x, coords = c("lon", "lat"), crs = 4326)
  }
  x
}



#' Read the special file provided to C Ross
#' 
#' @export
#' @param filename file specification
#' @param form character either 'tibble' or 'sf'
#' @return tibble or sf Points object
read_cross <- function(filename = get_data_path("EcoMon_CalfinStage_Thru_12_30_2019_10m2.csv"),
                       form = c("tibble", "sf")[1]){
 
  x <- readr::read_csv(filename, 
    col_types = readr::cols(
      cruise_name = readr::col_character(),
      station = readr::col_double(),
      latitude = readr::col_double(),
      longitude = readr::col_double(),
      date = readr::col_date(format = "%d-%b-%y"),
      sta_depth = readr::col_double(),
      tow_depth = readr::col_double(),
      gear_volume_filtered = readr::col_double(),
      zoo_aliquot = readr::col_double(),
      total_10m2 = readr::col_double(),
      c6_10m2 = readr::col_double(),
      c5_10m2 = readr::col_double(),
      c4_10m2 = readr::col_double(),
      c3_10m2 = readr::col_double(),
      c2_10m2 = readr::col_double(),
      c1_10m2 = readr::col_double(),
      unk_10m2 = readr::col_double()),
    na = c("", "NA", "#DIV/0!"))
  
  if (tolower(form[1]) == 'sf'){
    x <- sf::st_as_sf(x, coords = c("longitude", "latitude"), crs = 4326)
  }
  x
}


#' Given a species, retirve the name of the staged species file
#' 
#' @export
#' @param species char, the species to read
#' @param path char, the path to the species staged data
#' @param must_exist logical, if TRUE test for the existence of the file, throw an
#'   error if not found
#' @return character, file specification
staged_filename <- function(species = "calfin",
                            path = get_data_path("staged"),
                            must_exist = TRUE){
  sp <- tolower(species[1])
  filename <- switch(sp,
    "foo" = "foo.csv",
    { # here we pattern match and then select the most recently modified 
      # file which is a terrible system but works for now
      
      # first get the csv files
      ff <- list.files(path, pattern =  "^.*\\.csv$")
      # now the species files
      ix <- grep(sp, tolower(ff), fixed = TRUE)
      # now get the most recently modified one
      ff <- file.path(path, ff[ix])
      fi <- file.info(ff) |> 
        dplyr::as_tibble(rownames = "filename") |>
        dplyr::arrange(dplyr::desc(.data$mtime))
      filename <- basename(fi$filename[1])
    }) 
  filename <- file.path(path, filename)
  if (must_exist && !file.exists(filename[1])){
      stop("species file not found:", species)
  }
  filename
}

#' Read the staged data provided via 2022-03-03 personal communication (to modeling group)
#' by Harvey Walsh 
#' 
#' @export
#' @param species character, the species to read
#' @param form character either 'tibble' or 'sf'
#' @param ... other arguments for \code{\link{staged_filename}}
#' @return tibble or sf Points object
read_staged <- function(species = "calfin",
                       form = c("tibble", "sf")[1],
                       ...){
  
  filename <- staged_filename(species, ...)
  
  # known to work with calfin
  x <- readr::read_csv(filename, 
                       col_types = readr::cols(
                         seq = readr::col_double(),
                         cruise_name = readr::col_character(),
                         station = readr::col_double(),
                         latitude = readr::col_double(),
                         longitude = readr::col_double(),
                         date = readr::col_date(format = "%d-%b-%y"),
                         sta_depth = readr::col_double(),
                         tow_depth = readr::col_double(),
                         gear_volume_filtered = readr::col_double(),
                         zoo_aliquot = readr::col_double(),
                         total_10m2 = readr::col_double(),
                         c6_10m2 = readr::col_double(),
                         c5_10m2 = readr::col_double(),
                         c4_10m2 = readr::col_double(),
                         c3_10m2 = readr::col_double(),
                         c2_10m2 = readr::col_double(),
                         c1_10m2 = readr::col_double(),
                         unk_10m2 = readr::col_double()),
                       na = c("", "NA", "#DIV/0!"))
  
  if (tolower(form[1]) == 'sf'){
    x <- sf::st_as_sf(x, coords = c("longitude", "latitude"), crs = 4326)
  }
  x
}


#' Read NOAA CTD table provided by personal communication by Chris Melrose to Chris Orphanides 
#'
#' @export
#' @param path character, the path to the data
#' @param simplify logical, if TRUE simply the dataset
#' @param form character, one of 'tibble' or 'sf'
#' @param match_cruise NULL or data frame (tibble), if not \code{NULL}
#'   then filter the output to match the cruise IDs in the match_cruise
#'   object.
#' @return 'tibble' or 'sf' object
read_ctd <- function(path = get_data_path("ctd"),
                     match_cruise = NULL,
                     form = c("tibble", "sf")[1]){
  
  columns <- list(
    CRUISE_ID = readr::col_character(),
    STA = readr::col_character(),
    SITE_ID = readr::col_double(),
    CAST = readr::col_character(),
    GEAR_TYPE = readr::col_character(),
    PURPOSE_CODE = readr::col_double(),
    OPSID = readr::col_character(),
    GMT_DATE = readr::col_date(format = "%d-%b-%y"),
    GMT_TIME = readr::col_double(),
    LAT_DD = readr::col_double(),
    LON_DD = readr::col_double(),
    PRES = readr::col_double(),
    TEMP = readr::col_double(),
    SALT = readr::col_double(),
    O2 = readr::col_double(),
    CHLOROPHYLL = readr::col_double(),
    CHL_FL = readr::col_double(),
    PAR_SENSOR = readr::col_double(),
    STA_BTM_DEPTH = readr::col_double()
  )
  
  filename <- file.path(path, "noaa_ctd.csv.gz")
  
  x <- readr::read_csv(filename,
                       col_names = names(columns),
                       col_types = columns,
                       skip = 1)
  if (!is.null(match_cruise)){
    stopifnot("cruise_name" %in% colnames(match_cruise))
    x <- dplyr::filter(x, .data$CRUISE_ID %in% match_cruise$cruise_name)
  }
  if (tolower(form[1]) == 'sf'){
    x <- sf::st_as_sf(x, coords = c("LON_DD", "LAT_DD"), crs = 4326)
  }
  
  x
}

#' Read data from the Sorochan paper
#' 
#' @export
#' @param what character, the name of the item to read
#' @param taxa character, one or more taxa to filter or "all" for all
#' @return tibble
Sorochan <- function(what = "table_1",
                     taxa = c("all", "C. finmarchicus")[1]){
  filename <- system.file(
    switch(tolower(what[1]),
           "table_1" = "extdata/Sorochan-table_1.csv"),
    package = "ecomon")
  x <- readr::read_csv(filename, show_col_types = FALSE)
  if (!any("all" %in% taxa)){
    x <- dplyr::filter(x, .data$taxon %in% taxa)
  }
  x
}