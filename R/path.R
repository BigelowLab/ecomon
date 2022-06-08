#' Test if a data path config file exists
#' 
#' @export
#' @param filename char, the name of the config files
#' @return logical TRUE if the config file exists
has_data_path <- function(filename = "~/.ecomon"){
  file.exists(filename[1])
}

#' Write the configuration file
#' 
#' @export
#' @param path char, the path to the data - like '/mnt/ecocast/coredata/noaa/nmfs/ecomon'
#' @param filename char, the name of the config files
set_data_path <- function(path, filename = "~/.ecomon"){
  cat(path[1], sep = "\n", file = filename[1])
}

#' Retrieve the user specified data path
#'
#' @export
#' @param ... elements for \code{\link[base]{file.path}}
#' @param root char, the root path directory
#' @return character, the path to the data
get_data_path <- function(...,
                          root = readLines("~/.ecomon")){
  file.path(root, ...)
}

#' Retrieve the path to one or more ecomon datasets
#'
#' @export
#' @param id character, the accession id of the data set ala "0187513"
#' @param version character, defaults to "recent" but could be "1.1", "2.2", etc
#' @param dataset character, one "0-data" (default) or "1-data"
#' @param path character the root path 
#' @return character vector of filenames
list_data <- function(id = "0187513",
                      most_recent = TRUE,
                      version = c("2.2", "recent")[2],
                      dataset = "0-data",
                      path = get_data_path()){
  # /mnt/ecocast/coredata/noaa/nmfs/nefsc/0187513/1.1/data/0-data/EcoMon_Plankton_Data_v3_5.csv                    
  pattern = "^EcoMon_Plankton_Data_.*\\.csv$"
  if (tolower(version[1]) == "recent"){
    p <- file.path(path, id)
  } else {
    p <- file.path(path, id, version[1], "data", dataset)
  }

  ff <- list.files(p,
             pattern = pattern,
             full.names = TRUE,
             recursive = TRUE)
  if (version == "recent") ff <- ff[length(ff)]
  ff
}