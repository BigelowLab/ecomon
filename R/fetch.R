#' Retrieve a NCEI url to a dataset
#' 
#' @export
#' @param id char, a dataset id like '0187513' 
#' @param version character, defaults to "2.2" but could be "1.1"
#' @param base_url char, the root path to NCEI datasets
#' @return chaacter URL
ecomon_url <- function(id = "0187513", 
                       version = "3.3",
                       base_url = file.path("https://www.ncei.noaa.gov/archive",
                                            "archive-management-system/OAS/bin/prd/jquery/download")){
  # https://www.ncei.noaa.gov/archive/archive-management-system/OAS/bin/prd/jquery/download/187513.1.1.tar.gz
  # https://www.ncei.noaa.gov/archive/archive-management-system/OAS/bin/prd/jquery/download/187513.2.2.tar.gz
  # https://www.ncei.noaa.gov/archive/archive-management-system/OAS/bin/prd/jquery/download/187513.3.3.tar.gz
  file.path(base_url, sprintf("%s.%s.tar.gz", id, version))
}

#' Fecth EcoMon data from NCEI
#' 
#' @export
#' @param id char, a dataset id like '0187513' 
#' @param dest character, the path to save the data to
#' @param decompress logical, if TRUE unpack the downloaded file
#' @param cleanup logical, if TRUE remove the downloaded zip file. Ignored unless
#'   \code{decompress} is \code{TRUE}
#' @inheritDotParams ecomon_url -id version base_url
#' @return the result of \code{\link[utils]{download.file}}
fetch_ecomon<- function(id = "0187513",
                          dest = get_data_path(),
                          decompress = TRUE,
                          cleanup = decompress, 
                        ...){
  
  u <- ecomon_url(id = id, ...)
  destfile <- file.path(dest, sprintf("%s.tar.gz", basename(u)))
  ok <- try(download.file(u, destfile, mode = 'wb'))
  if (inherits(ok, 'try-error') || ok > 0){
    warning("unable to download id:", id)
    return(ok)
  }
  if (decompress){
    filelist <- utils::untar(destfile, exdir = dest)
    if (cleanup) unlink(destfile)
  }
  ok
}

