#' Retrieve a NCEI url to a dataset
#' 
#' @export
#' @param id char, a dataset id like '0187513' 
#' @param base_url char, the root path to NCEI datasets
#' @return chaacter URL
ecomon_url <- function(id = "0187513", 
                     base_url = "https://www.ncei.noaa.gov/archive/accession/download"){
  
  # https://www.ncei.noaa.gov/archive/accession/download/187513
  file.path(base_url, id)
}

#' Fecth EcoMon data from NCEI
#' 
#' @export
#' @param id char, a dataset id like '0187513' 
#' @param dest character, the path to save the data to
#' @param decompress logical, if TRUE unpack the downloaded file
#' @param cleanup logical, if TRUE remove the downloaded zip file. Ignored unless
#'   \code{decompress} is \code{TRUE}
#' @return the result of \code{\link[utils]{download.file}}
fetch_ecomon<- function(id = "0187513",
                          dest = get_data_path(),
                          decompress = TRUE,
                          cleanup = decompress){
  
  u <- ecomon_url(id = id)
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

