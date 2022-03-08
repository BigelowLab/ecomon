#' Given a cruise data frame of staged data with \code{cruise_name}
#' generate a matching subset of the ctd data
#' 
#' @export
#' @param x tibble of staged cruise data
#' @param ctd tibble of ctd data
#' @param form character, the form as a named 'list' (each element one data frame) or 
#'  a 'tibble' where all results are bound in one table
#' @return tibble of ctd data, possibly empty, for matches by cruise 
#'   or a list of the same
staged_match_ctd <- function(x = read_staged() |> 
                        dplyr::filter(.data$cruise_name %in% c('PE9602', "HB1906")),
                      ctd = read_ctd(),
                      form = c("list", "tibble")[1]){
  
  match_one_cruise <- function(tbl, key, ctd = NULL){
    ix <- ctd$CRUISE_ID %in% key$cruise_name[1] 
    if(!any(ix)){
      warning("no match found: ",tbl$cruise_name[1])
      return(dplyr::slice(ctd, 0))
    }
    ctd |> dplyr::filter(ix)
  }
  
  x <- dplyr::group_by(x, .data$cruise_name)
  r <- x |>
    dplyr::group_map(match_one_cruise, ctd = ctd) |>
    rlang::set_names(dplyr::group_data(x)$cruise_name)
  
  if (tolower(form[1]) == "tibble"){
    r <- dplyr::bind_rows(r)
  }
  r
}
