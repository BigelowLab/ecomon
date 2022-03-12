#' Given a table of observations as biomass as computed in Sorochan et al 2019
#' 
#' @export
#' @param x tibble or sf, staged data from ecomon
#' @param region character, the region for application - see \code{\link[Sorochan]}
#' @param params tibble, parameters as per \code{\link{Sorochan}}
#' @return the input with an added column called "biomass"
add_biomass <- function(x = read_staged(form = "sf"),
                        region = c("GoM-GBK, SS", "All regions", "GSL, NLS", "southwest GSL")[1],
                        params = Sorochan(taxa = "C. finmarchicus")){
  if (FALSE){
    x = read_staged(form = "sf")
    region = c("GoM-GBK, SS", "All regions", "GSL, NLS", "southwest GSL")[1]
    params = Sorochan(taxa = "C. finmarchicus")
  }
  params <- dplyr::filter(params, .data$applied %in% region)
  
  biomass <- x$c4_10m2 * params$CIV[1] + 
             x$c5_10m2 * params$CV[1] + 
             x$c6_10m2 * params$CVI[1]
  dplyr::mutate(x, biomass = biomass, .before= "geometry")
}