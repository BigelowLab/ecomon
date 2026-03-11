#' Given ecomon data, extract all copernicus layers

suppressPackageStartupMessages({
  library(ecopmo)
  library(ecomon)
  library(dplyr)
  library(andreas)
  library(stars)
  library(sf)
})

cat("loading data\n")
x = ecomon::scale_ecomon() |>
  tidytable::as_tidytable() |>
  tidytable::arrange(date) |>
  sf::st_as_sf(coords = c("lon", "lat"), crs = 4326)
  
# save these for subsequent use
xy = sf::st_coordinates(x)

# we filter the two DBs to just the dates we need 
udates = unique(x$date)

# we use tidytables to leverage high speed filtering
cat("loading PHYDB\n")
PHYPATH = andreas::copernicus_path("chfc")
PHYDB = andreas::read_database(PHYPATH, multiple = TRUE) |>
  mutate(.name = paste(.data$name, .data$depth, sep = "_")) |>
  tidytable::as_tidytable() |>
  tidytable::filter(period == "day", date %in% udates)

cat("loading BGCDB\n")
BGCPATH = andreas::copernicus_path("world")
BGCDB = andreas::read_database(BGCPATH, multiple = TRUE) |>
  mutate(.name = paste(.data$name, .data$depth, sep = "_"))  |>
  tidytable::as_tidytable() |>
  tidytable::filter(period == "day", date %in% udates)

# create a dummy to help use fill in with NAs when there is the stray chance of 
# a date mismatch
START = min(PHYDB$date)
phydb = tidytable::filter(PHYDB, date == START) |>
  dplyr::as_tibble()
bgcdb = tidytable::filter(BGCDB, date == START) |>
  dplyr::as_tibble()
DUMMY = sapply(c(phydb$.name, bgcdb$.name), function(n) NA_real_) |>
  as.list() |>
  dplyr::as_tibble()


  # to mark loop prgress
as_year = function(x = Sys.Date()){
  format(x, "%Y") |> as.numeric()
}
YEAR = as_year(START - 360)

cat("loading extraction loops\n")
x = x |>
  dplyr::group_by(date) |>
  dplyr::group_map(
    function(grp, key){
      year = as_year(grp$date[1])
      if (year > YEAR){
        cat("starting new year", year, "\n")
        YEAR <<- YEAR + 1
      }
      if (grp$date[1] < START) return(bind_cols(grp, DUMMY))
      phydb = tidytable::filter(PHYDB, date == grp$date[1]) |>
        dplyr::as_tibble()
      phy = try(andreas::read_andreas(phydb, PHYPATH))
      if (inherits(phy, 'try-error')){
        cat("issue with PHY on", format(grp$date[1], '%Y-%m-%d'), "\n")
        return(bind_cols(grp, DUMMY))
      }
      phy = phy |>
        extract_points(grp, form = "wide") |>
        dplyr::select(-1) |>
        rlang::set_names(phydb$.name)
      
      bgcdb = tidytable::filter(BGCDB, date == grp$date[1]) |>
        dplyr::as_tibble()
      bgc = try(andreas::read_andreas(bgcdb, BGCPATH))
      if (inherits(bgc, 'try-error')){
        cat("issue with BGC on", format(grp$date[1], '%Y-%m-%d'), "\n")
        return(bind_cols(grp, DUMMY))
      }
      
      bgc = bgc |>
        extract_points(grp, form = "wide")|>
        dplyr::select(-1) |>
        rlang::set_names(bgcdb$.name)
      bind_cols(grp, phy, bgc)
    }, .keep = TRUE
  ) |>
  dplyr::bind_rows()

  cat("extracting static layers\n")
  static = andreas::read_static(
      path = file.path(PHYPATH, "GLOBAL_MULTIYEAR_PHY_001_030")) |>
    extract_points(x, form = "wide") |>
    select(-1)
 
  cat("computing daylength\n")   
  doy = format(x$date, "%j") |> as.numeric()
  dynamic = dplyr::tibble(
    daylength = get_day_length(doy, xy[,2]),
    ddx_day_length = get_ddx_day_length(doy, xy[,2])) 
  
  x = dplyr::bind_cols(x, static, dynamic)
  
  cat("computing daylength\n")
  x = sf::st_drop_geometry(x) |>
    mutate(lon = xy[,1], lat = xy[,2], .after = 1) |>
    readr::write_csv(root_data_path("_general_data", "ecomon", "ecomon_scaled-copernicus.csv.gz"))
  
