#' @title Spatial Points of Streamgage
#' @description
#' Creates spatial point object given the point attributes in '*.csv' format.
#' @param file character, filepath of list of stations in '*.csv' format
#' @param x character, field name of longitude
#' @param y character, field name of latitude
#' @param from_crs character, initial coordinate reference system (CRS) by default is
#' World Geodetic System (WGS84) (EPSG:4326)
#' @param to_crs character, target CRS if conversion between projections is needed. Default is NULL.
#'
#' @return
#' @export
#' @importFrom sp coordinates proj4string CRS spTransform
#' @examples
#'
#'  \dontrun{
#'
#' file <- "C:/PIK_2018/SWIM_PROJECT/1_DATA/1_Discharge/Lurin_flow_stations.csv"
#' station <- streamgage(file, x = "Longitude", y = "Latitude",
#'                       from_crs = "+init=epsg:4326",  to_crs = "+init=epsg:32718")
#' plot(station)
#'
#' # open R studio in GRASS to export station as GRASS vector point
#' # in grass gis cmd write:
#' # rstudio 'Grass Mapset path' &
#' library(rgrass7)
#' library(SWIM)
#' file <- "C:/PIK_2018/SWIM_PROJECT/1_DATA/1_Discharge/Lurin_flow_stations.csv"
#' station <- streamgage(file, x = "Longitude", y = "Latitude",
#'                       from_crs = "+init=epsg:4326",  to_crs = "+init=epsg:32718")
#'
#' writeVECT(station, "station", v.in.ogr_flags=c("o", "overwrite"), driver="ESRI Shapefile")
#' writeVECT(station, "station", v.in.ogr_flags=c("o", "overwrite"))
#'
#'# to export as ESRI Shapefile
#'shapefile(station, 'station')# writes to the current working directory
#'
#' }

streamgage <- function(file, x, y, from_crs = "+init=epsg:4326", to_crs = NULL){
  stopifnot(is.character(file))
  if(!file.exists(file)) stop(paste0(file, " does not exist"))
  stopifnot(is.character(x))
  stopifnot(is.character(y))
  stopifnot(is.character(from_crs))

  if(!is.null(to_crs)) stopifnot(is.character(to_crs))

  station <- read.csv(file, check.names = F, stringsAsFactors = F)
  station$x <- station[[x]]
  station$y <- station[[y]]

  sp::coordinates(station) <- ~x + y
  sp::proj4string(station) <- sp::CRS(from_crs)

  if(!is.null(to_crs)) station <- sp::spTransform(station, sp::CRS(to_crs))

  return(station)

}
