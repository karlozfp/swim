# Useful functions --------------------------------------------------------
#' @title Raster Cell Numbers
#' @description Extracts the raster cell numbers of all cells intersecting with a spatial
#' object (polygon) sp
#' @param grid RasterLayer object
#' @param geom polygon shapefile
#' @return list with raster cell numbers for each polygon
#' @export
#' @note This function was adopted from Cesar Aybar (aybar1994@gmail.com)
#'
#'
cell_numbers <- function(grid, geom){
  specialcov <- grid
  specialcov[] <- 1:raster::ncell(grid)
  Position_rowcol <- function(i){
    quad1 <- unlist(raster::extract(specialcov, geom[i,], small=T))
  }
  position <- lapply(1:length(geom), Position_rowcol)
  return(position)
}
