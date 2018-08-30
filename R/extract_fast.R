# Useful functions --------------------------------------------------------
#' @title Extract Raster Values Given Cell Numbers
#' @description Extracts the raster values of all cells intersecting with a spatial
#' object (polygon) sp given by \code{cell_numbers} function and applies R function fun.
#' @param grid raster object
#' @param cells list with raster cell numbers for
#' each polygon given by \code{cell_numbers} function
#' @param fun An R function
#' @param na.rm logical, argument for fun
#'
#' @return a vector with mean areal values of input data for each subbasin
#' @export
#' @note This function was adopted from Cesar Aybar (aybar1994@gmail.com)
#'
#'
extract_fast <- function(grid, cells, fun = mean, na.rm = T){
  matrix_R <- t(raster::as.matrix(grid))
  res <- sapply(1:length(cells), function(i){
    Value <- matrix_R[cells[[i]]]
    fun(Value, na.rm = na.rm)})
  return(res)
}
