#' @title wstor.dat file for SWIM model
#' @description
#' This function creates the \code{'wstor.dat'} file for SWIM
#'
#' @param subbasins character, name of boundary vector object of subbasins in
#' Grass Gis project
#' @param output.path character, output directory path
#' @param plot logical, plot the subbasins boundaries if TRUE
#' @return write the \code{'wstor.dat'} file
#' @export
#' @importFrom rgrass7 readVECT
#' @examples
#' \dontrun{
#' # open R studio in GRASS
#' # in grass gis cmd write:
#' # rstudio 'Grass Mapset path' &
#' library(rgrass7)
#' library(sp)
#' wstor.dat(subbasins = "subbasins", output.path = NULL)
#' }
#'
wstor.dat <- function(subbasins = "subbasins", output.path = NULL, plot = TRUE) {

  if (is.null(output.path)) {
    output.path <- getwd()
  } else {
    if(!file.exists(output.path)) stop(paste0("It does not exist: '", output.path, "' "))
  }

  subb <- rgrass7::readVECT(subbasins, plugin=NULL)
  if (plot == TRUE) plot(subb)

  wstor.dat <- data.frame(ID=subb@data$subbasinID, zero=0, ELEV=subb@data$average_elevation)
  wstor.dat <- wstor.dat[order(wstor.dat[,1]), ]
  write.table(wstor.dat, paste(output.path,"wstor.dat", sep = "/"), row.names = F, col.names = F, quote=F)

  cat("Done : wstor.dat was written to: ", "\n", output.path )
}



