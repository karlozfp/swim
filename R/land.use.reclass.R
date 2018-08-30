#' @title Land Use Reclassification - SWIM Hydrotope Table
#' @description
#' This function reclassifies the land use values by SWIM land use IDs in the
#' hydrotope table, the \code{'*.str'} file
#' @param file1 character, path of land use table for reclassification
#' @param file2 character, path of \code{'*.str'} file
#'
#' @return
#' @export
#' @note The land use table should contain the \code{raster} and \code{swim} fields
#'
#' @examples
#'  \dontrun{
#' file1 <- "C:/grassdata/huancane/huan1/huan1_s/input/huan.str"
#' file2 <- "C:/PIK_2018/SWIM_PROJECT/Land_use/land_use_table_ESAC_2_SWIM.csv"
#' land.use.reclass(file1, file2)
#' }
#'

land.use.reclass <- function(file1, file2) {

  # Funtions to open, write and close a file --------------------------------

  open_file <- function (file=file[i]) {
    hrufile <- file(file,"r+")
    text <- readLines(hrufile)
    close(hrufile)
    return (text)
  }

  close_file <- function (file=file[i], text=text){
    hrufile <- file(file,"w+")
    writeLines(text,hrufile)
    close(hrufile)
  }


  # Open and read the files -------------------------------------------------

  land.recla <- read.csv(file2, header = T,stringsAsFactors = F)
  #land.recla <- t(sapply(strsplit(land.recla[[1]], "="),as.numeric))

  text <- open_file(file1)

  # land use column from file1
  land <- c()
  for (i in 1:(length(text)-2)) {
    land[i] <- as.numeric(substr(text[i+1],start = 15 ,stop = 29 ))
  }

  land.types <- unique(land)

  # soil reclasification
  if (!all(land.types %in% land.recla[,'raster'])) {
    print("These land uses should be defined: ")
    print(as.character(land.types[!land.types[] %in% land.recla[,'raster'] ]))
    stop()
  }else{
    land.recla <- land.recla[land.recla[,'raster'] %in% land.types, ]# subset
    for (i in 1:nrow(land.recla)) land[ land == land.recla[i,'raster'] ] <- land.recla[i,'swim']
  }

  # it rewrites the file1

  for (i in 1:(length(text)-2)) {
    text[i+1] <- paste0(substr(text[i+1],start = 1 ,stop = 14 ),
                        sprintf("%15.0f", land[i]),
                        substr(text[i+1],start = 30 ,stop = nchar(text[i+1]) ) )
  }

  close_file(file1, text=text)

}


