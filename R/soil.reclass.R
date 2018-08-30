#' @title Soil Reclassification - SWIM Hydrotope Table
#' @description
#' This function replaces the mapping units of HWSD by SWIM soil ID in the
#' hydrotope table, the \code{'*.str'} file
#' @param file1 character, path of \code{swim_reclassification.dat} file
#' @param file2 character, path of \code{'*.str'} file
#'
#' @return
#' @export
#'
#' @examples
#'  \dontrun{
#' file1 <- "C:/grassdata/huancane/huan1/huan1_s/input/huan.str"
#' file2 <- "C:/PIK_2018/SWIM_PROJECT/hwsd2swim/output/swim_reclassification.dat"
#' soil.reclass(file1, file2)
#' }


soil.reclass <- function(file1, file2) {

  # Funtions to open, write and close a file --------------------------------

  open_file <- function (file=file[i]) {
    hrufile <- file(file,"r+")
    text <- readLines(hrufile)
    close(hrufile)
    return (text)
  }

  close_file <- function (file=file[i],text=text){
    hrufile <- file(file,"w+")
    writeLines(text,hrufile)
    close(hrufile)
  }


  # Open and read the files -------------------------------------------------

  soil.recla <- read.table(file2, header = F, stringsAsFactors = F)
  soil.recla <- t(sapply(strsplit(soil.recla[[1]], "="), as.numeric))

  text <- open_file(file1)

  # soil column from file1
  soil <- c()
  for (i in 1:(length(text)-2)) {
    soil[i] <- as.numeric(substr(text[i+1], start = 30 , stop = 44 ))
  }

  soil.types <- unique(soil)

  # soil reclasification
  if (!all(soil.types %in% soil.recla[,1])) {
    print("These soils from '*.str' file should be defined in swim_reclassification.dat file")
    print(as.character(soil.types[!soil.types[] %in% soil.recla[,1] ]))
    stop()
  }else{
    for (i in 1:nrow(soil.recla)) soil[ soil == soil.recla[i,1] ] <- soil.recla[i,2]
  }

  # it rewrites the file1
  for (i in 1:(length(text)-2)) {
    text[i+1] <- paste0(substr(text[i+1],start = 1 ,stop = 29 ),
                        sprintf("%15.0f", soil[i]),
                        substr(text[i+1],start = 45 ,stop = nchar(text[i+1]) ) )
  }

  close_file(file1, text=text)

}
