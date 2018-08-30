
# SWIM INPUTS -------------------------------------------------------------

# open_file <- function (file=file[i]) {
#   hrufile <- file(file,"r+")
#   text <- readLines(hrufile)
#   close(hrufile)
#   return (text)
# }
#
# close_file <- function (file=file[i],text=text){
#   hrufile <- file(file,"w+")
#   writeLines(text,hrufile)
#   close(hrufile)
# }
#
# file_txt <- list.files("C:/PIK_2018/SWIM_PROJECT/input_swim/txt", full.names = T, recursive = T)
#
# input_txt <- lapply(file_txt, open_file)
# names(input_txt) <- basename(file_txt)
#
#
# file_csv <- list.files("C:/PIK_2018/SWIM_PROJECT/input_swim/csv", full.names = T, recursive = T)
# input_csv <- lapply(file_csv, read.csv, check.names = F)
# names(input_csv) <- basename(file_csv)
#
# usethis::use_data(input_txt, input_csv)


