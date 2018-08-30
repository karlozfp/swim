#' @title Creates a New SWIM Project
#' @description
#' This function creates  a new SWIM Project with base files
#' @param dirname character, directory path where the project will be created
#' @param proj_name character, project name
#'
#' @return
#' @export
#'
#' @examples
#'
#' \dontrun{
#' dirname <- "C:/PIK_2018/SWIM_PROJECT/PERU/Pacific_drainage"
#' proj_name <- "lurin"
#'
#' swim_project(dirname, proj_name)
#' }
#'
#'

swim_project <- function(dirname,
                         proj_name

){
  #current_wd <- getwd()
  #setwd(dirname)
  # Check
  stopifnot(is.character(dirname))
  stopifnot(is.character(proj_name))
  if(!file.exists(dirname)) stop(paste0("It does not exist: '", dirname, "' "))


  # Create project directory
  dir.create(paste(dirname, proj_name, sep = "/"))
  # Create input directories
  dir.create(paste(dirname, proj_name, "input", sep = "/"))
  dir.create(paste(dirname, proj_name, "input", "climate", sep = "/"))
  dir.create(paste(dirname, proj_name, "input", "Soil", sep = "/"))
  dir.create(paste(dirname, proj_name, "input", "Sub", sep = "/"))

  # Create output directories
  dir.create(paste(dirname, proj_name, "output", sep = "/"))
  dir.create(paste(dirname, proj_name, "output", "Flo", sep = "/"))
  dir.create(paste(dirname, proj_name, "output", "GIS", sep = "/"))
  dir.create(paste(dirname, proj_name, "output", "Res", sep = "/"))

  # copy base files to the project directory
  #txt files
  input_txt <- SWIM::input_txt

  names(input_txt) <- ifelse(names(input_txt)=="project.bsn",  paste0(proj_name,".bsn"),
                             ifelse(names(input_txt)=="project.cod", paste0(proj_name,".cod"),
                                    ifelse(names(input_txt)=="project.lut", paste0(proj_name,".lut"), names(input_txt)
                                           )
                                    )
                             )


  for (i in 1:length(input_txt)) {
    file.out = paste(dirname, proj_name, "input",   names(input_txt[i]), sep = "/")
    cat(input_txt[[i]], file = file.out, sep = "\n")

  }

  # csv files

  input_csv <- SWIM::input_csv

  for (i in 1:length(input_csv)) {
    file.out2 = paste(dirname, proj_name, "input",   names(input_csv[i]), sep = "/")
    write.csv(input_csv[[i]], file = file.out2, quote = F, row.names = F, na = "")

  }

}
