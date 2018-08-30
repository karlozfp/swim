#' @title WATCH Forcing Data to SWIM
#' @description
#' This function computes the mean areal of climate data for each subbasin.
#' Then, it writes the climate data to SWIM climate files.
#' For these purposes, the WATCH Forcing Data is used.
#'
#' @param ncdf list, pathname of WATCH ncdf files. The following climates data are required: \cr \cr
#' \code{tmx} \cr  Maximum (2m) Air Temperature (K) \cr \cr
#'
#' \code{tmp} \cr  Mean (2m) Air Temperature (K) \cr \cr
#'
#' \code{tmn} \cr  Minimum (2m) Air Temperature (K) \cr \cr
#'
#' \code{rai} \cr  rainfall flux (kg/m2/s) \cr \cr
#'
#' \code{sno} \cr  snowfall flux (kg/m2/s) \cr \cr
#'
#' \code{rhu} \cr  relative humidity (percentage) \cr \cr
#'
#' \code{rad} \cr  surface downwelling shortwave radiation flux (W/m2) \cr \cr
#'
#'
#' @param subbasins SpatialPolygonsDataFrame object of subbasins with projection
#' @param output.path character, path of output directory
#' @param resample logical, if TRUE, the spatial resolution of input files are resampled
#' using the nearest neighbor method to the target resolution
#' then cells value inside of subbasin are averaged to estimate
#' the mean areal of climate data for each subbasin.
#' If FALSE, cells value covering a subbasin are identified and averaged
#' to compute the mean areal of climate data for each subbasin.
#' @param resolution numeric, target resolution for resample process
#' @param scale numeric, the subbasin box is multiplied by scale to mask the input ndcf files for resample process
#' @param date.ini character, initial date of input data in 'YYYY-MM-DD' format
#'
#' @return watch2swim writes 'clim1.dat', 'clim2.dat' files required by SWIM model.
#' Furthermore, each climate data for subbasins is wrote in '*.csv' files,
#' The latter is returned as xts objects in a list.
#' The climate data units are:
#' Minimum (tmn), maximum (tmx) and mean (tmp) temperature in °C,
#' rainfall (rai) in mm/day,
#' snow (sno) in mm/day,
#' relative humidity (rhu) in percentage,
#' radiation (rad) in MJ/d
#' and precipitation (pre) is the sum of rainfal and snow in mm/day.
#'
#' @export
#' @importFrom raster ncell brick raster extract projection extent nlayers crop resample res
#' @importFrom sp spTransform is.projected
#' @importFrom parallel makeCluster detectCores clusterEvalQ clusterExport parLapply stopCluster
#' @examples
#'\dontrun{
#'# open R studio in GRASS
#'# in grass gis cmd write:
#'# rstudio 'Grass Mapset path' &
#' library(rgrass7)
#' library(sp)
#' require(raster)
#' require(rgdal)
#' require(parallel)
#' require(ncdf4)
#' require(rgrass7)
#' library(xts)
#'
#' # List of ncdf files of WATCH forcing data
#' ncdf <- list(
#'  tmx = "C:/PIK_2018/SWIM_PROJECT/1_DATA/2_Wheater/watch/tasmax_watch+wfdei_1981_2013.nc4",
#' tmp = "C:/PIK_2018/SWIM_PROJECT/1_DATA/2_Wheater/watch/tas_watch+wfdei_1981_2013.nc4",
#'  tmn = "C:/PIK_2018/SWIM_PROJECT/1_DATA/2_Wheater/watch/tasmin_watch+wfdei_1981_2013.nc4",
#'  rai = "C:/PIK_2018/SWIM_PROJECT/1_DATA/2_Wheater/watch/pr_gpcc_watch+wfdei_1981_2013.nc4",
#'  sno = "C:/PIK_2018/SWIM_PROJECT/1_DATA/2_Wheater/watch/prsn_gpcc_watch+wfdei_1981_2013.nc4",
#'  rhu = "C:/PIK_2018/SWIM_PROJECT/1_DATA/2_Wheater/watch/hurs_watch+wfdei_1981_2013.nc4",
#'  rad = "C:/PIK_2018/SWIM_PROJECT/1_DATA/2_Wheater/watch/rsds_watch+wfdei_1981_2013.nc4"
#'  # spr = "C:/PIK_2018/SWIM_PROJECT/ncdf_star/ncdf_star/ncdf/ps_watch+wfdei_cut_1981_2013.nc4", # opt
#'  # win = "C:/PIK_2018/SWIM_PROJECT/ncdf_star/ncdf_star/ncdf/wind_watch+wfdei_cut_1981_2013.nc4" # opt
#'  )
#'
#' #read the subbasins vector object from Grass Gis project
#' subbasins <- rgrass7::readVECT("subbasins", plugin=NULL) # or read a polygon shapefile of subbasins
#' plot(subbasins)
#'
#' output.path ="C:/PIK_2018/SWIM_PROJECT/PERU/Grass_peru/Pacific_drainage/Lurin/InOut/input/climate"
#'
#'
#' watch2swim_out <- watch2swim(ncdf = ncdf,
#'                   subbasins = subbasins,
#'                   output.path = output.path,
#'                   resample = TRUE,
#'                   resolution = 0.05,
#'                   scale = 2,
#'                   date.ini = "1981-01-01"
#'                   )
#'
#'}
#'
#'
#'
#'
watch2swim <- function (ncdf,
                        subbasins,
                        output.path = NULL,
                        resample = FALSE,
                        resolution = 0.05,
                        scale = 2,
                        date.ini




){

# check -------------------------------------------------------------------
  cat("Cheking inputs \n")
  cat("...\n")
  cat("...\n")

  stopifnot(is.character(ncdf$tmx))
  stopifnot(is.character(ncdf$tmp))
  stopifnot(is.character(ncdf$tmn))
  stopifnot(is.character(ncdf$rai))
  stopifnot(is.character(ncdf$sno))
  stopifnot(is.character(ncdf$rhu))
  stopifnot(is.character(ncdf$rad))
  #stopifnot(is.character(ncdf$spr)) # currently presure is not used by swim
  #stopifnot(is.character(ncdf$win)) # currently wind is not used by swim

  if(!file.exists(ncdf$tmx)) stop(paste0("It does not exist: '", ncdf$tmx, "' "))
  if(!file.exists(ncdf$tmp)) stop(paste0("It does not exist: '", ncdf$tmp, "' "))
  if(!file.exists(ncdf$tmn)) stop(paste0("It does not exist: '", ncdf$tmn, "' "))
  if(!file.exists(ncdf$rai)) stop(paste0("It does not exist: '", ncdf$rai, "' "))
  if(!file.exists(ncdf$sno)) stop(paste0("It does not exist: '", ncdf$sno, "' "))
  if(!file.exists(ncdf$rhu)) stop(paste0("It does not exist: '", ncdf$rhu, "' "))
  if(!file.exists(ncdf$rad)) stop(paste0("It does not exist: '", ncdf$rad, "' "))
  #if(!file.exists(ncdf$spr)) stop(paste0("It does not exist: '", ncdf$spr, "' ")) # currently presure is not used by swim
  #if(!file.exists(ncdf$win)) stop(paste0("It does not exist: '", ncdf$win, "' ")) # currently wind is not used by swim


  if(!class(subbasins)=="SpatialPolygonsDataFrame") stop("subbasins should be a SpatialPolygonsDataFrame object")
  stopifnot(is.logical(resample))


  if(is.null(output.path)) {
    output.path <- getwd()
  } else {
    stopifnot(is.character(output.path))
    if(!file.exists(output.path)) stop(paste0("It does not exist: '", output.path, "' "))
  }


# Inputs ------------------------------------------------------------------
  cat("Reading inputs \n")
  cat("...\n")
  cat("...\n")

  # ncdf
  ncdfs <- lapply(ncdf, function (x) {
    return(raster::brick(x))
  })
  #plot(ncdfs[[1]][[1]])

  # subbasins shapefile
  subbasins <- subbasins

  if (!is.projected(subbasins)) {
    stop("Error: subbasins does not have projection")
  } else {
    subbasins <- spTransform(subbasins, projection(ncdfs$tmx))
  }

  subbasins <- subbasins[order(subbasins@data$subbasinID), ]# order shapefiles by subbasinID
  #plot(subbasins, add=T)
  #plot(subbasins[1, ], add=T, col="red")

  # Period of data
  date.ini <- as.Date( date.ini)
  dates <- seq.Date(date.ini, length.out = raster::nlayers(ncdfs$tmx), by = "day")#[[1:10]]
  date.fin <- tail(dates, 1)
  cat("Data period is from:" , as.character(date.ini), "to", as.character(date.fin), "\n")
  cat("...\n")
  cat("...\n")


# Mean areal estimation of inputs for each subbasin -----------------------
# Option 1: Accurate mean areal estimation of input for each subbasin -----

  if(resample == TRUE) {

    # Check
    stopifnot(is.numeric(resolution))
    if(resolution < 0) stop("resolution should be greater than 0 !!!")
    if(resolution >  raster::res(ncdfs$tmx[[1]])[1] ) stop("resolution should be lower than ncdf resolution !!!")

    stopifnot(is.numeric(scale))
    if(scale < 1) stop("scale should be greater than 1 !!!")

    # main raster
    grid <- ncdfs$tmx[[1]]
    #plot(grid)

    # crop for basin domain
    grid.crop <- raster::crop(grid, extent(subbasins) * scale)# raster for resampling
    plot(grid.crop, main = "Are the subbasins inside of the raster?")
    plot(subbasins, add=T)

    que <- menu(c("Yes", "No"), title = "See the plot: are the subbasins inside of the raster?")
    if(que == 2) stop("<< define again the scale value >>")

    # mask for resampling using 'ngb' method
    high.res <- raster::raster(raster::extent(grid.crop[[1]]))
    raster::projection(high.res) <- raster::projection(grid)
    raster::res(high.res) <- resolution
    #plot(high.res)
    #plot(subbasins, add=T)
    # Celds within each subbasin
    cell_numbers <- SWIM::cell_numbers(grid = high.res[[1]], geom = subbasins)

    # parallel processing

    cat("Mean areal estimation of inputs for each subbasin \n")
    cat("...\n")
    cat("...\n")

    cl <- parallel::makeCluster(parallel::detectCores()-1)# number of cluster to use
    parallel::clusterEvalQ(cl, c(library(raster), library(SWIM)))# load the package to each node
    parallel::clusterExport(cl,varlist=c("high.res", "grid.crop", "ncdfs", "cell_numbers"), envir=environment())
    mean <-  parallel::parLapply(cl, 1:raster::nlayers(ncdfs$tmx), function(z) {#[[1:10]]
      tmx.r <- raster::resample(ncdfs$tmx[[z]], high.res, method='ngb')
      tmp.r <- raster::resample(ncdfs$tmp[[z]], high.res, method='ngb')
      tmn.r <- raster::resample(ncdfs$tmn[[z]], high.res, method='ngb')
      rai.r <- raster::resample(ncdfs$rai[[z]], high.res, method='ngb')
      sno.r <- raster::resample(ncdfs$sno[[z]], high.res, method='ngb')
      rhu.r <- raster::resample(ncdfs$rhu[[z]], high.res, method='ngb')
      rad.r <- raster::resample(ncdfs$rad[[z]], high.res, method='ngb')
      #spr.r <- raster::resample(ncdfs$spr[[z]], high.res, method='ngb')
      #win.r <- raster::resample(ncdfs$win[[z]], high.res, method='ngb')

      tmx <- SWIM::extract_fast(grid = tmx.r, cell_numbers, fun = mean, na.rm=T) - 273.15  # kelvin to celsius
      tmp <- SWIM::extract_fast(grid = tmp.r, cell_numbers, fun = mean, na.rm=T) - 273.15
      tmn <- SWIM::extract_fast(grid = tmn.r, cell_numbers, fun = mean, na.rm=T) - 273.15
      rai <- SWIM::extract_fast(grid = rai.r, cell_numbers, fun = mean, na.rm=T) * 86400 # kg/m2/s in mm/day
      sno <- SWIM::extract_fast(grid = sno.r, cell_numbers, fun = mean, na.rm=T) * 86400 # kg/m2/s in mm/day
      rhu <- SWIM::extract_fast(grid = rhu.r, cell_numbers, fun = mean, na.rm=T) # in %
      rad <- SWIM::extract_fast(grid = rad.r, cell_numbers, fun = mean, na.rm=T) * 8.64  # W/m2 in J/cm2  #W/m2 in MJ/d
      #spr <- SWIM::extract_fast(grid = spr.r, cell_numbers, fun = mean, na.rm=T) /100 # Pascal in hPA
      #win <- SWIM::extract_fast(grid = win.r, cell_numbers, fun = mean, na.rm=T) # m/s
      pre <- rai + sno # prec = rainfall + snow in mm/day

      result <- round(data.frame(tmx, tmp, tmn, rai, sno,rhu, rad,
                                 #spr, win,
                                 pre, stringsAsFactors = F), 2)
      return(result)
    })

    stopCluster(cl)# Close the cluster

  } else {

# Option 2: Mean areal estimation of inputs for each subbasin -------------
    # Celds within each subbasin
    cat("Mean areal estimation of inputs for each subbasin \n")
    cat("...\n")
    cat("...\n")

    plot(ncdfs$tmx[[1]], main = "Input domain and subbasins")
    plot(subbasins, add=T)

    cell_numbers <- SWIM::cell_numbers(grid = ncdfs$tmx[[1]], geom = subbasins)

    cl <- parallel::makeCluster(detectCores()-1)# number of cluster to use
    clusterEvalQ(cl, c(library(raster), library(SWIM)))# load the package to each node
    clusterExport(cl, varlist=c("ncdfs", "cell_numbers"), envir=environment())
    mean <-  parLapply(cl, 1:raster::nlayers(ncdfs$tmx), function(z) {

      tmx <- SWIM::extract_fast(grid = ncdfs$tmx[[z]], cell_numbers, fun = mean, na.rm=T) - 273.15  # kelvin to celsius
      tmp <- SWIM::extract_fast(grid = ncdfs$tmp[[z]], cell_numbers, fun = mean, na.rm=T) - 273.15
      tmn <- SWIM::extract_fast(grid = ncdfs$tmn[[z]], cell_numbers, fun = mean, na.rm=T) - 273.15
      rai <- SWIM::extract_fast(grid = ncdfs$rai[[z]], cell_numbers, fun = mean, na.rm=T) * 86400 # kg/m2/s in mm/day
      sno <- SWIM::extract_fast(grid = ncdfs$sno[[z]], cell_numbers, fun = mean, na.rm=T) * 86400 # kg/m2/s in mm/day
      rhu <- SWIM::extract_fast(grid = ncdfs$rhu[[z]], cell_numbers, fun = mean, na.rm=T) # in %
      rad <- SWIM::extract_fast(grid = ncdfs$rad[[z]], cell_numbers, fun = mean, na.rm=T) * 8.64  # W/m2 in J/cm2  #W/m2 in MJ/d
      #spr <- SWIM::extract_fast(grid = ncdfs$spr[[z]], cell_numbers, fun = mean, na.rm=T) /100 # Pascal in hPA
      #win <- SWIM::extract_fast(grid = ncdfs$win[[z]], cell_numbers, fun = mean, na.rm=T) # m/s
      pre <- rai + sno # prec = rainfall + snow in mm/day

      result <- round(data.frame(tmx, tmp, tmn, rai, sno, rhu, rad,
                                 #spr, win,
                                 pre, stringsAsFactors = F), 2)
      return(result)
    })

    stopCluster(cl)#Close the cluster

  }


# Variables for each subbasins -------------------------------------------

  vars <- list()
  for (i in 1:base::ncol(mean[[1]])) {
    vars[[i]] <- t(sapply(mean, function (x) {x[,i]}))
  }

  names(vars) <- colnames(mean[[1]])


# Writes the clim1.dat file (rad, rhu, pre) -------------------------------

  clim1 <- list()
  for (i in 1:nrow(subbasins)) {

  clim1[[i]] <- data.frame(formatC(vars$rad[ ,i], width=7, digits=1, format="f"),
                         formatC(vars$rhu[ ,i], width=7, digits=1, format="f"),
                         formatC(vars$pre[ ,i], width=7, digits=1, format="f")
                         )

  colnames(clim1[[i]]) <- c(formatC(paste0("rad", sprintf("%04.0f", i)), width=7),
                          formatC(paste0("rhu", sprintf("%04.0f", i)), width=7),
                          formatC(paste0("pre", sprintf("%04.0f", i)), width=7)
                          )
  }

  clim1 <- do.call(cbind, clim1)

  write.table(clim1, paste(output.path, "clim1.dat", sep = "/"), quote = F, row.names = F, col.names = T, sep = "")

  cat("The 'clim1.dat' file was written to", '\n', output.path, "\n")
  cat("...\n")
  cat("...\n")


# Writes the clim2.dat file (tmn, tmx, tmp) -------------------------------

  clim2 <- list()
  for (i in 1:nrow(subbasins)) {

    clim2[[i]] <- data.frame(formatC(vars$tmn[ ,i], width=7, digits=1, format="f"),
                             formatC(vars$tmx[ ,i], width=7, digits=1, format="f"),
                             formatC(vars$tmp[ ,i], width=7, digits=1, format="f")
    )

    colnames(clim2[[i]]) <- c(formatC(paste0("tmn", sprintf("%04.0f", i)), width=7),
                              formatC(paste0("tmx", sprintf("%04.0f", i)), width=7),
                              formatC(paste0("tmp", sprintf("%04.0f", i)), width=7)
    )
  }

  clim2 <- do.call(cbind, clim2)

  write.table(clim2, paste(output.path, "clim2.dat", sep = "/"), quote = F, row.names = F, col.names = T, sep = "")

  cat("The 'clim2.dat' file was written to", '\n', output.path, "\n")
  cat("...\n")
  cat("...\n")



# Time series of mean areal inputs ----------------------------------------
  #clim1_xts <- xts(apply(clim1, 2, as.numeric), order.by = dates)
  #clim2_xts <- xts(apply(clim2, 2, as.numeric), order.by = dates)

  # Each input for each subbasins

  vars_xts <- lapply(vars, function(x) {
    xts::xts(x, order.by = dates)
  })

  # Writes Each input for each subbasins

  lapply(1:length(vars_xts), function(x) {
    df <- data.frame(dates = zoo::index(vars_xts[[x]]), zoo::coredata(vars_xts[[x]]))
    colnames(df) <- c("dates", as.character(1:base::ncol(vars_xts[[x]])))
    write.table(df,
                paste0(output.path, "/", names(vars_xts[x]),".csv"),
                sep = "," , quote = F, row.names = F, col.names = T)
  })

  cat("'*.csv' files for each climate input for subbasins were written to", '\n', output.path)
  cat("...\n")
  cat("...\n")


 return(vars_xts)


 cat('>> Done! \n', sep = '')

}


