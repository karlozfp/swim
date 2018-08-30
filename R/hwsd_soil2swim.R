
#' @title Harmonized World Soil Database (HWSD) to SWIM
#' @description
#' This function computes the soil hydraulic properties from easily available soil properties
#' using pedotransfer functions (PTFs) for the SWIM model. Then, it writes the soil properties
#' to SWIM soil files. For these purposes, the basin soil mapping units (MU)
#' from HWSD raster map and the database of soil infomation of HWSD are needed. \cr
#'
#' HWSD (FAO/IIASA/ISRIC/ISSCAS/JRC, 2012) has a global coverage, made up of five soil databases.
#' Refer to Nachtergaele et al. (2009) for further information.  \cr
#' Based on sand, silt, clay, reference bulk density and organic carbon soil parameters
#' from HWSD, the other soil properties such as porosity, available water capacity,
#' field capacity, saturated conductivity, soil erodibility factor K(usle) and
#' organic nitrogen (N) content are computed using PTFs for two soil horizons,
#' the topsoil (0-30 cm) and subsoil (30-100 cm).  \cr
#'
#' Erodability factor K(usle) es computed according to
#' Wischmeier and Smith (1962) (reviewed in Romkens (1985)). \cr
#'
#' Currently, there are two sets of PTFs implemented:
#' PTF for European soils according to Wosten et al. (2001) and Schaap  van Genuchten (2006)
#' and PTF for tropical soils according to Minasny and Hartemink (2011).
#'
#'
#' @references
#'
#' Budiman Minasny, Alfred E. Hartemink,
#' Predicting soil properties in the tropics,
#' Earth-Science Reviews,
#' Volume 106, Issues 1–2,
#' 2011,
#' Pages 52-62,
#' https://doi.org/10.1016/j.earscirev.2011.01.005. \cr \cr
#'
#'
#' FAO/IIASA/ISRIC/ISSCAS/JRC, 2012. Harmonized World Soil Database (version 1.2).
#' FAO, Rome, Italy and IIASA, Laxenburg, Austria. \cr  \cr
#'
#' M. Romkens. Soil erodibility factor: A perspective. Soil Erosion and Conservation.
#' Soil Conservation Society of America, Ankeny, Iowa. 1985. p 445-461,  1 fig, 5 tab, 31 ref., 1985.
#'\cr
#'
#' M. G. Schaap and M. T. van Genuchten. A modified mualem-van genuchten formulation for
#' improved description of the hydraulic conductivity near saturation. Vadose Zone Journal, 5(1):
#' 27, 2006. ISSN 1539-1663. doi: 10.2136/vzj2005.0005.
#'\cr
#'
#'
#' Nachtergaele, H. van Velthuizen, and L. Verelst. Harmonized world soil database (version 1.1).
#' Technical report, FAO, 2009.
#'\cr
#'
#' J.H.M. Wösten, Ya.A. Pachepsky, W.J. Rawls,
#' Pedotransfer functions: bridging the gap between available basic soil data and missing soil hydraulic characteristics,
#' Journal of Hydrology,
#' Volume 251, Issues 3–4,
#' 2001,
#' Pages 123-150,
#' https://doi.org/10.1016/S0022-1694(01)00464-4.  \cr
#'
#'
#' W. Wischmeier and D. Smith. Soil-loss estimation as a tool in soil and water management planning.
#' International Association Science Hydrology Comm. on Land Erosion, 59:146-159, 1962.
#'\cr
#'
#' @param input list of arguments: \cr  \cr
#'        \code{hwsd.path} \cr character, location path of the 'HWSD.sqlite' file \cr \cr
#'        \code{hwsd.soil.data} \cr character, table name that contain soil information, default value is 'HWSD_DATA'  \cr \cr
#'        \code{hru.path} \cr character, '*.str' file path (hydrotope table) \cr \cr
#'        \code{hru.soil.col} \cr integer, column number with soil mapping units in '*.str' file (default is 3) \cr \cr
#'        \code{mu.number} \cr vector, soil mapping units if 'hru.path' and 'hru.soil.col' are not specified \cr \cr
#'        \code{ptf} \cr        integer, define the method of pedotransfer function (PTF) to compute the soil hydraulic properties \cr
#'                     1 (2) use PTFs for European (Tropical) soils \cr \cr
#'        \code{layer_100} \cr vector, soil horizons in mm for mapping units with depth soils of 100 cm. \cr
#'                          e.g. c(10, 155, 300, 440, 580, 720, 860, 1000) \cr \cr
#'        \code{str_100} \cr vector, define each soil horizons in topsoil (1: 0-30 cm) or subsoil (2: 30-100 cm) \cr
#'                        e.g. c( 1, 1,  1,  2,  2,  2,  2,  2) \cr \cr
#'        \code{layer_30} \cr vector, soil horizons in mm for mapping units with depth soils of 30 cm. \cr
#'                         e.g. c(10, 150, 300) \cr \cr
#'        \code{str_30} \cr vector, define each soil horizons in topsoil only (1) \cr
#'                        e.g. c( 1,  1,  1) \cr \cr
#'        \code{layer_10} \cr vector, soil horizons in mm for mapping units with depth soils of 10 cm. \cr
#'                         e.g. c(10, 100) \cr \cr
#'        \code{str_10}  \cr vector, define each soil horizons in topsoil only (1) \cr
#'                        e.g. c( 1,  1) \cr \cr
#'        \code{arable} \cr  integer, define the arable horizons \cr \cr
#'        \code{output.path} \cr  character, path of output directory
#'
#' @return soil2swim writes 'non-soil_mappingunits.dat', 'swim_reclassification.dat'
#' and soil files for SWIM model (e.g. 'soil01.dat', 'soil02.dat', ...) in the 'output.path'.
#' Besides, it returns a data frame with the soil hydraulic properties.
#' @export
#' @importFrom dplyr %>% tbl mutate filter select collect
#' @importFrom utils read.table write.table
#' @author Carlos Fernandez, \email{cafpxl@gmail.com}, \email{palomino@pik-potsdam.de}, 2018 (first draft) \cr \cr
#' Initial script development: Stefan Ludtke (sluedtke@gfz-potsdam.de)
#' and Michel Wortmann (wortmann@pik-potsdam.de)
#' @examples
#' library(DBI)
#' library(RSQLite)
#' library(dplyr)
#'
#' input <- list(
#' hwsd.path       = "C:/PIK_2018/SWIM_PROJECT/hwsd2swim/HWSD.sqlite",
#' hwsd.soil.data  = "HWSD_DATA",
#' hru.path        = "C:/grassdata/huancane/huan1/huan1_s/input/huan.str",
#' hru.soil.col    = 3,
#' mu.number       = NULL,
#' ptf             = 2,
#' layer_100       = c(10, 155, 300, 440, 580, 720, 860, 1000),
#' str_100         = c( 1, 1, 1, 2, 2, 2, 2, 2),
#' layer_30        = c(10, 155, 300),
#' str_30          = c( 1, 1, 1),
#' layer_10        = c(10, 100),
#' str_10          = c(1, 1),
#' arable          = 5,
#' output.path = "C:/PIK_2018/SWIM_PROJECT/hwsd2swim/cafp"
#' )
#'
#' soil2swim(input)
#'

soil2swim <- function(input) {

# Pedotransfer Functions (PTFs) -------------------------------------------
  # Organic matter & nitrogen -----------------------------------------------
  # compute organic matter OM based on the organic carbon content (bodenkundliche
  # kartieranleitung 94)
  OM <- function(organcarbon) {return(organcarbon * 1.72)}

  # adding organic nitrogen content - this is just guess and far from the reality
  ON <- function(organcarbon) {return(organcarbon / 15.0)}

  # Field capacity (%) ------------------------------------------------------
  #	Minasny 2011 - PTF for tropical soils
  # with bulk_dens in g/cm3

  FC.MINASNY <- function(OM, sand, bulk_dens)
  {
    # bulk density corrected with organic matter %

    bulk_dens_OM <- 100 / ((OM / 0.224) + ((100 - 0.224) / bulk_dens))

    FC <- (59.9 - 8.78 * bulk_dens_OM - 0.31 * sand)
    return(FC)
  }

  # Wilting point (15bar) (%) -----------------------------------------------

  PWP.MINASNY <- function(org_C, clay)
  {
    # organic carbon org_C
    PWP <- (7.95 + 0.86 * org_C + 0.4 * clay - 0.004 * (clay - 37.7)^2)
    return(PWP)
  }

  # Available water capacity ------------------------------------------------

  AWC <- function(FC, PWP)
  {
    AWC <- (FC - PWP)
    return(AWC)
  }

  SATCOND.WOSTEN <- function(silt, clay, OM, bulk_dens, layer)
  {
    #OM = organic matter
    KSAT <- 7.755 + 0.0352*silt + 0.93*layer - 0.967*(bulk_dens^2) - 0.000484*(clay^2) -
      0.000322*(silt^2) + 0.001*(silt^(-1)) - 0.0748*(OM^-1) - 0.643*log(silt) -
      0.01398*bulk_dens*clay - 0.1673*bulk_dens*OM + 0.02986*layer*clay -
      0.03305*layer*silt

    KSAT <- exp(KSAT)

    # Correcting saturated conductivity to limits
    # SATURATED CONDUCTIVITY LIMITS: (from scheffer98 in cm/day 3*10^4 > ksat > 1*10^-2)
    # is later converted to mm/h for SWIM (assuming it's in cm/day in the database)
    ksat_max <- 3*(10^4)
    ksat_min <- 1*(10^-2)

    KSAT[which(KSAT > ksat_max)] <- ksat_max
    KSAT[which(KSAT < ksat_min)] <- ksat_min


    #transfer into mm/h from cm/d
    KSAT <- KSAT / 2.4
    return(KSAT)
  }


  # Drainage clasification --------------------------------------------------

  GetDrainageClass <- function(Ksat){
    GetDrainageClass <- NULL
    if (Ksat > 7.62) GetDrainageClass = 1
    if (Ksat <= 7.62 & Ksat >  3.81 ) GetDrainageClass = 2
    if (Ksat <= 3.81  & Ksat >  1.27 ) GetDrainageClass = 3
    if (Ksat <= 1.27  ) GetDrainageClass = 4
    return(GetDrainageClass)
  }

  # Porosity ----------------------------------------------------------------
  POR <- function(bulk_dens)
  {
    POR <- 100 - (bulk_dens / 2.65)*100  # 2.65 g/cm?
    return(POR)
  }

  # Soil erodibility factor (wIlliams 1995) ---------------------------------

  KUSLE.WILLIAMS <- function(sand, silt, clay, org_C)
  {
    #compute factors for the main equation
    fcsand <- (0.2 + 0.3*exp(-0.256*sand*(1 - (silt / 100))))

    fclsi <- (silt / (org_C + silt))^0.3

    forgc <- (1 - ((0.25*org_C) / (org_C + exp(3.72 - 2.95*org_C))))

    fhisand <- (1 - ((0.7*(1 - (silt / 100))) / ((1 - (silt / 100)) + exp(-5.51 + 22.9*(1 - (silt / 100))))))

    #main equation

    KUSLE <- fcsand*fclsi*forgc*fhisand

    return(KUSLE)
  }


  # Parameters for Mualem-van Genuchten Model (MVG) - water retention -------

  #	Different parameters estimated by WOSTEN 2001, pp135, to predict
  #	model parameters for the Mualem-van Genuchten equations (van Genuchten
  #	1980) See WOSTEN 1995 about the transformation of the parameters-
  #	alpha, n and ksat are all transfered parameters, denoted as [para]* in
  #	the literature.  We first compute [para]* and transform the parameter
  #	before we give to the return function

  THETA_S.WOSTEN <- function(silt, clay, OM, bulk_dens, layer)
  {
    THETA_S <- 0.7919 + 0.001691 * clay - 0.29619 * bulk_dens - 0.000001491 *
      (silt^2) + 0.0000821 * (OM^2) + 0.02427 * (clay^(-1)) + 0.01113 *
      silt^(-1) + 0.01472 * log(silt) - 0.0000733 * OM * clay - 0.000619 *
      bulk_dens * clay - 0.001183 * bulk_dens * OM - 0.0001664 * layer *
      silt
    return(THETA_S)

  }


  ALPHA.WOSTEN <- function(silt, clay, OM, bulk_dens, layer)
  {
    ALPHA <- -14.96 + 0.03135 * clay + 0.0351 * silt  + 0.646 * OM +
      15.29 * bulk_dens - 0.192 * layer - 4.671 * (bulk_dens^2) -
      0.000781 * (clay^2) - 0.00687 * (OM^2) + 0.0449 * (OM^(-1)) +
      0.0663 * log(silt) + 0.1482 * log(OM) - 0.04546 * bulk_dens * silt  -
      0.4852 * bulk_dens * OM + 0.00673 * layer * clay

    ALPHA <- exp(ALPHA)
    return(ALPHA)
  }


  N.WOSTEN <- function(silt, clay, OM, bulk_dens, layer)
  {
    N <- -25.23 - 0.02195 * clay + 0.0074 * silt  - 0.1940 * OM + 45.5 *
      bulk_dens - 7.24 * (bulk_dens^2) + 0.0003658 * (clay^2) + 0.002885 * (OM^2) - 12.81 *
      (bulk_dens^-1) - 0.1524 * silt^(-1) - 0.01958 * (OM^(-1)) - 0.2876 * log(silt) -
      0.0709 * log(OM)- 44.6 * log(bulk_dens) - 0.02264 * bulk_dens * clay +
      0.0896 * bulk_dens * OM + 0.00718 * layer * clay

    N <- exp(N)+1

    return(N)
  }


  # Mualem-van Genuchten Model (MVG) -water retention curve -----------------

  MVG <- function(THETA_S, ALPHA, N, hydraulic_head)
  {
    #
    #	water content (THETA) in respect to suction head  (H)
    #	the parameters are going to be computed based on the empirical function
    #	developed by WOSTEN 2001

    #	However, due to the limited information at the dry end of the water
    #	retention curve the parameter "THETA_R" was fixed at a value of
    #	0.01.(WOSTEN 1995, page 231)

    THETA <- 0.01 + ((THETA_S-0.01) / ((1 + (ALPHA * hydraulic_head)^N)^(1 - 1/N)))

    return(THETA)
  }


  # Saturated hydraulic conductivity (Wosten 2001, pp135) -------------------

  SATCOND.WOSTEN <- function(silt, clay, OM, bulk_dens, layer)
  {
    KSAT <- 7.755 + 0.0352*silt + 0.93*layer - 0.967*(bulk_dens^2) - 0.000484*(clay^2) -
      0.000322*(silt^2) + 0.001*(silt^(-1)) - 0.0748*(OM^-1) - 0.643*log(silt) -
      0.01398*bulk_dens*clay - 0.1673*bulk_dens*OM + 0.02986*layer*clay -
      0.03305*layer*silt

    KSAT <- exp(KSAT)

    # Correcting saturated conductivity to limits
    # SATURATED CONDUCTIVITY LIMITS: (from scheffer98 in cm/day 3*10^4 > ksat > 1*10^-2)
    # is later converted to mm/h for SWIM (assuming it's in cm/day in the database)
    ksat_max <- 3*(10^4)
    ksat_min <- 1*(10^-2)

    KSAT[which(KSAT > ksat_max)] <- ksat_max
    KSAT[which(KSAT < ksat_min)] <- ksat_min

    #transfer into mm/h from cm/d
    KSAT <- KSAT/2.4
    return(KSAT)
  }


# Pedotransfer functions for European soils -------------------------------
  # Note: all the applied function stick to the column names given in the
  #	HWSD soil database
  ptf.europ <- function(DataFrame, layer)
  {
    if(layer==1)
    {
      # organic matter & nitrogen
      DataFrame$T_OM <- OM(DataFrame$T_OC)
      DataFrame$T_ON <- ON(DataFrame$T_OC)

      # MVG-Model parameters first
      DataFrame$THETA_S <- THETA_S.WOSTEN(DataFrame$T_SILT, DataFrame$T_CLAY, DataFrame$T_OM,
                                        DataFrame$T_REF_BULK_DENSITY, layer=layer)

      DataFrame$ALPHA <- ALPHA.WOSTEN(DataFrame$T_SILT, DataFrame$T_CLAY, DataFrame$T_OM,
                                    DataFrame$T_REF_BULK_DENSITY, layer=layer)

      DataFrame$N <- N.WOSTEN(DataFrame$T_SILT, DataFrame$T_CLAY, DataFrame$T_OM,
                            DataFrame$T_REF_BULK_DENSITY, layer=layer)

      #field capacity
      DataFrame$T_FC <- MVG(THETA_S = DataFrame$THETA_S,
                         ALPHA = DataFrame$ALPHA, N = DataFrame$N,
                         hydraulic_head = 100)*100

      #permanent wilting point
      DataFrame$T_PWP <- MVG(THETA_S = DataFrame$THETA_S,
                          ALPHA = DataFrame$ALPHA, N = DataFrame$N,
                          hydraulic_head = 15295.5)*100

      DataFrame$THETA <- NULL
      DataFrame$THETA_S <- NULL
      DataFrame$ALPHA <- NULL
      DataFrame$N <- NULL

      #PWP never greater than FC
      DataFrame$T_PWP[which(DataFrame$T_PWP > DataFrame$T_FC)] = DataFrame$T_FC[which(DataFrame$T_PWP > DataFrame$T_FC)]
      #available water capacity
      DataFrame$T_AWC <- AWC(DataFrame$T_FC, DataFrame$T_PWP)

      #saturated conductivity
      DataFrame$T_SAT_CON <- SATCOND.WOSTEN(DataFrame$T_SILT,
                                          DataFrame$T_CLAY,
                                          DataFrame$T_OM,
                                          DataFrame$T_REF_BULK_DENSITY,
                                          layer)

      # drainage class based on Kartieranleitung 5
      DataFrame$DrainClass <- mapply(GetDrainageClass, DataFrame$T_SAT_CON)

      #porosity
      DataFrame$T_POR <- POR(DataFrame$T_REF_BULK_DENSITY)

      #erodibility factor K
      DataFrame$T_KUSLE <- KUSLE.WILLIAMS(DataFrame$T_SAND,
                                       DataFrame$T_SILT,
                                       DataFrame$T_CLAY,
                                       DataFrame$T_OM)
      return(DataFrame)

    }
    if(layer==0)
    {
      # organic matter & nitrogen
      DataFrame$S_OM <- OM(DataFrame$S_OC)
      DataFrame$S_ON <- ON(DataFrame$S_OC)

      # MVG-Model parameters first
      DataFrame$THETA_S <- THETA_S.WOSTEN(DataFrame$S_SILT, DataFrame$S_CLAY, DataFrame$S_OM,
                                        DataFrame$S_REF_BULK_DENSITY, layer=layer)

      DataFrame$ALPHA <- ALPHA.WOSTEN(DataFrame$S_SILT, DataFrame$S_CLAY, DataFrame$S_OM,
                                    DataFrame$S_REF_BULK_DENSITY, layer=layer)

      DataFrame$N <- N.WOSTEN(DataFrame$S_SILT, DataFrame$S_CLAY, DataFrame$S_OM,
                            DataFrame$S_REF_BULK_DENSITY, layer=layer)

      #field capacity
      DataFrame$S_FC <- MVG(THETA_S = DataFrame$THETA_S,
                         ALPHA = DataFrame$ALPHA, N = DataFrame$N,
                         hydraulic_head = 100)*100

      #permanent wilting point
      DataFrame$S_PWP <- MVG(THETA_S = DataFrame$THETA_S,
                          ALPHA = DataFrame$ALPHA, N = DataFrame$N,
                          hydraulic_head = 15295.5)*100
      #
      DataFrame$THETA <- NULL
      DataFrame$THETA_S <- NULL
      DataFrame$ALPHA <- NULL
      DataFrame$N <- NULL
      #

      #PWP never greater than FC
      DataFrame$S_PWP[which(DataFrame$S_PWP > DataFrame$S_FC)]=
        DataFrame$S_FC[which(DataFrame$S_PWP > DataFrame$S_FC)]

      #available water capacity
      DataFrame$S_AWC <- AWC(DataFrame$S_FC, DataFrame$S_PWP)

      #saturated conductivity
      DataFrame$S_SAT_CON <- SATCOND.WOSTEN(DataFrame$S_SILT,
                                          DataFrame$S_CLAY,
                                          DataFrame$S_OM,
                                          DataFrame$S_REF_BULK_DENSITY,
                                          layer)

      #porosity
      DataFrame$S_POR <- POR(DataFrame$S_REF_BULK_DENSITY)

      #erodibility factor K
      DataFrame$S_KUSLE <- NULL

      return(DataFrame)
    }

  }


# Pedotransfer functions for tropical soils -------------------------------

  ptf.trop <- function(DataFrame, layer)
  {
    if(layer==1)
    {
      # organic matter & nitrogen
      DataFrame$T_OM <- OM(DataFrame$T_OC)
      DataFrame$T_ON <- ON(DataFrame$T_OC)

      #field capacity
      DataFrame$T_FC <- FC.MINASNY(DataFrame$T_OM, DataFrame$T_SAND, DataFrame$T_REF_BULK_DENSITY)

      #permanent wilting point
      DataFrame$T_PWP <- PWP.MINASNY(DataFrame$T_OC, DataFrame$T_CLAY)

      #PWP never greater than FC
      DataFrame$T_PWP[which(DataFrame$T_PWP > DataFrame$T_FC)]=
        DataFrame$T_FC[which(DataFrame$T_PWP > DataFrame$T_FC)]


      #available water capacity
      DataFrame$T_AWC <- AWC(DataFrame$T_FC, DataFrame$T_PWP)

      #saturated conductivity
      DataFrame$T_SAT_CON <- SATCOND.WOSTEN(DataFrame$T_SILT,
                                          DataFrame$T_CLAY,
                                          DataFrame$T_OM,
                                          DataFrame$T_REF_BULK_DENSITY,
                                          layer)

      # drainage class based on Kartieranleitung 5
      DataFrame$DrainClass <- mapply(GetDrainageClass, DataFrame$T_SAT_CON)

      #porosity
      DataFrame$T_POR <- POR(DataFrame$T_REF_BULK_DENSITY)

      #erodibility factor K
      DataFrame$T_KUSLE <- KUSLE.WILLIAMS(DataFrame$T_SAND,
                                       DataFrame$T_SILT,
                                       DataFrame$T_CLAY,
                                       DataFrame$T_OC)
      return(DataFrame)

    }
    if(layer==0)
    {
      # organic matter & nitrogen
      DataFrame$S_OM <- OM(DataFrame$S_OC)
      DataFrame$S_ON <- ON(DataFrame$S_OC)

      #field capacity
      DataFrame$S_FC <- FC.MINASNY(DataFrame$S_OM, DataFrame$S_SAND, DataFrame$S_REF_BULK_DENSITY)

      #permanent wilting point
      DataFrame$S_PWP <- PWP.MINASNY(DataFrame$S_OC, DataFrame$S_CLAY)

      #PWP never greater than FC
      DataFrame$S_PWP[which(DataFrame$S_PWP > DataFrame$S_FC)]=
        DataFrame$S_FC[which(DataFrame$S_PWP > DataFrame$S_FC)]

      #available water capacity
      DataFrame$S_AWC <- AWC(DataFrame$S_FC, DataFrame$S_PWP)

      #saturated conductivity
      DataFrame$S_SAT_CON <- SATCOND.WOSTEN(DataFrame$S_SILT,
                                          DataFrame$S_CLAY,
                                          DataFrame$S_OM,
                                          DataFrame$S_REF_BULK_DENSITY,
                                          layer)
      #porosity
      DataFrame$S_POR <- POR(DataFrame$S_REF_BULK_DENSITY)

      #erodibility factor K
      DataFrame$S_KUSLE <- KUSLE.WILLIAMS(DataFrame$S_SAND,
                                       DataFrame$S_SILT,
                                       DataFrame$S_CLAY,
                                       DataFrame$S_OC)

      #erodibility factor K
      DataFrame$S_KUSLE <- NULL

      return(DataFrame)
    }
  }




# Check -------------------------------------------------------------------

  stopifnot(is.character(input$hwsd.path))
  stopifnot(is.character(input$hwsd.soil.data))
  stopifnot(is.character(input$output.path))

  if(!file.exists(input$hwsd.path)) stop(paste0("It does not exist: '", input$hwsd.path, "' "))
  if(!file.exists(input$output.path)) stop(paste0("It does not exist: '", input$output.path, "' "))

  stopifnot(is.numeric(input$ptf))
  stopifnot(input$ptf %in% c(1,2))
  stopifnot(is.numeric(input$arable))

  stopifnot(is.numeric(input$layer_100) & is.vector(input$layer_100))
  stopifnot(is.numeric(input$layer_30) & is.vector(input$layer_30))
  stopifnot(is.numeric(input$layer_10) & is.vector(input$layer_10))
  stopifnot(is.numeric(input$str_100) & is.vector(input$str_100))
  stopifnot(is.numeric(input$str_30) & is.vector(input$str_30))
  stopifnot(is.numeric(input$str_10) & is.vector(input$str_10))

  stopifnot(length(input$layer_100) == length(input$str_100))
  stopifnot(length(input$layer_30) == length(input$str_30))
  stopifnot(length(input$layer_10) == length(input$str_10))

  stopifnot(input$arable < length(input$layer_100))


# Soil Mapping Units for the basin ----------------------------------------
  # read the hydrotope table

  if (is.null(input$mu.number)) {

    stopifnot(is.character(input$hru.path))
    if(!file.exists(input$hru.path)) stop(paste0("It does not exist: '", input$hru.path, "' "))
    stopifnot(is.numeric(input$hru.soil.col))

    hru.soils <- read.table(input$hru.path, header = T)
    hru.soils <- unique(as.numeric(hru.soils[,input$hru.soil.col]))
    hru.soils <- hru.soils[hru.soils > 0]
    cat(paste(date(),': Read ', length(hru.soils), 'mapping units from hydrotope file: \n'))
    cat(input$hru.path, "\n")

    cat("...\n")
    cat("...\n")
  } else {
    stopifnot(is.numeric(input$mu.number))
    stopifnot(is.vector(input$mu.number))

    hru.soils <- input$mu.number
    cat(paste(date(),': Read ', length(hru.soils), 'mapping units \n'))

    cat("...\n")
    cat("...\n")

  }


# Connect to database -----------------------------------------------------

  con.hwsd <- DBI::dbConnect(RSQLite::SQLite(), input$hwsd.path)
  hwsd <- dplyr::tbl(con.hwsd, input$hwsd.soil.data)# hwsd data

  SU90 <- dplyr::tbl(con.hwsd, "D_SYMBOL90") %>% dplyr::collect() %>% data.frame()
  SU85 <- dplyr::tbl(con.hwsd, "D_SYMBOL85") %>% dplyr::collect() %>% data.frame()
  SU74 <- dplyr::tbl(con.hwsd, "D_SYMBOL74") %>% dplyr::collect() %>% data.frame()

# Subset data via MUs -----------------------------------------------------

  mu.subset <- hwsd %>%
    dplyr::filter(ISSOIL == 1) %>%
    dplyr::filter(MU_GLOBAL %in% hru.soils) %>%
    #filter(SHARE == max(SHARE)) %>%
    dplyr::select(ID, MU_GLOBAL, REF_DEPTH, ISSOIL, SHARE, SU_CODE74, SU_SYM74, SU_CODE85, SU_SYM85, SU_CODE90, SU_SYM90,#soil attributes
           T_CLAY, T_SILT, T_SAND, T_REF_BULK_DENSITY, T_OC, T_USDA_TEX_CLASS,
           S_CLAY, S_SILT, S_SAND, S_REF_BULK_DENSITY, S_OC, S_USDA_TEX_CLASS
    ) %>%
    dplyr::collect()

  # split data by MU_GLOBAL

  mu.split <- base::split(mu.subset, mu.subset$MU_GLOBAL)

  # Only MUs with greater share values
  mu.max.share <- lapply(mu.split, function (x) {
    a <- data.frame(x, stringsAsFactors = F)
    # select MUs with greater share value
    max.share <- max(a$SHARE)
    b <- a[a$SHARE == max.share, ][1, ]# select a MU if there are greater 1 MUs with same share
    # replace missing subsoils values with topsoil values
    if (all(c(as.integer(b$S_CLAY),
              as.integer(b$S_SILT) ,
              as.integer(b$S_SAND) ,
              as.integer(b$S_REF_BULK_DENSITY) ,
              as.integer(b$S_OC) ,
              as.integer(b$S_USDA_TEX_CLASS)
    )==0)) {

      b$S_CLAY <- b$T_CLAY
      b$S_SILT <- b$T_SILT
      b$S_SAND <- b$T_SAND
      b$S_REF_BULK_DENSITY <- b$T_REF_BULK_DENSITY
      b$S_OC <- b$T_OC
      b$S_USDA_TEX_CLASS <- b$T_USDA_TEX_CLASS

    }

    # select the soil clasification
    b$VALUE <- ifelse(!is.na(b$SU_SYM90), SU90[SU90$SYMBOL==b$SU_SYM90, "VALUE"],
                      ifelse(!is.na(b$SU_SYM85), SU85[SU85$SYMBOL==b$SU_SYM85, "VALUE"],
                             ifelse(!is.na(b$SU_SYM74), SU74[SU74$SYMBOL==b$SU_SYM74, "VALUE"],
                                    "No soil clasification"
                                    )
                             )
                      )

    b$SYMBOL <- ifelse(!is.na(b$SU_SYM90), b$SU_SYM90,
                      ifelse(!is.na(b$SU_SYM85), b$SU_SYM85,
                             ifelse(!is.na(b$SU_SYM74), b$SU_SYM74,
                                    "No soil symbol"
                             )
                      )
    )

    return(b)
  })

  mu.max.share <- do.call(rbind, mu.max.share)

  # Defining ID number for WSIM
  mu.max.share$SWIM_ID <- 1:nrow(mu.max.share)

  # soil attributes
  soil.att <-  mu.max.share %>% dplyr::select(ID, MU_GLOBAL, REF_DEPTH, ISSOIL, SHARE,
                                       #SU_CODE74, SU_SYM74, SU_CODE85, SU_SYM85, SU_CODE90, SU_SYM90,
                                       VALUE, SYMBOL, SWIM_ID
                                       )

  # topsoil attributes
  topsoil.data <- mu.max.share %>% dplyr::select(T_CLAY, T_SILT, T_SAND, T_REF_BULK_DENSITY, T_OC, T_USDA_TEX_CLASS)
  # subsoil attributes
  subsoil.data <- mu.max.share %>% dplyr::select(S_CLAY, S_SILT, S_SAND, S_REF_BULK_DENSITY, S_OC, S_USDA_TEX_CLASS)


# Soil parameters estimation ----------------------------------------------
  #	compute  the different soil parameters for each MU using the pedotranfer function

  # Pedotransfer functions for European soils
  if (input$ptf == 1)   {
    topsoil <- round(ptf.europ(topsoil.data, layer=1), 2)
    subsoil <- round(ptf.europ(subsoil.data, layer=0), 2)
  }

  # Pedotransfer functions for tropical soils
  if (input$ptf == 2)  {
    topsoil <- round(ptf.trop(topsoil.data, layer=1), 2)
    subsoil <- round(ptf.trop(subsoil.data, layer=0), 2)
  }

 # data frame of all soil properties
  hwsd.hyd.prop <- cbind(soil.att, topsoil, subsoil)


# Write soil files to SWIM model -----------------------------------------
# function to write soil files
  write2file <- function(DF, horizons, structure, arable, soilsfolder)
  { #	The function will produce line by line required for the soil file.

    # file location and name
    fn <- paste(soilsfolder, paste("soil", sprintf('%02i',DF$SWIM_ID),".dat", sep="" ),sep='/')
    # open file
    f_con <- file(fn,'w')

    # lines vector and seprator
    l=c() ; sep='\t'
    #	soil file number and number of horizons and arable horizons
    arable2 <- arable
    if ( arable2>length(horizons)) arable2 <- length(horizons)
    l[1] <- paste(DF$SWIM_ID, length(horizons), arable2,DF$DrainClass , "SOIL NR, LAYERS, ARABLE-LAYERS Drainge_Class ", sep=sep)
    #	name of the soil
    l[2] <- paste(DF$VALUE, "SOIL NAME", sep=sep)
    #	soil symbol
    l[3] <- paste(DF$SYMBOL, "SOIL SYMBOL", sep=sep)

    # write to file
    writeLines(l, f_con)

    # horizon data depeding on structure and horizon, make empty dataframe and fill with data
    d <- data.frame(t(rep(NA,length(horizons)+1))) # +1 for discriptions
    ltop <- length(which(structure==1))
    lsub <- length(which(structure==2)) # is null for class2+3
    fmt <- '%5.1f'
    #	texture class
    d[1,] <- c(sprintf('%5.0f', c(rep(DF$T_USDA_TEX_CLASS, ltop), rep(DF$S_USDA_TEX_CLASS, lsub))), 'USDA TEXTURE CLASS')
    #	depth for each horizon in [mm]
    d[2,] <- c(sprintf('%5i',horizons), "DEPTH in mm")
    #	clay content in %
    d[3,] <- c(sprintf(fmt, c(rep(DF$T_CLAY, ltop), rep(DF$S_CLAY, lsub))), "CLAY in %")
    #	silt content in %
    d[4,] <- c(sprintf(fmt, c(rep(DF$T_SILT, ltop), rep(DF$S_SILT, lsub))), "SILT in %")
    #	sand content in %
    d[5,] <- c(sprintf(fmt, c(rep(DF$T_SAND, ltop), rep(DF$S_SAND, lsub))), "SAND in %")
    #	bulk density g/cm³
    d[6,] <- c(sprintf(fmt, c(rep(DF$T_REF_BULK_DENSITY, ltop), rep(DF$S_REF_BULK_DENSITY, lsub))), "BULK DENS IN g/cm3")
    #	porosity in %
    d[7,] <- c(sprintf(fmt, c(rep(DF$T_POR, ltop), rep(DF$S_POR, lsub))), "POROSITY in %")
    #	available water capacity in %
    d[8,] <- c(sprintf(fmt, c(rep(DF$T_AWC, ltop), rep(DF$S_AWC, lsub))), "AWC in %")
    #	field capacity in %
    d[9,] <- c(sprintf(fmt, c(rep(DF$T_FC, ltop), rep(DF$S_FC, lsub))), "FIELD CAPAC in %")
    #	organic carbon content %
    d[10,] <- c(sprintf(fmt, c(rep(DF$T_OC, ltop), rep(DF$S_OC, lsub))), "ORGANIC C in %")
    #	organic nitrogen content %
    d[11,] <- c(sprintf(fmt, c(rep(DF$T_ON, ltop), rep(DF$S_ON, lsub))), "ORGANIC NITRO in %")
    #	saturated conductivity (mm/h)
    d[12,] <- c(sprintf(fmt, c(rep(DF$T_SAT_CON, ltop), rep(DF$S_SAT_CON, lsub))), "SATURATED COND in mm/h")

    # write to file
    write.table(d, f_con, quote=FALSE, col.names=FALSE, row.names=FALSE)

    #	erodibility factor k for the first horizon only
    l <- paste(sprintf(fmt, DF$T_KUSLE), "ERODIBILITY FACTOR", sep=sep)
    writeLines(l, f_con)

    close(f_con)

    return(hwsd.hyd.prop)

  }



  lfiles <- nrow(subsoil)
  cat('Writing',lfiles,'soil files to', input$output.path, '\n')
  cat("...\n")
  cat("...\n")

  for (i in 1:nrow(hwsd.hyd.prop)){

    if(hwsd.hyd.prop$REF_DEPTH[i] == 100) {
      write2file(hwsd.hyd.prop[i,], input$layer_100, input$str_100, input$arable, input$output.path)

    }else if(hwsd.hyd.prop$REF_DEPTH[i] == 30) {
      write2file(hwsd.hyd.prop[i,], input$layer_30, input$str_30, input$arable, input$output.path)
    }else if(hwsd.hyd.prop$REF_DEPTH[i] == 10) {
      write2file(hwsd.hyd.prop[i,], input$layer_10, input$str_10, input$arable, input$output.path)
    }

  }


  # Writing soil.cio files
  soil.cio <- c()
  for (j in 1:nrow(hwsd.hyd.prop)) {
    soil.cio[j] <- paste("soil", sprintf('%02i', hwsd.hyd.prop$SWIM_ID [j]),".dat", sep="" )
  }

  write.table(soil.cio, paste(input$output.path, "soil.cio",sep='/'),
              col.names = F, row.names = F, quote = F)

  cat("The 'soil.cio' file was written to", '\n', input$output.path)
  cat("...\n")
  cat("...\n")



# Report and write nonsoilmus ---------------------------------------------
  nonsoilmu=setdiff(hru.soils, hwsd.hyd.prop$MU_GLOBAL)

  if(length(nonsoilmu) > 0 ) {

    write.table(nonsoilmu,
                paste0(input$output.path, "/", "non-soil_mappingunits.dat"),
                row.names = F, col.names = F, quote = F)

    cat( "Warning: ",length(nonsoilmu), ' non-soil mapping units written to ', '\n',
         paste0(input$output.path, "/", "non-soil_mappingunits.dat", '\n')
    )

    cat("...\n")
    cat("...\n")

  }


# Write out reclassification with MU_GLOBAL = SWIM_ID ---------------------
  mu2swim <- hwsd.hyd.prop %>% dplyr::select(MU_GLOBAL, SWIM_ID)


  # add the MU which is necessary to define
  if (length(nonsoilmu) > 0) {
    nonsoilmu2 <- data.frame(MU_GLOBAL = nonsoilmu, SWIM_ID = "Define")
    mu2swim <- rbind(mu2swim, nonsoilmu2)
  }

  write.table(mu2swim,
              file=paste0(input$output.path, "/", "swim_reclassification.dat"),
              col.names=FALSE,
              row.names=FALSE,
              quote = FALSE,
              sep='=')

  cat('A table for hydrotope soils reclassification (MU) based on SWIM soil ID was written to','\n',
      paste0(input$output.path, "/", "swim_reclassification.dat", '\n')
      )

  cat("...\n")
  cat("...\n")

  cat('>> Done! \n', sep = '')
}

