# NestInundation: manually calculate water depth at logger from raw pressure readings

#' Convert kilopascals to millibars
#'
#' @param kPa A number in kilopascals
#' @return A number in millibars
kPa_mbar <- function(kPa){
  return(kPa * 10)
}

#' Calculate water depth above sensor based on pressure in millibars
#'
#' @param Pressure Reported pressure in millibars
#' @param Density Density of water - preset to 1025 for seawater
#' @return Depth in meters of water above the pressure sensor
#'
WaterDepth_mbar <- function(Pressure, Density = 1025){ # Pressure should be in mbar, Density is set to kg/m3 saltwater by default
  return(round((Pressure * 100) / (Density * 9.8), 3)) # Converts mbar to N/m2 and multiplies Density by acceleration due to gravity
}

#' Calculate water depth above sensor based on pressure in kilopascals
#'
#' @param Pressure Reported pressure in kilopascals
#' @param Density Density of water - preset to 1025 for seawater
#' @return Depth in meters of water above the pressure sensor
#'
WaterDepth_kPa <- function(Pressure, Density = 1025){ # Pressure should be in kPa, Density is set to kg/m3 saltwater by default
  return(round((Pressure * 1000) / (Density * 9.8), 3)) # Converts kPa to N/m2 and multiplies Density by acceleration due to gravity
}
