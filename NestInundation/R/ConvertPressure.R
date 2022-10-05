# NestInundation: manually calculate water depth at logger from raw pressure readings

#' Convert kilopascals to millibars
#'
#' @description Pressure can be reported in kilopascals (kPA) or millibars (mb). This function transforms kPa to mb.
#' @param kPa A number in kilopascals
#' @return A number in millibars
#' @example kPa_mbar(101.1)
#'
kPa_mbar <- function(kPa){
  stopifnot("Input must be numeric" = is.numeric(kPa))
  return(kPa * 10)
}

#' Convert millibars to kilopascals
#'
#' @description Pressure can be reported in kilopascals (kPA) or millibars (mb). This function transforms mb to kPa.
#' @param mb A number in millibars
#' @return A number in kilopascals
#' @example mbar_kPa(1011)
#'
mbar_kPa <- function(mb){
  stopifnot("Input must be numeric" = is.numeric(mb))
  return(mb / 10)
}

#' Calculate water depth
#'
#' @description Calculate water depth in meters above sensor based on pressure in millibars
#' @param Pressure Reported pressure in millibars
#' @param Density Density of water - preset to 1025 for seawater but can be adjusted by user
#' @return Depth in meters of water above the pressure sensor
#' @example WaterDepth_mbar(Pressure = 100)
#'
WaterDepth_mbar <- function(Pressure, Density = 1025){ # Pressure should be in mbar, Density is set to kg/m3 saltwater by default
  stopifnot("Input must be numeric" = is.numeric(kPa))
  return(round((Pressure * 100) / (Density * 9.8), 3)) # Converts mbar to N/m2 and multiplies Density by acceleration due to gravity
}

#' Calculate water depth
#'
#' @description Calculate water depth in meters above sensor based on pressure in kilopascals
#' @param Pressure Reported pressure in kilopascals
#' @param Density Density of water - preset to 1025 for seawater but can be adjusted by user
#' @return Depth in meters of water above the pressure sensor
#' @example WaterDepth_kPa(Pressure = 10)
#'
WaterDepth_kPa <- function(Pressure, Density = 1025){ # Pressure should be in kPa, Density is set to kg/m3 saltwater by default
  stopifnot("Input must be numeric" = is.numeric(kPa))
  return(round((Pressure * 1000) / (Density * 9.8), 3)) # Converts kPa to N/m2 and multiplies Density by acceleration due to gravity
}
