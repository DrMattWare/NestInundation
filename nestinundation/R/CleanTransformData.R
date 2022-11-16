# Clean water depth data and identify inundation events


utils::globalVariables(c("Data", "movingWindow", "hoboData", "windowSize", "inundationStart", "inundationEnd", "inundationDuration",
                         "inundationSeverity", "Preference", "maxClutchDepth", "minClutchDepth", "hoboDepth", "dateHatched", "dateRecovered",
                         "dateLaid", "dateDeployed", "avgIncubationDuration", "backgroundNoise", "TimeZone", "nestID"))

#' Calculate the proportion of the egg chamber affected based on reported water level (i.e., inundation severity)
#'
#' @param maxClutch The maximum depth of the egg chamber from the ground surface at the time of deposition in centimeters
#' @param minClutch The minimum depth of the egg chamber from the ground surface at the time of deposition in centimeters
#' @param inundLevel The depth of the water above the sensor in cm
#' @return Proportion of egg chamber affected by inundation assuming the nest chamber is a sphere with radius (maxClutch - minClutch)/2
#'
Severity <- function(maxClutch, minClutch, inundLevel){

  radius <- (abs(maxClutch) - abs(minClutch))/2 # calculate radius of the nest chamber based on clutch depth measurements

  if(abs(inundLevel) >= abs(maxClutch) - abs(minClutch)){
    return(1) # if the water level is above the clutch, the entire clutch is underwater so return a proportion of 1
  } else {
    return(round(((pi * abs(inundLevel)^2 * radius) - ((pi * abs(inundLevel)^3)/3)) / ((4/3) * pi * radius^3), 3))
  } # otherwise determine the height of the water level into the nest chamber and calculate the affected volume as a proportion
} # Assumes clutch is a sphere, values should all be in cm


#' Check that reported water levels are above logger as opposed to depth below surface
#'
#' @param DATA_Check Uses a processing copy of the inundation data (i.e., "Data") for a QC check
#' @return Water depth above logger in meters, transformed as necessary.
#'
Depth_Check <- function(DATA_Check = Data){

  Data_Check <- DATA_Check

  Depth <- NULL
  if(Data_Check$Depth_m[1] < -0.01){
    Depth <- Data_Check$Depth_m - Data_Check$Depth_m[1]
  } else {
    Depth <- Data_Check$Depth_m
  }
  Data_Check$Depth_m <- Depth
  assign(x = "Data", value = Data_Check, envir = .GlobalEnv) # Assigns working copy back to global environment

}

#' Transform raw depth measurements above sensor and calculate depth of incursion, if any, into the egg chamber when no moving window is selected
#'
#' @param DATA_Trans Uses a processing copy of the inundation data (i.e., "Data") for transformation
#' @return Water depth above logger in centimeters, depth of incursion into nest chamber, and proportion of nest chamber affected (i.e., severity).
#'
Transform_Data_Normal <- function(DATA_Trans = Data){

  Data_Trans <- DATA_Trans

  # Convert observed depths to cm above the water level logger
  transDepth <- NULL
  for(i in 1:length(Data_Trans$Depth_m)){
    if(is.na(Data_Trans$Depth_m[i])){ # Maintain NAs
      transDepth[i] <- NA
    } else if(Data_Trans$Depth_m[i] < backgroundNoise/100){ # Replace values below backgroundNoise threshold with 0s
      transDepth[i] <- 0
    } else {
      transDepth[i] <- round(Data_Trans$Depth_m[i] * 100, 2) # Transform remaining values from m to cm
    }
  }

  Data_Trans$Depth_cm <- transDepth

  # Calculate water level incursion into nest chamber
  inundDepth <- NULL
  for(i in 1:length(Data_Trans$Depth_cm)){
    if(is.na(Data_Trans$Depth_cm[i])){ # Maintain NAs
      inundDepth[i] <- NA
    } else if(Data_Trans$Depth_cm[i] < (abs(hoboDepth) - abs(maxClutchDepth))){
      inundDepth[i] <- 0 # If water depth is below the maximum clutch depth, the clutch is not inundated
    } else {
      inundDepth[i] <- Data_Trans$Depth_cm[i] - (abs(hoboDepth) - abs(maxClutchDepth)) # Observed water depth above the maximum clutch depth
    }
  }

  Data_Trans$Inund_Depth <- inundDepth

  # Calculate the proportion of the nest chamber affected by inundation
  inundSeverity <- NULL
  for(i in 1:length(Data_Trans$Depth_cm)){
    if(is.na(Data_Trans$Depth_cm[i])){ # Maintain NAs
      inundSeverity[i] <- NA
    } else {
      inundSeverity[i] <- Severity(maxClutch = abs(maxClutchDepth), minClutch = abs(minClutchDepth), inundLevel = Data_Trans$Inund_Depth[i])
    }
  }

  Data_Trans$Inund_Severity <- inundSeverity

  assign(x = "Data", value = Data_Trans, envir = .GlobalEnv) # Assigns working copy back to global environment

} # Used for initial data transformations within Clean_Depth_Data function under normal circumstances

#' Transform raw depth measurements above sensor and calculate depth of incursion, if any, into the egg chamber when a moving window is selected
#'
#' @param DATA_Trans Uses a processing copy of the inundation data (i.e., "Data") for transformation
#' @return Water depth above logger in centimeters, depth of incursion into nest chamber, and proportion of nest chamber affected (i.e., severity).
#'
Transform_Data_Window <- function(DATA_Trans = Data){

  Data_Trans <- DATA_Trans

  # Convert observed depths to cm above the water level logger
  transDepth <- NULL
  for(i in 1:length(Data_Trans$Depth_m_Window)){
    if(is.na(Data_Trans$Depth_m_Window[i])){ # Maintain NAs
      transDepth[i] <- NA
    } else if(Data_Trans$Depth_m_Window[i] < backgroundNoise/100){ # Replace values below backgroundNoise threshold with 0s
      transDepth[i] <- 0
    } else {
      transDepth[i] <- round(Data_Trans$Depth_m_Window[i] * 100, 2) # Transform remaining values from m to cm
    }
  }

  Data_Trans$Depth_cm <- transDepth

  # Calculate water level incursion into nest chamber
  inundDepth <- NULL
  for(i in 1:length(Data_Trans$Depth_cm)){
    if(is.na(Data_Trans$Depth_cm[i])){ # Maintain NAs
      inundDepth[i] <- NA
    } else if(Data_Trans$Depth_cm[i] < (abs(hoboDepth) - abs(maxClutchDepth))){
      inundDepth[i] <- 0 # If water depth is below the maximum clutch depth, the clutch is not inundated
    } else {
      inundDepth[i] <- Data_Trans$Depth_cm[i] - (abs(hoboDepth) - abs(maxClutchDepth)) # Observed water depth above the maximum clutch depth
    }
  }

  Data_Trans$Inund_Depth <- inundDepth

  # Calculate the proportion of the nest chamber affected by inundation
  inundSeverity <- NULL
  for(i in 1:length(Data_Trans$Depth_cm)){
    if(is.na(Data_Trans$Depth_cm[i])){ # Maintain NAs
      inundSeverity[i] <- NA
    } else {
      inundSeverity[i] <- Severity(maxClutch = abs(maxClutchDepth), minClutch = abs(minClutchDepth), inundLevel = Data_Trans$Inund_Depth[i])
    }
  }

  Data_Trans$Inund_Severity <- inundSeverity

  assign(x = "Data", value = Data_Trans, envir = .GlobalEnv) # Assigns working copy back to global environment

} # Used for initial data transformations within Clean_Depth_Data function when a moving window is applied

#' Umbrella function to transform depth data, determine severity, and output results to a new dataset when no moving window is selected
#'
#' @return A new dataset (i.e., hoboData) containing calculated values
#'
Process_Normal <- function(){

  assign(x = "movingWindow", value = "No", envir = .GlobalEnv) # Assigns "No" value to movingWindow variable to global environment

  Transform_Data_Normal(DATA_Trans = Data) # Transform raw input data

  assign(x = "hoboData", value = Data, envir = .GlobalEnv) # Reassign working copy back to global dataset

} # Normal step-wise processing for raw HOBO data

#' Umbrella function to transform depth data, determine severity, and output results to a new dataset when a moving window is selected
#'
#' @param DATA_Window Uses a processing copy of the inundation data (i.e., "Data") for analysis
#' @return A new dataset (i.e., hoboData) containing the smoothed calculated values and a variable defining the width of the moving window
#'
Process_MovingWindow <- function(DATA_Window = Data){

  Data_Window <- DATA_Window

  if(regexpr(movingWindow, 'Yes', ignore.case = TRUE) == 1){ # If user wants a moving window ...

    windowSize <<- readline("Specify window size (odd number): ") # Specify window size and create new column with smoothed data
    windowSize <<- as.integer(windowSize)
    Depth_Window <- zoo::rollmean(Data_Window$Depth_m, k = windowSize)
    Depth_Window <- c(rep(NA, floor(windowSize/2)), Depth_Window, rep(NA, floor(windowSize/2)))
    Data_Window$Depth_m_Window <- Depth_Window

    Temp_Window <- zoo::rollmean(Data_Window$Temp_C, k = windowSize)
    Temp_Window <- c(rep(NA, floor(windowSize/2)), Temp_Window, rep(NA, floor(windowSize/2)))
    Data_Window$Temp_C <- Temp_Window

    assign(x = "Data", value = Data, envir = .GlobalEnv) # Assigns working copy back to global environment

    Transform_Data_Window(DATA_Trans = Data) # Transform raw input data

    assign(x = "hoboData", value = Data, envir = .GlobalEnv) # Reassign working copy back to global dataset

  } else {

    Process_Normal()

  }
} # Transform raw data based on Moving Window input

#' QC Check: Ask whether to apply moving window smoother to high-frequency data
#'
#' @return A new dataset (i.e., hoboData) containing calculated values based on the user input
#'
QCCheck_Window <- function(){

  # Ask if user wants to smooth data
  movingWindow <- readline("Sampling interval is less than 30 minutes. Do you want to apply a moving-window smoother? (Yes/No): ")
  assign(x = "movingWindow", value = movingWindow, envir = .GlobalEnv) # Assigns moving window decision to global environment

  if(regexpr(movingWindow, 'Yes', ignore.case = TRUE) == 1){ # If user wants a moving window ...

    Process_MovingWindow(DATA_Window = Data)

  } else { # User does not want a moving window ...

    Process_Normal()

  }

}

#' Data record is long enough or user permits continued processing
#'
#' @return A new dataset (i.e., hoboData) containing calculated values based on the user input
#'
Pass_QC02 <- function(){

  Depth_Check(DATA_Check = Data)

  # QC Check #3: Use a moving window to smooth high-frequency sampling?
  if(as.numeric(difftime(Data$Date_Time[2], Data$Date_Time[1], units = "mins")) < 30){

    QCCheck_Window()

  } else { # Data does not require smoothing

    Process_Normal()

  }

}

#' Identifies inundation events relative to nest chamber
#'
#' @description Identifies inundation events relative to the nest chamber
#' @param HOBO The name of the dataframe to be used in processing
#' @return The start and end Date-Time stamps, duration, and maximum severity of each inundation event
#' @examples
#' \dontrun{ID_Inundation(HOBO = hoboData)}
#' @export
#'
ID_Inundation <- function(HOBO){

  Data <<- HOBO # makes a copy of the data in the global environment

  # Determine at what time steps the nest is underwater
  inundSev <- Data %>%
    dplyr::select("Inund_Severity") %>%
    dplyr::pull()
  assign(x = "inundSeverity", value = inundSev, envir = .GlobalEnv) # Assigns Inundation Severity column back to global environment

  classifyInundation <- NULL
  for(i in 1:length(inundSeverity)){
    if(is.na(inundSeverity)[i]){ # Maintain NAs
      classifyInundation[i] <- NA
    } else if(inundSeverity[i] == 0){ # If Severity = 0, then no inundation event is currently underway
      classifyInundation[i] <- 0
    } else {
      classifyInundation[i] <- 1 # If Severity > 0, then inundation is occurring
    }
  }

  # Identify when inundation events start and stop based on transition between inundation states (0 = not inundation, 1 = inundated)
  inundationStartEnd <- NULL
  if(regexpr(movingWindow, 'Yes', ignore.case = TRUE) == 1){ # If a moving window was used, NAs have been introduced and need to be dealt with

    inundationStartEnd <- rep(NA, floor(windowSize/2)) # Repeat NAs based on moving window size
    if(classifyInundation[floor(windowSize/2) + 1] == 1){
      inundationStartEnd <- 1 # If the first available observation is inundated, assign the first available time step as the start of the inundation event
    } else {
      inundationStartEnd <- 0 # Otherwise, the first available time step is assigned "not inundated"
    }
    for(i in (floor(windowSize/2) + 2):(length(classifyInundation) - (floor(windowSize/2) + 2))){
      inundationStartEnd[i] <- classifyInundation[i] - classifyInundation[i-1] # 1 represents the start of an inundation event, -1 represents the end
    }
    if(is.na(classifyInundation[length(classifyInundation) - (floor(windowSize/2) + 1)])){
      inundationStartEnd[length(classifyInundation) - (floor(windowSize/2) + 1)] <- NA # If the last available observation is missing, maintain NAs
    } else if(classifyInundation[length(classifyInundation) - (floor(windowSize/2) + 1)] == 1
              && classifyInundation[length(classifyInundation) - floor(windowSize/2)] == 0){
      inundationStartEnd[length(classifyInundation) - (floor(windowSize/2) + 1)] <- 2 # If the last observation is the start of inundation event, mark this time step as both the start and end of an inundation event
    } else if(classifyInundation[length(classifyInundation) - (floor(windowSize/2) + 1)] == 1
              && classifyInundation[length(classifyInundation) - floor(windowSize/2)] == 1){
      inundationStartEnd[length(classifyInundation) - (floor(windowSize/2) + 1)] <- -1 # If the last observation is "inundated" by an ongoing event, mark this time step as the end of an inundation event
    } else {
      inundationStartEnd[length(classifyInundation) - (floor(windowSize/2) + 1)] <- 0 # Otherwise, the last available observation is "not inundated"
    }
    inundationStartEnd <- c(inundationStartEnd, rep(NA, floor(windowSize/2))) # Repeat NAs based on moving window size

  } else {

    if(classifyInundation[1] == 1){
      inundationStartEnd <- 1 # If the first observation is inundated, assign the first time step as the start of the inundation event
    } else {
      inundationStartEnd <- 0 # Otherwise, the first time step is assigned "not inundated"
    }
    for(i in 2:length(classifyInundation)){
      inundationStartEnd[i] <- classifyInundation[i] - classifyInundation[i-1] # 1 represents the start of an inundation event, -1 represents the end
    }
    if(is.na(tail(classifyInundation, 1))){
      inundationStartEnd[length(inundationStartEnd)] <- NA # If the last observation is missing, maintain NAs
    } else if(tail(classifyInundation, 1) == 1 && classifyInundation[length(classifyInundation) - 1] == 0){
      inundationStartEnd[length(inundationStartEnd)] <- 2 # If the last observation is the start of inundation event, mark this time step as both the start and end of an inundation event
    } else if(tail(classifyInundation, 1) == 1 && classifyInundation[length(classifyInundation) - 1] == 1){
      inundationStartEnd[length(inundationStartEnd)] <- -1 # If the last observation is "inundated" by an ongoing event, mark this time step as the end of an inundation event
    } else {
      inundationStartEnd[length(inundationStartEnd)] <- 0 # Otherwise, the last observation is "not inundated"
    }

  }

  # Extract the date-time stamps for the start of inundation events
  dateTime <- Data %>%
    dplyr::select("Date_Time") %>%
    dplyr::pull()
  assign(x = "Date_Time", value = dateTime, envir = .GlobalEnv) # Assigns Date_Time column back to global environment

  inundationStart <<- NULL
  if(sum(abs(inundationStartEnd), na.rm = TRUE) == 0){
    inundationStart <<- NA # If no inundations occurred, return NA
  } else {
    inundationStart <<- Date_Time[which(inundationStartEnd == 1 | inundationStartEnd == 2)]
  }

  # Extract the date-time stamps for the end of inundation events
  inundationEnd <<- NULL
  if(sum(abs(inundationStartEnd), na.rm = TRUE) == 0){
    inundationEnd <<- NA # If no inundations occurred, return NA
  } else {
    inundationEnd <<- Date_Time[which(inundationStartEnd == -1 | inundationStartEnd == 2)]
  }

  # Calculate the duration of individual inundation events
  inundationDuration <<- NULL
  if(lubridate::is.POSIXct(inundationStart) == FALSE){
    inundationDuration <<- 0 # If no inundation occurred, return 0
  } else {
    inundationDuration <<- as.numeric(difftime(inundationEnd, inundationStart, units = "hours"))
  }
  inundationDuration[length(inundationDuration)] <<- ifelse(tail(inundationStartEnd, 1) == 2, 1, tail(inundationDuration, 1))

  # Calculate the maximum proportion of the nest chamber affected by each inundation event
  inundationSeverity <<- NULL
  if(lubridate::is.POSIXct(inundationStart) == FALSE){
    inundationSeverity <<- 0 # If no inundation occcurred, return 0
  } else {
    for(i in 1:length(inundationStart)){

      inundationSeverity[i] <<- max(inundSeverity[which(Date_Time >= inundationStart[i] & Date_Time <= inundationEnd[i])], na.rm = TRUE)
    }
  }

  rm(Data, envir = .GlobalEnv)
  rm(inundSeverity, envir = .GlobalEnv)
  rm(Date_Time, envir = .GlobalEnv)
}

#' Clean raw user depth data
#'
#' @description Process observed depth data to identify inundation events. Processing includes removing minor fluctuations around the sensor, applying a moving window to smooth high-frequency data (if desired for data sampled <30 min apart), and truncating the record to only observations which occurred pre-hatchling emergence (if desired). A quality control check will prompt the user to continue with or abort processing for records less than 45 days.
#' @param HOBO The name of the dataframe to be used in processing
#' @return Returns a new dataframe (named "hoboData" by default) containing calculated inundation values based on the user input
#' @examples
#' \dontrun{Clean_Depth_Data(HOBO = hoboData)}
#' @export
#'
Clean_Depth_Data <- function(HOBO){

  Data <<- HOBO # makes a copy of the data in the global environment

  # QC Check #1: Is nest deeper than logger deployment?
  if(abs(maxClutchDepth) > abs(hoboDepth)){
    stop("Clutch depth cannot be deeper than HOBO logger deployment as this may result in unidentified inundation events")
  }

  # Preference check: does user want to subset data to average incubation duration if hatch date is missing
  Preference <<- "No"

  if(is.na(dateHatched)){ # Hatch Date is not available ...

    Preference <<- readline("Hatch date is missing. Do you want to subset the data to within the specified average incubation duration? (Yes/No): ")

    # Data Subsetting
    if(regexpr(Preference, 'Yes', ignore.case = TRUE) == 1){ # If user wants to subset data ...

      # Data filtering
      Data_Clean <- Data %>%
        dplyr::filter(Date_Time <= as.Date(dateLaid) + avgIncubationDuration)
      assign(x = "Data", value = Data_Clean, envir = .GlobalEnv) # Assigns filtered copy back to global environment

      # QC Check #2: Is data record too short?
      if(as.numeric(difftime(dateRecovered, dateDeployed, units = "days")) < 45){ # Data is too short
        Question1 <- readline("HOBO record is less than 45 days and may not cover a full incubation. If you want to continue, enter Yes. Otherwise, enter No to abort.: ")

        # User proceeds with data processing
        if(regexpr(Question1, 'Yes', ignore.case = TRUE) == 1){

          Pass_QC02()

        } else { # User aborts processing

          stop("Cleaning aborted")

        }

      } else { # Passed QC #1 & 2. Proceed to further processing.

        Pass_QC02()

      }

    } else { # If user does not want to subset data ...

      # QC Check #2: Is data record too short?
      if(as.numeric(difftime(dateRecovered, dateDeployed, units = "days")) < 45){ # Data is too short
        Question1 <- readline("HOBO record is less than 45 days and may not cover a full incubation. If you want to continue, enter Yes. Otherwise, enter No to abort.: ")

        # User proceeds with data processing
        if(regexpr(Question1, 'Yes', ignore.case = TRUE) == 1){

          Pass_QC02()

        } else { # User aborts processing

          stop("Cleaning aborted")

        }

      } else { # Passed QC #1 & 2. Proceed to further processing.

        Pass_QC02()

      }

    }

  } else { # Hatch Date is available ...

    # Preference check: does user want to subset data to pre-emergence timeframe?
    Preference <<- readline("Do you want to subset the data to within the observed incubation duration? (Yes/No) ")

    # Data Subsetting
    if(regexpr(Preference, 'Yes', ignore.case = TRUE) == 1){ # If user wants to subset data ...

      # Data filtering
      Data_Clean <- Data %>%
        dplyr::filter(Date_Time <= dateHatched)
      assign(x = "Data", value = Data_Clean, envir = .GlobalEnv) # Assigns filtered copy back to global environment

      # QC Check #2: Is data record too short?
      if(as.numeric(difftime(dateRecovered, dateDeployed, units = "days")) < 45){ # Data is too short
        Question1 <- readline("HOBO record is less than 45 days and may not cover a full incubation. If you want to continue, enter Yes. Otherwise, enter No to abort.: ")

        # User proceeds with data processing
        if(regexpr(Question1, 'Yes', ignore.case = TRUE) == 1){

          Pass_QC02()

        } else { # User aborts processing

          stop("Cleaning aborted")

        }

      } else { # Passed QC #1 & 2. Proceed to further processing.

        Pass_QC02()

      }

    } else { # If user does not want to subset data ...

      # QC Check #2: Is data record too short?
      if(as.numeric(difftime(dateRecovered, dateDeployed, units = "days")) < 45){ # Data is too short
        Question1 <- readline("HOBO record is less than 45 days and may not cover a full incubation. If you want to continue, enter Yes. Otherwise, enter No to abort.: ")

        # User proceeds with data processing
        if(regexpr(Question1, 'Yes', ignore.case = TRUE) == 1){

          Pass_QC02()

        } else { # User aborts processing

          stop("Cleaning aborted")

        }

      } else { # Passed QC #1 & 2. Proceed to further processing.

        Pass_QC02()

      }

    }

  }

  rm(Data, envir = .GlobalEnv)

}

#' Example data

#' @description An example dataframe reported from a HOBO U20L-04 water level logger deployed adjacent to a sea turtle nest ...
#' @format ## `Demo_Data`
#' @name "hoboData"
#' A dataframe with 1586 hourly observations of 5 variables:
#'\describe{
#'   \item{Date_Time_CDT}{Date-Time stamp of observation}
#'   \item{Abs_Press_mbar}{Absolute pressure reported by water level sensor in mbar}
#'   \item{Temp_C}{Temperature reported by water level sensor in degrees Celsius}
#'   \item{Baro_Press_mbar}{Baromatric pressure reported by a local weather station in mbar used for calculating observed water depth}
#'   \item{Depth_m}{Depth in meters of the water level sensor after accounted for barometric pressure changes}
#'}
#'
#' @source Dr. Matthew Ware, University of North Carolina Wilmington Center for Marine Science
#' @export
Demo_Data <- load("R/sysdata.rda")
