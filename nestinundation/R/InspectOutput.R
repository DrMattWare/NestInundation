# NestInundation: Inspect output


utils::globalVariables(c("Table_Obs", "Table_Events", "Temp_Obs", "dataset_names"))

#' Inspect depth and temperature time-series
#'
#' @description Plot water level and temperature for a basic visualization of the inundation time-series. The plot will adapt based on user inputs during Clean_Depth_Data().
#' @return A 3-panel plot of cleaned water depth, depth of incursions into nest chamber, and water temperature
#' @examples Plot_Inspection()
#' @export
#'
Plot_Inspection <- function(){

  if(regexpr(Preference, 'Yes', ignore.case = TRUE) == 1){ # User subsets data ...

    par(mfrow = c(4, 1), family = "serif", mar = c(4.5, 4.5, 3, 0.75), bty = "n", las = 1, cex.axis = 1.2, cex.lab = 1.3)
    plot(hoboData[["Date_Time"]], hoboData[["Depth_cm"]], main = "Logger Depth", xlab = "Date", ylab = "Depth (cm)", type = "l", lwd = 2)
    plot(hoboData[["Date_Time"]], hoboData[["Inund_Depth"]], main = "Inundation Depth", xlab = "Date", ylab = "Depth (cm)", type = "l", lwd = 2)
    plot(hoboData[["Date_Time"]], hoboData[["Inund_Severity"]], main = "Inundation Severity", xlab = "Date", ylab = "Proportion Exposed", ylim = c(0, 1),
         type = "l", lwd = 2)
    plot(hoboData[["Date_Time"]], hoboData[["Temp_C"]], main = "HOBO Temperature", xlab = "Date", ylab = "Temperature (°C)", type = "l", lwd = 2)

  } else { # User does not subset data ...

    if(is.na(dateHatched)){ # if no hatch date is available ...

      par(mfrow = c(4, 1), family = "serif", mar = c(4.5, 4.5, 3, 0.75), bty = "n", las = 1, cex.axis = 1.2, cex.lab = 1.3)
      plot(hoboData[["Date_Time"]], hoboData[["Depth_cm"]], main = "Logger Depth", xlab = "Date", ylab = "Depth (cm)", type = "l", lwd = 2)
      abline(v = dateLaid + (avgIncubationDuration*24*60*60), col = "red", lwd = 2, lty = 2)
      legend("topleft", legend = c("Avg Incubation"), col = "red", lty = 2, lwd = 2, bty = 'n')
      plot(hoboData[["Date_Time"]], hoboData[["Inund_Depth"]], main = "Inundation Depth", xlab = "Date", ylab = "Depth (cm)", type = "l", lwd = 2)
      abline(v = dateLaid + (avgIncubationDuration*24*60*60), col = "red", lwd = 2, lty = 2)
      plot(hoboData[["Date_Time"]], hoboData[["Inund_Severity"]], main = "Inundation Severity", xlab = "Date", ylab = "Proportion Exposed", ylim = c(0, 1),
           type = "l", lwd = 2)
      abline(v = dateLaid + (avgIncubationDuration*24*60*60), col = "red", lwd = 2, lty = 2)
      plot(hoboData[["Date_Time"]], hoboData[["Temp_C"]], main = "HOBO Temperature", xlab = "Date", ylab = "Temperature (°C)", type = "l", lwd = 2)
      abline(v = dateLaid + (avgIncubationDuration*24*60*60), col = "red", lwd = 2, lty = 2)
      legend("topleft", legend = c("Avg Incubation"), col = "red", lty = 2, lwd = 2, bty = 'n')

    } else { # a hatch date is available ...

      par(mfrow = c(4, 1), family = "serif", mar = c(4.5, 4.5, 3, 0.75), bty = "n", las = 1, cex.axis = 1.2, cex.lab = 1.3)
      plot(hoboData[["Date_Time"]], hoboData[["Depth_cm"]], main = "Logger Depth", xlab = "Date", ylab = "Depth (cm)", type = "l", lwd = 2)
      abline(v = dateHatched, col = "red", lwd = 2, lty = 2)
      legend("topleft", legend = c("Hatch Date"), col = "red", lty = 2, lwd = 2, bty = 'n')
      plot(hoboData[["Date_Time"]], hoboData[["Inund_Depth"]], main = "Inundation Depth", xlab = "Date", ylab = "Depth (cm)", type = "l", lwd = 2)
      abline(v = dateHatched, col = "red", lwd = 2, lty = 2)
      plot(hoboData[["Date_Time"]], hoboData[["Inund_Severity"]], main = "Inundation Severity", xlab = "Date", ylab = "Proportion Exposed", ylim = c(0, 1),
           type = "l", lwd = 2)
      abline(v = dateHatched, col = "red", lwd = 2, lty = 2)
      plot(hoboData[["Date_Time"]], hoboData[["Temp_C"]], main = "HOBO Temperature", xlab = "Date", ylab = "Temperature (°C)", type = "l", lwd = 2)
      abline(v = dateHatched, col = "red", lwd = 2, lty = 2)
      legend("topleft", legend = c("Hatch Date"), col = "red", lty = 2, lwd = 2, bty = 'n')

    }

  }

}

#' Inundation summary table
#'
#' @description Generates a table summarizing inundation exposure across the logger deployment
#' @param HOBO The processed data to be summarized - Defaults to "hoboData" output by Clean_Depth_Data()
#' @return A table summarizing the length of the observation period, number of inundation events, maximum depth of inundation, various duration factors, and measures of inundation severity
#' @examples Summary_Table(HOBO = hoboData)
#' @export
#'
Summary_Table <- function(HOBO = hoboData){

  Data <- HOBO # Make a copy of the data to work with

  obsLength <- as.numeric(round(tail(Data$Date_Time, 1) - head(Data$Date_Time, 1), 1)) # Length of observation window (days)
  obsMaxDepth <- round(max(Data$Depth_cm, na.rm = TRUE), 1) # Maximum observed water level above the logger (cm)
  obsMaxProp <- max(Data$Inund_Severity, na.rm = TRUE)*100 # Maximum proportion of the nest chamber affected by inundation (%)
  nInundations <- ifelse(sum(inundationDuration) == 0, 0, length(inundationDuration)) # Number of separate inundation events
  maxDuration <- round(max(inundationDuration), 1) # Duration of longest individual inundation event (hrs)
  avgDuration <- round(mean(inundationDuration), 1) # Average duration across all inundation events (hrs)
  sumDuration <- round(sum(inundationDuration, na.rm = TRUE), 1) # Cumulative duration across all inundation events (hrs)
  avgSeverity <- round(mean(inundationSeverity), 3) * 100 # Average proportion of nest chamber affected across all inundation events
  CISI_hr <- sum(Data$Inund_Severity, na.rm = TRUE) # Cumulative inundation stress index across all inundation events (NOTE DAILY VS HOURLY DIFFERENCE COMPARED TO PVC CISI)
  # Can also be calculated as DescTools::AUC(c(1:length(Data$Inund_Severity)), Data$Inund_Severity, method = "trapezoid",
  # absolutearea = TRUE, na.rm = TRUE)
  # COMPARING DAILY VS HOURLY-BASED CISI VALUES IS ILL-ADVISED DUE TO TIDAL FLUCTUATIONS NOT RECORDED IN DAILY MEASUREMENTS.
  # THUS CISI VALUES WILL ALWAYS BE DRASTICALLY UNDER-ESTIMATED IF YOU ATTEMPT TO TRANSFORM HOURLY INTO DAILY VALUES (i.e., divide by 24)

  # Merge into table for easy viewing
  Table_Obs <<- NULL
  Table_Obs <<- data.frame(cbind(obsLength, obsMaxDepth, nInundations, obsMaxProp, maxDuration, sumDuration, avgDuration, avgSeverity,
                                 CISI_hr))
  colnames(Table_Obs) <<- c("Observation Period (days)", "Maximum Depth (cm)", "Number of Events", "Maximum Severity (%)",
                            "Maximum Duration (single event, hrs)", "Cumulative Duration (hrs)", "Average Duration (hrs)",
                            "Average Severity (%)", "CISI_hr")
  rownames(Table_Obs) <<- nestID
  return(Table_Obs)

}

#' Inundation event table
#'
#' @description Generates a table detailing of each inundation event
#' @param HOBO The processed data to be summarized - Defaults to hoboData output by Clean_Depth_Data()
#' @return A table reporting the start/end, duration, various severity indicators, proportion of incubation completed, and average temperature per inundation event
#' @examples Event_Table(HOBO = hoboData)
#' @export
#'
Event_Table <- function(HOBO = hoboData){

  Data <- HOBO # Make a copy of data for processing

  # Calculate the number of days since deposition at which inundation events occurred
  eventDaysDeposit <- NULL
  if(lubridate::is.POSIXct(inundationStart) == FALSE){
    eventDaysDeposit <- NA # If no inundations occurred, return NA
  } else {
    eventDaysDeposit <- round(as.numeric(difftime(inundationStart, dateLaid, units = "days")), 1)
  }

  # Calculate proportion of incubation completed at which inundation events occurred based on incubation duration
  eventPropDevelop <- NULL
  if(is.na(dateHatched)){
    eventPropDevelop <- round(eventDaysDeposit/avgIncubationDuration, 3) # If hatch date is unavailable, use average incubation duration
  } else if(lubridate::is.POSIXct(inundationStart) == FALSE){
    eventPropDevelop <- NA # if no inundation occurred, return NA
  } else {
    eventPropDevelop <- round(eventDaysDeposit/as.numeric(difftime(dateHatched, dateLaid, units = "days")), 3)
  }

  # Calculate cumulative inundation stress index per inundation event
  eventCISI_hr <- NULL
  if(lubridate::is.POSIXct(inundationStart) == FALSE){
    eventCISI_hr <- 0 # if no inundations occurred, return 0
  } else {
    for(i in 1:length(inundationStart)){
      eventCISI_hr[i] <- sum(Data[which(Data$Date_Time >= inundationStart[i] &
                                          Data$Date_Time <= inundationEnd[i]),]$Inund_Severity, na.rm = TRUE) # NOTE DAILY VS HOURLY DIFFERENCE COMPARED TO PVC CISI
    }
    # COMPARING DAILY VS HOURLY-BASED CISI VALUES IS ILL-ADVISED DUE TO TIDAL FLUCTUATIONS NOT RECORDED IN DAILY MEASUREMENTS.
    # THUS CISI VALUES WILL ALWAYS BE DRASTICALLY UNDER-ESTIMATED IF YOU ATTEMPT TO TRANSFORM HOURLY INTO DAILY VALUES (i.e., divide by 24)
  }

  # Calculate average temperature at the HOBO logger during each inundation event
  eventTemp <- NULL
  if(lubridate::is.POSIXct(inundationStart) == FALSE){
    eventTemp <- NA # if no inundation occurred, return NA
  } else {
    for(i in 1:length(inundationStart)){
      eventTemp[i] <- round(mean(Data[which(Data$Date_Time >= inundationStart[i] &
                                              Data$Date_Time <= inundationEnd[i]),]$Temp_C, na.rm = TRUE), 2)
    }
  }

  # Add quality flags to alert user to possible interpretation complications
  eventFlags <- NULL
  if(lubridate::is.POSIXct(inundationStart) == FALSE){
    eventFlags <- NA # if no inundation occurred, return NA
  } else {
    for(i in 1:length(inundationStart)){
      if(eventPropDevelop[i] > 1){
        if(is.na(dateHatched)){
          eventFlags[i] <- "After Expected Emergence" # if inundation event occurred after average incubation duration
        } else {
          eventFlags[i] <- "After Observed Emergence" # if inundation event occurred after observed incubation duration
        }
      } else {
        eventFlags[i] <- 0 # Else, no flag
      }
    }
  }

  # Create a table for easier viewing
  Table_Events <<- NULL
  if(lubridate::is.POSIXct(inundationStart) == FALSE){
    Table_Events <<- paste0("No inundation events observed") # Return if no inundation occurred
  } else {
    Table_Events <<- data.frame(cbind(inundationStart, inundationEnd, round(inundationDuration, 2), inundationSeverity * 100, eventCISI_hr,
                                      eventDaysDeposit, eventPropDevelop, eventTemp, eventFlags))
    Table_Events$inundationStart <<- format(as.POSIXct(as.numeric(Table_Events$inundationStart), origin = "1970-01-01", tz = TimeZone),
                                            "%m/%d/%Y %H:%M")
    Table_Events$inundationEnd <<- format(as.POSIXct(as.numeric(Table_Events$inundationEnd), origin = "1970-01-01", tz = TimeZone),
                                          "%m/%d/%Y %H:%M")
    colnames(Table_Events) <<- c("Inundation Start", "Inundation End", "Duration (hrs)", "Maximum Severity (%)", "CISI_hr",
                                 "Days Since Deposition", "Proportion of Development", "Mean Temp (°C)", "Flags")

    Table_Events$`Inundation Start` <- as.POSIXct(Table_Events$`Inundation Start`, format = "%m/%d/%Y %H:%M", tz = TimeZone)
    Table_Events$`Inundation End` <- as.POSIXct(Table_Events$`Inundation End`, format = "%m/%d/%Y %H:%M", tz = TimeZone)
    Table_Events$`Duration (hrs)` <<- as.numeric(Table_Events$`Duration (hrs)`)
    Table_Events$`Maximum Severity (%)` <<- as.numeric(Table_Events$`Maximum Severity (%)`)
    Table_Events$CISI_hr <<- as.numeric(Table_Events$CISI_hr)
    Table_Events$`Days Since Deposition` <<- as.numeric(Table_Events$`Days Since Deposition`)
    Table_Events$`Proportion of Development` <<- as.numeric(Table_Events$`Proportion of Development`)
    Table_Events$`Mean Temp (°C)` <<- as.numeric(Table_Events$`Mean Temp (°C)`)
  }

  return(Table_Events)

}

#' Temperature summary table
#'
#' @description Generates a table summarizing reported temperature across the logger deployment
#' @param HOBO The processed data to be summarized - Defaults to hoboData output by Clean_Depth_Data()
#' @return A table summarizing the length of the observation period and descriptive deployment- and daily-level statistics
#' @examples Temperature_Table(HOBO = hoboData)
#' @export
#'
Temperature_Table <- function(HOBO = hoboData){

  Data <- HOBO # Make a copy for data processing

  obsLength <- as.numeric(round(tail(Data$Date_Time, 1) - head(Data$Date_Time, 1), 1)) # Length of observation record (days)
  obsMeanTemp <- round(mean(Data$Temp_C, na.rm = TRUE), 2) # Average temperature at logger throughout deployment (°C)
  obsSDTemp <- round(sd(Data$Temp_C, na.rm = TRUE), 2) # Standard deviation of temperature at logger throughout deployment (°C)
  obsMaxTemp <- round(max(Data$Temp_C, na.rm = TRUE), 2) # Maximum temperature at logger throughout deployment (°C)
  obsMinTemp <- round(min(Data$Temp_C, na.rm = TRUE), 2) # Minimum temperature at logger throughout deployment (°C)

  # Calculate typical daily temperature values
  Data$Date <- as.Date(Data$Date_Time, tz = TimeZone) # simplify date-time to just date
  DailyValues <- Data %>% # Calculate daily mean, min, max, and range of temperature values per day at logger
    dplyr::group_by(Date) %>%
    dplyr::summarise(Mean = round(mean(Temp_C), 1), Min = min(Temp_C), Max = max(Temp_C), Range = round(max(Temp_C) - min(Temp_C), 2),
              N = dplyr::n_distinct(Date_Time))
  DailyValues <- DailyValues %>% # Remove any days with less than 20 observations as incomplete days
    dplyr::filter(N > 20)

  DailyValues <- as.data.frame(DailyValues) # Transform tibble into dataframe
  colnames(DailyValues) <- c("Date", "Mean", "Min", "Max", "Range", "N")
  dailyMeanMaxTemp <- round(mean(DailyValues$Max, na.rm = TRUE), 2) # Average daily maximum temperature at logger (°C)
  dailyMeanMinTemp <- round(mean(DailyValues$Min, na.rm = TRUE), 2) # Average daily minimum temperature at logger (°C)
  dailyMeanRangeTemp <- round(mean(DailyValues$Range, na.rm = TRUE), 2) # Average daily temperature range at logger (°C)

  # Create table for easier viewing
  Temp_Obs <<- NULL
  Temp_Obs <<- data.frame(cbind(obsLength, obsMeanTemp, obsSDTemp, obsMaxTemp, obsMinTemp, dailyMeanMaxTemp, dailyMeanMinTemp,
                                dailyMeanRangeTemp))
  colnames(Temp_Obs) <<- c("Observation Period (days)", "Mean (°C)", "SD (°C)", "Max (°C)", "Min (°C)", "Daily Mean Max (°C)",
                           "Daily Mean Min (°C)", "Daily Mean Range (°C)")
  rownames(Temp_Obs) <<- nestID
  return(Temp_Obs)

}

#' Export tables to Excel
#'
#' @description Prepares tables for export to Excel for additional reporting or data analysis. Overall inundation summary, individual inundation events, and overall temperature summary details are output to their own sheets. User must separately specify a write function to define the filepath and name.
#' @param FILEPATH Specify the file path (as a text string) where the Excel file should be placed. If not specified, this defaults to the current working directory.
#' @param FILENAME Specify the desired file name including the .xlsx file extension. If not specified, this defaults to "nestID Inundation Data.xlsx"
#' @return Exports summary and per-event tables to separate Excel sheets
#' @examples Export_Tables()
#' @export
#'
Export_Tables <- function(FILEPATH = getwd(), FILENAME = paste0(nestID, " Inundation Data.xlsx")){

  dataset_names <<- NULL

  if(lubridate::is.POSIXct(inundationStart) == FALSE){
    dataset_names <<- list("Summary" = Table_Obs, "Temperature" = Temp_Obs) # If no inundation, only export overall and temperature summaries
  } else { # Else, export overall summary, temperature summary, and individual event data on separate Excel sheets
    Table_Events$`Inundation Start` <- as.POSIXct(Table_Events$`Inundation Start`, format = "%m/%d/%Y %H:%M", tz = TimeZone)
    Table_Events$`Inundation End` <- as.POSIXct(Table_Events$`Inundation End`, format = "%m/%d/%Y %H:%M", tz = TimeZone)
    dataset_names <<- list("Summary" = Table_Obs, "Per Event" = Table_Events, "Temperature" = Temp_Obs)
  }

  openxlsx::write.xlsx(dataset_names, file = paste0(FILEPATH, FILENAME))

}
