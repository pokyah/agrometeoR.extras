#####
## utilities functions used inside package functions
## not exposed to the user

# Make a list of records a big dataframe
dataframizeRecords = function(records){
  if (class(records) == "list") {
    records = records %>%
      purrr::reduce(dplyr::bind_rows)
  }
}


# check which sensor are available compared to those wanted
checkSensors = function(records, sensors){
  sensorsInRecords = colnames(records)[colnames(records) %in% sensors]
  stopifnot(length(sensorsInRecords) > 0)
  if (length(sensorsInRecords) < length(sensors)) {
    warning("agrometeor::makeDescStat warning : Not all the sensors are available in the records.",
      "\n",
      "Here are the missing sensors : ",
      paste(sensors[!sensors %in% sensorsInRecords], collapse = ", "),
      ". ",
      "\n",
      "The analysis will ignore these missing sensors. ")
  }
  sensors = sensorsInRecords
}
