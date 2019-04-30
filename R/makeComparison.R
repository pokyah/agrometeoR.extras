#' @export
#' @title make a comparison between 2 stations
#' @author Thomas Goossens
#' @import mlr
#' @import ggplot2
#' @importFrom magrittr %>%
#' @param dataset a dataframe containing the records from the 2 stations you want to compare. ::TODO ::Must contain sunset and sunrise !
#' @param sensor a character specifying the name of the sensor you want to compare
#' @param sid a numeric vector specifying the sids you want to compare
#' @return a list which elements are objects of class mlr::benchmark()

makeComparison = function(
  dataset,
  sensor,sids, dfrom, dto){

  dataset = dataset %>%
    dplyr::filter(sid %in% sids) %>%
    dplyr::filter(mtime >= as.POSIXct(dfom)) %>%
    dplyr::filter(mtime <= as.POSIXct(dto))


  # removing potential NA values and raise warning
  logNa = dataset[rowSums(is.na(dataset[sensor])) > 0,]
  datasetDfNoNa = dataset[rowSums(is.na(dataset[sensor])) == 0,]
  if (!is.null(nrow(logNa))) {
    warning("Your dataset contains NA values that will be removed from the comparison.
      These values were logged and are accessible")
  }

  # keeping only the commonly shared mtime by the 2 stations
  datasetDfclean = subset(dataset, !as.character(mtime) %in% as.character(logNa$mtime))

  # adding day or night
  # datasetDfDay = data.frame(
  #   datasetDfclean %>%
  #     rowwise() %>% dplyr::mutate(day = am_returnDayState(mtime, sunrise, sunset)))


  # compute the classic plots
  output_plots = lapply(sensors, function(x){
    am_makePlots(datasetDfDayDmax, x, TRUE)
  })
  names(output_plots) = sensors

}


# declaration of the function to return a boolean for day state
am_returnDayState = function(mtime, sunrise, sunset){
  if (times(strftime(mtime,"%H:%M:%S")) >= sunrise && chron::times(strftime(mtime, format = "%H:%M:%S")) <= sunset){
    day = TRUE
  } else{
    day = FALSE
  }
  return(day)
}





