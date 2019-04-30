#' @export
#' @title make plots for a sensor of a station for a specific period of time
#' @author Thomas Goossens
#' @import mlr
#' @import ggplot2
#' @importFrom lubridate hour
#' @importFrom lubridate date
#' @importFrom lubridate week
#' @importFrom lubridate month
#' @importFrom magrittr %>%
#' @param records a list of hourly dataframes or a single dataframe ::TODO ::Must contain sunset and sunrise !
#' @param dfrom a character
#' @param dto a character
#' @param sensor (optional) a character vector specifying the name of the sensors you want to get the descriptive statistics.
#' If not provided, the function will compute the plots for all the sensors available in your records dataset. These must be one of c("tsa", "plu", "ens", "vvt", "hct")
#' @param combs (optional) a named list which elements are chatacter vectors containing the sid's combinations of the stations you want to put on the same plot.
#' If not provided, the function will create a single plot for each sid and will not compute blandAltman and scatter
#' @param grouping (optional) a character specifying how you want to group points on scatter and blandAltman plots. One of lubridate functions : "month", "week","as_date", "hour".
#' if not provided, the function will choose the most appropriate according to the whole timeframe of your records
#' @return a list
#'
#' records = makeDataset(dfrom = "2016-02-04T15:00:00Z", dto = "2016-03-04T15:00:00Z", sensor = c("tsa", "vvt"), stations = stations.df$sid)
makeStationPlots = function(
  records,
  dfrom = NULL,
  dto = NULL,
  interactive = TRUE,
  sensors = NULL,
  combs = NULL,
  grouping = NULL){

  # from list of df's to big df
  records = dataframizeRecords(records)

  # filter according to dfrom and dto
  records = records %>%
    dplyr::filter(mtime >= as.POSIXct(dfrom)) %>%
    dplyr::filter(mtime <= as.POSIXct(dto))

  # extracting the unique sids
  sids = as.character(unique(records$sid))

  # check if sids available are compatible with the combs ::TODO::
  # sensors = checkSensors(records, sensors)
  if (!missing(sensors)) {
    records_sensors = sensors
  } else{
    records_sensors = colnames(records)[colnames(records) %in% c("tsa", "plu", "ens", "vvt", "hct")]
  }
  records_sensors = as.list(records_sensors)
  names(records_sensors) = records_sensors

  # settings the sids as factor for proper plotting
  records = records %>%
    dplyr::mutate(sid = as.factor(sid))

  getSensorPlots = function(records_sensor){

    # if not combs provided, create the list of each unique sid
    if (is.null(combs)){
      combs = as.list(unique(records$sid))
      names(combs) = unique(records$sid)
    }

    # iterate on each comb
    lapply(combs, function(comb){

      # instancing the plot list
      plots = list(density = NULL,
        timeSerie = NULL,
        scatter = NULL,
        blandAltman = NULL
      )

      # filter the records to only keep the sids that correspond to the comb
      records = records %>%
        dplyr::filter(sid %in% comb)

      # title for the plots
      title = paste0(records_sensor, " (", min(records$mtime), " ==> ", max(records$mtime), ")")

      # density
      plots$density = records %>%
        ggplot2::ggplot(data = .,
          x = records$mtime,
          y = records[records_sensor]) +
        aes_string(
          x = records_sensor,
          colour = "sid") +
        geom_density() +
        labs(caption = title) +
        theme(plot.caption = element_text(hjust = 0.5, size = rel(1.2)))

      # timeSerie
      plots$timeSerie = records %>%
        ggplot(data = .,
          x = records$mtime,
          y = records[records_sensor]) +
        aes_string(
          x = "mtime",
          y = records_sensor,
          colour = "sid"
        ) +
        geom_line() +
        labs(caption = title) +
        theme(plot.caption = element_text(hjust = 0.5, size = rel(1.2)))

      # if comb is only 2 stations then make scatter and BA
      if (length(comb) == 2) {

        if (is.null(grouping)) {
          # adding a color factor column for plots that depends of the time extent of the records.
          if (length(unique(lubridate::as_date(records$mtime))) > 28) {
            lubridate_TimeGroup = "month"
          }
          else {
            if (length(unique(lubridate::as_date(records$mtime))) > 10) {
              lubridate_TimeGroup = "week"
            }
            else {
              if (length(unique(lubridate::as_date(records$mtime))) > 1) {
                lubridate_TimeGroup = "as_date"
              } else{
                lubridate_TimeGroup = "hour"
              }
            }
          }
        }else {
          lubridate_TimeGroup = grouping
        }

        records = records %>%
          dplyr::mutate(timeGroup = as.factor(get(lubridate_TimeGroup)(mtime)))
        colnames(records)[colnames(records) == "timeGroup"] = lubridate_TimeGroup

        if ("as_date" %in% colnames(records)) {
          colnames(records)[colnames(records) == "as_date"] = "date"
          lubridate_TimeGroup = "date"
        }

        # putting in wide format for scatter & blandAltman
        recordsWide = records %>%
          dplyr::select(one_of(c("mtime", lubridate_TimeGroup, "sid", records_sensor))) %>%
          tidyr::spread_("sid", records_sensor) %>%
          dplyr::rename_at(vars(comb),function(x) paste0("station ", x))

        # scatter plot
        plots$scatter = recordsWide %>%
          ggplot(
            data = .,
            aes_string(
              x = as.name(names(recordsWide)[3]),
              y = as.name(names(recordsWide)[4]),
              colour = as.name(names(recordsWide)[2])
            )) +
          geom_point() +
          geom_smooth(method = lm, color = "black", fill = "blue") +
          labs(caption = title) +
          theme(plot.caption = element_text(hjust = 0.5, size = rel(1.2)))

        # bland-Altman plot
        blandAlt = BlandAltmanLeh::bland.altman.stats(
          dplyr::select(recordsWide, paste0("station ", comb[1]))[[1]],
          dplyr::select(recordsWide, paste0("station ", comb[2]))[[1]])
        blandAltDf = data.frame(means = blandAlt$means, diffs = blandAlt$diffs)
        blandAltDf = data.frame(dplyr::bind_cols(blandAltDf, recordsWide))

        plots$blandAltman = blandAltDf %>%
          ggplot(
            aes_string(
              x = as.name(names(blandAltDf)[1]),
              y = as.name(names(blandAltDf)[2]),
              colour = as.name(names(blandAltDf)[4]))
          ) +
          #colour = as.name(colnames(blandAltDf)[which(names(blandAltDf) %in% scatterColors)])) +
          geom_point() +
          geom_smooth(method = glm, se = TRUE, color = "black", linetype = "dashed") +
          geom_hline(yintercept =  0, color = "black", size = 0.5) +
          geom_hline(yintercept = blandAlt$lines[2], color = "red", size = 0.5) +
          geom_hline(yintercept = blandAlt$lines[1], color = "blue", size = 0.5) +
          geom_hline(yintercept = blandAlt$lines[3], color = "blue", size = 0.5) +
          geom_hline(yintercept = blandAlt$CI.lines[1], linetype = "dashed", color = "blue", size = 0.5) +
          geom_hline(yintercept = blandAlt$CI.lines[2], linetype = "dashed", color = "blue", size = 0.5) +
          geom_hline(yintercept = blandAlt$CI.lines[5], linetype = "dashed", color = "blue", size = 0.5) +
          geom_hline(yintercept = blandAlt$CI.lines[6], linetype = "dashed", color = "blue", size = 0.5) +
          geom_hline(yintercept = blandAlt$CI.lines[3], linetype = "dashed", color = "red", size = 0.5) +
          geom_hline(yintercept = blandAlt$CI.lines[4], linetype = "dashed", color = "red", size = 0.5) +
          labs(caption = title) +
          theme(plot.caption = element_text(hjust = 0.5, size = rel(1.2)))
      }

      # only keep the non-null plots
      plots = plots[lengths(plots) != 0]

      # interactivity ?
      if (isTRUE(interactive)) {
        plots = lapply(plots, plotly::ggplotly)
      }
      # return everything
      return(plots)
    })
  }

  plotsBySensor = lapply(records_sensors, getSensorPlots)
  names(plotsBySensor) = records_sensors
  return(plotsBySensor)
}