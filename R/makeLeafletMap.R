#' @export
#' @title make an interactive leaflet map of your spatialized data
#' @author Thomas Goossens
#' @import leaflet
#' @import leaflet.extras
#' @import htmltools
#' @import agrometeoR
#' @param target a character specifying the sensor your want to map
#' @param spatialized a dataframe containing the spatialized data and their px reference
#' @param polygon_grid a sf object containing the polygonized grid and the px references
#' @param stations a dataframe containing the stations observations of the sensor
#' @param key a character specifying the key used to match the spatialized dataframe with the polygonized grid
#' @param stations_coords a character vector specifying the name of the coordinates columns of the stations dataframe
#' @param crs a numeric corresponding to the EPSG code of the stations spatial coordinates
#' @examples
#' myDataset = makeDataset(
#'   dfrom = "2017-03-04T15:00:00Z",
#'   dto = "2017-03-04T15:00:00Z",
#'   sensor = "tsa")
#' myTask = makeTask(dataset = myDataset$output$value, target = "tsa")
#' myModel = makeModel(
#'   task = mytask$out$value,
#'   learner = learners$baseLearners$lrn.lm.alt)
#' mySpatialization = makeSpatialization(model = myModel$output$value)
#' lmap = makeLeafletMap(target = "tsa", spatialized = ex_makeSpatialization$output$value$spatialized, stations = stations = ex_bad_makeDataset[[1]])
#'https://stackoverflow.com/questions/28665918/create-square-polygons-from-single-centre-coordinates-and-area-in-r
# https://github.com/ldavadan/agromet_test/blob/master/R/create_map.
# https://stackoverflow.com/questions/33084728/plotting-shp-file-in-leaflet-works-in-ggplot

makeLeafletMap = function(
  target,
  spatialized,
  polygon_grid = agrometeoR::grid.squares.sf,
  stations_data,
  stations_meta = agrometeoR::stations.df,
  key_grid = "px",
  key_stations = "sid",
  stations_coords = c("x", "y"),
  crs = 3812,
  title
){

  require(sf)

  browser()

  # injecting the spatialized data into the polygon grid ::todo:: check for same nrow
  spatialized = dplyr::left_join(polygon_grid, spatialized, by = key_grid)

  # injecting the station meta information into the passed stations dataframe
  stations = stations_data %>%
    dplyr::left_join(stations_meta, by = c(key_stations, "x", "y", "elevation"))

  # making the stations data a spatial object
  stations = sf::st_set_crs(sf::st_as_sf(stations, coords = stations_coords), crs)

  # be sure we are in the proper 4326 EPSG => ::TODO:: must be a warning
  spatialized = sf::st_transform(spatialized, 4326)
  stations = sf::st_transform(stations, 4326)

  # to make the map responsive
  responsiveness = "\'<meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">\'"

  # Sometimes the interpolation and the stations don't have values in the same domain.
  # this lead to mapping inconsistency (transparent color for stations)
  # Thus we create a fullDomain which is a rowbinding of interpolated and original data
  fullDomain = c(spatialized$response, stations[[target]])

  # defining the color palette for the response
  varPal <- leaflet::colorNumeric(
    palette = "RdYlBu", #"RdBl",
    reverse = TRUE,
    domain = fullDomain, #spatialized$response,
    na.color = "black"
  )

  # Definition of the function to create whitening
  alphaPal <- function(color) {
    alpha <- seq(0,1,0.1)
    r <- col2rgb(color, alpha = T)
    r <- t(apply(r, 1, rep, length(alpha)))
    # Apply alpha
    r[4,] <- alpha*255
    r <- r/255.0
    codes <- (rgb(r[1,], r[2,], r[3,], r[4,]))
    return(codes)
  }

  # actually building the map
  prediction.map = leaflet::leaflet(spatialized) %>%
    # basemaps
    addProviderTiles(group = "Stamen",
      leaflet::providers$Stamen.Toner,
      options = providerTileOptions(opacity = 0.25)
    ) %>%
    addProviderTiles(group = "Satellite",
      leaflet::providers$Esri.WorldImagery,
      options = providerTileOptions(opacity = 1)
    ) %>%
    # centering the map
    fitBounds(sf::st_bbox(spatialized)[[1]],
      sf::st_bbox(spatialized)[[2]],
      sf::st_bbox(spatialized)[[3]],
      sf::st_bbox(spatialized)[[4]]
    ) %>%
    # adding title
    addControl(tags$div(
      HTML(paste0(title))
    )  , position = "bottomleft") %>%
    # adding layer control button
    addLayersControl(baseGroups = c("Stamen", "Satellite"),
      # overlayGroups = c("prediction"),
      overlayGroups = c(paste0(target, ".pred"), "se", "Stations", "Admin"),
      options = layersControlOptions(collapsed = TRUE)
    ) %>%
    # fullscreen button
    leaflet.extras::addFullscreenControl() %>%
    # location button
    addEasyButton(easyButton(
      icon = "fa-crosshairs", title = "Locate Me",
      onClick = JS("function(btn, map){ map.locate({setView: true}); }"))) %>%
    htmlwidgets::onRender(paste0("
      function(el, x) {
      $('head').append(",responsiveness,");
      }")
    ) %>%
    # admin boundaries
    # addPolygons(
    #   data = wallonia,
    #   group = "Admin",
    #   color = "#444444", weight = 1, smoothFactor = 0.5,
    #   opacity = 1, fillOpacity = 0, fillColor = FALSE) %>%
    # predictions
    addPolygons(
      group = paste0(target, ".pred"),
      color = "#444444", stroke = FALSE, weight = 1, smoothFactor = 0.8,
      opacity = 1.0, fillOpacity = 1.0,
      fillColor = ~varPal(response),
      label = ~ paste(
        "prediction:", format(round(spatialized$response, 2), nsmall = 2),
        "°C ",
        "se: ", format(round(spatialized$se, 2), nsmall = 2)),
      # popup = ~as.character(response),
      highlightOptions = highlightOptions(color = "white", weight = 2,
        bringToFront = TRUE)
    ) %>%
    # observations (stations)
    addCircleMarkers(
      data = stations,
      group = "Stations",
      color = "black",
      weight = 2,
      fillColor = ~varPal(tsa),
      stroke = TRUE,
      fillOpacity = 1,
      label = ~htmltools::htmlEscape(paste0(poste, " (", sid, ") - ", tsa, "°C"))) %>%
    # layer opacity slider :: https://cran.r-project.org/web/packages/leaflet.opacity/vignettes/leaflet-opacity.html
    # leaflet.opacity::addOpacitySlider(layerId = paste0(target, ".pred")) %>%
    addLegend(
      position = "bottomright", pal = varPal, values = ~response,
      title = paste0(target, ".pred"),
      group = paste0(target, ".pred"),
      opacity = 1
    )

  # if se.bool = TRUE
  # if (!is.null(spatialized$se)) {
  #   uncPal <- leaflet::colorNumeric(
  #     palette = alphaPal("#5af602"),
  #     domain = spatialized$se,
  #     alpha = TRUE
  #   )
  #
  #   prediction.map = prediction.map %>%
  #     addPolygons(
  #       group = "se",
  #       color = "#444444", stroke = FALSE, weight = 1, smoothFactor = 0.5,
  #       opacity = 1.0, fillOpacity = 1,
  #       fillColor = ~uncPal(se),
  #       highlightOptions = highlightOptions(color = "white", weight = 2,
  #         bringToFront = TRUE),
  #       label = ~ paste("prediction:", signif(spatialized$response, 2), "\n","se: ", signif(spatialized$se, 2))
  #     ) %>%
  #     addLegend(
  #       group = "se",
  #       position = "bottomleft", pal = uncPal, values = ~se,
  #       title = "se",
  #       opacity = 1
  #     )
  # }

  return(prediction.map)


}



