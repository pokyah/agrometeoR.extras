#' #' Build a static map displaying predictions and their related error
#' #' @author Loïc Davadan <- ldavadan.github.io
#' #' @param gridded.data.df A data frame obtained from a SpatialGridDataFrame containing data
#' #' @param boundaries.sf A sf containing data from Wallonia boundaries
#' #' @param layer.error.bool A boolean specifying if you want to display the layer with error
#' #' @param legend.error.bool A boolean specifying if you want to display the legend of the error layer
#' #' @param pretty_breaks.bool A boolean specifying the type of legend you want. TRUE for pretty breaks, FALSE for quantile scale
#' #' @param title.chr A character specifying the title you want for your map
#' #' @param target.chr A character specifying the predicted parameter. One of "tsa", "hra" or "hct"
#' #' @param nb_classes.num A numeric for the number of classes you want
#' #' @param reverse_pal.bool A boolean if you want to reverse color palette. Default is TRUE, which means that 'red' is for high values and 'blue' for low values
#' #' @param resolution.chr A character which specifies the resolution of the map
#' #' @return a ggplot map object
#' #' @export
#' build.static.ggmap <- function(
#'   gridded.data.df,
#'   boundaries.sf,
#'   layer.error.bool,
#'   legend.error.bool,
#'   pretty_breaks.bool,
#'   title.chr,
#'   legend.chr,
#'   target.chr,
#'   nb_classes.num,
#'   reverse_pal.bool = TRUE,
#'   resolution.chr = NULL
#' ){
#'
#'   if (pretty_breaks.bool == TRUE) {
#'     # inspired by https://timogrossenbacher.ch/2016/12/beautiful-thematic-maps-with-ggplot2-only/
#'     # prepare legend with pretty breaks
#'     # compute quantiles from predictions values
#'     quantiles <- unique(stats::quantile(gridded.data.df[[target.chr]],
#'       probs = seq(0, 1, length.out = nb_classes.num), na.rm = T))
#'     labels <- c()
#'     breaks <- unique(round(c(min(gridded.data.df[[target.chr]] - 1, na.rm = TRUE),
#'       min(gridded.data.df[[target.chr]], na.rm = TRUE),
#'       quantiles,
#'       max(gridded.data.df[[target.chr]], na.rm = TRUE)), 1))
#'
#'     labels <- paste0(labels, paste0(format(round(breaks, 1), nsmall = 1)))
#'     labels <- labels[2:length(labels)]
#'     gridded.data.df$response_quantiles <- cut(gridded.data.df[[target.chr]],
#'       breaks = breaks,
#'       labels = labels,
#'       include.lowest = T)
#'     breaks_scale <- levels(gridded.data.df$response_quantiles)
#'     labels_scale <- rev(breaks_scale)
#'   }
#'   if (pretty_breaks.bool == FALSE) {
#'     # inspired by https://timogrossenbacher.ch/2016/12/beautiful-thematic-maps-with-ggplot2-only/
#'     quantiles <- unique(stats::quantile(gridded.data.df[[target.chr]],
#'       probs = seq(0, 1, length.out = nb_classes.num), na.rm = T))
#'     labels <- c()
#'     labels <- paste0(labels, paste0(format(round(quantiles, 1), nsmall = 1),
#'       " – ",
#'       format(round(quantiles[2:length(quantiles)], 1), nsmall = 1)))
#'     labels <- labels[1:length(labels) - 1]
#'     gridded.data.df$response_quantiles <- cut(gridded.data.df[[target.chr]],
#'       breaks = quantiles,
#'       labels = labels,
#'       include.lowest = T)
#'   }
#'
#'   pal = RColorBrewer::brewer.pal(n = length(labels_scale), name = "RdYlBu")
#'   if(reverse_pal.bool == TRUE){
#'     pal = rev(pal)
#'   }
#'
#'   ggmap <- ggplot2::ggplot(gridded.data.df) +
#'     # choose data to display on the layer
#'     ggplot2::geom_raster(mapping = ggplot2::aes(coords.x1, coords.x2, fill = response_quantiles), na.rm = TRUE, interpolate = T)
#'   # choose color palette and create a legend with pretty breaks
#'   if (pretty_breaks.bool == TRUE) {
#'     ggmap <- ggmap +
#'       ggplot2::scale_fill_manual(
#'         values = pal, # palette to use
#'         breaks = rev(breaks_scale), # legend breaks
#'         name = legend.chr,
#'         drop = FALSE,
#'         labels = labels_scale, # legend labels
#'         # legend parameters
#'         guide = ggplot2::guide_legend(
#'           direction = "vertical",
#'           keyheight = grid::unit(7, units = "mm"),
#'           keywidth = grid::unit(3, units = "mm"),
#'           title.position = 'top',
#'           title.vjust = 0.5,
#'           label.vjust = 1,
#'           ncol = 1,
#'           bycol = T,
#'           reverse = F,
#'           label.position = "right"
#'         )
#'       )
#'   }
#'   # color palette with discrete classes with quantile scale
#'   if(pretty_breaks.bool == FALSE){
#'     ggmap <- ggmap +
#'       ggplot2::scale_fill_brewer(legend.chr, palette = "RdYlBu", direction = -1)
#'   }
#'
#'   if(layer.error.bool == TRUE){
#'     ggmap <- ggmap +
#'       # display a layer with standard error
#'       ggplot2::geom_raster(ggplot2::aes(coords.x1, coords.x2, alpha = se), fill = "white", na.rm = TRUE, interpolate = TRUE) +
#'       # whitening it
#'       ggplot2::scale_alpha_continuous("Standard\nError",range = c(0.1,1), guide = legend.error.bool)
#'     # order the two legends if they both are displayed
#'     if(legend.error.bool == TRUE){
#'       ggmap <- ggmap + ggplot2::guides(fill = ggplot2::guide_legend(order = 1),
#'         alpha = ggplot2::guide_legend(order = 0))
#'     }
#'
#'   }
#'   ggmap <- ggmap +
#'     ggplot2::ggtitle(title.chr) +   # add title
#'     # add boundaries layer
#'     ggplot2::geom_sf(data = boundaries.sf, ggplot2::aes(fill = ISO), fill = NA, color = "black", size = 0.6) +
#'     # add north symbol
#'     ggsn::north(data = boundaries.sf, scale = 0.1, location = "bottomleft",
#'       anchor = c(x = 780000, y = 550000), symbol = 12) +
#'     # add scalebar
#'     ggsn::scalebar(data = boundaries.sf, dist = 50, dd2km = FALSE, model = "GRS80",
#'       st.dist = 0.03, st.size = 4, anchor = c(x = 700000, y = 520000)) +
#'
#'     # add copyright
#'     ggplot2::annotation_custom(grob = grid::textGrob("© CRA-W"),
#'       xmin = 790000, xmax = 790000, ymin = 520000, ymax = 520000)
#'   # display resolution of the map
#'   if(is.null(resolution.chr) == FALSE){
#'     ggmap <- ggmap +
#'       ggplot2::annotation_custom(grob = grid::textGrob(resolution.chr),
#'         xmin = 558000, xmax = 558000, ymin = 671000, ymax = 671000)
#'   }
#'
#'   # parameters for visualization
#'   ggmap <- ggmap +
#'     ggplot2::theme(panel.background = ggplot2::element_rect(fill = "white"),
#'       axis.title = ggplot2::element_text(color = NA),
#'       panel.grid = ggplot2::element_line(color = NA),
#'       axis.ticks = ggplot2::element_line(color = NA),
#'       axis.text = ggplot2::element_text(colour = NA),
#'       legend.title = ggplot2::element_text(size = 12, face = "bold", vjust = 1),
#'       legend.text = ggplot2::element_text(size = 11),
#'       legend.background = ggplot2::element_rect(fill = "transparent"),
#'       legend.position = c(0.12,0.38),
#'       legend.box = "horizontal")
#'   ggmap
#' }
#'
#' #' #' #' @export
#' #' #' #' @title make an interactive leaflet map of your sptialized data
#' #' #' #' @author Thomas Goossens
#' #' #' #' @import leaflet
#' #' #' #'
#' #' #' #'
#' #' #' #'
#' #' #' #'https://stackoverflow.com/questions/28665918/create-square-polygons-from-single-centre-coordinates-and-area-in-r
#' #' # https://github.com/ldavadan/agromet_test/blob/master/R/create_map.R
#' #'
#' #' stations = mlr::getTaskData(ex_makeTask$output$value$task)
#' #' spatialized = ex_makeSpatialization$output$value$spatialized
#' #' #'
#' #' # Definition of the function to build a leaflet map for prediction with associated uncertainty
#' #'
#' #' target = "tsa"
#' #' grid = grid.df
#' #' spatialized
#' #' key = "px"
#' #' grid.coords = c("X", "Y")
#' #' stations.coords = c("x", "y")
#' #' crs = 3812
#' #'
#' #' # Find the positions
#' #' spatialized = dplyr::left_join(spatialized, grid.df, by = key)
#' #'
#' #' # find the lower x position for later offset :
#' #' offset.x =  min(spatialized[grid.coords[1]])
#' #' offset.y =  min(spatialized[grid.coords[2]])
#' #'
#' #' # make it a spatial object
#' #' spatialized = sf::st_set_crs(sf::st_as_sf(spatialized, coords = grid.coords), crs)
#' #' stations = sf::st_set_crs(sf::st_as_sf(stations, coords = stations.coords), crs)
#' #'
#' #' ## create polygonized prediction data based on the centroids of the grid
#' #' # bimgrid = sf::st_make_grid(x = spatialized, cellsize = 1000, what = "polygons", offset = c(offset.x, offset.y))
#' #' # # inject data
#' #' # bimGrid = st_sf(bimgrid) %>%
#' #' #   sf::st_join(spatialized)
#' #' # bimGrid = na.omit(bimGrid)
#' #' #
#' #' # spatialized = bimGrid
#' #'
#' #' # be sure we are in the proper 4326 EPSG
#' #' spatialized = sf::st_transform(spatialized, 3812)
#' #' stations = sf::st_transform(stations, 3812)
#' #'
#' #' create_map_tsa <- function(
#' #'   spatial_data.sp = NULL,
#' #'   method.chr = NULL,
#' #'   type.chr = NULL,
#' #'   logo = "~/Desktop/craw.png"){
#' #'
#' #'   browser()
#' #'
#' #'   static <- tmap::tm_shape(spatial_data.sp, projection="3812") +            # Projection : Belgian Lambert
#' #'     tmap::tm_raster("response",                                             # spatialize temperature
#' #'       palette = "-RdYlBu",
#' #'       title = "Temperature (°C)",
#' #'       auto.palette.mapping=FALSE,
#' #'       breaks = c(stats::quantile(spatial_data.sp$response, 0, na.rm = TRUE),
#' #'         stats::quantile(spatial_data.sp$response, 0.1, na.rm = TRUE),
#' #'         stats::quantile(spatial_data.sp$response, 0.2, na.rm = TRUE),
#' #'         stats::quantile(spatial_data.sp$response, 0.3, na.rm = TRUE),
#' #'         stats::quantile(spatial_data.sp$response, 0.4, na.rm = TRUE),
#' #'         stats::quantile(spatial_data.sp$response, 0.5, na.rm = TRUE),
#' #'         stats::quantile(spatial_data.sp$response, 0.6, na.rm = TRUE),
#' #'         stats::quantile(spatial_data.sp$response, 0.7, na.rm = TRUE),
#' #'         stats::quantile(spatial_data.sp$response, 0.8, na.rm = TRUE),
#' #'         stats::quantile(spatial_data.sp$response, 0.9, na.rm = TRUE),
#' #'         stats::quantile(spatial_data.sp$response, 1, na.rm = TRUE))) +
#' #'     tmap::tm_compass(position = c(0.9,0.15), color.light = "grey20") +      # north
#' #'     tmap::tm_scale_bar(breaks = NULL, width = NA, size = 0.8, text.color = "grey20",  # scale bar
#' #'       color.dark = "grey20", color.light = "white", lwd = 1, position = c(0.22,0.01),
#' #'       just = NA) +
#' #'     tmap::tm_logo(logo, height = 2, halign = "center", margin = 0.2,        # CRA-W logo
#' #'       position = c(-0.01,-0.04), just = NA) +
#' #'     tmap::tm_shape(wallonia) +                                      # outline of Wallonia
#' #'     tmap::tm_borders("grey20", lwd = 1.5) +
#' #'     tmap::tm_layout(legend.position = c(0.01,0.14),                         # parameters
#' #'       legend.height = 0.55,
#' #'       legend.text.size = 0.7,
#' #'       legend.bg.color = "white",
#' #'       legend.title.size = 0.9,
#' #'       inner.margins = c(0.03, 0.03, 0.07, 0.03),
#' #'       frame.lwd = 0,
#' #'       bg.color = "grey85",
#' #'       main.title = base::paste("Interpolated temperature with ", method.chr),
#' #'       title = "Resolution : 1 km²",
#' #'       title.size = 0.6,
#' #'       title.position = c(0.01, 0.96)) +
#' #'     tmap::tm_credits("© CRA-W", position = c(.87, 0))
#' #'
#' #'   if(type.chr == "static") {
#' #'     return(static)
#' #'   }else{
#' #'     interactive <- tmap::tmap_leaflet(static)
#' #'     return(interactive)
#' #'   }
#' #' }
