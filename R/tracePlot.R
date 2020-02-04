#' tracePlot
#'
#' @description This function plots water depth (averaged across clustered simulations) for selected indicator regions
#' 
#' @param namesMatching       a list of netCDF simulation data (e.g., "datList" object)
#' @param simMeansObject      locations to extract data from; must be class SpatialPointsDataFrame or SpatialPolygonsDataFrame
#' @param title               option to specify the name of target locations (e.g., pts$gage)
#'
#' @return a ggplot object
#'
#' @importFrom plyr ddply
#' @importFrom plyr summarise
#' @importFrom stats sd
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 scale_color_discrete
#' @importFrom ggplot2 theme_classic
#' @importFrom ggplot2 ggtitle
#' @importFrom ggplot2 ylab
#' @importFrom ggplot2 scale_color_discrete
#' @importFrom ggplot2 geom_pointrange
#' @importFrom ggplot2 position_dodge
#' @importFrom ggplot2 facet_wrap
#' @importFrom stats ave
#' 
#' @export
#' 




tracePlot <- function(namesMatching = "[[:alnum:]]", simMeansObject = sim_means, title = "default") {
  if (title %in% "default") {
    titleObject <- paste0("Clustered traces for IRs matching ", namesMatching)
  } else {
    titleObject <- title
  }
  ### plot water depth (averaged across clustered simulations) for a subset of indicator region(s)
  dd.trace <- plyr::ddply(simMeansObject[grep(x = simMeansObject$name, pattern = namesMatching), ], c("cluster", "name", "date"), plyr::summarise, 
                          ave   = mean(value),
                          stdev = stats::sd(value))
  dd.trace$cluster <- as.factor(dd.trace$cluster)
  ggplot2::ggplot(dd.trace, ggplot2::aes(y = ave, x = date)) + ggplot2::theme_classic() + ggplot2::ggtitle(titleObject) + ggplot2::xlab("") + ggplot2::ylab(paste0("Water level (cm)")) + ggplot2::scale_color_discrete(name = "cluster") +
    ggplot2::geom_pointrange(ggplot2::aes(ymin = ave - stdev, ymax = ave + stdev, col = cluster), 
                             position = ggplot2::position_dodge(width = 1)) + 
    ggplot2::facet_wrap(name ~ .)
}

