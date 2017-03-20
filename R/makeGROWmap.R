#' @title makeGROWmap
#'
#' @description
#' \code{makeGROWmap} takes a dataset with values and a GROW column and makes a map in \code{\link{ggplot2}}. 
#' Code is modified and extended from @jebyrnes package meowR {\link{https://github.com/jebyrnes/meowR}}.
#' 
#' @details  \code{makeGROWmap} takes a dataset with values and a GROW column and makes a map in \code{\link{ggplot2}}
#'
#' @author Sarah Supp.
#' @param newdata dataset to be used
#' @param fillColName name of the column with information to be used for a fill value
#' @param regionColName data name of column with ecoregion, province, or realm names
#' @param type are these values from an ECOREGION, PROVINCE, or REALM map. Defaults to "ECOREGION". Note all caps.
#' @param realm What geographic realm are the data from (Marine, Freshwater, Terrestrial, Hotspot, IPBES)?
#' @param noAxisLabels Should axis labels for latitude and longitude be plotted. Defaults to TRUE
#' @param fillPal The palatte used by \code{\link{scale_fill_gradientn}}. Defaults to \code{\link{brewer.pal}(11, "Spectral")}
#' @param pal The palatte used by \code{\link{scale_fill_manual}} if data is categorical. Defaults to "spectral" from \code{\link{RColorBrewer}}
#' @param pathCol The path color for regional outlines. Defaults to "black"
#' @param pathAlpha The alpha of the regional outlines. Defaults to 1.
#' @param guide What kind of guide should be used, and what should it's title be? Defaults to \code{\link{guide_colourbar}(title=fillColName)}
#' @param dataOut Return a merged dataframe for plotting instead of a plot? Defaults to \code{FALSE}
#' @param na.value How are NAs areas handled for fill? Defaults to making them not plot - NA.
#' @param add.worldmap Should a map of the world be plotted under the regions? Defaults to FALSE
#' @param fillAlphaColName Colname of column used to scale alpha level of fill
#' @param excludeNoDataAreas Exclude areas/regions from the plot with no data?
#' @param prevggplot A ggplot argument that this plot will be added on top of. If null, a new
#' ggplot object is created.
#' @param ... Other arguments to be supplied to color scale
#' 
#' 
#' @export
#' @return Returns \code{\link{ggplot}} object
#' 
#'
#' @examples
#' 
#' data(regions.df)
#' 
#' ndf <- data.frame(Ecoregions = levels(regions.df$ECOREGION), 
#' Values = runif(length(levels(regions.df$ECOREGION)), 0,100))
#' 
#' makeGROWmap(ndf, fillColName="Values", regionColName="Ecoregions")
#' 

###Function to make a map for provinces or regions
# Takes a dataframe as an argument. You specify the column that has the region
# in it - be it ECOREGION, ecoregion_name, or whatever. You also specify the 
# geographic realm to be mapped - this determines which dataset for the function
# to find and source boundaries from.
# type is whether this is an ECOREGION, PROVINCE, or REALM map where
# the regionColName's values match the appropriate type of map
# and fillColName is the name of the column that will determine the fill color

# Function to map values into Geographic Regions Of the World (GROW), depending on the realm (modified from makeMEOWmap)
makeGROWmap <- function (newdata, fillColName, regionColName = type, type = "ECOREGION", realm = "Marine", noAxisLabels = T,
                         fillPal = brewer.pal(11, "Spectral"), pal = "Spectral", pathCol = "black", 
                         pathAlpha = 1, guide = guide_colourbar(title = fillColName), 
                         dataOut = FALSE, na.value = NA, add.worldmap = FALSE, fillAlphaColName = NULL, 
                         excludeNoDataAreas = T, prevggplot = NULL, ...) 
{
  regionData <- makeGROWmapData(newdata, fillColName, fillAlphaColName, 
                                regionColName, type, realm, excludeNoDataAreas)
  if (dataOut) 
    return(regionData)
  if (is.null(prevggplot)) 
    prevggplot <- ggplot()
  if (add.worldmap) {
    ret <- prevggplot + geom_path(data = worldmap.df, aes(x = longitude, y = latitude, group = NA), color="gray70") 
  }
  ret <- ret + theme_bw(base_size = 17) + 
    geom_polygon(data = regionData, mapping = aes(x = long, y = lat, fill = score, group = group, alpha = fillAlpha)) + 
    geom_path(data = regionData, color = pathCol, alpha = pathAlpha, mapping = aes(x = long, y = lat, group = group)) + 
    coord_equal()
  if (is.numeric(regionData$score)) {
    ret <- ret + scale_fill_gradientn(colours = fillPal, guide = guide, na.value = na.value, ...)
  }
  else {
    ret <- ret + scale_fill_manual(values = fillPal, guide = guide, na.value = na.value, ...)
  }
  if (noAxisLabels) {
    ret <- ret + theme_void()
  }
  ret
}
