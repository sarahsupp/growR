#' @title makeMEOWmapData
#'
#' @description
#' \code{makeGROWmapData} takes a dataset with values and a GROW column merges them for use by \code{\link{makeGROWmap}}. 
#' Code is modified and extended from @jebyrnes package meowR {\link{https://github.com/jebyrnes/meowR}}.
#' 
#' @details  \code{makeGROWmapData} takes a dataset with values and a GROW column and makes a joined \code{\link{data.frame}}
#'
#' @author Sarah Supp
#' @param newdata dataset to be used
#' @param fillColName name of the column with information to be used for a fill value
#' @param regionColName data name of column with ecoregion, province, or realm names
#' @param type are these values from an ECOREGION, PROVINCE, or REALM map. Defaults to "ECOREGION". Note all caps.
#' @param fillAlphaColName Colname of column used to scale alpha level of fill
#' @param realm What geographic realm are the data from (Marine, Freshwater, Terrestrial, Hotspot, IPBES)?
#' @param excludeNoDataAreas Exclude areas/regions from the plot with no data?
#' 
#' 
#' @export
#' @return Returns \code{\link{data.frame}} object
#' 
#'
#' @examples
#' 
#' data(regions.df)
#' 
#' ndf <- data.frame(Ecoregions = levels(regions.df$ECOREGION), 
#' Values = runif(length(levels(regions.df$ECOREGION)), 0,100))
#' 
#' head(makeGROWmapData(ndf, fillColName="Values", regionColName="Ecoregions"))
#' 

# Function to get data needed to map values into Geographic Regions Of the World (GROW), 
# depending on the realm (modified from makeMEOWmapData)
makeGROWmapData <- function (newdata, fillColName, fillAlphaColName = NULL, regionColName = type, 
                             type = "ECOREGION", realm = "Marine", excludeNoDataAreas = T) 
{
  if (realm == "Marine")
    regionData <- regions.df
  if (realm == "Terrestrial")
    regionData <- terr.df
  if (realm == "Freshwater")
    regionData <- fresh.df
  if (realm == "Hotspot")
    regionData <- hotspots.df
  if (realm == "IPBES")
    regionData <- ipbes_regions.df
  if (type == "PROVINCE") 
    regionData <- provinces.df
  if (type == "REALM") 
    regionData <- realms.df
  if (regionColName != type) 
    regionData[[regionColName]] <- regionData[[type]]
  regionData <- join(regionData, newdata)
  regionData$score <- regionData[[fillColName]]
  regionData$fillAlpha <- NA
  if (!is.null(fillAlphaColName)) 
    regionData$fillAlpha <- regionData[[fillAlphaColName]]
  if (excludeNoDataAreas && sum(is.na(regionData$score)) > 
      0) 
    regionData <- regionData[-which(is.na(regionData$score)), 
                             ]
  return(regionData)
}
