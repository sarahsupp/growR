#' @title getRegionalData
#'
#' @description
#' \code{getRegions} Gets regional information for your dataset, depending on the realm of interest.
#' #' Code is modified and extended from @jebyrnes package meowR {\link{https://github.com/jebyrnes/meowR}}.
#' 
#'
#' @author Sarah Supp, Shane Blowes.
#' @param lats Latitude values in decimal degrees
#' @param longs Longitude values in decimal degrees
#' @param realm "Marine", "Terrestrial", "Freshwater", "IPBES", "Hotspot"
#' 
#' 
#' @export
#' @return Returns a data frame of geographic regions for each point
#' 
#' seealso \code{\link{sp::over}}
#'
#' @examples
#' 
#' data(regions)
#' 
#' latlong <- data.frame(lat = c(50.0944444, 33.4691667, 34.0348833, 
#'      55.0869876, 51.7787776, 
#'      49.6088889, -35.750729),
#'      long=c(-127.55889, -119.53028, 
#'      -119.70308, -161.87444, 
#'      178.30199, -126.78056, 150.259155))
#'      
#' regionalData <- getRegionalData(latlong$lat, latlong$long, realm="Marine")
#' 
#' newdata <- cbind(latlong, regionalData)
#' 
#' newdata


##	Function to take coordinate and return geographic regions 
getRegionalData <- function(Longitude, Latitude, realm=c("Marine", "Terrestrial", "Freshwater", "IPBES", "Hotspot"))
{
  data(regions)
  pts <- SpatialPoints(cbind(Longitude, Latitude),
                       proj4string=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
  pts <- SpatialPointsDataFrame(pts, data= data.frame(Longitude=Longitude, Latitude=Latitude))
  if(realm=='Marine'){
    dataRegions <- over(pts, regions) %>%
      dplyr::select(ECOREGION, PROVINCE, REALM)
  } else if(realm=='Terrestrial'){
    dataRegions <- over(pts, terrestrial_regions) %>%
      dplyr::select(ECO_NAME, WWF_REALM)
  } else if(realm=="Freshwater"){
    dataRegions <- over(pts, freshwater_regions) %>%
      dplyr::select(ECOREGION, ECO_ID)
  } else if(realm=="IPBES"){
    dataRegions <- over(pts, ipbes) %>%
      dplyr::select(IPBES_regi, IPBES_sub)
  } else if(realm=="Hotspot"){
    dataRegions <-over(pts, hotspots) %>%
      dplyr::select(NAME, Type)
  }
  
  return(dataRegions)
}