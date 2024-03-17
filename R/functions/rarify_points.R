#' Rarify points
#' Subsample point data so they contain n points per grid cell of a mask raster.
#'
#' @param point_data Spatial Point. Points to subsample.
#' @param raster_data Raster. Mask used to sample points to.
#' @param n Numeric. Sample size. Default is 1.
#'
#' @return
#' @export
#'
#' @examples
rarify_points <- function(point_data, raster_data, n = 1)
{
  if(missing(raster_data) | missing(point_data)){
    stop("Input data missing.")
    }
  
  # Convert raster to a spatial grid
  raster_grid <- as(raster_data, "SpatialGrid")
  
  # Get point data
  initial_pt_data <- point_data
  if(
    !identical(crs(raster_data, asText = TRUE), proj4string(initial_pt_data))
  ){
    initial_pt_data <- spTransform(initial_pt_data, crs(raster_data, asText = TRUE))
  }
  initial_pt_data$grid <- over(initial_pt_data, raster_grid)
  gridlist <- split(initial_pt_data, initial_pt_data$grid)
  
  # Take n points per grid
  samples <- lapply(gridlist, function(x) x[sample(1:nrow(x), n, FALSE),])
  
  # Bind those rows back together in a new data frame
  sampledgrid <- do.call(rbind, samples)
  
  return(sampledgrid)
}