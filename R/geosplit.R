#' geosplit Split geographic data into a training set and a test set
#' All the points within inner.radius of a center point will be in the test set.
#' Everything more than outer.radius away will be in the training set.
#' radii are in meters
#' @export
#' @examples
#' locations = cbind(
#'    longitude = runif(100, min = 80, max = 120),
#'    latitude = runif(100, min = 30, max = 80)
#' )
#' locations = cbind(
#'   longitude = runif(2000, min = 80, max = 110),
#'   latitude = runif(2000, min = 30, max = 50)
#' )
#' centers = geosphere::regularCoordinates(16)
#' split = geosplit(locations, centers, inner.radius = 100000, outer.radius = 200000)
#' colors = split$in.train + 3 * split$in.test
#' plot(
#'   locations[split$in.train|split$in.test, ], 
#'   col = colors[split$in.train|split$in.test],
#'   pch = 16,
#'   cex = .8
#' )
geosplit = function(locations, centers, inner.radius, outer.radius){
  
  assert_that(
    length(inner.radius) == 1,
    length(outer.radius) == 1,
    inner.radius < outer.radius,
    ncol(locations) == 2,
    ncol(centers) == 2
  )
  
  dists = pointDistance(centers, locations, longlat = TRUE)
  
  data.frame(
    in.train = apply(
      dists, 
      2,
      function(x) min(x) > outer.radius
    ),
    in.test = apply(
      dists, 
      2,
      function(x) min(x) < inner.radius
    )
  )
}
