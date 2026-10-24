#' getCompetitiveArea.R
#' 
#' Finds the competitive area for a batted ball
#' @title "Finds the competitive area for a batted ball."
#' @param startingLocation c(x,y) coordinate pair for the original batted ball location
#' @param catchRadius The radius of the competitive area
#' @returns A list of points within the circle, down to a foot
#' @export
#' @examples {getCompetitiveArea(c(-129,188),58.8)}

getCompetitiveArea <- function(startingLocation, catchRadius) {

  ## Get extrema and initalize pointList
  x_max <- round(1*(startingLocation[1] + catchRadius))#/1
  x_min <- round(1*(startingLocation[1] - catchRadius))#/1
  y_max <- round(1*(startingLocation[2] + catchRadius))#/1
  y_min <- round(1*(startingLocation[2] - catchRadius))#/1
  pointList <- data.frame(x=NULL,y=NULL)
  
  ## Sweep across x and y, ensuring that points are within circle but not already in list before adding
  for (x in seq(x_min, x_max, by = 1)) {
    for (y in seq(y_min, y_max, by = 1)) {
      if ((x - startingLocation[1])^2 + (y - startingLocation[2])^2 <= catchRadius^2) { # Check if points are within comp. area
        if (nrow(merge(data.frame(x=x,y=y),pointList)) == 0) { # Check if points are already within point list
          pointList <- rbind(pointList,list(x=x,y=y)) # If both checks pass, add to list
        }
      }
    }
  }
  
  ## Return list
  pointList
}