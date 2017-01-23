#' @importFrom grDevices gray


#creates a single 3D cube
cube <- function(x=0,y=0,z=0, bordered=TRUE,
                 filled = TRUE, lwd=2, scale=1,
                 fillcol = gray(.95),
                 bordercol ='black', ...) {
  
  mycube <- cube3d() #3d cude. There are others i.e., oh3d, qmesh 3d ... read help. 
  
  # Reduce size to unit
  mycube$vb[4,] <- mycube$vb[4,]/scale*2 # why convert to 2 using scale? 
  
  for (i in 1:length(x)) {
    # Add cube border
    if (bordered) {
      bcube <- mycube
      bcube$material
      bcube$material$lwd <-2 # lwd (thickness of the black line)
      bcube$material$front <- 'line' # lines, points or culls on the material used to render the object
      bcube$material$back <- 'line'
      #bcube %>% translate3d(x[i], y[i], z[i]) %>% shade3d # or wire 3d is possible #translate 3d does not work. 
      bcube %>% shade3d #do not need translate3d 
      #translate3d(x[i], y[i], z[i])
      
    }
    # Add cube fill
    if (filled) {
      fcube <- mycube
      fcube$vb[4,] <- fcube$vb[4,]*1.01
      fcube$material$col <- fillcol
      #fcube %>% translate3d(x[i], y[i], z[i]) %>% shade3d # translate3d is not working
      fcube %>% shade3d
    }
  }
}
