#' @export
#' @importFrom rgl rgl.light
#' @importFrom rgl rgl.viewpoint
#' @importFrom rgl clear3d
#' @importFrom rgl rotationMatrix
#' @importFrom rgl axes3d
#' @importFrom rgl title3d
#' @param seed To generate the same set of item(s) on the local computer.
#' @param angle,x,y,z  See details
#' @param axis Showing the axis is helpful when first testing the function.
#' @description This function generates a 3 dimensional display figure. It acts as a wrap because the creation of the figure is done using functions from the rgl package.
#' @details For 3D figures, some of the cubes may be hidden in sight when automatically generated. Hence, one would need to rotate the display figure several times to ensure that none of the cubes are hidden. The rotation can be done by changing the theta and phi value in the function. To ensure that the same image is generated again, please provide a seed value. You will need to use the rgl post script function to save the figure. Currently, the function returns the matrix that is used to generate the display figure. The matrix can be used for the \code{\link{spatial3d_mirror}} to generate a mirror image of the display figure.
#'
#' The arguments angle, x, y, z represents the rotation of angle radians based on the x, y and z axis. This is a wrapper to the rotationMatrix function from the rgl package. Changing the values in the arguments angle, x, y, z coordinates allows one to programmatically change angles to study potential cognitive operators at work.
#'
#' You can also rotate the figure interactively by clicking on the figure and moving it in different direction.
#' @references
#' \url{https://en.wikipedia.org/wiki/Radian}
#'
#' \url{https://cran.r-project.org/web/packages/rgl/vignettes/rgl.html}
#'
#' Bejar, I. I. (1990). A generative analysis of a three-dimensional spatial task. \emph{Applied Psychological Measurement}, 14(3), 237-245.
#'
#' @author Aiden Loe
#' @title Spatial 3D Reasoning Item
#' @seealso \code{\link{lisy}}, \code{\link{arith}}, \code{\link{spatial2d}}, \code{\link{spatial3d_mirror}}
#' @return
#' \describe{
#' \item{figure}{Return the matrix that generates the display figure.}
#'     }
#' @examples
#' a <- spatial3d(seed=4, angle=pi/1.3, x=0.3,y=4,z=0.8,axis = TRUE)
#'
#' # To save the figure (not run)
#' # wd<- '~/desktop'
#' # item <- 1
#' # save <- paste0(wd,"/display3d_",item,".pdf")
#' # rgl.postscript(save,"pdf")

spatial3d <- function(seed=1, angle=pi/1.3, x=0.3,y=3, z=0.8,axis = TRUE){
  # This will finalise the item
  # seed = 2
  # theta = 210
  # phi = 20
  # angle=2.6
  # x=0.3
  # y=4
  # z=1.0

  set.seed(seed)
  clear3d()
  rgl.light()
  vlist <- c(1,2,2)
  (zlist <- matrix(NA,ncol=3,nrow=8))
  for (i in 1:8) { # 8 is the max. Arbitary number. Can be anything. #Does not take last row
    cube(vlist[1],vlist[2],vlist[3])
    (step <- sample(1:3, 1))
    (vlist[step] <- vlist[step]+1^rbinom(1,1,.25))
    (zlist[i,] <- vlist)
    zlist
  }

  zlist
  clear3d()
  cube(zlist[1:8,1],zlist[1:8,2],zlist[1:8,3])
  rgl.viewpoint(fov = 0, userMatrix = rotationMatrix(angle=angle, x=x,y=y,z=z))
  roMatrix <- rotationMatrix(angle=angle, x=x,y=y,z=z)
  if(axis == TRUE){
  axes3d( edges=c("x", "y", "z") )
  title3d(main=NULL, sub=NULL, xlab='xlab', ylab='ylab', zlab='zlab')
  }
  result <- list(figure=zlist, rotationMatrix = roMatrix)
  class(result) <- "threeD"
  return(result)
}


#' @export
#' @importFrom rgl rgl.light
#' @importFrom rgl clear3d
#' @importFrom rgl rotationMatrix
#' @importFrom rgl axes3d
#' @importFrom rgl title3d
#' @param obj An object with class of threeD.
#' @param angle,x,y,z  See details
#' @param axis Showing the axis is helpful when first testing the function.
#' @description This function generates the mirror image of the 3 dimensional display figure. It acts as a wrap because the creation of the figure is done using functions from the rgl package.
#' @details For 3D figures, some of the cubes may be hidden in sight when automatically generated. Hence, one would need to rotate the display figure several times to ensure that none of the cubes are hidden.
#'
#' The arguments angle, x, y, z represents the rotation of angle radians based on the x, y and z axis. This is a wrapper to the rotationMatrix function from the rgl package. Changing the values in the arguments angle, x, y, z coordinates allows one to programmatically change angles to study potential cognitive operators at work.
#'
#' You can also rotate the figure interactively by clicking on the figure and moving it in different direction.
#'
#' @references
#' \url{https://en.wikipedia.org/wiki/Radian}
#'
#' \url{https://cran.r-project.org/web/packages/rgl/vignettes/rgl.html}
#'
#' Bejar, I. I. (1990). A generative analysis of a three-dimensional spatial task. \emph{Applied Psychological Measurement}, 14(3), 237-245.
#'
#' @author Aiden Loe
#' @title Mirror Spatial 3D Reasoning Item
#' @seealso \code{\link{lisy}}, \code{\link{arith}}, \code{\link{spatial2d}}, \code{\link{spatial3d}}
#' @return
#' \describe{
#' \item{figure}{Return the matrix that generates the mirror image of the display figure.}
#'     }
#' @examples
#' display <- spatial3d(seed=4, angle=pi/1.3, x=0.3,y=4,z=0.8,axis = TRUE)
#' display_mirror <- spatial3d_mirror(display, angle=pi/1.3, x=0.3,y=4,z=0.8,axis = TRUE)
#'
#' #' # To save the figure (not run)
#' # wd<- '~/desktop'
#' # item <- 1
#' # save <- paste0(wd,"/mirror3d_",item,".pdf")
#' # rgl.postscript(save,"pdf")

spatial3d_mirror <- function(obj, angle=pi/1.3, x=0.3,y=3, z=0.8,axis = TRUE){
  result <- obj
  if(class(result)!="threeD"){
    stop("You need to use an object of class threeD.")
  }
  clear3d()
  rgl.light()
  cube(result$figure[1:nrow(result$figure),3],result$figure[1:nrow(result$figure),2],result$figure[1:nrow(result$figure),1]) #mirror
  rgl.viewpoint(fov = 0, userMatrix = rotationMatrix(angle=angle, x=x,y=y,z=z))
  mirrorResult<- cbind(result$figure[1:8,3],result$figure[1:8,2],result$figure[1:8,1])
  if(axis == TRUE){
    axes3d( edges=c("x", "y", "z") )
    title3d(main=NULL, sub=NULL, xlab='xlab', ylab='ylab', zlab='zlab')
  }
  return(mirrorResult)
}


