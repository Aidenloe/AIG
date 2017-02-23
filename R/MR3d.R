#' @export
#' @importFrom rgl rgl.light
#' @importFrom rgl rgl.viewpoint
#' @importFrom rgl clear3d
#' @param seed To generate the same set of item(s) on the local computer.
#' @param theta Changes the polar coordinates
#' @param phi Changes the polar coordinates
#' @description This function generates a 3 dimensional display figure. It acts as a wrap because the creation of the figure is done using functions from the rgl package.
#' @details For 3D figures, some of the cubes may be hidden in sight when automatically generated. Hence, one would need to rotate the display figure several times to ensure that none of the cubes are hidden. The rotation can be done by changing the theta and phi value in the function. To ensure that the same image is generated again, please provide a seed value. You will need to use the rgl post script function to save the figure. Currently, the function returns the matrix that is used to generate the display figure. The matrix can be used for the \code{\link{spatial3d_mirror}} to generate a mirror image of the display figure.
#'
#' Apart from changing the polar coordiates (theta and phi), you can also rotate the figure manually by clicking on the figure and moving it in different direction.
#'
#' @author Aiden Loe
#' @title Spatial 3D Reasoning Item
#' @seealso \code{\link{lisy}}, \code{\link{arith}}, \code{\link{spatial2d}}, \code{\link{spatial3d_mirror}}
#' @return
#' \describe{
#' \item{figure}{Return the matrix that generates the display figure.}
#'     }
#' @examples
#' a <- spatial3d(seed=4)
#'
#' # To save the figure (not run)
#' # wd<- '~/desktop'
#' # item <- 1
#' # save <- paste0(wd,"/display3d_",item,".pdf")
#' # rgl.postscript(save,"pdf")

spatial3d <- function(seed=1, theta=380,phi=50){
  # This will finalise the item
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
  rgl.viewpoint(theta = theta, phi = phi, fov = 0) # become 2D display item
  result <- list(figure=zlist)
  class(result) <- "threeD"
  return(result)

}


#' @export
#' @importFrom rgl rgl.light
#' @importFrom rgl clear3d
#' @param x An object with class of threeD.
#' @description This function generates the mirror image of the 3 dimensional display figure. It acts as a wrap because the creation of the figure is done using functions from the rgl package.
#' @details For 3D figures, some of the cubes may be hidden in sight when automatically generated. Hence, one would need to rotate the display figure several times to ensure that none of the cubes are hidden. You can also rotate the figure manually by clicking on the figure and moving it in different direction.
#'
#' @author Aiden Loe
#' @title Mirror Spatial 3D Reasoning Item
#' @seealso \code{\link{lisy}}, \code{\link{arith}}, \code{\link{spatial2d}}, \code{\link{spatial3d}}
#' @return
#' \describe{
#' \item{figure}{Return the matrix that generates the mirror image of the display figure.}
#'     }
#' @examples
#' display <- spatial3d(seed=4)
#' display_mirror <- spatial3d_mirror(display)
#'
#' #' # To save the figure (not run)
#' # wd<- '~/desktop'
#' # item <- 1
#' # save <- paste0(wd,"/mirror3d_",item,".pdf")
#' # rgl.postscript(save,"pdf")

spatial3d_mirror <- function(x){
  result <- x
  if(class(result)!="threeD"){
    stop("You need to use an object of class threeD.")
  }
  clear3d()
  rgl.light()
  cube(result$figure[1:8,3],result$figure[1:8,2],result$figure[1:8,1]) #mirror
  mirrorResult<- cbind(result$figure[1:8,3],result$figure[1:8,2],result$figure[1:8,1])
  return(mirrorResult)
}
