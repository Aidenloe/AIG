#' @export
#' @importFrom rgl light3d
#' @importFrom rgl view3d
#' @importFrom rgl clear3d
#' @importFrom rgl rotationMatrix
#' @importFrom rgl axes3d
#' @importFrom rgl title3d
#' @param seed To generate the same set of item(s) on the local computer.
#' @param angle,x,y,z  See details
#' @param cubes The number of connected cubes together.
#' @param axis Showing the axis is helpful when first testing the function.
#' @param filled yes, no, random will determine the colour of the cubes. Ignore for now.
#' @description This function generates a 3 dimensional display figure. It acts as a wrap because the creation of the figure is done using functions from the rgl package.
#' @details For 3D figures, some of the cubes may be hidden in sight when automatically generated. Hence, one would need to rotate the display figure several times to ensure that none of the cubes are hidden. To ensure that the same image is generated again, please provide a seed value. You will need to use the rgl post script function to save the figure. Currently, the function returns the a list object that is used to generate the display figure. The figure matrix in the list object can be used for the \code{\link{spatial3d_mirror}} to generate a mirror image of the display figure.
#'
#' The arguments angle, x, y, z represents the rotation of angle radians based on the x, y and z axis. This is a wrapper to the rotationMatrix function from the rgl package. Changing the values in the arguments angle, x, y, z coordinates allows one to programmatically change angles to study potential cognitive operators at work.
#'
#' You can also rotate the figure interactively by clicking on the figure and moving it in different direction.
#' @references
#' \url{https://en.wikipedia.org/wiki/Radian}
#'
#'
#' Bejar, I. I. (1990). A generative analysis of a three-dimensional spatial task. \emph{Applied Psychological Measurement}, 14(3), 237-245.
#'
#' @author Aiden Loe and Francis Smart
#' @title Spatial 3D Reasoning Item (double shapes)
#' @seealso \code{\link{lisy}}, \code{\link{arith}}, \code{\link{spatial2d}}, \code{\link{spatial3d_mirrorDouble}}
#' @return
#' \describe{
#' \item{figure}{Return the matrix that generates the display figure.}
#' \item{rotationMatrix}{Return the cordinates of x,y,z,w}
#'     }
#' @examples
#' item <- spatial3dDouble(seed=4, angle=pi/1.3, x=0.3, y=4, z=0.8, axis = TRUE)
#'
#' # To save the figure (not run)
#' # library(rgl)
#' # item <- spatial3dDouble(seed=4, angle=pi/1.3, x=0.3, y=4, z=0.8, cubes=9, axis = TRUE)
#'
#' # save in pdf
#' # wd<- '~/desktop'
#' # item <- 1
#'
#' # save <- paste0(wd,"/display3d_",item,".pdf")
#' # rgl.postscript(save,"pdf")
#'
#' # save in png
#' # rgl.snapshot(filename="image3D.png",fmt="png")

spatial3dDouble <- function(seed=1, angle=pi/1.3, x=0.3,y=3, z=0.8, cubes=8, axis = TRUE, filled='yes'){


  # This will finalise the item
  set.seed(seed)
  clear3d()
  light3d()

  # first one
  vlist <- c(1,2,2)
  (zlist <- matrix(NA,ncol=3,nrow=cubes))
  for (i in 1:cubes) { # 8 is the max. Arbitary number. Can be anything.

    if(filled=='random'){
      sam <- sample(1:2,1)
      if(sam==1){
        mycube <-  cube(vlist[1],vlist[2],vlist[3],filled=TRUE, fillcol='#4c4c4c')
      }else{
        mycube <- cube(vlist[1],vlist[2],vlist[3],filled=TRUE)
      }
    }
    if(filled=='yes'){
      cube(vlist[1],vlist[2],vlist[3],filled=TRUE)
    }

    if(filled=='no'){
      cube(vlist[1],vlist[2],vlist[3],filled=FALSE)
    }


    (step <- sample(1:3, 1))
    (vlist[step] <- vlist[step]+1^rbinom(1,1,.25))
    (zlist[i,] <- vlist)
    zlist
  }

  # second one
  vlist <- c(8,2,2)
  (xlist <- matrix(NA,ncol=3,nrow=cubes))
  for (i in 1:cubes) { # 8 is the max. Arbitary number. Can be anything.

    if(filled=='random'){
      sam <- sample(1:2,1)
      if(sam==1){
         cube(vlist[1],vlist[2],vlist[3],filled=TRUE, fillcol='#4c4c4c')
      }else{
        cube(vlist[1],vlist[2],vlist[3],filled=TRUE)
      }
    }
    if(filled=='yes'){
      cube(vlist[1],vlist[2],vlist[3],filled=TRUE)
    }

    if(filled=='no'){
      cube(vlist[1],vlist[2],vlist[3],filled=FALSE)
    }


    (step <- sample(1:3, 1))
    (vlist[step] <- vlist[step]+1^rbinom(1,1,.25))
    (xlist[i,] <- vlist)
    xlist
  }

  (zxlist <- rbind(zlist, xlist))
  clear3d()
  cubesDouble <-cubes*2
  cube(zxlist[1:cubesDouble,1],zxlist[1:cubesDouble,2],zxlist[1:cubesDouble,3])
  view3d(fov = 0, userMatrix = rotationMatrix(angle=angle, x=x,y=y,z=z))
  roMatrix <- rotationMatrix(angle=angle, x=x,y=y,z=z)
  if(axis == TRUE){
    axes3d( edges=c("x", "y", "z") )
    title3d(main=NULL, sub=NULL, xlab='xlab', ylab='ylab', zlab='zlab')
  }
  result <- list(figure=zxlist, rotationMatrix = roMatrix)
  class(result) <- "threeD"
  return(result)
}


#' @export
#' @importFrom rgl light3d
#' @importFrom rgl clear3d
#' @importFrom rgl rotationMatrix
#' @importFrom rgl axes3d
#' @importFrom rgl title3d
#' @param obj An object with class of threeD.
#' @param angle,x,y,z  See details
#' @param method There are 4 methods. See details.
#' @param axis Showing the axis is helpful when first testing the function.
#' @description This function generates the variation of the images of the 3 dimensional display figure. There are four methods in total. See details for more information. It acts as a wrap because the creation of the figure is done using functions from the rgl package.
#' @details For 3D figures, some of the cubes may be hidden in sight when automatically generated. Hence, one would need to rotate the display figure several times to ensure that none of the cubes are hidden. This may be even more so when it comes to 2 figures within an image. The longer the number of connected cubes, the more likely it will overlap with each other. About 8 is more or less right.
#'
#' The arguments angle, x, y, z represents the rotation of angle radians based on the x, y and z axis. This is a wrapper to the rotationMatrix function from the rgl package. Changing the values in the arguments angle, x, y, z coordinates allows one to programmatically change angles to study potential cognitive operators at work.
#'
#' You can also rotate the figure interactively by clicking on the figure and moving it in different direction.
#'
#' There are 4 methods to create distractors. The first method generates, the first image as a mirror, and the second image as normal. The second method is vice versa from the first method. The third method generates both mirror images. The first method generates correct rotation but the image is always short of one cube.
#'
#' @references
#' \url{https://en.wikipedia.org/wiki/Radian}
#'
#' Bejar, I. I. (1990). A generative analysis of a three-dimensional spatial task. \emph{Applied Psychological Measurement}, 14(3), 237-245.
#'
#' @author Aiden Loe
#' @title Mirror Spatial 3D Reasoning Item (2)
#' @seealso \code{\link{lisy}}, \code{\link{arith}}, \code{\link{spatial2d}}, \code{\link{spatial3dDouble}}
#' @return
#' \describe{
#' \item{figure}{Return the matrix that generates the mirror image of the display figure.}
#'     }
#' @examples
#' display <- spatial3dDouble(seed=4, angle=pi/1.3, x=0.3,y=4,z=0.8,cubes=8, axis = TRUE)
#' display_mirror <- spatial3d_mirrorDouble(display, angle=pi/1.3, x=0.3, y=4, z=0.8,
#'                                          method="one", axis = TRUE)
#'
#' # To save the figure (not run)
#' # library(rgl)
#' # display_mirror <- spatial3d_mirrorDouble(display, angle=pi/1.3, x=0.3, y=4, z=0.8,
#' #                                           method="one", axis = TRUE)
#' # wd<- '~/desktop'
#'
#' # save in pdf
#' # item <- 1
#' # save <- paste0(wd,"/mirror3d_",item,".pdf")
#' # rgl.postscript(save,"pdf")
#'
#' # save in png
#' # rgl.snapshot(filename="image3D.png",fmt="png")
#'

spatial3d_mirrorDouble <- function(obj, angle=pi/1.3, x=0.3,y=3, z=0.8, method="one", axis = TRUE){
  result <- obj
  (cubes <- nrow(result$figure)/2)
  (cubesDouble = nrow(result$figure))

  if(class(result)!="threeD"){
    stop("You need to use an object of class threeD.")
  }

  # first shape mirror, second shape non mirror
  if(method=="one"){
  clear3d()
  light3d()
  cube(result$figure[1:(nrow(result$figure)/2),3],result$figure[1:(nrow(result$figure)/2),2],result$figure[1:(nrow(result$figure)/2),1]) #mirror
  cube(result$figure[cubes+1:cubes,1],result$figure[cubes+1:cubes,2],result$figure[cubes+1:cubes,3]) # non mirror
  }

  # first shape non mirror, second shape mirror
  if(method=="two"){
    clear3d()
    light3d()
    cube(result$figure[1:(nrow(result$figure)/2),1],result$figure[1:(nrow(result$figure)/2),2],result$figure[1:(nrow(result$figure)/2),3]) # non-mirror
    cube(result$figure[cubes+1:cubes,3],result$figure[cubes+1:cubes,2],result$figure[cubes+1:cubes,1]) #mirror
  }

  # both shapes are mirror
  if(method=="three"){
    clear3d()
    light3d()
    cube(result$figure[1:nrow(result$figure),3],result$figure[1:nrow(result$figure),2],result$figure[1:nrow(result$figure),1]) #mirror
  }

  if(method=="four"){
    clear3d()
    light3d()
    cube(result$figure[1:(nrow(result$figure)/2-1),1],result$figure[1:(nrow(result$figure)/2-1),2],result$figure[1:(nrow(result$figure)/2-1),3])
    cube(result$figure[(cubes+2):cubesDouble,1],result$figure[(cubes+2):cubesDouble,2],result$figure[(cubes+2):cubesDouble,3])
  }

  view3d(fov = 0, userMatrix = rotationMatrix(angle=angle, x=x,y=y,z=z))
  mirrorResult<- cbind(result$figure[1:(nrow(result$figure)/2),3],result$figure[1:(nrow(result$figure)/2),2],result$figure[1:(nrow(result$figure)/2),1])
  if(axis == TRUE){
    axes3d( edges=c("x", "y", "z") )
    title3d(main=NULL, sub=NULL, xlab='xlab', ylab='ylab', zlab='zlab')
  }
  return(mirrorResult)
}
