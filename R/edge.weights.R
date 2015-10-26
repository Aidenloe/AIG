 #' This allows us to select the weights of the edges
 #' @export
 #' @author Aiden Loe
 #' @title edge.v
 #' @details
 #' This tells us that the value of the edge is dependent on the randomly drawing
 #' number 1:10


# randomly draws number 1:10
 edge.v <- function(edge.value=NULL){
 if(!is.null(edge.value)){
 items <- rep(1:10,10)
 	edge.v <- sample(items, edge.value, replace=TRUE)
 	return(edge.v)
 }
}

