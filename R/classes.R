#' @export
#'
print.lisy <- function(x,...){
  finalList<- x
  cat("Linear Syllogism Item \n")
  cat(paste0("Item ", finalList$seed, "\n" ))
  cat(paste0("\nSentence: ",finalList$Question, "\n"))
  cat(paste0("\nNumber of inferences to solve the question: ", finalList$ninfer))
  cat(paste0("\nClues: ",finalList$clues.1," ",finalList$clues.2," ",finalList$clues.3 ,"\n"))
  cat(paste("\nAnswer:",finalList$Answer))
  cat(paste0("\nDistractor 1:",finalList$dist1, "(",finalList$dtype1 ,")"))
  cat(paste0("\nDistractor 2:",finalList$dist2, "(",finalList$dtype2 ,")"))
  cat(paste0("\nDistractor 3:",finalList$dist3, "(",finalList$dtype3 ,")"))
  cat(paste0("\nDistractor 4: ",finalList$dist4, "(",finalList$dtype4 ,")"))
  cat(paste0("\nDistractor 5: ",finalList$dist5, "(",finalList$dtype5 ,")"))
}


#' @export
#'
print.ICARarith <- function(x,...){
  results<- x
  cat("Arithmetic Questions:")
  cat(paste0("\n",results$prompt))
  cat("\n\nResponse Options:")
  cat(paste0("\n",results$choices))
  cat("\n\nAnswers:")
  cat(paste0("\n",results$answer))
  cat("\n\nCode:")
  cat(paste0("\n",results$code))
}


