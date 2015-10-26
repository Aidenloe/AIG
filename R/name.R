#' @export
#' @author Aiden Loe
#' @title v.labels


#Change dataframe to as.character
#fNames[] <- lapply(fNames, as.character)
#z[] <- lapply(z, as.character)

#Upload data file if not use default dataset in function.

v.labels <- function(z=NULL, newValue)
{
    if(!is.null(z)){
  #       if(class(z) != "data.frame")
  #         {
  #         cat("Incorrect data.frame")
  #         }else{
  #         cat("New dataset is loaded \n Make sure dataset uses character vector")
          z[] <- lapply(z, as.character)
          #sample dataset
          d.names <- z[sample(1:nrow(z), newValue, replace=FALSE),]
          # create list of items
          d.names <- list(d.names = d.names)

          class(d.names) <- "v.labels"
          invisible(d.names)

    }else{

          # cat("default items overwrites new items")

          default.items <- c("Astro","Barnstormer", "Big Railroad","Buzz", "Soak Station", "Cin. Castle", "Jamboree", "Dumbo", "Enchanted", "Haunted Mansion", "Jungle Cruise", "Mad Tea", "Aladdin", "Winnie","Monsters Inc", "Peter Pan", "7 Dwarfs", "Space Mountain")

          d.names <- sample(default.items, newValue, replace=FALSE)
          # create list of items
          d.names <- list(d.names = d.names)

          class(d.names) <- "v.labels"
          invisible(d.names)
    }
}

