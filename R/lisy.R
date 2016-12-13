#' @import mgcv stats

#' @param seed This generates the same question again from local computer.
#' @param nclues This is the number of sentences to make up the item
#' @param nspread This is the spread of the number of incidentals in total.
#' @param reverseprob This calculates how you want to divided the comparison terms.
#' @param Ndist This returns the number of distractors per question.
#' @param incidentals This tells the function whether the item features are 'names' or 'objects'.
#' @param dist This allows you to select the type of distractors. No. of False distract must be less than No. of clues.
#' @param itemSet This is the choice of itemset you want. If random then the generator will randomly select one (People, Fruits, Superheroes). Change to 'own' if you are using your own item set.
#' @param items This inputs your own item type. At least 10.
#' @param scales This is the comparison terms
#' @description This function generates linear syllogistic reasoning items.
#' @details There are several things to bear in mind. To use own item set, please have at least 10 items within the itemset. In order for scale comparison to make sure. Please ensure that you have at least 2 comparisons. The function will stop if the criteria is not met. The genearation of items are slower if you have a huge item set.
#' @author Aiden Loe and Francis Smart
#' @title lisy
#' @examples \dontrun{
#'
#'
#'  # Generate an item
#'  item <-  lisy(seed=4, nclues=5, nspread=5,reverseprob=.5,
#'  Ndist=4, incidentals='names', dist="false",
#'  itemSet='random',items= NULL, scales = NULL)
#'
#'  # Generate 100 items
#'  nitems <- 100
#'
#'  # Use different number of names and clues
#'  params <- data.frame(seed=1:nitems,
#'  nclues=ceiling((1:nitems)/20)+3,
#'  nspread=ceiling((1:nitems)/15)+3)
#'
#'  # Loop through
#'  qtable <- NULL
#'  for (i in 1:nitems) {
#'  runs <- lisy(seed=i,
#'  nclues=params$nclues[i],
#'  nspread=params$nspread[i],
#'  reverseprob=.5,  Ndist=4, incidentals='names',
#'  dist="mixed",itemSet='random',items= NULL, scales = NULL)
#'  qtable[[i]] <- runs
#'  }
#'  qtable
#'
#'  # Save csv file
#'  write.csv(do.call("rbind",qtable), file="~/desktop/test.csv"  )
#'
#'  #############
#'  # Example using own item set
#'  library("babynames")
#'  bNames <- sapply(babynames[,3], as.character)
#'  compare <- c("taller", "older", "smaller", "bigger","stronger", "weaker")
#'
#'  #Generate items
#'  lisy(seed=4, nclues=5, nspread=7,reverseprob=.5, Ndist=2,
#'  incidentals= 'names',dist="false",
#'  itemSet='own',items= bNames, scales = compare)
#'
#'  item
#' }
#'
#'
#'


lisy <- function( seed=1,
                   nclues=4,
                   nspread = 5,
                   reverseprob=.5,
                   Ndist=4,
                   incidentals='names',
                   dist="mixed",
                   itemSet='random',
                   items=NULL,
                   scales = NULL
                   ){

  if(itemSet != 'random' && itemSet !='own'){
    stop("Please declare either 'random' or 'own' item set.")
  }

  if(dist != 'mixed' && dist !='invalid' && dist !='false'){
    stop("Please declare either 'mixed', 'invalid' or 'false' distractors.")
  }

  if(incidentals != 'names' && incidentals !='objects'){
    stop("Please declare either 'names' or 'objects'.")
  }

  if((is.null(items) == is.null(scales)) == FALSE){
    stop("Please ensure that both items and scales are read in together.")
  }

  if((is.null(items) == is.null(scales)) == FALSE && itemSet=='random'){
    stop("Please change set == 'random' to 'own' when using your own item set.")
  }

  if(is.null(items)==TRUE && is.null(scales) == FALSE && itemSet=='own' | is.null(items)==FALSE && is.null(scales) == TRUE && set=='own' | !is.null(items)  && !is.null(scales)  && itemSet =='random'){
    stop("Please make sure that items and scales = NULL if the set = random.")
  }

  if(is.null(items) && is.null(scales)  && itemSet=='own'){
    stop("Pleast insert item set and scales. If not please change set = random")
  }

  if(!is.null(items) && length(items) < 10){
    stop("Please create a character vector of names > or more than 10.")
  }

  if(!is.null(scales) && length(scales) < 2 | length(scales) %% 2 != 0 ){
    stop("Please create a character vector scale that has event characters.")
  }

  if(!is.null(items) && !is.character(items) | !is.null(scales) && !is.character(scales)){
    stop("Please make sure that both items and scales are character vectors")
  }



  set.seed(seed)
  p <- paste0
  cap <- function(x) paste0(
    #take the first words and make upper case, combine the lower case of the remaining words
    toupper(substr(x,1,1)), substr(x,2,nchar(x)))


if(is.null(items)  | is.null(scales)){
  # we sets of items in a list item features
  sets <- c('people', 'fruit', 'superheroes')
  items <- list(people=c('Amy', 'Susan', 'Bob', 'Mary', 'Robert', 'Ernest', 'Henry', 'Peter','Jake','Jenny',
                         'Edward','Sam','Marcus','Mario'),
                fruit=c('apple','pear','netarine','tomato','avocado','lemon','orange','mango','peach','plum','tangerine'),
                superheroes=c('Spiderman','Superman','Batman','Wolverine','Catwoman','Thor','The Shadow','Silver Surfer',
                              'Captain America','Hurcleus','Harry Potter','Hulk','Gandalf'))

  # scale comparison
  scales <- list(people=matrix(c('taller', 'shorter','older', 'younger','faster', 'slower'),
                               nrow=2),
                 fruit = matrix(c('more fresh', 'less fresh','bigger', 'smaller',
                                  'heavier', 'lighter','tastier', 'less tasty'), nrow=2),
                 superheroes=matrix(c('stronger', 'weaker','cooler', 'less cool','braver',
                                      'less brave','less powerful', 'more powerful'), nrow=2))

  }else{
    sets <- "own"
    items <- list(own=c(items))
    scales <- list(own=matrix(c(scales),nrow=2))
  }

  if(incidentals == 'objects'){
    articles <- list(people='', fruit='the', superheroes='the', own = 'the')
    Articles <- list(people='', fruit='The', superheroes='The', own = 'The')
  }else{
    articles <- list(people='', fruit='the', superheroes='the', own = '')
    Articles <- list(people='', fruit='The', superheroes='The', own = '')
  }

  # Choose a random set if the argument of set == random. Here we can change item type
  if(itemSet== 'random' | itemSet== "own"){
    set <- sample(sets,1)
    }

  # randomly place Nouns in selected itemset
  itemlist <- sample(items[[set]])

  # articles
  article  <- articles[[set]]
  Article  <- Articles[[set]]

  # all syllogism based on selected item set
  scaleset <- scales[[set]]

  #randomly select syllogism column wise
  thescale <- scaleset[,sample(ncol(scaleset),1)]

  # List of possible clues
  # column wise, ascending order.
  # row wise, second digit always smaller than first digit. So that we can create invalid or false distractors.
  pclues <- NULL #possible clues

  for (i in 2:nspread) for (ii in (i-1):1) {
    pclues <- rbind(pclues, c(i,ii))
  }

  # Require that the solution is from the infer df
  # kill if nclues is more than nspread


  if(nclues > nspread){
   stop("Please make sure that the value of nclues is smaller or equal to nspread")
  }else{
    nclues <- uclues <- nclues  #given by user
  }

  #if(nspread == 3 && minsteps == 3){ stop("Please increase nspread in order to find a suitable logical combination")}
  #if(minsteps > 4) {warning("The function may be in an infinite because it cannot find a suitable logical combination. \n Recommend setting minstep of less than 4.")}
  if(nspread > uclues +3 ){ warning("The large combinatorics value of nspread may result in making the distractors obviously wrong. \nSuggest making  nspread at most + 1 > nclues.")}


    #At least one value from the 2nd col exist in the 1st col
  clues<- NULL
    while(any(clues[,1] %in% clues[,2]) == FALSE){
      nclues <- sample(nrow(pclues), uclues)


      #extract the pclues rows based on the random sample of nclues given by user.
      clues<-pclues[nclues,]
      clues <- uniquecombs(clues)  # keep unique rows only
      any(clues[,1] %in% clues[,2]) == TRUE
    }

    # 1 step
    infer <- data.frame(left =pclues[nclues,1],  #nclues first column become left
                        right=pclues[nclues,2],  #nclues second column become right
                        steps=1,
                        rclues=sapply(nclues,toString), #change values to string type #row clues
                        stringsAsFactors=FALSE) # don't convert strings to factor


    i <- 1
    # while i is less than nrow() + 1
    while (i<nrow(infer)+1) {
      #check for all values in first column that is equals to second column 1st cell value
      #selecting a row within the matrix
      #the row is based on whether the element is a match with the element within the vector
      # i.e. if the infer[,2] == 2, it will extract all rows where the first column value == 2
      # It may extract more than one row, or no rows
      # this will repeat until the while loop is fulfilled

      #sub <- infer[infer[,1]==infer[i,2],]
      sub <- infer[infer[,1]==infer[i,2],]
      #(sub <- infer[infer[,1]==infer[1,2],])


      # if nrow(sub) is greater than 0
      # so if no sub is extracted, then don't do this step. if not, do this step.
      if (nrow(sub) > 0) {
        sub[,1] <- infer[i,1] #first value of sub changes to value depending on first column values
        sub
        #pasting the infer clues in the sub clues
        sub$rclues <- paste0(infer$rclues[i],'.',sub$rclues)
        sub

        bothFALSE <-  paste0(sub[,1],sub[,2]) %in% paste0(infer[,1],infer[,2])
        if(all(bothFALSE == FALSE )==TRUE){
          infer[nrow(infer)+1,] <- c(sub$left[ii],  # repeat
                                     sub$right[ii], # repeat
                                     as.numeric(sub$step[ii])+1,  #Achieve maximum steps]
                                     sub$rclues[ii])

        }else{
          sub[-which(paste0(sub[,1],sub[,2]) %in% paste0(infer[,1],infer[,2])),]
          infer[nrow(infer)+1,] <- c(sub$left[ii],  # repeat
                                     sub$right[ii], # repeat
                                     as.numeric(sub$step[ii])+1,  #Achieve maximum steps]
                                     sub$rclues[ii])
         }
      }

       i <- i+1
    }

     # if(any(infer[,3] == 1) == TRUE ) stop("iteration did not achieve max steps: run again")



   infer[,1:3] <- sapply(infer[,1:3], as.numeric)
   (infer <- infer[order(infer[,1], decreasing=TRUE),])

  ############# VALID RESPONSES ##############
  valid    <- paste0(infer[,1] ,'.',infer[,2]) #combine left / right #those selected within pclues
  possible <- paste0(pclues[,1],'.',pclues[,2]) #combine all pclues combination together
  valid
  possible
  #search for number of possible in valid
  (Nval <- (1:length(possible))[possible %in% valid])

  ############### INVALID RESPONSES ##########
  (Ninv <- (1:length(possible))[!possible %in% valid]) #search for impossible in valid
  invalids <- cbind(pclues[Ninv,2],pclues[Ninv,1]) # creating invalid response options.
  invalids #names different placeholders
  ulist    <- unique(c(clues[,1:2]))
  invalids
#  invkeeps2 <- invalids[invalids[,1] %in% ulist |  invalids[,2] %in% ulist,] #names list
  invkeeps <- invalids

  if (length(invkeeps) > 0 ) iinvkeeps <- rbind(
    cbind(itemlist[invkeeps[,1]],itemlist[invkeeps[,2]]),
    cbind(itemlist[invkeeps[,2]],itemlist[invkeeps[,1]])) # why flip around? [increase more invalids combinations]

  #if (length(invkeeps)==0) iinvkeeps<- matrix(NA, nrow=0, ncol=2)
  invkeeps
  ############ FALSE RESPONSES ##########
  falses  <- cbind(pclues[Nval,2],pclues[Nval,1])
  ifalses <- cbind(itemlist[falses[,1]],itemlist[falses[,2]]) #names
  reverseprob <- .5


  #### ###### #### CLUES PROVIDED IN THE SENTENCE ### #### ####
  #extract the nclues rows based on the random sample of nclues given by user.
  #the number of sentences
  (iclues <- cbind(itemlist[clues[,1]],itemlist[clues[,2]])) # name list


  ##### FUNCTION TO GENERATE SENTENCE #####
  join <- function(clue, thescale, article, forward=TRUE) {
    if (forward) return(paste0(Article, " " ,clue[1], ' is ', thescale[1], ' than ', article ," ", clue[2])) # Going from Big to small value (scale)
    if (!forward) return(paste0(Article, " ",clue[2], ' is ', thescale[2], ' than ',article ," ", clue[1])) # Going from Small to big value (scale reversed)
  }

  #### GENERATE CORRECT RESPONSE OPTION #####
  (maxsteps <- max(infer$steps))
  (maxinferlist <- infer[infer$steps==maxsteps,]) # select those rows with max steps. So it can be more than one.
  maxinfer <- maxinferlist[sample(nrow(maxinferlist), 1),] # here you randomly chose one of the max step row
  maxitems <- itemlist[as.numeric(maxinfer[1:2])] # subsuite numbers for names.
  maxanswer <- cap(p(join(maxitems, thescale, article,
                          forward=rbinom(1, 1, reverseprob)==1),'.'))

  # Generate the distractors
  q <- 'Clues: '
  dreturn <- c()
  dtype <- c()

  # create matrix of invalid and false ####
  dlist <- NULL
  if(dist=="mixed"){
    dlist <- rbind(cbind(iinvkeeps, type='invalid'),
                   cbind(ifalses, type='false'))
  }

  if(dist == "false"){
    if(Ndist > uclues | Ndist == uclues) stop("False distractors are only allowed if it is 1 less than the number of clues")
    dlist <- cbind(ifalses, type='false')
  }

  if(dist=="invalid"){
      dlist <- cbind(iinvkeeps, type='invalid')
  }

  # sentence structure
  # iclues all come from the same clues now.
  q <- NULL
  for (i in 1:nrow(iclues)) {
    q <- p(q, join(iclues[i,], thescale, article,
                   forward=rbinom(1, 1, reverseprob)==1))
    if (i<nrow(iclues)) q <- p(q, ', ')
  }

  q <- p(q, '. Which of the following is implied?')

  if(Ndist > 5) stop("Please choose a lower number of distractors")

  Ndist
  dlist
  # repeat sampling the distractions rows in the dlist matrix that was created previously
  for (i in 1:Ndist){
    if(length(nrow(dlist))>0) {
     draw <- sample(nrow(dlist), 1) #randomly draw from the dlist matrix made up of invalid and false
      dreturn[i] <- cap(p(join(dlist[draw,1:2], thescale, article,
                               forward=rbinom(1, 1, reverseprob)==1),'.')) # create an incorrect sentence
      dtype[i] <- dlist[draw,3]
      dlist <- dlist[-draw,] #minus that row.
      dlist
      dreturn
  }
    #if(!is.null(nrow(dlist))) dtype[i] <- dreturn[i] <- '999'
  }

  seed <- seed
  data.frame(seed=seed,
             Question=q,
             Answer=maxanswer,
             dist1=dreturn[1],
             dtype1=dtype[1],
             dist2=dreturn[2],
             dtype2=dtype[2],
             dist3=dreturn[3],
             dtype3=dtype[3],
             dist4=dreturn[4],
             dtype4=dtype[4],
             dist5=dreturn[5],
             dtype5=dtype[5]
  )

}
### End Logical Item

### generate item ####
# lisy(seed=4, nclues=5, nspread=5,reverseprob=.5, Ndist=5, incidentals='names',
#               dist="false",itemSet='random',items= NULL, scales = NULL)
#
# # Test with dataset
# library("babynames")
# bNames <- sapply(babynames[,3], as.character)
# compare <- c("taller","shorter", "older", "younger", "smaller", "bigger","stronger", "weaker")
#
# #Generate item
# lisy(seed=4, nclues=4, nspread=8,reverseprob=.5, Ndist=4, incidentals= 'names',dist="mixed",
#      itemSet='own',items= bNames, scales = compare)
#
# #loop through 30 items
# nitems <- 30
# params <- data.frame(seed=1:nitems,
#                      nclues=ceiling((1:nitems)/20)+3,
#                      nspread=ceiling((1:nitems)/15)+3)
#
# qtable <- NULL
# for (i in 1:nitems) {
#   runs <- lisy(seed=i,
#                nclues=params$nclues[i],
#                nspread=params$nspread[i],
#                reverseprob=.5,  Ndist=4, incidentals='names',
#                dist="mixed",itemSet='own',items= bNames, scales = compare)
#   qtable[[i]] <- runs
# }
#
# qtable

#write.csv(do.call("rbind",qtable), file="~/desktop/test.csv"  )

