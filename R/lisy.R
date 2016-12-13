#' @import mgcv stats
#' @export
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

  if(nclues == 2 && nspread == 2){
    stop("There isn't enough names to create the sentences.")
  }

  set.seed(seed)
  p <- paste0
  cap <- function(x) paste0(
    toupper(substr(x,1,1)), substr(x,2,nchar(x)))


if(is.null(items)  | is.null(scales)){

  sets <- c('people', 'fruit', 'superheroes')
  items <- list(people=c('Amy', 'Susan', 'Bob', 'Mary', 'Robert', 'Ernest', 'Henry', 'Peter','Jake','Jenny',
                         'Edward','Sam','Marcus','Mario'),
                fruit=c('apple','pear','netarine','tomato','avocado','lemon','orange','mango','peach','plum','tangerine'),
                superheroes=c('Spiderman','Superman','Batman','Wolverine','Catwoman','Thor','The Shadow','Silver Surfer',
                              'Captain America','Hurcleus','Harry Potter','Hulk','Gandalf'))


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


  if(itemSet== 'random' | itemSet== "own"){
    set <- sample(sets,1)
    }


  itemlist <- sample(items[[set]])


  article  <- articles[[set]]
  Article  <- Articles[[set]]


  scaleset <- scales[[set]]


  thescale <- scaleset[,sample(ncol(scaleset),1)]


  pclues <- NULL

  for (i in 2:nspread) for (ii in (i-1):1) {
    pclues <- rbind(pclues, c(i,ii))
  }




  if(nclues > nspread){
   stop("Please make sure that the value of nclues is smaller or equal to nspread")
  }else{
    nclues <- uclues <- nclues
  }

  if(nspread > uclues +3 ){ warning("The large combinatorics value of nspread may result in making the distractors obviously wrong. \nSuggest making  nspread at most + 1 > nclues.")}


  clues<- NULL
    while(any(clues[,1] %in% clues[,2]) == FALSE){
      nclues <- sample(nrow(pclues), uclues)



      clues<-pclues[nclues,]
      clues <- uniquecombs(clues)
      any(clues[,1] %in% clues[,2]) == TRUE
    }

    infer <- data.frame(left =pclues[nclues,1],
                        right=pclues[nclues,2],
                        steps=1,
                        rclues=sapply(nclues,toString),
                        stringsAsFactors=FALSE)


    i <- 1

    while (i<nrow(infer)+1) {

      sub <- infer[infer[,1]==infer[i,2],]

      if (nrow(sub) > 0) {
        sub[,1] <- infer[i,1]
        sub$rclues <- paste0(infer$rclues[i],'.',sub$rclues)


        bothFALSE <-  paste0(sub[,1],sub[,2]) %in% paste0(infer[,1],infer[,2])
        if(all(bothFALSE == FALSE )==TRUE){
          infer[nrow(infer)+1,] <- c(sub$left[ii],
                                     sub$right[ii],
                                     as.numeric(sub$step[ii])+1,
                                     sub$rclues[ii])

        }else{
          sub[-which(paste0(sub[,1],sub[,2]) %in% paste0(infer[,1],infer[,2])),]
          infer[nrow(infer)+1,] <- c(sub$left[ii],
                                     sub$right[ii],
                                     as.numeric(sub$step[ii])+1,
                                     sub$rclues[ii])
         }
      }

       i <- i+1
    }


   infer[,1:3] <- sapply(infer[,1:3], as.numeric)
   (infer <- infer[order(infer[,1], decreasing=TRUE),])


  valid    <- paste0(infer[,1] ,'.',infer[,2])
  possible <- paste0(pclues[,1],'.',pclues[,2])

  (Nval <- (1:length(possible))[possible %in% valid])


  (Ninv <- (1:length(possible))[!possible %in% valid])
  invalids <- cbind(pclues[Ninv,2],pclues[Ninv,1])

  ulist    <- unique(c(clues[,1:2]))


  invkeeps <- invalids

  if (length(invkeeps) > 0 ) iinvkeeps <- rbind(
    cbind(itemlist[invkeeps[,1]],itemlist[invkeeps[,2]]),
    cbind(itemlist[invkeeps[,2]],itemlist[invkeeps[,1]]))

  if(length(invkeeps)==0) {
    iinvkeeps<- matrix(NA, nrow=0, ncol=2)
    warning(paste0("This results because all the combinations in the matrix are possible. \n Invalid distractors cannot be created. \n Return only False distractors. \n Caution when studying distractors.\n This is located in question ", seed ,".\n Solution: Try changing seed number or increasing nspread."))
  }

  falses  <- cbind(pclues[Nval,2],pclues[Nval,1])
  ifalses <- cbind(itemlist[falses[,1]],itemlist[falses[,2]])
  reverseprob <- .5



  (iclues <- cbind(itemlist[clues[,1]],itemlist[clues[,2]])) # name list

  join <- function(clue, thescale, article, forward=TRUE) {
    if (forward) return(paste0(Article, " " ,clue[1], ' is ', thescale[1], ' than ', article ," ", clue[2]))
    if (!forward) return(paste0(Article, " ",clue[2], ' is ', thescale[2], ' than ',article ," ", clue[1]))
  }


  (maxsteps <- max(infer$steps))
  (maxinferlist <- infer[infer$steps==maxsteps,])
  maxinfer <- maxinferlist[sample(nrow(maxinferlist), 1),]
  maxitems <- itemlist[as.numeric(maxinfer[1:2])]
  maxanswer <- cap(p(join(maxitems, thescale, article,
                          forward=rbinom(1, 1, reverseprob)==1),'.'))


  q <- 'Clues: '
  dreturn <- c()
  dtype <- c()
  dlist <- NULL
  if(dist=="mixed"){
    #warnings appear when there is no invalid distractors
    suppressWarnings(dlist <- rbind(cbind(iinvkeeps, type='invalid'),
                   cbind(ifalses, type='false')))
  }


  if(dist == "false"){
    if(Ndist > uclues | Ndist == uclues) stop("False distractors are only allowed if it is 1 less than the number of clues")
    dlist <- cbind(ifalses, type='false')
  }

  if(dist=="invalid"){
      dlist <- cbind(iinvkeeps, type='invalid')
  }


  q <- NULL
  for (i in 1:nrow(iclues)) {
    q <- p(q, join(iclues[i,], thescale, article,
                   forward=rbinom(1, 1, reverseprob)==1))
    if (i<nrow(iclues)) q <- p(q, ', ')
  }

  q <- p(q, '. Which of the following is implied?')
  if(Ndist > 5) stop("Please choose a lower number of distractors")


  for (i in 1:Ndist){
    if(length(nrow(dlist))>0) {
     draw <- sample(nrow(dlist), 1)
      dreturn[i] <- cap(p(join(dlist[draw,1:2], thescale, article,
                               forward=rbinom(1, 1, reverseprob)==1),'.'))
      dtype[i] <- dlist[draw,3]
      dlist <- dlist[-draw,]
      dlist
      dreturn
    }
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

