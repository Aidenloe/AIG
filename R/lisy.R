#' @importFrom mgcv uniquecombs
#' @importFrom dplyr filter
#' @importFrom dplyr lag
#' @importFrom dplyr group_by
#' @importFrom dplyr sample_n
#' @importFrom stats rbinom
#' @export
#' @param seed Generates the same question again from local computer.
#' @param nclues Generates the number of sentences to make up the item.
#' @param nspread Calculates the spread of possible incidentals in total.
#' @param clone Null means that every generated item may or may not have a different position in the inference. If given a numeric value, then the items will have the same inference position.
#' @param incidental Tells the function whether the item features are 'names' or 'objects'.
#' @param linear If linear = TRUE, a matching operator(i.e. name or object) will appear in all the adjacent clues in the sentence.
#' @param antonym Determine whether to use both antonyms ('both') or only one type ("first" or "second").
#' @param ninfer Generate answers that requires a X amount of inference from the items. Up to 3 is the maximum.
#' @param direct Deciding on whether the clues are organised in an ordered("of" = ordered forward / "ob" = ordered backward) or unordered ('alt' = alternative) fashion. Note. 'alt' can only be used when ninfer is 3 or greater.
#' @param Ndist Returns the number of distractors per question.
#' @param dist Select the type of distractors. You have three options ('mixed', 'invalid','false'). If dist='false', then the number of false distractors must be less than the number of clues by 1.
#' @param distprob Calculates the number of comparison variation for the distractors.
#' @param itemSet This is the choice of itemset you want. If itemSet='random' then the generator will randomly select one ('People', 'Fruits', 'Superheroes'). Change itemset='own' if you are using your own item set.
#' @param items Input own item type. At least 10 items. Default items are used when items = NULL.
#' @param scales Input own antonyms. At least 2 antonyms (i.e."bigger","smaller"). Default antonyms are used when scales = NULL.
#' @description This function generates linear syllogistic reasoning items. This is for research purposes.
#' @details There are several things to note. To use own item set, please have at least 10 items within the itemset. In order for antonyms comparison to work, please ensure that you have at least 2 antonyms The function will stop if the criteria is not met. The genearation of items are slower if you have a huge item set (e.g. In the millions!).
#'
#' When nspread and nclue is = 3. This means that there are 3 sentences, and only 3 names. This makes it impossible to generate an invalid distractor. As such, only the false distractors will be created. Since there are only three clues, then at most 2 false distractors can be created.
#' When nspread and nclues are the same, all the names of the invalid distractors will be taken from the names that are used in the clues. As nspread value increases, the likelihood of having names not taken from the clues increases. Making the distractors fairly easy as there is a higher likelihood that the names taken from the matrix might not appear in the clues. Hence, keeping the value of nspread and nclue as close as possible is recommended.
#'
#'
#' This function only generates items that requires up to 3 inferences. As the required inferences increases, then number of clues needed also increases. Inference is the implied comparison between sentences which allows the test taker to make an inform decision. When ninfer = 1 and the antonym is declared as either 'first' or 'second', then the correct answer will always be the opposite of the antonym used in the sentence. When ninfer = 2, the correct answer will be in the right direction.
#'
#' Direct is the direction of the line of thought. If direct = "ob" it means that solving the items requires the test taker to work 'ordered backward'. If it is 'of', it means 'ordered  forward' and finally if it is 'alt', then it means the clues are not in order. direct = 'alt' can only be used when ninfer = 3.
#'
#'When linear = FALSE, there might be a possible of having just a single mental model, or having completing models. This is random and will depend on the seed selected.
#'
#'When linear = TRUE, the direct and antonym position follow together. i.e. If direct = "of" and antonym = "first". The antonym will be the same for the question and the answer. Same when direct = "of" and antonym = "second". In such suitation, the names will follow in a linear sequence (A > B, B > C, C > D). However, when direct is changed to "ob",  then the sentence structure changes to becomes (C > D, B > C, A > B). For both situations, this is directly looking at the difficulty of mental array. Under such circumstances, 2 answers will be generated. Either one is correct, but they are the inverse antonym of each other. When direct = "both", the names will most likely not follow a linear sequence, and the antonyms will interchange between sentence (i.e. A > B, C > B, C < E).
#'
#'When linear = TRUE and infer = 3, the last sentence will always not be one of the clues for the inference. If you want to study distance effect, then what is recommended is to generate the items with ninfer = 3, and remove the last clues in the sentence structure.
#'
#'When distprob = 0.5, the distribution of the antonym for the distractors will be mixed. When distprob is either 1 or 0, then only one of the two antonym will be used. This is only used if one wishes to study distractor analysis.
#' @references
#'
#' Leth-Steensen, C., & Marley, A. A. J. (2000). A model of response time effect in symbolic comparison. \emph{Psychological Review, 107}, 62-100.
#'
#' Sternberg, R. J. (1980). Representation and process in linear syllogistic reasoning. Journal of Experimental Psychology: General, 109(2), 119.
#'
#' Sedek, G., & Von Hecker, U. (2004). Effects of subclinical depression and aging on generative reasoning about linear orders: Same or different processing limitations?. \emph{Journal of Experimental Psychology: General, 133(2)}, 237.
#' @author Aiden Loe and Francis Smart
#' @title Linear Syllogism Generator
#' @seealso \code{\link{arith}}, \code{\link{spatial2d}}, \code{\link{spatial3d}}
#' @examples
#' #Generate an item with default item set
#' lisy(seed=10,nclues=4,nspread=6,clone = NULL,incidental='names',linear=FALSE,
#'     antonym="first",ninfer = 3, direct='ob', Ndist=3,
#'     dist="mixed",distprob=0.5,itemSet='random',
#'     items= NULL,scales = NULL)
#'
#' #Item set
#' superheroes <- c('Spider man','Super man','Batman','Wolverine',
#' 'Catwoman','Thor','The Shadow','Silver Surfer', 'Flash','Wonder woman',
#' 'Mr. Fantastic', 'Aqua man', "Hawkeye", 'Starfire', 'Venom', "General Zod")
#'
#' #Antonym
#' compare <- c("taller","shorter", "older", "younger",
#'  "smaller", "bigger","stronger", "weaker")
#'
#' #Generate item with own dataset
#' lisy(seed=1,nclues=4,nspread=6,clone = NULL,incidental='names',linear=FALSE,
#'     antonym="first",ninfer = 3, direct='ob',
#'     Ndist=3, dist="mixed",distprob=0.5,
#'     itemSet='own',items= superheroes, scales = compare)
#'
#' #loop through 30 items
#' run <- NULL
#' for(i in 1:30){
#'  run[[i]]<-AIG::lisy(seed=i,nclues=4,nspread=5,clone = 1,incidental='names',linear=TRUE,
#'                      dist="false",distprob=0.5,itemSet='random',
#'                      antonym="both",ninfer = 3, direct='of', Ndist=3,
#'                     items= NULL,scales = NULL)
#' }



lisy <- function( seed=1,
                  nclues=4,
                  nspread = 5,
                  clone = NULL,
                  incidental='names',
                  linear=FALSE,
                  antonym = "both",
                  ninfer = 1,
                  direct= 'ob',
                  Ndist=4,
                  dist="mixed",
                  distprob=.5,
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

  if(incidental != 'names' && incidental !='objects'){
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
    stop("Please insert item set and scales. If not please change set = random")
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

  if(nclues< 2){
    stop("You need to generate at least 2 clues. Please increase nclue.")
  }

  if(nclues == 2 && nspread == 2){
    stop("There isn't enough names to create the sentences.")
  }

  # if(Ndist > nclues && dist== "false" | (Ndist == nclues) == TRUE && dist== "false"){
  #   stop("False distractors are only allowed if Ndist is 1 less than the number of clues")
  # }

  if(ninfer == 3 && nclues < 3 ){
    stop("To have 3 inferences you need to generate a minimum of 3 sentence with 4 names ")
  }

  if(direct =="alt" && ninfer==2 | direct =="alt" && ninfer== 1 ){
    stop("You can't have alternative clues when it is less than ninfer= 3. Increase 'ninfer' value or change arg 'direct' to 'of' or 'ob'.")
  }

  if(direct =="alt" && ninfer==2 | direct =="alt" && ninfer== 1 ){
    stop("You can't have alternative clues when it is less than ninfer= 3. Increase 'ninfer' value or change arg 'direct' to 'of' or 'ob'.")
  }

  if(nclues ==3 && nspread ==3 && ninfer == 2){
    stop("You do not have enough names to make at least 2 inference. Consider increasing nspread or nclues.")
  }

  if(ninfer > 3){
    stop("The current generator can only create up to 3 inferences per items.")
  }

if(Ndist > 4) stop("Please choose a lower number of distractors")

 if(Ndist <1){
   stop("Please increase the number of distractors.")
 }

 if(linear==TRUE && ninfer==3 && nclues==3 ){
   stop("Please reduce ninfer by 1 or increase nclues + 1 greater than ninfer.")
}

  if(linear==TRUE && direct=="alt"){
  stop("Cannot be linear when direct ='alt'.")
  }

  if(linear==TRUE && nclues==nspread){
    stop("please increase nspread by at least [nclue + 1] when linear = TRUE")
  }

  if(linear==TRUE && dist=="invalid"){
    stop("Cannot create invalid distractors. Only false. ")
  }

#
  # seed=2
  # nclues=6
  # nspread = 8
  # clone = NULL
  # incidental='names'
  # linear=FALSE
  # antonym = "both"
  # ninfer = 3
  # direct= 'ob'
  # Ndist=4
  # dist="false"
  # distprob=.5
  # itemSet='random'
  # items=NULL
  # scales = NULL



nnclues <- nnspread <- 1

  set.seed(seed)
  p <- paste0
  cap <- function(x) paste0(
    toupper(substr(x,1,1)), substr(x,2,nchar(x)))

#### DEFAULT ITEM SET ####
  if(is.null(items)  | is.null(scales)){
    sets <- c('people', 'fruit', 'superheroes')
    items <- list(people=c('Amy', 'Susan', 'Bob', 'Mary', 'Robert', 'Ernest', 'Henry', 'Peter','Jake','Jenny',
                           'Edward','Sam','Marcus','Mario'),
                  fruit=c('apple','pear','nectarine','tomato','avocado','lemon','orange','mango','peach','plum','tangerine'),
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

  if(incidental == 'objects' && itemSet == 'random' | incidental=="names" && itemSet == 'random' ){
    articles <- list(fruit='the', superheroes='the',people='')
  }else if(incidental == "objects" && itemSet == 'own'){
    articles <- list(own="the")
  }else if(incidental == "names" && itemSet == 'own'){
    articles <- list(own='')
  }else{
    stop("Please select either 'objects' or 'names'")
  }


  if(itemSet== 'random' | itemSet== "own"){
    set <- sample(sets,1)
  }

  itemlist <- sample(items[[set]])

  if(length(itemlist) < nspread){
    stop("There is insufficient unique names. Either reduce nspread, or add more items into the item set.")
  }


  article  <- articles[[set]]
  scaleset <- scales[[set]]
  thescale <- scaleset[,sample(ncol(scaleset),1)]

  # List of possible clues ####
  pclues <- NULL
  if(direct=="of" | direct == 'alt'){
    for (i in 2:nspread) for (ii in (i-1):1) {
      pclues <- rbind(pclues, c(i,ii))
    }
  }else if(direct=='ob'){
    for (i in 2:nspread) for (ii in (i-1):1) {
      pclues <- rbind(pclues, c(ii,i))
    }
  }else{
    stop("Please declare 'of', 'ob' or 'alt' for the label arg.")
  }

  # no choice. can't get 3 infer otherwise.
if(direct == 'of' && ninfer == 3 | direct == 'alt' && ninfer == 3){
    (pclues <- pclues[,c(2,1)])
  }

  if(nclues > nspread){
    stop("Please make sure that the value of nclues is smaller or equal to nspread")
  }else{
    nclues <- uclues <- nclues
  }

  if(nspread > uclues +3 ){ warning("The large combinatorics value of nspread may result in making the distractors obviously wrong. \nSuggest making nspread at most + 1 > nclues.")}


  if(ninfer == 2){
    if(!is.null(clone)){
      set.seed(clone)
    }
    redo <- 1
    while(redo==1){
      if(linear==TRUE){ #just one model
        (pclues2 <-   as.data.frame(pclues))
        (nclues <-pclues2[!duplicated(pclues2[,1]),])
        (nclues <- nclues[c(1:uclues),])
        (nclues <- as.numeric(row.names(nclues)))
        (clues<-pclues[nclues,])
        (clues <- uniquecombs(clues))
      }else{
      clues<- NULL
      while(any(clues[,1] %in% clues[,2]) == FALSE){
        (nclues <- sample(nrow(pclues), uclues))
        clues<-pclues[nclues,]
        clues <- uniquecombs(clues)
        any(clues[,1] %in% clues[,2]) == TRUE
       }
      }
       infer <- data.frame(left =pclues[nclues,1],
                          right=pclues[nclues,2],
                          steps=1,
                          rclues=sapply(nclues,toString),
                          stringsAsFactors=FALSE)
      (infer <- infer[ order(-infer[,1], -infer[,2]), ])
      # if(nnclues== 3 && nnspread == 4 && linear==TRUE){ #To remove last sentence
      #   (infer <- infer[-nrow(infer),])
      # }
      (minstep <- nrow(infer))
      i <- 1
      while (i< minstep) {
        sub <- infer[infer[,1]==infer[i,2],]
        if (nrow(sub) > 0) {
          sub[,1] <- infer[i,1]
          sub$rclues <- paste0(infer$rclues[i],'.',sub$rclues)
          exist <- paste0(sub[,1],sub[,2]) %in% paste0(infer[,1],infer[,2])

          if(any(exist)==TRUE){
            dup <- which(exist == TRUE, arr.ind=TRUE)
            sub <- sub[-dup,]
            if(all(is.na(sub))==FALSE){
              for(k in 1:nrow(sub)){
                infer[nrow(infer)+1,] <- c(sub$left[k],
                                           sub$right[k],
                                           as.numeric(sub$step[k])+1,
                                           sub$rclues[k])
              }
            }
          }
          if(all(exist==FALSE) == TRUE){
            for(k in 1:nrow(sub)){
              infer[nrow(infer)+1,] <- c(sub$left[k],
                                         sub$right[k],
                                         as.numeric(sub$step[k])+1,
                                         sub$rclues[k])
            }
          }
        }
        i <- i+1
      }

      if(any(infer$steps==2) == TRUE ){
        redo <- 2
      }

    }
  }

  if(ninfer==1){

    if(linear==TRUE){
    pclues2 <-   as.data.frame(pclues)
    (nclues <-pclues2[!duplicated(pclues2[,1]),])
    (nclues <- nclues[c(1:uclues),])
    (nclues <- as.numeric(row.names(nclues)))
    (clues<-pclues[nclues,])
    clues <- uniquecombs(clues)
    }else{
    clues<- NULL
    while(any(clues[,1] %in% clues[,2]) == FALSE){
      if(!is.null(clone)){
        set.seed(clone)
      }
      (nclues <- sample(nrow(pclues), uclues))
      clues<-pclues[nclues,]
      clues <- uniquecombs(clues)
      any(clues[,1] %in% clues[,2]) == TRUE
      }
    }

    infer <- data.frame(left =pclues[nclues,1],
                        right=pclues[nclues,2],
                        steps=1,
                        rclues=sapply(nclues,toString),
                        stringsAsFactors=FALSE)
    (infer <- infer[ order(-infer[,1], -infer[,2]), ])
    # if(nnclues== 3 && nnspread == 4 && linear==TRUE){ #To remove last sentence
    #   (infer <- infer[-nrow(infer),])
    # }
  }
  if(ninfer == 3){
    if(!is.null(clone)){
      set.seed(clone)
    }
    redo <- 1
    while(redo==1){
      if(linear==TRUE){ #just one model
        (pclues2 <-   as.data.frame(pclues))
        (nclues <-pclues2[!duplicated(pclues2[,1]),])
        (nclues <- nclues[c(1:uclues),])
        (nclues <- as.numeric(row.names(nclues)))
        (clues<-pclues[nclues,])
        (clues <- uniquecombs(clues))
      }else{
      clues<- NULL
      while(any(clues[,1] %in% clues[,2]) == FALSE){
        nclues <- sample(nrow(pclues), uclues)
        clues<-pclues[nclues,]
        (clues <- uniquecombs(clues))
        any(clues[,1] %in% clues[,2]) == TRUE
       }
      }
      infer <- data.frame(left =pclues[nclues,1],
                          right=pclues[nclues,2],
                          steps=1,
                          rclues=sapply(nclues,toString),
                          stringsAsFactors=FALSE)

      if(linear==TRUE) (infer <- infer[ order(-infer[,1], -infer[,2]), ])
      # if(nnclues== 3 && nnspread == 4 && linear==TRUE){ #To remove last sentence
      #     (infer <- infer[-nrow(infer),])
      #     }
      (minstep <- nrow(infer))
      i <- 1
      while (i< minstep) {
        sub <- infer[infer[,1]==infer[i,2],]
        if (nrow(sub) > 0) {
          sub[,1] <- infer[i,1]
          sub$rclues <- paste0(infer$rclues[i],'.',sub$rclues)
          exist <- paste0(sub[,1],sub[,2]) %in% paste0(infer[,1],infer[,2])
          if(any(exist)==TRUE){
            dup <- which(exist == TRUE, arr.ind=TRUE)
            sub <- sub[-dup,]
            if(all(is.na(sub))==FALSE){
              for(k in 1:nrow(sub)){
                infer[nrow(infer)+1,] <- c(sub$left[k],
                                           sub$right[k],
                                           as.numeric(sub$step[k])+1,
                                           sub$rclues[k])
              }
            }
          }


          if(all(exist==FALSE) == TRUE){
            for(k in 1:nrow(sub)){
              infer[nrow(infer)+1,] <- c(sub$left[k],
                                         sub$right[k],
                                         as.numeric(sub$step[k])+1,
                                         sub$rclues[k])
            }
          }
        }
        i <- i+1
      }
      infer
      if(any(infer$steps==3) == TRUE ){
        redo <- 3
      }

    }

  }


  (infer[,1:3] <- sapply(infer[,1:3], as.numeric))
  (infer <- infer[order(infer[,1], decreasing=TRUE),])

##### FUNCTION TO GENERATE ITEM #####
join <- function(clue, thescale, article, forward=TRUE) {
  if (forward) return(paste0(article, " " ,clue[1], ' is ', thescale[1], ' than ', article ," ", clue[2]))
  if (!forward) return(paste0(article, " ",clue[2], ' is ', thescale[2], ' than ',article ," ", clue[1]))
}

  ############# VALID RESPONSES ##############
  (valid    <- paste0(infer[,1] ,'.',infer[,2]))
  (possible <- paste0(pclues[,1],'.',pclues[,2]))

  (Nval <- (1:length(possible))[possible %in% valid])

  ############### INVALID RESPONSES ##########
  (Ninv <- (1:length(possible))[!possible %in% valid]) #checking for invalid
  (invalids <- cbind(pclues[Ninv,2],pclues[Ninv,1]))

  invkeeps <- invalids

  if(ninfer==2 & direct=='ob'){
    (result = invalids[invalids[, 2] == invalids[, 1] - 1,]) # keep unique rows
    (invkeeps = result[!duplicated(result[, 1]), ]) # ensure no duplicates
      }
  if(ninfer==2 & direct=='of'){
        (result = invalids[invalids[, 2] == invalids[, 1] + 1,]) # keep unique rows
        (invkeeps = result[!duplicated(result[, 1]), ]) # ensure no duplicates
    }

  if (length(invkeeps) > 0 ) iinvkeeps <- rbind(
    cbind(itemlist[invkeeps[,1]],itemlist[invkeeps[,2]]),
    cbind(itemlist[invkeeps[,2]],itemlist[invkeeps[,1]]))

if(length(invkeeps)==0) {
    iinvkeeps<- matrix(NA, nrow=0, ncol=2)
}

if(linear==TRUE){
  iinvkeeps<- matrix(NA, nrow=0, ncol=2)
}

  ############ FALSE RESPONSES ##########
  (falses  <- cbind(pclues[Nval,2],pclues[Nval,1]))
  (ifalses <- cbind(itemlist[falses[,1]],itemlist[falses[,2]]))

if(ninfer==3 && linear==TRUE && direct == "of"){ #Flip back the matrix to forward
  (falses  <- cbind(pclues[Nval,1],pclues[Nval,2]))
  (ifalses <- cbind(itemlist[falses[,1]],itemlist[falses[,2]]))
}


  #### GENERATE CORRECT RESPONSE OPTION #####
  (maxsteps <- max(infer$steps))
  if(ninfer ==2){
    (maxinferlist <- infer[infer$steps==2,])
  }
  if(ninfer ==1){
    (maxinferlist <- infer[infer$steps==1,])
  }
  if(ninfer ==3){
    (maxinferlist <- infer[infer$steps==3,])
  }
  if(!is.null(clone)){
    set.seed(clone)
  }

(maxinfer <- maxinferlist[sample(nrow(maxinferlist), 1),])
(maxitems <- itemlist[as.numeric(maxinfer[1:2])])
if(ninfer==3 && linear==TRUE && direct == "of"){ #Flip back the matrix to forward
  (maxitems <- rev(itemlist[as.numeric(maxinfer[1:2])]))# The answer has to flip as well.
}


  ##### Answer ####
  if(antonym=='both'){
    #maxanswer <- cap(p(join(maxitems, thescale, article,
     #                       forward=rbinom(1, 1, distprob)==1),'.'))
      maxanswer <- cap(p(join(maxitems, thescale, article,
                              forward=TRUE),'.'))

      if(ninfer == 1 && linear==TRUE ){
        maxanswer <- cap(p(join(maxitems, thescale, article,
                                forward=TRUE),'.'))
        maxanswer2 <- cap(p(join(maxitems, thescale, article,
                                 forward=FALSE),'.'))
      }

      if(ninfer == 2 && linear==TRUE ){
        maxanswer <- cap(p(join(maxitems, thescale, article,
                                forward=TRUE),'.'))
        maxanswer2 <- cap(p(join(maxitems, thescale, article,
                                 forward=FALSE),'.'))
      }

    if(ninfer == 3 && linear==TRUE){
      maxanswer <- cap(p(join(maxitems, thescale, article,
                              forward=TRUE),'.'))
      maxanswer2 <- cap(p(join(maxitems, thescale, article,
                              forward=FALSE),'.'))
    }


  }else if(antonym == 'first'){
    (maxanswer <- cap(p(join(maxitems, thescale, article,
                            forward=TRUE),'.')))

    if(ninfer == 1){
      maxanswer <- cap(p(join(maxitems, thescale, article,
                              forward=FALSE),'.'))
    }

    if(ninfer == 1 && linear==TRUE ){
      maxanswer <- cap(p(join(maxitems, thescale, article,
                              forward=TRUE),'.'))
      maxanswer2 <- cap(p(join(maxitems, thescale, article,
                               forward=FALSE),'.'))
    }

    if(ninfer == 2 && linear==TRUE ){
      maxanswer <- cap(p(join(maxitems, thescale, article,
                              forward=TRUE),'.'))
      maxanswer2 <- cap(p(join(maxitems, thescale, article,
                               forward=FALSE),'.'))

    }

    if(ninfer==3 && linear==TRUE){
      maxanswer <- cap(p(join(maxitems, thescale, article,
                              forward=FALSE),'.'))
      maxanswer2 <- cap(p(join(maxitems, thescale, article,
                              forward=TRUE),'.'))
    }


  }else if (antonym == "second"){
    maxanswer <- cap(p(join(maxitems, thescale, article,
                            forward=FALSE),'.'))
    maxanswer
    if(ninfer == 1){
      maxanswer <- cap(p(join(maxitems, thescale, article,
                              forward=TRUE),'.'))
    }

    if(ninfer == 1 && linear==TRUE ){
      maxanswer <- cap(p(join(maxitems, thescale, article,
                              forward=TRUE),'.'))
      maxanswer2 <- cap(p(join(maxitems, thescale, article,
                               forward=FALSE),'.'))

    }

    if(ninfer == 2 && linear==TRUE ){
      maxanswer <- cap(p(join(maxitems, thescale, article,
                              forward=TRUE),'.'))
      maxanswer2 <- cap(p(join(maxitems, thescale, article,
                              forward=FALSE),'.'))

    }

    if(ninfer==3 && linear==TRUE){
      maxanswer <- cap(p(join(maxitems, thescale, article,
                              forward=TRUE),'.'))
      maxanswer2 <- cap(p(join(maxitems, thescale, article,
                               forward=FALSE),'.'))
    }

  }else{
    stop("Please select either 'both', 'first' or 'second' comparison.")
  }

    #for linear TRUE
    if(exists("maxanswer2")==FALSE){
      maxanswerFinal <-maxanswer
    }else{
      maxanswerFinal <- paste(maxanswer,maxanswer2)
      rm(maxanswer2)
    }


#### ###### #### CLUES ORDERING IN THE SENTENCE ### #### ####
if(direct  == "of"){

  (clues<- clues[order(clues[,1], decreasing = TRUE),])
  (rclues <- unlist(strsplit(maxinfer[,4], "[.]")))
  (rclues <- (1:length(infer$rclues))[infer$rclues %in% rclues])
  rclues <- infer[rclues,1:4] #clues

  (pos    <- paste0(rclues[,1] ,'.',rclues[,2]))
  (pos2 <- paste0(clues[,1],'.',clues[,2]))
  infer
  check<- NULL
  for(i in pos){
    (check[i] <- (1:length(pos2))[pos2 %in% i])
  }
  check
  (iclues <- cbind(itemlist[clues[,1]],itemlist[clues[,2]]))
  # if(nnclues== 3 && nnspread == 4 && linear==TRUE){ #To remove last sentence
  #   ( iclues <- iclues[-nrow(iclues),])
  # }

  if(ninfer==3 && linear==TRUE){ #Flip back the matrix to forward
    (iclues <- cbind(iclues[,2], iclues[,1]))
  }

}else if(direct == "ob"){
  maxinfer
  (clues<- clues[order(clues[,1], decreasing = TRUE),])
  (rclues <- unlist(strsplit(maxinfer[,4], "[.]")))
  (rclues <- (1:length(infer$rclues))[infer$rclues %in% rclues])
  (rclues <- infer[rclues,1:4]) #clues
  (pos    <- paste0(rclues[,1] ,'.',rclues[,2]))
  (pos2 <- paste0(clues[,1],'.',clues[,2]))

  check<- NULL
  for(i in pos){
    (check[i] <- (1:length(pos2))[pos2 %in% i])
  }
  check <- rev(check)
  check

  (iclues <- cbind(itemlist[clues[,1]],itemlist[clues[,2]]))
  # if(nnclues== 3 && nnspread == 4 && linear==TRUE){  #To remove last sentence
  #   ( iclues <- iclues[-nrow(iclues),])
  # }



}else if(direct == "alt"){
  if(ninfer==3){
    mixed <- FALSE
    while(mixed==FALSE){ #always get mix
      (rclues <- unlist(strsplit(maxinfer[,4], "[.]")))
      (rclues <- (1:length(infer$rclues))[infer$rclues %in% rclues])

      (rclues <- infer[rclues,1:4]) #clues
      (rclues <- sample_n(rclues, nrow(rclues)))

      (pos    <- paste0(rclues[,1] ,'.',rclues[,2]))
      ((pos2 <- paste0(clues[,1],'.',clues[,2])))
      check<- NULL
      for(i in pos){
        (check[i] <- (1:length(pos2))[pos2 %in% i])
      }

      if(is.unsorted(rev(check)) == TRUE| is.unsorted(check)==TRUE)
        mixed <- TRUE
    }

    (pos    <- paste0(rclues[,1] ,'.',rclues[,2])) # check for clues
    (pos2 <- paste0(clues[,1],'.',clues[,2])) # check for all combination
    (a <- (1:length(pos2))[!pos2 %in% pos]) # remove left out combination
    (leftOut <- clues[a,1:2])
    altclues<- rbind(leftOut, rclues[,1:2]) # rearrange order

    (iclues <- cbind(itemlist[altclues[,1]],itemlist[altclues[,2]])) # new ordering for names

  }

  maxinfer
  (clues<- clues[order(clues[,1], decreasing = TRUE),])
  (rclues <- unlist(strsplit(maxinfer[,4], "[.]")))
  (rclues <- (1:length(infer$rclues))[infer$rclues %in% rclues])
  (rclues <- infer[rclues,1:4]) #clues
  (pos    <- paste0(rclues[,1] ,'.',rclues[,2]))
  (pos2 <- paste0(clues[,1],'.',clues[,2]))

  check<- NULL
  for(i in pos){
    (check[i] <- (1:length(pos2))[pos2 %in% i])
  }

  (randomised <- sample(1:2,1))
  if(randomised == 1){
  (check <- rev(check))
  }

  (iclues <- cbind(itemlist[clues[,1]],itemlist[clues[,2]]))


}else{
  stop("Please declare 'of', 'ob' or 'alt' for the label arg.")
}


#if(direct == "alt" && ninfer==3 && nnclues== 3 && nnspread == 4 && linear==TRUE){
# message("Cannot be linear when direct =")
# clues<- clues[order(clues[,1], decreasing = TRUE),]
# rclues <- unlist(strsplit(maxinfer[,4], "[.]"))
# (rclues <- (1:length(infer$rclues))[infer$rclues %in% rclues])
# rclues <- infer[rclues,1:4] #clues
# pos    <- paste0(rclues[,1] ,'.',rclues[,2])
# pos2 <- paste0(clues[,1],'.',clues[,2])
#
# check<- NULL
# for(i in pos){
#   (check[i] <- (1:length(pos2))[pos2 %in% i])
# }
# check <- rev(check)
# alt1 <- rbind(c(2,1,3),c(3,1,2))
# check <- alt1[sample(1:2,1),]
# check
#
# (cclues <- clues[-nrow(clues),])
# cclues <- cbind(c(1,2,3),cclues)
# cclues<- cclues[match(check, cclues),]
# cclues <- cclues[,-1] # To move last sentence
# (iclues <- cbind(itemlist[cclues[,1]],itemlist[cclues[,2]]))
# }


(inferClues<- (t(as.numeric(check))))

if(is.na(inferClues[2])){
  inferClues[2] <- " "
}
if(is.na(inferClues[3])){
  inferClues[3] <- " "
}
inferClues



# CREATE SENTENCE STRUCTURE ####
q <- 'Clues: '
dreturn <- c()
dtype <- c()

q <- NULL
if(antonym=='both'){
  for (i in 1:nrow(iclues)) {
    q <- p(q, join(iclues[i,], thescale, article,
                   forward=rbinom(1, 1, distprob)==1))
    if (i<nrow(iclues)) q <- p(q, ', ')
  }
}else if(antonym == 'first'){
  for (i in 1:nrow(iclues)) {
    q <- p(q, join(iclues[i,], thescale, article,
                   forward=TRUE))
    if (i<nrow(iclues)) q <- p(q, ', ')
  }


}else if (antonym == "second"){
  for (i in 1:nrow(iclues)) {
    q <- p(q, join(iclues[i,], thescale, article,
                   forward=FALSE))
    if (i<nrow(iclues)) q <- p(q, ', ')
  }
}else{
  stop("Please select declare either 'both', 'first' or 'second' comparison.")
}


q
q <- p(q, '. Which of the following is implied?')
q <-   paste(toupper(substring(q, 1,1)),substring(q, 2),sep="", collapse=" ")
q


dist
#### DISTRACTORS #####
    # ###### create matrix of invalid and false distractors ########
    dlist <- NULL
    if(dist=="mixed" && linear == FALSE){
      if(all(is.na(iinvkeeps)) == TRUE ){
        message(paste0("This results because all the combinations in the matrix are possible. \nInvalid distractors cannot be created. \nReturn only False distractors. \nCaution when studying distractors.\nThis is located in question ", seed ,".\nSolution: Try increasing nspread"))
        suppressWarnings(dlist <- rbind(cbind(iinvkeeps, type='invalid'),
                                        cbind(ifalses, type='false')))
      }else{
       (dlist <- rbind(cbind(iinvkeeps, type='invalid'),
                       cbind(ifalses, type='false')))
      }
    }
dlist
    if(dist=="mixed" && linear==TRUE){
      if(all(is.na(iinvkeeps)) == TRUE ){
        message("\nInvalid distractors cannot be created. \nReturn only False distractors.")
        suppressWarnings(dlist <- rbind(cbind(iinvkeeps, type='invalid'),
                                        cbind(ifalses, type='false')))
      dlist
        }else{
        dlist <- rbind(cbind(iinvkeeps, type='invalid'),
                       cbind(ifalses, type='false'))
      }
    }

    dlist
    if(dist == "false"){
      (dlist <- cbind(ifalses, type='false'))
    }

    if(dist=="invalid"){
      if(all(is.na(iinvkeeps)) == TRUE){
        stop("Please increase nclues and nspread. Impossible to generate invalid distractors.")
      }
      if(uclues ==4 && nspread ==4 && dist== "invalid"){
        warning("Only a maximum of 3 invalid distractors can be generated when nclues is equals to 4.")
      }
      dlist <- cbind(iinvkeeps, type='invalid')
    }

    if(nrow(dlist) < Ndist){
      stop("Can't create that many distractors. Consider lowing to 3 distractors when infer = 1")
    }


      # Create actual distractors ####
      if(dist=="mixed"){
        (dlist.df <- as.data.frame(dlist))
        if(length(unique(dlist.df$type))==2){ #if 2 = "invalid" and "falses" distractor type
          (type <- dlist.df$type)
          (dlist.df <- sample_n(group_by(dlist.df,type),size=ceiling(Ndist/2),replace=FALSE,weight=NULL ,0.5))
          (dlist.df <- dlist.df[sample(nrow(dlist.df)),])

          if(Ndist %% 2 == 0){
            (dtype <- dlist.df[,3])
          }else{
            dtype <- dlist.df[-nrow(dlist.df),3]
          }

        (dtype <- as.character(as.matrix(dtype)))

        }else{ # if only one distractor type
          (dlist.df <- as.data.frame(dlist))
          (type <- dlist.df$type)
          (dlist.df <- sample_n(group_by(dlist.df,type), size=Ndist,replace=FALSE,weight=NULL ,0.5))
          (dtype <- dlist.df[,3])
          (dtype <- as.character(as.matrix(dtype)))
        }

        (dlist.df <- as.matrix(dlist.df))
        dreturn<- NULL
        for (i in 1:Ndist){
          dreturn[i] <- cap(p(join(dlist.df[i,1:2], thescale, article,
                                   forward=rbinom(1, 1, distprob)==1),'.')) # create an incorrect sentence
        }
        dreturn
      }else{
        if(Ndist > nrow(dlist)){
          stop("There are not enough invalid distractors. Please change the arg dist to 'mixed' or 'false'")
        }
        draw  <- dlist[sample(nrow(dlist),Ndist),]
        if(Ndist==1){
          draw <- t(as.matrix(draw))
          dtype <- draw[,3]
          for (i in 1:Ndist){
            dreturn[i] <- cap(p(join(draw[i,1:2], thescale, article,
                                     forward=rbinom(1, 1, distprob)==1),'.')) # create an incorrect sentence
          }
        }else{
          dtype <- draw[,3]
          for (i in 1:Ndist){
            dreturn[i] <- cap(p(join(draw[i,1:2], thescale, article,
                                     forward=rbinom(1, 1, distprob)==1),'.')) # create an incorrect sentence
          }
        }
      }

  finalList <- data.frame(seed=seed,
             Question=q,
             ninfer=ninfer,
             Answer=maxanswerFinal,
             dist1=dreturn[1],
             dtype1=dtype[1],
             dist2=dreturn[2],
             dtype2=dtype[2],
             dist3=dreturn[3],
             dtype3=dtype[3],
             dist4=dreturn[4],
             dtype4=dtype[4],
             clues.1 = inferClues[1],
             clues.2 = inferClues[2],
             clues.3 = inferClues[3]
  )


  class(finalList) <- c("lisy")
  return(finalList)
finalList
}

