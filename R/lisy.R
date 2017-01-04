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
#' @param incidental Tells the function whether the item features are 'names' or 'objects'.
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
#'
#' Direct is the direction of the line of thought. If direct = "ob" it means that solving the items requires the test taker to work 'ordered backward'. If it is 'of', it means 'ordered  forward' and finally if it is 'alt', then it means the clues are not inorder. direct = 'alt' can only be used when ninfer = 3.
#'
#'When distprob = 0.5, the distribution of the antonym for the distractors will be mixed. When distprob is either 1 or 0, then only one of the two antonym will be used. This is only used if one wishes to study distractor analysis.
#'
#' @author Aiden Loe and Francis Smart
#' @title lisy
#' @examples
#' #Generate an item with default item set
#' lisy(seed=10,nclues=4,nspread=6,incidental='names',
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
#' lisy(seed=1,nclues=4,nspread=6,incidental='names',
#'     antonym="first",ninfer = 3, direct='ob',
#'     Ndist=3, dist="mixed",distprob=0.5,
#'     itemSet='own',items= superheroes, scales = compare)
#'
#' #loop through 30 items
#' nitems <- 30
#' params <- data.frame(seed=1:nitems,
#'                      nclues=ceiling((1:nitems)/20)+3,
#'                      nspread=ceiling((1:nitems)/15)+4)
#'
#' qtable <- NULL
#' for (i in 1:nitems) {
#'   runs <- lisy(seed=i,
#'                nclues=params$nclues[i],
#'                nspread=params$nspread[i],
#'                incidental= 'names',antonym="first",ninfer = 2,
#'                direct='of', Ndist=4,dist="mixed",distprob=.5,
#'                 itemSet='own', items= superheroes, scales = compare)
#'   qtable[[i]] <- runs
#' }
#'
#' qtable
#'


lisy <- function( seed=1,
                  nclues=4,
                  nspread = 5,
                  incidental='names',
                  antonym = "both",
                  ninfer = 1,
                  direct= 'of',
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

  if(Ndist > nclues && dist== "false" | (Ndist == nclues) == TRUE && dist== "false"){
    stop("False distractors are only allowed if Ndist is 1 less than the number of clues")
  }

  if(ninfer == 3 && nclues < 3 | ninfer==3 && nspread < 4){
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

  if(Ndist <1){
    stop("Please increase the number of distractors.")
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

  # List of possible clues
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



  if(nclues > nspread){
    stop("Please make sure that the value of nclues is smaller or equal to nspread")
  }else{
    nclues <- uclues <- nclues
  }

  if(nspread > uclues +3 ){ warning("The large combinatorics value of nspread may result in making the distractors obviously wrong. \nSuggest making nspread at most + 1 > nclues.")}


  if(ninfer == 2){
    redo <- 1
    while(redo==1){
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

      minstep <- nrow(infer)
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


  }
  if(ninfer == 3){
    redo <- 1
    while(redo==1){
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

      minstep <- nrow(infer)

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

      if(any(infer$steps==3) == TRUE ){
        redo <- 3
      }

    }

  }


  infer[,1:3] <- sapply(infer[,1:3], as.numeric)
  (infer <- infer[order(infer[,1], decreasing=TRUE),])

  ############# VALID RESPONSES ##############
  valid    <- paste0(infer[,1] ,'.',infer[,2])
  possible <- paste0(pclues[,1],'.',pclues[,2])

  (Nval <- (1:length(possible))[possible %in% valid])


  ############### INVALID RESPONSES ##########
  (Ninv <- (1:length(possible))[!possible %in% valid])
  invalids <- cbind(pclues[Ninv,2],pclues[Ninv,1])
  ulist    <- unique(c(clues[,1:2]))
  invkeeps <- invalids

  if (length(invkeeps) > 0 ) iinvkeeps <- rbind(
    cbind(itemlist[invkeeps[,1]],itemlist[invkeeps[,2]]),
    cbind(itemlist[invkeeps[,2]],itemlist[invkeeps[,1]]))


  if(length(invkeeps)==0) {
    iinvkeeps<- matrix(NA, nrow=0, ncol=2)
  }


  ############ FALSE RESPONSES ##########
  falses  <- cbind(pclues[Nval,2],pclues[Nval,1])
  ifalses <- cbind(itemlist[falses[,1]],itemlist[falses[,2]])




  ##### FUNCTION TO GENERATE ITEM #####
  join <- function(clue, thescale, article, forward=TRUE) {
    if (forward) return(paste0(article, " " ,clue[1], ' is ', thescale[1], ' than ', article ," ", clue[2]))
    if (!forward) return(paste0(article, " ",clue[2], ' is ', thescale[2], ' than ',article ," ", clue[1]))
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
  maxinfer <- maxinferlist[sample(nrow(maxinferlist), 1),]
  maxitems <- itemlist[as.numeric(maxinfer[1:2])]


  if(antonym=='both'){
    maxanswer <- cap(p(join(maxitems, thescale, article,
                            forward=rbinom(1, 1, distprob)==1),'.'))
  }else if(antonym == 'first'){
    maxanswer <- cap(p(join(maxitems, thescale, article,
                            forward=TRUE),'.'))
    if(ninfer == 1){
      maxanswer <- cap(p(join(maxitems, thescale, article,
                              forward=FALSE),'.'))
    }

  }else if (antonym == "second"){
    maxanswer <- cap(p(join(maxitems, thescale, article,
                            forward=FALSE),'.'))
    if(ninfer == 1){
      maxanswer <- cap(p(join(maxitems, thescale, article,
                              forward=TRUE),'.'))
    }
  }else{
    stop("Please select either 'both', 'first' or 'second' comparison.")
  }


  #### ###### #### CLUES ORDERING IN THE SENTENCE ### #### ####
  if(direct  == "of"){
    clues<- clues[order(clues[,1], decreasing = TRUE),]
    rclues <- unlist(strsplit(maxinfer[,4], "[.]"))
    (rclues <- (1:length(infer$rclues))[infer$rclues %in% rclues])
    rclues <- infer[rclues,1:4] #clues

    pos    <- paste0(rclues[,1] ,'.',rclues[,2])
    pos2 <- paste0(clues[,1],'.',clues[,2])


    check<- NULL
    for(i in pos){
      (check[i] <- (1:length(pos2))[pos2 %in% i])
    }
    (iclues <- cbind(itemlist[clues[,1]],itemlist[clues[,2]]))
  }else if(direct == "ob"){
    clues<- clues[order(clues[,1], decreasing = TRUE),]
    rclues <- unlist(strsplit(maxinfer[,4], "[.]"))
    (rclues <- (1:length(infer$rclues))[infer$rclues %in% rclues])
    rclues <- infer[rclues,1:4] #clues
    pos    <- paste0(rclues[,1] ,'.',rclues[,2])
    pos2 <- paste0(clues[,1],'.',clues[,2])

    check<- NULL
    for(i in pos){
      (check[i] <- (1:length(pos2))[pos2 %in% i])
    }
    check <- rev(check)
    (iclues <- cbind(itemlist[clues[,1]],itemlist[clues[,2]]))
  }else if(direct == "alt"){
    if(ninfer==3){
      mixed <- FALSE
      while(mixed==FALSE){
        rclues <- unlist(strsplit(maxinfer[,4], "[.]"))

        (rclues <- (1:length(infer$rclues))[infer$rclues %in% rclues])
        rclues <- infer[rclues,1:4] #clues

        pos    <- paste0(rclues[,1] ,'.',rclues[,2])
        pos2 <- paste0(clues[,1],'.',clues[,2])

        check<- NULL
        for(i in pos){
          (check[i] <- (1:length(pos2))[pos2 %in% i])
        }
        if(is.unsorted(check)==TRUE){
          mixed <- TRUE
        }
        if(is.unsorted(check)==FALSE | is.unsorted(rev(check)) == FALSE){
          clues <-   clues[sample(nrow(clues)),]
          mixed<-FALSE
        }
      }
    }
    (iclues <- cbind(itemlist[clues[,1]],itemlist[clues[,2]]))
  }else{
    stop("Please declare 'of', 'ob' or 'alt' for the label arg.")
  }

  inferClues<- (t(as.numeric(check)))

  if(is.na(inferClues[2])){
    inferClues[2] <- " "
  }
  if(is.na(inferClues[3])){
    inferClues[3] <- " "
  }



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

  q <- p(q, '. Which of the following is implied?')
  if(Ndist > 5) stop("Please choose a lower number of distractors")

  q <-   paste(toupper(substring(q, 1,1)),substring(q, 2),sep="", collapse=" ")

  # create matrix of invalid and false distractors ####
  dlist <- NULL
  if(dist=="mixed"){
    if(all(is.na(iinvkeeps)) == TRUE){
      warning(paste0("This results because all the combinations in the matrix are possible. \nInvalid distractors cannot be created. \nReturn only False distractors. \nCaution when studying distractors.\nThis is located in question ", seed ,".\nSolution: Try increasing nspread."))
      suppressWarnings(dlist <- rbind(cbind(iinvkeeps, type='invalid'),
                                      cbind(ifalses, type='false')))
    }else{
      dlist <- rbind(cbind(iinvkeeps, type='invalid'),
                     cbind(ifalses, type='false'))
    }
  }

  if(dist == "false"){
    dlist <- cbind(ifalses, type='false')
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

  # Create actual distractors ####
  if(dist=="mixed"){
    dlist.df <- as.data.frame(dlist)
    if(length(unique(dlist.df$type))==2){ #if 2 = "invalid" and "falses" distractor type
      type <- dlist.df$type
      dlist.df <- sample_n(group_by(dlist.df,type),ceiling(Ndist/2), 0.5)

      dlist.df <- dlist.df[sample(nrow(dlist.df)),]

      if(Ndist %% 2 == 0){
        dtype <- dlist.df[,3]

      }else{
        dtype <- dlist.df[-nrow(dlist.df),3]
      }

      dtype <- as.character(as.matrix(dtype))

    }else{ # if only one distractor type
      dlist.df <- as.data.frame(dlist)
      type <- dlist.df$type
      dlist.df<- sample_n(group_by(dlist.df,type),Ndist, 0.5)
      dtype <- dlist.df[,3]
      dtype <- as.character(as.matrix(dtype))
    }
    dlist.df <- as.matrix(dlist.df)
    dreturn<- NULL
    for (i in 1:Ndist){
      dreturn[i] <- cap(p(join(dlist.df[i,1:2], thescale, article,
                               forward=rbinom(1, 1, distprob)==1),'.')) # create an incorrect sentence
    }

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
             dtype5=dtype[5],
             clues.1 = inferClues[1],
             clues.2 = inferClues[2],
             clues.3 = inferClues[3]
  )

  class(finalList) <- "lisy"
  return(finalList)


}



