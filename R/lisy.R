#' @importFrom mgcv uniquecombs
#' @importFrom dplyr filter
#' @importFrom dplyr lag
#' @importFrom dplyr group_by
#' @importFrom dplyr sample_n
#' @importFrom stats rbinom
#' @export
#' @param seed This generates the same question again from local computer.
#' @param nclues This is the number of sentences to make up the item.
#' @param nspread This is the spread of the number of incidentals in total.
#' @param distprob This calculates how you much comparison variation you want for the distractors.  
#' @param Ndist This returns the number of distractors per question.
#' @param incidental This tells the function whether the item features are 'names' or 'objects'.
#' @param dist This allows you to select the type of distractors. You have three options ('mixed', 'invalid','false'). If dist='false', then the number of false distractors must be less than the number of clues.
#' @param itemSet This is the choice of itemset you want. If itemset='random' then the generator will randomly select one (People, Fruits, Superheroes). Change itemset='own' if you are using your own item set.
#' @param direct This determine if the clues are organised in an ordered("of" = ordered forward / "ob" = ordered backward) or unordered ('alt' = alternative) fashion. 'alt' can only be used when ninfer is 3 or greater. 
#' @param terms This determine whether you want to use both comparsion terms ('both') or only one type ("forward" or "backward"). 
#' @param ninfer This generate answers that requires a X amount of inference from the items. 
#' @param items This inputs your own item type. At least 10 items.
#' @param scales This is the comparison terms. At least 2 comparison terms (i.e."bigger","smaller")
#' @description This function generates linear syllogistic reasoning items.This is for research purposes.
#' @details There are several things to bear in mind. To use own item set, please have at least 10 items within the itemset. In order for scale comparison to make sure. Please ensure that you have at least 2 comparisons. The function will stop if the criteria is not met. The genearation of items are slower if you have a huge item set.
#'
#' When nspread and nclue is = 3. This means that there are 3 sentences, and only 3 names. This makes it impossible to generate an invalid distractor. As such, only the false distractors will be created. Since there are only three clues, then 3 false distractors will be created.
#'
#' When nspread and nclues are the same. All the names of the invalid distractors will be taken from the names that are used in the clues. As nspread value increases, the likelihood of having names not taken from the clues increases.
#' 
#' When ninfer = 1 and the terms is declared as either 'forward' or 'backward', then the correct answer will always be the opposite of the comparing statements in the sentence. When ninfer = 2, the correct answer will be in the right direction. 
#' 
#' When distprob = 0.5, the distribution of the comparsion terms for the distractors will be mixed. When distprob is either 1 or 0, then only one of the two comparison terms will be used. 
#' 
#' This function only generates items that requires up to 3 inferences. As the required inferences increases, then number of clues increases. Inference is the implied comparison between sentences which allows you to make select the correct answer.  
#' 
#' Direct is the direction of the line of thought. If direct = "ob" it means that solving the items require you work 'ordered backwards'. If it is 'of', it means 'ordered  forwards' and finally if it is 'alt', then it means the clues are not inorder. 
#' @author Aiden Loe and Francis Smart
#' @title lisy
#' @examples \dontrun{
#' 
#' #Generate an item
#' item <- lisy(seed=4,nclues=5,nspread=5,Ndist=4, incidental='names',dist="false",distprob=.5,
#'    itemSet='random',direct='of', terms="backward",ninfer = 2,items= NULL,scales = NULL)
#'    
#' #Save csv file
#' write.csv(item, file="~/desktop/test.csv"  )
#'  
#' #Test with data set     
#' library("babynames")
#' bNames <- sapply(babynames[,3], as.character)
#' compare <- c("taller","shorter", "older", "younger", "smaller", "bigger","stronger", "weaker")
#' 
#' lisy(seed=4, nclues=4, nspread=5,Ndist=4, incidental= 'names',dist="mixed", distprob=.5, 
#'      itemSet='own',direct='of',terms="backward",ninfer = 2,items= bNames, scales = compare)
#'
#' #loop through 30 items
#' nitems <- 30
#' params <- data.frame(seed=1:nitems,
#'                      nclues=ceiling((1:nitems)/20)+3,
#'                      nspread=ceiling((1:nitems)/15)+4)
#' params$nclues
#' params$nspread
#' qtable <- NULL
#' for (i in 1:nitems) {
#'   runs <- lisy(seed=i,
#'                nclues=params$nclues[i],
#'                nspread=params$nspread[i],
#'                Ndist=4, incidental= 'names',dist="mixed", distprob=.5, 
#'                itemSet='own',direct='of',terms="backward",ninfer = 2,
#'                items= bNames, scales = compare)
#'   qtable[[i]] <- runs
#' }
#'
#' qtable
#' write.csv(do.call("rbind",qtable), file="~/desktop/test.csv"  )
#' }
#'
#'


lisy <- function( seed=1,
                  nclues=4,
                  nspread = 5,
                  distprob=.5,
                  Ndist=4,
                  incidental='names',
                  dist="mixed",
                  itemSet='random',
                  direct= 'of',
                  terms = "both",
                  ninfer = 1,
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
  
  if(ninfer > 3){
    stop("The current generator can only create up to 3 inferences per items.")
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
  
  
  if(incidental == 'objects' && itemSet == 'random'){
    articles <- list(fruit='the', superheroes='the')
  }else if(incidental=="names" && itemSet == 'random'){
    articles <- list(people='')
  }else if(incidental == "objects" && itemSet == 'own'){
    articles <- list(own="the")
  }else if(incidental == "names" && itemSet == 'own'){
    articles <- list(own='')
  }else{
    stop("Please select either 'objects' or 'names'")
  }
  
  # Choose a random set if the argument of set == random. Here we can change item type
  if(itemSet== 'random' | itemSet== "own"){
    set <- sample(sets,1)
  }
  
  # randomly place Nouns in selected itemset
  itemlist <- sample(items[[set]])
  
  # articles
  article  <- articles[[set]]
  
  # all syllogism based on selected item set
  scaleset <- scales[[set]]
  
  #randomly select syllogism column wise
  thescale <- scaleset[,sample(ncol(scaleset),1)]
  
  # List of possible clues
  # column wise, ascending order.
  # row wise, second digit always smaller than first digit. So that we can create invalid or false distractors.
  # pclues determine the direction of the rclues. aka, linear or non linear solving of the items. 
  pclues <- NULL #possible clues
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
  pclues
  
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
  thought<- "NON-linear"
  # keep repeating until there is a max 3 inference step. 
  if(ninfer == 2){
    redo <- 1
    while(redo==1){
      clues<- NULL
      #At least one value from the 2nd col exist in the 1st col
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
      infer
      minstep <- nrow(infer)
      i <- 1
      # while i is less than nrow() + 1
      while (i< minstep) {
        #check for all values in first column that is equals to second column 1st cell value
        #selecting a row within the matrix
        #the row is based on whether the element is a match with the element within the vector
        # i.e. if the infer[,2] == 2, it will extract all rows where the first column value == 2
        # It may extract more than one row, or no rows
        # this will repeat until the while loop is fulfilled
        #sub <- infer[infer[,1]==infer[i,2],]
        sub <- infer[infer[,1]==infer[i,2],]
        sub
        nrow(sub)
        # if nrow(sub) is greater than 0
        # so if no sub is extracted, then don't do this step. if not, do this step.
        if (nrow(sub) > 0) {
          sub[,1] <- infer[i,1] #first value of sub changes to value depending on first column values
          sub
          #pasting the infer clues in the sub clues
          sub$rclues <- paste0(infer$rclues[i],'.',sub$rclues)
          sub
          # check if sub already exist in previous case
          exist <- paste0(sub[,1],sub[,2]) %in% paste0(infer[,1],infer[,2])
          exist
          
          # remove those that exists
          if(any(exist)==TRUE){
            dup <- which(exist == TRUE, arr.ind=TRUE)
            sub <- sub[-dup,]
            if(all(is.na(sub))==FALSE){
              for(k in 1:nrow(sub)){
                infer[nrow(infer)+1,] <- c(sub$left[k],  # repeat
                                           sub$right[k], # repeat
                                           as.numeric(sub$step[k])+1,  #Achieve maximum steps]
                                           sub$rclues[k])
              }
            }
          }
          
          # If values does not exist
          if(all(exist==FALSE) == TRUE){
            for(k in 1:nrow(sub)){
              infer[nrow(infer)+1,] <- c(sub$left[k],  # repeat
                                         sub$right[k], # repeat
                                         as.numeric(sub$step[k])+1,  #Achieve maximum steps]
                                         sub$rclues[k])
            }
          }
        }
        infer
        
        i <- i+1
      }
      infer
      if(any(infer$steps==2) == TRUE ){
        redo <- 2
      }
      
    }
  }
  if(ninfer==1){
    clues<- NULL
    #At least one value from the 2nd col exist in the 1st col
    while(any(clues[,1] %in% clues[,2]) == FALSE){
      nclues <- sample(nrow(pclues), uclues)
      #extract the pclues rows based on the random sample of nclues given by user.
      clues<-pclues[nclues,]
      clues <- uniquecombs(clues)  # keep unique rows only
      any(clues[,1] %in% clues[,2]) == TRUE
    }
    clues
    # 1 step
    infer <- data.frame(left =pclues[nclues,1],  #nclues first column become left
                        right=pclues[nclues,2],  #nclues second column become right
                        steps=1,
                        rclues=sapply(nclues,toString), #change values to string type #row clues
                        stringsAsFactors=FALSE) # don't convert strings to factor
    infer
    (infer)
    
  }
  if(ninfer == 3){
    redo <- 1
    while(redo==1){
      clues<- NULL
      #At least one value from the 2nd col exist in the 1st col
      while(any(clues[,1] %in% clues[,2]) == FALSE){
        nclues <- sample(nrow(pclues), uclues)
        #extract the pclues rows based on the random sample of nclues given by user.
        clues<-pclues[nclues,]
        clues <- uniquecombs(clues)  # keep unique rows only
        any(clues[,1] %in% clues[,2]) == TRUE
      }
      clues
      nclues
      # 1 step
      infer <- data.frame(left =pclues[nclues,1],  #nclues first column become left
                          right=pclues[nclues,2],  #nclues second column become right
                          steps=1,
                          rclues=sapply(nclues,toString), #change values to string type #row clues
                          stringsAsFactors=FALSE) # don't convert strings to factor
      infer
      minstep <- nrow(infer)
      
      i <- 1
      # while i is less than nrow() + 1
      while (i< minstep) {
        #check for all values in first column that is equals to second column 1st cell value
        #selecting a row within the matrix
        #the row is based on whether the element is a match with the element within the vector
        # i.e. if the infer[,2] == 2, it will extract all rows where the first column value == 2
        # It may extract more than one row, or no rows
        # this will repeat until the while loop is fulfilled
        #sub <- infer[infer[,1]==infer[i,2],]
        sub <- infer[infer[,1]==infer[i,2],]
        sub
        nrow(sub)
        # if nrow(sub) is greater than 0
        # so if no sub is extracted, then don't do this step. if not, do this step.
        if (nrow(sub) > 0) {
          sub[,1] <- infer[i,1] #first value of sub changes to value depending on first column values
          sub
          #pasting the infer clues in the sub clues
          sub$rclues <- paste0(infer$rclues[i],'.',sub$rclues)
          sub
          # check if sub already exist in previous case
          exist <- paste0(sub[,1],sub[,2]) %in% paste0(infer[,1],infer[,2])
          exist
          
          # remove those that exists
          if(any(exist)==TRUE){
            dup <- which(exist == TRUE, arr.ind=TRUE)
            sub <- sub[-dup,]
            if(all(is.na(sub))==FALSE){
              for(k in 1:nrow(sub)){
                infer[nrow(infer)+1,] <- c(sub$left[k],  # repeat
                                           sub$right[k], # repeat
                                           as.numeric(sub$step[k])+1,  #Achieve maximum steps]
                                           sub$rclues[k])
              }
            }
          }
          
          # If values does not exist
          if(all(exist==FALSE) == TRUE){
            for(k in 1:nrow(sub)){
              infer[nrow(infer)+1,] <- c(sub$left[k],  # repeat
                                         sub$right[k], # repeat
                                         as.numeric(sub$step[k])+1,  #Achieve maximum steps]
                                         sub$rclues[k])
            }
          }
        }
        infer
        
        i <- i+1
      }
      infer
      if(any(infer$steps==3) == TRUE ){
        redo <- 3
      }
      
    }
    infer
  }
  
  infer
  infer[,1:3] <- sapply(infer[,1:3], as.numeric)
  (infer <- infer[order(infer[,1], decreasing=TRUE),])
  
  ############# VALID RESPONSES ##############
  valid    <- paste0(infer[,1] ,'.',infer[,2]) #combine left / right #those selected within pclues
  possible <- paste0(pclues[,1],'.',pclues[,2]) #combine all pclues combination together
  valid
  possible
  #search for number of possible in valid
  (Nval <- (1:length(possible))[possible %in% valid])
  
  pclues
  ############### INVALID RESPONSES ##########
  (Ninv <- (1:length(possible))[!possible %in% valid]) #search for impossible in valid
  invalids <- cbind(pclues[Ninv,2],pclues[Ninv,1]) # creating invalid response options.
  invalids #names different placeholders
  ulist    <- unique(c(clues[,1:2]))
  invalids
  #invkeeps2 <- invalids[invalids[,1] %in% ulist |  invalids[,2] %in% ulist,] #names list
  invkeeps <- invalids
  
  if (length(invkeeps) > 0 ) iinvkeeps <- rbind(
    cbind(itemlist[invkeeps[,1]],itemlist[invkeeps[,2]]),
    cbind(itemlist[invkeeps[,2]],itemlist[invkeeps[,1]])) # why flip around? [increase more invalids combinations]
  
  
  if(length(invkeeps)==0) {
    iinvkeeps<- matrix(NA, nrow=0, ncol=2)
  }
  
  
  ############ FALSE RESPONSES ##########
  falses  <- cbind(pclues[Nval,2],pclues[Nval,1])
  ifalses <- cbind(itemlist[falses[,1]],itemlist[falses[,2]]) #names
  
  infer
  
  
  ##### FUNCTION TO GENERATE ITEM #####
  join <- function(clue, thescale, article, forward=TRUE) {
    if (forward) return(paste0(article, " " ,clue[1], ' is ', thescale[1], ' than ', article ," ", clue[2])) # Going from Big to small value (scale)
    if (!forward) return(paste0(article, " ",clue[2], ' is ', thescale[2], ' than ',article ," ", clue[1])) # Going from Small to big value (scale reversed)
  }
  
  #### GENERATE CORRECT RESPONSE OPTION #####
  (maxsteps <- max(infer$steps))
  (maxinferlist <- infer[infer$steps==maxsteps,]) # select those rows with steps == 2 . So it can be more than one.
  maxinfer <- maxinferlist[sample(nrow(maxinferlist), 1),] # here you randomly chose one of the max step row
  maxitems <- itemlist[as.numeric(maxinfer[1:2])] # subsuite numbers for names.
  
  
  if(terms=='both'){
    maxanswer <- cap(p(join(maxitems, thescale, article,
                            forward=rbinom(1, 1, distprob)==1),'.'))
  }else if(terms == 'forward'){
    maxanswer <- cap(p(join(maxitems, thescale, article,
                            forward=TRUE),'.'))
    if(ninfer == 1){
      maxanswer <- cap(p(join(maxitems, thescale, article,
                              forward=FALSE),'.'))
    }
    
  }else if (terms == "backward"){
    maxanswer <- cap(p(join(maxitems, thescale, article,
                            forward=FALSE),'.'))
    if(ninfer == 1){
      maxanswer <- cap(p(join(maxitems, thescale, article,
                              forward=TRUE),'.'))
    }
  }else{
    stop("Please select declare either 'mixed', 'forward' or 'backward' comparison.")
  }
  
  
  #### ###### #### CLUES ORDERING IN THE SENTENCE ### #### ####
  #extract the nclues rows based on the random sample of nclues given by user.
  #ordering clues forces the sentence clues in a linear fashion. 
  # compare the position using, infer, clues, and check. 
  # rclues element correspond to the row position of the connected clues
  # check correspond to the row position in the clue matrix 
  # we want to find the position of the clues in the clue matrix 
  if(direct  == "of"){
    clues<- clues[order(clues[,1], decreasing = TRUE),]
    rclues <- unlist(strsplit(maxinfer[,4], "[.]"))
    (rclues <- (1:length(infer$rclues))[infer$rclues %in% rclues]) # search for row positions
    rclues <- infer[rclues,1:4] #clues 
    
    pos    <- paste0(rclues[,1] ,'.',rclues[,2]) #combine left / right #those selected within rclues
    pos2 <- paste0(clues[,1],'.',clues[,2]) #combine all cclues combination together
    
    #search for row order in sequence of pos characters. 
    # We want to make sure that the clues are not in a ordered fashion.  
    check<- NULL
    for(i in pos){
      (check[i] <- (1:length(pos2))[pos2 %in% i])
    }
    (iclues <- cbind(itemlist[clues[,1]],itemlist[clues[,2]])) # name list
  }else if(direct == "ob"){
    clues<- clues[order(clues[,1], decreasing = TRUE),]
    rclues <- unlist(strsplit(maxinfer[,4], "[.]"))
    (rclues <- (1:length(infer$rclues))[infer$rclues %in% rclues]) # search for row positions
    rclues <- infer[rclues,1:4] #clues 
    pos    <- paste0(rclues[,1] ,'.',rclues[,2]) #combine left / right #those selected within pclues
    pos2 <- paste0(clues[,1],'.',clues[,2]) #combine all pclues combination together
    
    #search for row order in sequence of pos characters. 
    # We want to make sure that the clues are not in a ordered fashion.  
    check<- NULL
    for(i in pos){
      (check[i] <- (1:length(pos2))[pos2 %in% i])
    }
    check <- rev(check)
    (iclues <- cbind(itemlist[clues[,1]],itemlist[clues[,2]])) # name list
  }else if(direct == "alt"){
    if(ninfer==3){
      mixed <- FALSE
      while(mixed==FALSE){
        rclues <- unlist(strsplit(maxinfer[,4], "[.]"))
        
        (rclues <- (1:length(infer$rclues))[infer$rclues %in% rclues]) # search for row positions
        rclues <- infer[rclues,1:4] #clues 
        
        pos    <- paste0(rclues[,1] ,'.',rclues[,2]) #combine left / right #those selected within pclues
        pos2 <- paste0(clues[,1],'.',clues[,2]) #combine all pclues combination together
        
        #search for row order in sequence of pos characters. 
        # We want to make sure that the clues are not in a ordered fashion.  
        check<- NULL
        for(i in pos){
          (check[i] <- (1:length(pos2))[pos2 %in% i])
        }
        if(is.unsorted(check)==TRUE){
          mixed <- TRUE
        }
        if(is.unsorted(check)==FALSE | is.unsorted(rev(check)) == FALSE){
          clues <-   clues[sample(nrow(clues)),]
          mixed=FALSE
        }
      }
    }
    (iclues <- cbind(itemlist[clues[,1]],itemlist[clues[,2]])) # name list
  }else{
    stop("Please declare 'of', 'ob' or 'alt' for the label arg.")
  }
  
  
  # CREATE SENTENCE STRUCTURE ####
  q <- 'Clues: '
  dreturn <- c()
  dtype <- c()
  
  # iclues all come from the same clues now.
  q <- NULL
  if(terms=='both'){
    for (i in 1:nrow(iclues)) {
      q <- p(q, join(iclues[i,], thescale, article,
                     forward=rbinom(1, 1, distprob)==1))
      if (i<nrow(iclues)) q <- p(q, ', ')
    }
  }else if(terms == 'forward'){
    for (i in 1:nrow(iclues)) {
      q <- p(q, join(iclues[i,], thescale, article,
                     forward=TRUE))
      if (i<nrow(iclues)) q <- p(q, ', ')
    }
  }else if (terms == "backward"){
    for (i in 1:nrow(iclues)) {
      q <- p(q, join(iclues[i,], thescale, article,
                     forward=FALSE))
      if (i<nrow(iclues)) q <- p(q, ', ')
    }
  }else{
    stop("Please select declare either 'mixed', 'forward' or 'backward' comparison.")
  }
  q
  q <- p(q, '. Which of the following is implied?')
  
  if(Ndist > 5) stop("Please choose a lower number of distractors")
  q
  ##### 
  
  # create matrix of invalid and false ####
  #select type of distractors 
  dlist <- NULL
  if(dist=="mixed"){
    if(all(is.na(iinvkeeps)) == TRUE){
      warning(paste0("This results because all the combinations in the matrix are possible. \n Invalid distractors cannot be created. \n Return only False distractors. \n Caution when studying distractors.\n This is located in question ", seed ,".\n Solution: Try increasing nspread."))
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
  #repeat sampling the distractions rows in the dlist matrix that was created previously
  if(dist=="mixed"){
    dlist.df <- as.data.frame(dlist)
    dlist.df
    if(length(unique(dlist.df$type))==2){ #if 2 = "invalid" and "falses" distractor type
      type <- dlist.df$type
      dlist.df <- sample_n(group_by(dlist.df,type),ceiling(Ndist/2), 0.5)  #divide by 2 since there are 2 types of distractors
      dlist.df
      dlist.df <- dlist.df[sample(nrow(dlist.df)),] #randomise position of distractors.
      dlist.df
      if(Ndist %% 2 == 0){
        dtype <- dlist.df[,3] #type
        dtype
      }else{
        dtype <- dlist.df[-nrow(dlist.df),3] # - last row if uneven number
      }
      dtype
      dtype <- as.character(as.matrix(dtype))
      dtype
    }else{ # if only one distractor type
      dlist.df <- as.data.frame(dlist)
      type <- dlist.df$type
      dlist.df<- sample_n(group_by(dlist.df,type),Ndist, 0.5) #do not divide since only one type of distractor
      dtype <- dlist.df[,3]
      dtype <- as.character(as.matrix(dtype))
    }
    dlist.df <- as.matrix(dlist.df) #convert to matrix
    dreturn<- NULL
    for (i in 1:Ndist){
      dreturn[i] <- cap(p(join(dlist.df[i,1:2], thescale, article,
                               forward=rbinom(1, 1, distprob)==1),'.')) # create an incorrect sentence
    }
    
  }else{
    if(Ndist > nrow(dlist)){
      stop("There is not enough invalid distractors. Please change the arg dist to 'mixed' or 'false'")
    }
    draw  <- dlist[sample(nrow(dlist),Ndist),]
    dtype <- draw[,3]
    for (i in 1:Ndist){
      dreturn[i] <- cap(p(join(draw[i,1:2], thescale, article,
                               forward=rbinom(1, 1, distprob)==1),'.')) # create an incorrect sentence
    }
    #if(!is.null(nrow(dlist))) dtype[i] <- dreturn[i] <- '999'
  }
  
  
  
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
             dtype5=dtype[5],
             clues = t(as.numeric(t(check)))
  )
  
  
}
### End Logical Item
