#' @export
#' @description A basic arithmetic item generator for ICAR.
#' @details The arithmetic function can currently generate additive and substractive operators. For the additive operators, you have the option of using both negative and positive numerical values. For the substractive operators, you are currently restricted to only using positive numerical values. There are also options for you to select the order of the values. It can be in ascending, descending or mixed order. Currently, there are 4 types of distractors that are generated now. 1a,1b distractor types are developed for addition and subtraction questions, whereas, 2a,2b distractor types are developed for multiplication questions.
#'
#' Distactor type 1a is used for all arithmetic operators. Distractor type 1a is generated such that the values are very near +1/+2/+3 or -1/-2/-3 from the actual answers. Distractor type 1b is generated such as it takes values from a range of values (i.e. number of values * (max(magnitude-1) + 1)). In this instance, distractor type 1b cannot be used in multiplication items because the range will be too big. Hence, elimination of distractors becomes very easy.
#'
#' Distractor type 2a is used in multiplication questions where the values are not the same. The distractors are generated such that one of the numbers are randomly assigned a value of 1,2 or 3. The product of the new values are considerd as one distractor. It will repeat until 5 unique distractors are created. If the multiplication question uses the same value, then a slightly different approach is adopted. In this case, the numbers are randomly drawn three times before we calcuate the product of the values. Again, this process is repeated until 5 unique distractors are achieved.
#' @author Aiden Loe and David Condon
#' @param numProbs Number of desired problems
#' @param numOps sets the number of operations in the stimuli (range of values is 1 to 5)
#' @param magnitude sets the range of values for the integers in the stimuli by powers of 10 (range of values is 1 to 5)
#' @param sameValue  Deciding if you want the values to be the same or different values.
#' @param arithOps There are two options in this argument, either 'add' or 'subtract', "multiply.
#' @param allowNeg Allow the use of negative values. Only for "add" arithmetic operator. Not allowed for "subtract".
#' @param order There are three options to order the values presented, ("mixed", "descending", "ascending"). Does not matter when same Value = TRUE.
#' @return
#' \describe{
#' \item{Question}{The arithmetic questions that are automatically generated.}
#' \item{Code}{First digit is the magnitude. Subsequent digits correspond to the positional values in the question. 0 is positive value, 1 is negative value.}
#' \item{Answers}{The answer to the question generated.}
#' \item{Response Options}{Eight response options. }
#'     }
#' @title ICAR Arithmetic Generator
#' @seealso \code{\link{lisy}}, \code{\link{twoD}}
#' @examples
#'
#' arith(numProbs=4, numOps = 4, magnitude = 3, arithOps = "subtract",
#'  allowNeg = FALSE, sameValue=FALSE, order = "mixed")
#'
#'

arith <- function(numProbs=5, numOps = 1, magnitude = 1, arithOps = "add", allowNeg = FALSE, sameValue=FALSE, order = "mixed"){
  # Addition
  # numOps sets the number of operations in the stimuli (range of values is 1 to 5)
  # magnitude sets the range of values for the integers in the stimuli by powers of 10 (range of values is 1 to 5)

   possible <- c(1:5) #maximum operators allowed

   #checks
   if(!(numOps %in% possible)) stop(' The only options for numOps are 1, 2, 3, 4, or 5')
   if(!(magnitude %in% possible)) stop(' The only options for magnitude are 1, 2, 3, 4, or 5')
   if(allowNeg == TRUE && arithOps=="subtract") stop("allowNeg must be FALSE when arithOps = 'subtract'")
   if(order !="mixed" && order !="descending" && order !="ascending") stop("Please use either of the 3 options given. " )
   if(arithOps !="add" && arithOps !="subtract" && arithOps !="multiply") stop("Please use select either 'add', 'subtract' or 'multiply' for the arithOps argument." )


  (results <- list(numProbs)) #Put item in list
  (num_levels <- numOps+1) # why + one? so that there are 2 values.

  (max <- 10^magnitude-1) # range of values from( 10,100,1000,10000,100000)

  # if it can be negative, then min will be - negative of max, if not min is just 0.
  # why? so it creates number_diff from start to end.
  ifelse(allowNeg == TRUE, min <- max*-1, min <- 0)
  (number_diff <- c(min:max)) # to include negative drawn items


    for(Probs in 1:numProbs) { #begin a loop to generate a X number of problems
    # Select the integers to be used in the operations

      zero = TRUE
      #sample numbers
    if(sameValue == FALSE){
      if(order == "ascending"){
        while(zero==TRUE){ # make sure no zeros are used
        (stimuli <- sort(sample(number_diff,  num_levels, replace=T))) #question
          while((length(unique(stimuli))==1)==TRUE){ # never get same values
            (stimuli <- sample(number_diff,  num_levels, replace=T)) #question
          }
          if(!0 %in% stimuli){ zero <-  FALSE} # stopping rule
        }
      }

      if(order == "descending"){
        while(zero==TRUE){ # make sure no zeros are used
        (stimuli <- sort(sample(number_diff,  num_levels, replace=T), decreasing=TRUE)) #question
          while((length(unique(stimuli))==1)==TRUE){ # never get same values
            (stimuli <- sample(number_diff,  num_levels, replace=T)) #question
          }
          if(!0 %in% stimuli){zero = FALSE} # stopping rule
        }
      }
      if(order == "mixed"){
        {
          while(zero==TRUE){ # make sure no zeros are used
        (stimuli <- sample(number_diff,  num_levels, replace=T)) #question
            while((length(unique(stimuli))==1)==TRUE){ # never get same values
              (stimuli <- sample(number_diff,  num_levels, replace=T)) #question
            }
          if(!0 %in% stimuli){zero = FALSE} # stopping rule
        }
      }
    }
  }


      # if users wants to have same numeric value
      if(sameValue==TRUE){
        zero <- TRUE
        while(zero==TRUE){ # make sure no zeros are used
            (stimuli <- sample(number_diff,  num_levels-numOps, replace=T))
            (stimuli<- rep(stimuli, num_levels))
          if(!0 %in% stimuli){zero = FALSE} # stopping rule
        }
      }


    # Do the math
  if(arithOps == "add"){
    (answer <- sum(stimuli)) # answer
  }

  if(arithOps == "subtract"){
    (stimuliCode <- stimuli) #used in the response option only
    (stimuli <- -stimuli)
    (stimuli[1] <- abs(stimuli[1]))
    (answer <- sum(stimuli))
  }

  if(arithOps == "multiply"){
    (stimuliCode <- stimuli) #used in the response option only
    (answer <- prod(stimuli))
    if(numOps > 1) message("The answer is likely to be very big. Consider reducing the numOps.")
    if(magnitude > 2) message("The answer is likely to be very big. Consider reducing the magnitude.")
      }

     answer2 <- cbind(answer, "answer")

    (posmin <- sample(10, 7)) #why 10 and why 7?
    (posmin <- ifelse(posmin>5, 1, -1)) # if greater than 5 all 1, if not -1. Why?

    ### 7 Distractors created###
    (near_answer1 <- round(.5+(sample(20,1)*.1)))
    (near_answer2 <- round(.5+(sample(30,1)*.1)))
    (near_answer3 <- round(.5+(sample(30,1)*.1)))
    (near_answer4 <- round(.5+(sample(30,1)*.1)))
    (near_answer5 <- round(.5+(sample(40,1)*.1)))
    (near_answer6 <- round(.5+(sample(40,1)*.1)))
    (near_answer7 <- round(.5+(sample(50,1)*.1)))


    (near_answers <- c(near_answer1, near_answer2, near_answer3, near_answer4, near_answer5, near_answer6, near_answer7))

    # distractors that are near the answers. Dependent on the sample above.
    (near_answers <- answer+(unique(near_answers)*posmin[1:length(unique(near_answers))]))
    (near_answers2 <- as.matrix(near_answers))
    (near_answers2 <- cbind(near_answers2, rep("dist1a", nrow(near_answers2)))) # near_answer matrix

    (opp_answer <- -1*answer) # negative answer as a problem of misunderstanding operator rules.
    opp_answer2 <- as.matrix(opp_answer)
    (opp_answer2 <- cbind(opp_answer,(rep("oppsite",nrow(opp_answer2)))))

    # Get a reasonable range of choices for the remaining distractors (DISTRACTORS 2) ####
    if(arithOps=="add" || arithOps=="subtract"){
    (range_max <- num_levels*(max+1)) #upper bound
    ifelse (allowNeg == TRUE, range_min <- -(num_levels*(max+1)), range_min <- 0) #lower bound
    (range_choices <- setdiff(range_min:range_max, answer)) #all choices except answer
    (range_choices <- setdiff(range_choices, near_answers)) #all choices except answer and near answers
    (range_choices <- if(allowNeg) setdiff(range_choices, opp_answer) else c(range_choices)) # only if allowNeg is possible

    # Select the distractors to insert in the response choices ####
    (distractors1 <- sample(range_choices, 4, replace=F)) #only 4 distractors of leftover.
    (distractors2 <- if(allowNeg) c(distractors1, near_answers, opp_answer) else c(distractors1, near_answers))
    (distractors2 <- as.matrix(distractors1))
    (distractors3 <- cbind(distractors2, rep("dist1b",nrow(distractors2))))
    (distractors4 <- rbind(distractors3, near_answers2, opp_answer2)) # combine all together.

    distractors4 <- distractors4[!duplicated(distractors4[,1]),]  # remove duplicated distractors
    (distractors4 <- distractors4[sample(nrow(distractors4), 6,replace=FALSE), ]) # draw 6 randomly.
    }

    if(arithOps=="multiply"){
    dist<- 6 # need to have at least 6 distractors
      i <- 1
      while(i < 6){
      if(sameValue==FALSE){
          # Distractor Type 2
          diffDistractor <- FALSE
          while(diffDistractor == FALSE){
              range_choices2 <- NULL
              for(k in 1:5){ # repeat 5 times
                (range_choices <- stimuli )
                (dist <- sample(length(stimuli),1)) #randomly draw one value from the stimuli
                (range_choices[dist]     <-   stimuli[dist]+ sample(1:3,1)) # randomly adding either 1,2,3 to a single value
                (range_choices2[k] <-prod(range_choices)) #create 5 distractors

              }
              if(anyDuplicated(range_choices2)==0){ #kill only when all distractors are different values
                diffDistractor <- TRUE
                  }
            }


      # Select the distractors to insert in the response choices ####
      (distractors1 <- sample(range_choices2, 5, replace=F)) #only 4 distractors of leftover.
      if(allowNeg) {
        (negValue   <- sample(length(distractors1),2)) #select two values
        #(distractors1[negValue] <- abs(distractors1[negValue]))
        (distractors1 <- c(distractors1, abs(distractors1[negValue]))) # flip the 2 values to abs.
        (distractors2 <- as.matrix(distractors1))
        (distractors3 <- cbind(distractors2, rep("dist2a",nrow(distractors2))))
        (distractors4 <- rbind(distractors3, near_answers2, opp_answer2)) # combine all together.
      }else{
        distractors2 <- as.matrix(distractors1)
        (distractors3 <- cbind(distractors2, rep("dist2a",nrow(distractors2))))
        (distractors4  <-rbind(distractors3, near_answers2))
        }
      }

      if(sameValue==TRUE){
        # Distractor Type 2
        diffDistractor <- FALSE
        while(diffDistractor == FALSE){
          range_choices2 <- NULL
          for(k in 1:5){
            (range_choices <- stimuli)
            (dist <- sample(length(stimuli),1)) #randomly draw one value from the stimuli
            (range_choices[dist]     <-   stimuli[dist] + sample(1:3,1)) # randomly adding either 1,2,3 to a single value
            (dist <- sample(length(range_choices),1)) #randomly draw a value from the range_choices now
            (range_choices[dist]     <-   range_choices[dist] + sample(1:3,1)) # randomly adding either 1,2,3 to a single value
            (dist <- sample(length(range_choices),1)) #randomly draw another value from the range_choices now
            (range_choices[dist]     <-   range_choices[dist] + sample(1:3,1)) # randomly adding either 1,2,3 to a single value
            (range_choices2[k] <-prod(range_choices)) #create 5 distractors
          }
          range_choices2
          if(anyDuplicated(range_choices2)==0){ #kill only when all distractors are different values
            diffDistractor <- TRUE
          }
        }
        # Select the distractors to insert in the response choices ####
        (distractors1 <- sample(range_choices2, 5, replace=F)) #only 4 distractors of leftover.
        if(allowNeg) {
          (negValue   <- sample(length(distractors1),2)) #select two values
          #(distractors1[negValue] <- abs(distractors1[negValue])) # flip the 2 values to abs.
          distractors1 <- c(distractors1, abs(distractors1[negValue])) # flip the 2 values to abs.
          (distractors2 <- as.matrix(distractors1))
          (distractors3 <- cbind(distractors2, rep("dist2b",nrow(distractors2))))
          (distractors4 <- rbind(distractors3, near_answers2, opp_answer2)) # combine all together.
        }else{
          distractors2 <- as.matrix(distractors1)
          (distractors3 <- cbind(distractors2, rep("dist2b",nrow(distractors2))))
          (distractors4  <-rbind(distractors3, near_answers2))
        }
      }


        (distractors4 <-distractors4[!duplicated(distractors4[,1]),])  # remove duplicated distractors
        if(nrow(distractors4)>5){i <- 6}
      }
      (distractors4 <- distractors4[sample(nrow(distractors4), 6,replace=FALSE), ]) # draw 6 randomly.
    }

    # Make the list of response choices
    (responses <- rbind(distractors4, answer2))
    (responses <- responses[sample(nrow(responses), 7,replace=FALSE), ]) # draw 6 randomly.

    #### Write the item prompt text ####
if(arithOps == "add"){
    if (numOps == 1) {
      prompt <- paste("Choose the best answer: ", stimuli[1], ifelse(stimuli[2] > -1, " + ", " - "), abs(stimuli[2]), " = ?", sep="")
    }
    if (numOps == 2) {
      prompt <- paste("Choose the best answer: ", stimuli[1], ifelse(stimuli[2] > -1, " + ", " - "), abs(stimuli[2]), ifelse(stimuli[3] > -1, " + ", " - "), abs(stimuli[3]), " = ?", sep="")
    }
    if (numOps == 3) {
      prompt <- paste("Choose the best answer: ", stimuli[1], ifelse(stimuli[2] > -1, " + ", " - "), abs(stimuli[2]), ifelse(stimuli[3] > -1, " + ", " - "), abs(stimuli[3]), ifelse(stimuli[4] > -1, " + ", " - "), abs(stimuli[4]), " = ?", sep="")
    }
    if (numOps == 4) {
      prompt <- paste("Choose the best answer: ", stimuli[1], ifelse(stimuli[2] > -1, " + ", " - "), abs(stimuli[2]), ifelse(stimuli[3] > -1, " + ", " - "), abs(stimuli[3]), ifelse(stimuli[4] > -1, " + ", " - "), abs(stimuli[4]), ifelse(stimuli[5] > -1, " + ", " - "), abs(stimuli[5]), " = ?", sep="")
    }
    if (numOps == 5) {
      prompt <- paste("Choose the best answer: ", stimuli[1], ifelse(stimuli[2] > -1, " + ", " - "), abs(stimuli[2]), ifelse(stimuli[3] > -1, " + ", " - "), abs(stimuli[3]), ifelse(stimuli[4] > -1, " + ", " - "), abs(stimuli[4]), ifelse(stimuli[5] > -1, " + ", " - "), abs(stimuli[5]), ifelse(stimuli[6] > -1, " + ", " - "), abs(stimuli[6]), " = ?", sep="")
    }
}


if(arithOps == "subtract"){
  if (numOps == 1) {
    prompt <- paste("Choose the best answer: ", abs(stimuli[1]), " - ", abs(stimuli[2]), " = ?", sep="")
  }
  if (numOps == 2) {
    prompt <- paste("Choose the best answer: ", abs(stimuli[1]), " - " , abs(stimuli[2]),  " - ", abs(stimuli[3]), " = ?", sep="")
  }
  if (numOps == 3) {
    prompt <- paste("Choose the best answer: ", abs(stimuli[1]),  " - " ,abs(stimuli[2]) , " - ", abs(stimuli[3]), " - " , abs(stimuli[4]), " = ?", sep="")
  }
  if (numOps == 4) {
    prompt <- paste("Choose the best answer: ", abs(stimuli[1]),  " - ", abs(stimuli[2]), " - ", abs(stimuli[3]), " - ", abs(stimuli[4]), " - ", abs(stimuli[5]), " = ?", sep="")
  }
  if (numOps == 5) {
    prompt <- paste("Choose the best answer: ", abs(stimuli[1]), " - ", abs(stimuli[2]), " - ", abs(stimuli[3]), " - ", abs(stimuli[4]), " - ", abs(stimuli[5]), " - ", abs(stimuli[6]), " = ?", sep="")
  }
}



    if(arithOps == "multiply"){
      if (numOps == 1) {
        prompt <- paste("Choose the best answer: ", stimuli[1], " x ", stimuli[2], " = ?", sep="")
      }
      if (numOps == 2) {
        prompt <- paste("Choose the best answer: ", stimuli[1], " x " , stimuli[2],  " x ", stimuli[3], " = ?", sep="")
      }
      if (numOps == 3) {
        prompt <- paste("Choose the best answer: ", stimuli[1], " x ",stimuli[2], " x ", stimuli[3], " x " , stimuli[4], " = ?", sep="")
      }
      if (numOps == 4) {
        prompt <- paste("Choose the best answer: ", stimuli[1],  " x ", stimuli[2], " x ", stimuli[3], " x ", stimuli[4], " x ", stimuli[5], " = ?", sep="")
      }
      if (numOps == 5) {
        prompt <- paste("Choose the best answer: ", stimuli[1], " x ", stimuli[2], " x ", stimuli[3], " x ", stimuli[4], " x ", stimuli[5], " x ", stimuli[6], " = ?", sep="")
      }
    }

prompt
answer
    ### Response option ####
if(arithOps == "add") (code <- paste(ifelse(stimuli < 0, 1, 0), sep="", collapse=""))
if(arithOps == "subtract") (code <- paste(ifelse(stimuliCode < 0, 1, 0), sep="", collapse=""))
if(arithOps == "multiply") (code <- paste(ifelse(stimuli < 0, 1, 0), sep="", collapse=""))
    (code <- paste(magnitude, "_", code, sep=""))


    # Write the item response choice text
    (choices<- c(responses[1,],responses[2,],responses[3,],responses[4,],responses[5,],responses[6,],"None of these", "I don't know"))

    # Write the correct answer text
    # (correct <- paste("(", match(answer, responses), ") ", answer, sep=""))
    (correct <- answer)

    ##### Put it all together ####
      item <-  c(prompt,code,correct,choices)
      (item <- t(as.data.frame(item)))
        colnames(item) <- c("prompt","code","answer", "option 1","distType1","option 2","distType2","option 3","distType3","option 4","distType4","option 5","distType5","option 6","distType6","option 7","option 8")
        (results[[Probs]] <- item)

  }


  # Now, do some reorganization so the problems are easier to handle
  (results1 <- do.call(rbind,results))
  rownames(results1) <- 1:nrow(results1)
  class(results1) <- "ICARarith"
  return(results1)
}

