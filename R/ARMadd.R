#' @export
#' @description A basic arithmetic item generator for ICAR.
#' @details The arithmetic function can currently generate additive and substractive operators. For the additive operators, you have the option of using both negative and positive numerical values. For the substractive operators, you are currently restricted to only using positive numerical values.
#' @author Aiden Loe and David Condon
#' @param numProbs Number of desired problems
#' @param numOps sets the number of operations in the stimuli (range of values is 1 to 5)
#' @param magnitude sets the range of values for the integers in the stimuli by powers of 10 (range of values is 1 to 5)
#' @param sameValue  Deciding if you want the values to be the same or different values.
#' @param arithOps There are two options in this argument, either 'add' or 'subtract'.
#' @param allowNeg Allow the use of negative values. Only for "add" arithmetic operator. Not allowed for "subtract".
#' @param order There are three options to order the values presented. "mixed", "decreasing", "increasing". Does not matter when same Value = TRUE.
#' @return
#' \describe{
#' \item{Question}{The arithmetic questions that are automatically generated.}
#' \item{Response Options}{Eight response options. }
#' \item{Answers}{The answer to the question generated.}
#' \item{Code}{First digit is the magnitude. Subsequent digits correspond to the positional values in the question. 0 is positive values, 1 is negative values.}
#'     }
#' @title ICAR Arithmetic Generator
#' @examples
#'
#' ICARarith(numProbs=4, numOps = 4, magnitude = 3, arithOps = "subtract",
#'  allowNeg = FALSE, sameValue=FALSE, order = "mixed")
#'
#'

ICARarith <- function(numProbs=4, numOps = 1, magnitude = 1, arithOps = "add", allowNeg = FALSE, sameValue=FALSE, order = "mixed"){
  # Addition
  # numOps sets the number of operations in the stimuli (range of values is 1 to 5)
  # magnitude sets the range of values for the integers in the stimuli by powers of 10 (range of values is 1 to 5)

   # numProbs=2
   # numOps = 2
   # magnitude = 3
   # allowNeg = FALSE
   # sameValue=TRUE
   # arithOps = "subtract"
   # order="mixed"

   possible <- c(1:5) #maximum operators allowed

   #checks
   if(!(numOps %in% possible)) stop(' The only options for numOps are 1, 2, 3, 4, or 5')
   if(!(magnitude %in% possible)) stop(' The only options for magnitude are 1, 2, 3, 4, or 5')
   if(allowNeg == TRUE && arithOps=="subtract") stop("allowNeg must be FALSE when arithOps = 'subtract'")
   if(order !="mixed" && order !="descending" && order !="ascending") stop("Please use either of the 3 options given. " )


  (results <- list(numProbs)) #Put item in list
  (num_levels <- numOps+1) # why + one? so that there are 2 values.

  (max <- 10^magnitude-1) # range of values from( 10,100,1000,10000,100000)

  # if it can be negative, then min will be - negative of max, if not min is just 0.
  # why? so it creates number_diff from start to end.
  ifelse(allowNeg == TRUE, min <- max*-1, min <- 0)
  (number_diff <- c(min:max)) # to include negative drawn items


    for(Probs in 1:numProbs) { #begin a loop to generate a X number of problems
    # Select the integers to be used in the operations

      #sample numbers
    if(sameValue == FALSE){
      if(order == "ascending"){
        (stimuli <- sort(sample(number_diff,  num_levels, replace=T))) #question
      }
      if(order == "descending"){
        (stimuli <- sort(sample(number_diff,  num_levels, replace=T), decreasing=TRUE)) #question
      }
      if(order == "mixed"){
        (stimuli <- sample(number_diff,  num_levels, replace=T)) #question
      }
    }

    # if users wants to have same numeric value
      # if(sameValue==TRUE){
      #   # if(arithOps == "subtract"){
      #   #   stop("The difference is always 0 when you select sameValue = TRUE. Consider changing arithOps = 'add' or sameValue = FALSE")
      #   # }
      #   sValue <- 0
      #     while(sValue < 1 ){
      #       stimuli <- sample(number_diff,  num_levels, replace=T)
      #       #message("Larger magnitude and longer number of operators take a long time to search for same value.")
      #       if(length(unique(stimuli)) == 1){
      #         sValue <-  2
      #       }
      #    }
      # }

      # if users wants to have same numeric value
      if(sameValue==TRUE){
        (stimuli <- sample(number_diff,  num_levels-numOps, replace=T))
        (stimuli<- rep(stimuli, num_levels))
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


    (posmin <- sample(10, 7)) #why 10 and why 7?
    (posmin <- ifelse(posmin>5, 1, -1)) # if greater than 5 all 1, if not -1. Why?

#### The incorrect answers are referred to as the distractors and typically represent common errors that are equally plausible only to students who have not attained the level of knowledge or understanding necessary to recognize the key as the correct answer (Linn and Gronlund, 2000; Popham 2000; Nitko, 2004)

    ### 7 Distractors created###
    (near_answer1 <- round(.5+(sample(20,1)*.1)))
    (near_answer2 <- round(.5+(sample(30,1)*.1)))
    (near_answer3 <- round(.5+(sample(30,1)*.1)))
    (near_answer4 <- round(.5+(sample(30,1)*.1)))
    (near_answer5 <- round(.5+(sample(40,1)*.1)))
    (near_answer6 <- round(.5+(sample(40,1)*.1)))
    (near_answer7 <- round(.5+(sample(50,1)*.1)))


    (near_answers <- c(near_answer1, near_answer2, near_answer3, near_answer4, near_answer5, near_answer6, near_answer7))


    unique(near_answers) #keep only unique numbers
    posmin[1:length(unique(near_answers))] #extract data from the position of the length of unique numbers

    unique(near_answers)*posmin[1:length(unique(near_answers))] # making it positive or negative, why multiple? maybe additive might be closer to the actual answer?
    #unique(near_answers)+posmin[1:length(unique(near_answers))]

    # distractors that are near the answers. Dependent on the sample above.
    (near_answers <- answer+(unique(near_answers)*posmin[1:length(unique(near_answers))]))

    (opp_answer <- -1*answer) # negative answer as a problem of misunderstanding operator rules.

    # Get a reasonable range of choices for the remaining distractors ####
    (range_max <- num_levels*(max+1)) #upper bound
    ifelse (allowNeg == TRUE, range_min <- -(num_levels*(max+1)), range_min <- 0) #lower bound
    (range_choices <- setdiff(range_min:range_max, answer)) #all choices except answer
    (range_choices <- setdiff(range_choices, near_answers)) #all choices except answer and near answers
    (range_choices <- if(allowNeg) setdiff(range_choices, opp_answer) else c(range_choices)) # only if allowNeg is possible


    # Select the distractors to insert in the response choices ####
    (distractors1 <- sample(range_choices, 4, replace=F)) #only 4 distractors of leftover.
    (distractors2 <- if(allowNeg) c(distractors1, near_answers, opp_answer) else c(distractors1, near_answers))

    (distractors2 <- unique(distractors2))
    (distractors2 <- sample(distractors2, 6, replace=F))


    # Make the list of response choices
    (responses <- c(distractors2, answer))
    (responses <- sample(responses, 7, replace=F))


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
    prompt <- paste("Choose the best answer: ", abs(stimuli[1]), abs(stimuli[2]) , " - ", abs(stimuli[3]), " - " , abs(stimuli[4]), " = ?", sep="")
  }
  if (numOps == 4) {
    prompt <- paste("Choose the best answer: ", abs(stimuli[1]),  " - ", abs(stimuli[2]), " - ", abs(stimuli[3]), " - ", abs(stimuli[4]), " - ", abs(stimuli[5]), " = ?", sep="")
  }
  if (numOps == 5) {
    prompt <- paste("Choose the best answer: ", abs(stimuli[1]), " - ", abs(stimuli[2]), " - ", abs(stimuli[3]), " - ", abs(stimuli[4]), " - ", abs(stimuli[5]), " - ", abs(stimuli[6]), " = ?", sep="")
  }
}
prompt
answer
    ### Response option ####
if(arithOps == "add") (code <- paste(ifelse(stimuli < 0, 1, 0), sep="", collapse=""))
if(arithOps == "subtract") (code <- paste(ifelse(stimuliCode < 0, 1, 0), sep="", collapse=""))

    (code <- paste(magnitude, "_", code, sep=""))
    # Write the item response choice text
    (choices <- paste("(1), ", responses[1], " ,(2), ", responses[2], ", (3), ", responses[3], ", (4), ", responses[4], " ,(5), ", responses[5], " ,(6), ", responses[6], " ,(7), None of these ,(8), I don't know", sep=""))


    # Write the correct answer text
    (correct <- paste("(", match(answer, responses), ") ", answer, sep=""))

    ##### Put it all together ####
    results[[Probs]] <- list(stimuli = prompt,
                             choices = choices,
                             answer = correct,
                             code = code)

  }

  # Now, do some reorganization so the problems are easier to handle
  results <- unlist(results)
  results <- matrix(results,ncol=numProbs)
  (results <- t(results))
  colnames(results) <- c("prompt", "choices", "answer", "code")
  results <- as.data.frame(results)
  return(results)
}




