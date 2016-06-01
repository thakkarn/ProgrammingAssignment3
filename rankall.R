rankall <- function(outcome, num = "best"){
  ## Read outcome data
  hospitals <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## Check that state and outcome are valid

  
  valid_outcomes <- c("heart attack", "heart failure", "pneumonia")
  if (!(outcome %in% valid_outcomes))
  {
    stop("invalid outcome")
  }
  
  #if (is.na(state.abb[match(hospitals$State, state.abb)]))
  
  #{
  # stop("invalid state")
  # }
  ## Return hospital name in that state with lowest 30-day death
  ## rate
  # new data frame with the hospital
  
  if (outcome == valid_outcomes[1]) {
    i <- 11
    
  } else if (outcome == valid_outcomes[2])
  {
    i <- 17
  } else if (outcome == valid_outcomes[3])
  {
    i <- 23
  } else
  {
    stop('invalid outcome')
  }
  # declare an empty dataframe
  ranked_hospital <- data.frame(hospital=character(0), st_name=character(0), stringsAsFactors = F)
  
  for (state in state.abb)
  { 
    ignore_state = FALSE
    print(state)
    state_hospitals <- subset(hospitals, hospitals[,7] == state)
    num_hospitals <- nrow(subset(state_hospitals, state_hospitals[,i]!="Not Available"))
    print(num_hospitals)
    if (num == "best")
    {
      num = 1
    }
    else if (num == "worst")
    {
      num = num_hospitals
    }
    else if(is.numeric(x=num)) 
    { 
      if(num<1 || num > nrow(state_hospitals)) 
      { 
        ignore_state = TRUE
      } 
    } 
    else { 
        stop('invalid num') 
    } 
    if (!(ignore_state))
    { 
      ordered_state_hospitals <- state_hospitals[order(state_hospitals[,i], state_hospitals$Hospital.Name), ]
      hospital_name <- ordered_state_hospitals[num, ]$Hospital.Name
      ranked_hospital[nrow(ranked_hospital)+1,] <- c(hospital_name[1], state)
    } else
    { 
      ranked_hospital[nrow(ranked_hospital)+1,] <- c("NA", state)
    }
  }
    ranked_hospital
}