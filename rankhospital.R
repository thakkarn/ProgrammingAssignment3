rankhospital <- function(state, outcome, num = "best"){
  ## Read outcome data
  hospitals <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## Check that state and outcome are valid
  if (is.na(state.abb[match(state, state.abb)]))
  {
    stop("invalid state")
  }
  
  valid_outcomes <- c("heart attack", "heart failure", "pneumonia")
  if (!(outcome %in% valid_outcomes))
  {
    stop("invalid outcome")
  }
  ## Return hospital name in that state with lowest 30-day death
  ## rate
  # new data frame with the hospital
  state_hospitals <- subset(hospitals, hospitals[,7] == state)
  
  num_hospitals <- nrow(state_hospitals)
  
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
        return(NA) 
      } 
  } 
  else { 
    stop('invalid num') 
  } 
  
  
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
  
  ordered_state_hospitals <- state_hospitals[order(state_hospitals[,i], state_hospitals$Hospital.Name), ]
  hospital_name <- ordered_state_hospitals[num, ]$Hospital.Name
  hospital_name[1]
  
}