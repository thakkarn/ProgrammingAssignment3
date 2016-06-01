best <- function(state, outcome){
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
  if (outcome == valid_outcomes[1]) {
        hospital_name <- state_hospitals[which.min(state_hospitals$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack), "Hospital.Name"]
        print(hospital_name)
  } else if (outcome == valid_outcomes[2])
  {
    hospital_name <- state_hospitals[which.min(state_hospitals$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure), "Hospital.Name"]
    print(hospital_name)
  } else if (outcome == valid_outcomes[3])
  {
    hospital_name <- state_hospitals[which.min(state_hospitals$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia), "Hospital.Name"]
    print(hospital_name)
  } else
  {
    hospital_name <- NULL
  }
  
}