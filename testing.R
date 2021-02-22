best <- function(state, outcome)
{
  sacredfart <- read.csv("outcome-of-care-measures.csv")  

  if(outcome == 'hearth attack')
   outcome <- 'Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack'
  else if(outcome == 'hearth failure')
   outcome <- 'Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure'
  else 
   outcome <- 'Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia' 
  
  xd <-sacredfart [ which(sacredfart$State == state), ]

  orderedDataSet <- xd[order(xd[outcome], xd$Hospital.Name, na.last = TRUE, decreasing = FALSE), ]

  return(orderedDataSet[1,2])

}

print(best("TX", "hearth attack"))