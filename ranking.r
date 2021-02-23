rankhospital <- function(state, outcome, num = "best")
{
    #read outcome data

    #check that state and outcome are valid

    #return hosptial name in that state with the given rank 

    #30-day rate death rate 

    sacredfart <- read.csv("outcome-of-care-measures.csv")  

    
    
    check = state %in% sacredfart$State
    
    if(check == FALSE)
    {
        stop("No such state")
    }



    outcomes <- c("heart attack", "heart failure", "pneumonia")

    check2 = outcome %in% outcomes
    
    if(check2 == FALSE)
    {
        stop("No Korona in the databanks")
    }



    numvals <- c("best", "worst", 1:length(sacredfart$State))

    check3 = num %in% numvals

    if(check3 == FALSE)
    {
        stop("Invalid Rank")
    }



    if(outcome == 'hearth attack')
        
        outcome <- 'Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack'
    
    else if(outcome == 'hearth failure')
        
        outcome <- 'Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure'
     
     else 
     outcome <- 'Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia'

onlystate <-sacredfart [ which(sacredfart$State == state), ]

onlystate$Rank <- NA

onlystate$Rank[order(onlystate$outcome)]<- nrow(onlystate)

final <- onlystate

return(head(final[1,2]))


}
