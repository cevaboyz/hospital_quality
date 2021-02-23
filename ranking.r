rankhospital <- function(state, outcome, num = "best")
{
    #read outcome data

    #check that state and outcome are valid

    #return hosptial name in that state with the given rank 

    #30-day rate death rate 

    sacredfart <- read.csv("outcome-of-care-measures.csv") 
    
    if(!state %in% sacredfart$State)
        stop(sprintf("No such state named %s", state))

    if(outcome == 'hearth attack')        
        outcome <- 'Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack'
    else if(outcome == 'hearth failure')        
        outcome <- 'Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure'
    else if(outcome == 'pneumonia')
        outcome <- 'Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia'
    else
        stop(sprintf("No outcome found for %s", outcome))

    numvals <- c("best", "worst", 1:length(sacredfart$State))

    if(!num %in% numvals)
        stop("Invalid Rank %s", num)

    
    onlystate <-sacredfart [ which(sacredfart$State == state), ]

    result <- onlystate[order(onlystate[[outcome]], na.last = NA, decreasing = TRUE),]

    print(result) 
    # final <- onlystate[order(onlystate[outcome],na.last = TRUE, decreasing = FALSE),]

    # print(onlystate[order(onlystate[outcome],na.last = NA, decreasing = TRUE),][outcome])

    
    # if(num == "best")
    #     num <- 1
    # else if (num == "worst")
    #     num <- nrow(final)
        
    # return(final$Hospital.Name[num])

    # # onlystate$Rank <- NA

    # # onlystate$Rank[order(onlystate$outcome)]<- nrow(onlystate)

    # # final <- onlystate

    # return(head(final[1,2]))

}
print(rankhospital("TX", "hearth failure", 4));
