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

    
    onlystate <-sacredfart [ which(sacredfart$State == state ), ]

    onlystate <- onlystate[!is.na(onlystate[outcome]),]   ##THIS SHIT IS NOT WORKING FUCK 

    # print(class(onlystate[[outcome]][1]))
    
    onlystate[outcome]<- lapply(onlystate[outcome], as.numeric)  ##THIS IS VERY IMPORTANT

    # xd<-onlystate[[outcome]][1]
    # print(class(xd))

    result <- onlystate[order(onlystate[outcome],onlystate$Hospital.Name, na.last = NA, decreasing = FALSE),]

    
    if(num == "best")
        num <- 1
    else if (num == "worst")
        num <- nrow(result)
        
    return(result$Hospital.Name[num])

}
print(rankhospital("MD", "hearth attack", "worst"));
