setwd("C:/Users/SH/Desktop/R_Programming_W4")
OOCM <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
OOCM <- OOCM[order(OOCM$Hospital.Name),]###ORDER BY HOSPITAL.NAME alphabetical
OOCM[,11] <- as.numeric(OOCM[,11])
OOCM[,17] <- as.numeric(OOCM[,17])
OOCM[,23] <- as.numeric(OOCM[,23])

source("rprog_scripts_submitscript3.R")

checker <- function(state,outcome){
    if(!(outcome %in% (names(OOCM))))       {stop("invalid outcome")}
    if(!(state %in% as.list(OOCM$State)))   {stop("invalid state")}
}

outcomeConvert <- function(outcome){
    "Hospital.30.Day.Death..Mortality..Rates.from"
    if(outcome=="heart attack") {return( "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack")}
    if(outcome=="heart failure"){return( "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure")}
    if(outcome=="pneumonia")    {return( "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia")}
    return(outcome)
}



best <- function(state, outcome) {
    outcome <- outcomeConvert(outcome)
    ## Read outcome data
    #OOCM <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    ## Check that state and outcome are valid
    if(!(outcome %in% (names(OOCM)))) {stop("invalid outcome")}
    if(!(state %in% as.list(OOCM$State)))    {stop("invalid state")}
    ## Return hospital name in that state with lowest 30-day death rate
    outcome <- outcomeConvert(outcome)
    stateDF <- subset(OOCM, State==state)
    mortalityRate <- as.numeric(stateDF[,outcome])
    championIndex<-which.min(mortalityRate) #NaN and NA are discarded in which.min & which.max
    championName <- stateDF$Hospital.Name[championIndex]
    championName
}

worst <- function(state, outcome) {
    outcome <- outcomeConvert(outcome)
    ## Read outcome data
    #OOCM <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    ## Check that state and outcome are valid
    if(!(outcome %in% (names(OOCM)))) {stop("invalid outcome")}
    if(!(state %in% as.list(OOCM$State)))    {stop("invalid state")}
    ## Return hospital name in that state with lowest 30-day death rate
    outcome <- outcomeConvert(outcome)
    stateDF <- subset(OOCM, State==state)
    mortalityRate <- as.numeric(stateDF[,outcome])
    championIndex<-which.max(mortalityRate) #NaN and NA are discarded in which.min & which.max
    championName <- stateDF$Hospital.Name[championIndex]
    championName
}


rankall <- function(outcome,num='best') {
    ## Read outcome data
    OOCM <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    OOCM <- OOCM[order(OOCM$Hospital.Name),]###ORDER BY HOSPITAL.NAME alphabetical
    ## Check that outcome are valid
    convertedOutcome <- outcomeConvert(outcome)
    if(!(convertedOutcome %in% names(OOCM))){stop("invalid outcome")}
    ## Return a data frame with the hospital names and the
    ## (abbreviated) state name
    states <-  sort(unique(OOCM$State))
    hospitals <- mapply(rankhospital,outcome=outcome,num=num, states)
    data.frame(hospital=hospitals, state=states)
}

rankhospital <- function(state, outcome,num) {
    if(num=='best'){return(best(state,outcome))}
    if(num=='worst'){return(worst(state,outcome))}
    ## Read outcome data
    outcome <- outcomeConvert(outcome)
    ## Check that state and outcome are valid
    checker(state,outcome)
    ## Return hospital name in that state with the given rank
    ## 30-day death rate
    stateDF <- subset(OOCM, State==state)
    rankedDF <- stateDF[order(stateDF[,outcome]),]
    NthHospitalName <- rankedDF$Hospital.Name[num]
    NthHospitalName
}

rankhospital("NV","heart failure",10)

head(rankall("heart attack",20),10)
tail(rankall("pneumonia", "worst"), 3)
tail(rankall("heart failure"), 10)
rankall("heart failure", 10)
