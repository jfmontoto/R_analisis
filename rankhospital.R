rankhospital <- function(state, outcome, num = "best") {
  outcome1 <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  outcome1[, c(11, 17, 23)] <- suppressWarnings(apply(outcome1[,c(11,17,23)],2, as.numeric))
  vector.outcome1 <- c("heart attack", "heart failure", "pneumonia")  
  vector.state <- c(outcome1$State)
  if(!(state %in% vector.state)){
    stop("invalid state")}
  if(!(outcome %in% vector.outcome1)){
    stop("invalid outcome")}
  if(outcome=="heart attack") {
    outcome2<-subset(outcome1[,c(2,7,11)])
    outcome2<-outcome2[outcome2$State==state,]
    outcome2<-outcome2[order(outcome2[,3],outcome2[,1], na.last=NA),]
    if(num =="best"){
    return(as.character(outcome2[1,1]))
    }
    else if(num == "worst"){
    return(as.character(tail(outcome2[,1] , n = 1)))
    }
    else {
      rank<-as.numeric(num)
        return(as.character(outcome2[rank,1]))
      }
      }
  if(outcome=="heart failure") {
    outcome2<-subset(outcome1[,c(2,7,17)])
    outcome2<-outcome2[outcome2$State==state,]
    outcome2<-outcome2[order(outcome2[,3],outcome2[,1], na.last=NA),]
    if(num =="best"){
      return(as.character(outcome2[1,1]))
    }
    else if(num == "worst"){
      return(as.character(tail(outcome2[,1] , n =1)))
    }
    else {
      rank<-as.numeric(num)
      return(as.character(outcome2[rank,1]))
    }
  }
  if(outcome=="pneumonia") {
    outcome2<-subset(outcome1[,c(2,7,23)])
    outcome2<-outcome2[outcome2$State==state,]
    outcome2<-outcome2[order(outcome2[,3],outcome2[,1], na.last=NA),]
    if(num =="best"){
      return(as.character(outcome2[1,1]))
    }
    else if(num == "worst"){
      return(as.character(tail(outcome2[,1] , n =1)))
    }
    else {
      rank<-as.numeric(num)
      return(as.character(outcome2[rank,1]))
  }
  }
}