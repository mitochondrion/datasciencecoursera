rankhospital <- function(state, outcome, num = "best")
{
  data = read.csv("outcome-of-care-measures.csv", stringsAsFactors=FALSE)

  if (!(state %in% data$State)) { stop("invalid state") }
  if (!(outcome %in% c('heart attack', 'heart failure', 'pneumonia'))) { stop("invalid outcome") }
  
  if (num == "best") { num = 1 }
  if (num == "worst") { num = -1 }
  
  outcomeColIdx = switch(outcome,
                         "heart attack"  = 11,
                         "heart failure" = 17,
                         "pneumonia"     = 23)
  
  outcomesForState = data[ data$State == state, ][c(2, outcomeColIdx)]
  outcomesForState[,2] = as.numeric(outcomesForState[,2])
  
  sortedOutcomes = outcomesForState[order(outcomesForState[2], outcomesForState$Hospital.Name),]
  
  sortedOutcomes = sortedOutcomes[ !is.na(sortedOutcomes[,2]), ]
  
  if (num == -1)
  {
    sortedOutcomes[nrow(sortedOutcomes),]$Hospital.Name
  }
  else
  {
    sortedOutcomes[num,]$Hospital.Name
  }
}