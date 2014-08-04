best = function(state, outcome) {
  data = read.csv("outcome-of-care-measures.csv", stringsAsFactors=FALSE)
  
  if (!(state %in% data$State)) { stop("invalid state") }
  if (!(outcome %in% c('heart attack', 'heart failure', 'pneumonia'))) { stop("invalid outcome") }
  
  outcomeColIdx = switch(outcome,
                         "heart attack"  = 11,
                         "heart failure" = 17,
                         "pneumonia"     = 23)
  
  outcomesForState = data[ data$State == state, ][c(2, outcomeColIdx)]
  outcomesForState[,2] = as.numeric(outcomesForState[,2])

  sortedOutcomes = outcomesForState[order(outcomesForState[2], outcomesForState$Hospital.Name),]
  
  sortedOutcomes[1,]$Hospital.Name
}