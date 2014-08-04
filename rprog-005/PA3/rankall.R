rankall <- function(outcome, num = "best") {
  data = read.csv("outcome-of-care-measures.csv", stringsAsFactors=FALSE)
  
  if (!(outcome %in% c('heart attack', 'heart failure', 'pneumonia'))) { stop("invalid outcome") }
  
  if (num == "best") { num = 1 }
  if (num == "worst") { num = -1 }
  
  outcomeColIdx = switch(outcome,
                         "heart attack"  = 11,
                         "heart failure" = 17,
                         "pneumonia"     = 23)
  
  outcomes = data[,c(2,7,outcomeColIdx)]
  outcomes[,3] = as.numeric(outcomes[,3])
  
  outcomesByState = split(outcomes, outcomes$State)
  
  outcomesByState = lapply(outcomesByState, function(df) { df[order(df[3], df$Hospital.Name), ] })
  outcomesByState = lapply(outcomesByState, function(df) { df[ !is.na(df[,3]), ] })
  
  states = sapply(outcomesByState, function(df) {    
    df[1,2]
  })
  
  hospitals = sapply(outcomesByState, function(df) {    
    if (num == -1)
    {
      df[nrow(df), 1]
    }
    else
    {
      df[num, 1]
    }    
  })

  data.frame(hospital = unname(hospitals), state = unname(states))
}