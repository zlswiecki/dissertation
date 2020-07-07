# function for adding OR probabilities

sum_probs = function(e1,e2){

  prob_sum = 1-((1-e1)*(1-e2))
  return(prob_sum)
}
