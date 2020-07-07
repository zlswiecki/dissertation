#function for calculating the probability of the union of n sets

union_prob = function(vec){

  comp_vec = 1 - vec
  comp_vec_prod = prod(comp_vec)
  up = 1 - comp_vec_prod
  return(up)
}
