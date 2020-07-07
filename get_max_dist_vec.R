### function that given a vector, retuns the vector that is the maxium euclidean distance away
#(under constraints of code prob matrix values) (i think)

get_max_dist_vec = function(vec){
  max_dist_vec = list()
  for(i in seq.int(length(vec))){
    max_dist_vec[[i]] = ifelse(vec[i] >= .5,vec[i],1)
  }
  max_dist_vec = unlist(max_dist_vec)
  max_dist = dist(rbind(vec,max_dist_vec))
  return(list(max_dist_vec = max_dist_vec,max_dist = as.numeric(max_dist)))
}

