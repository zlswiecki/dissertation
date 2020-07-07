adj_dist_sim = function(i,j){

  lw_1 = i
  lw_2 = j

  # browser()
  ad_dist = sapply(seq.int(dim(lw_1)[1]), function(i) dist(rbind(lw_1[i,], lw_2[i,]))[1])
  return(ad_dist)



  # avg_dist = mean(ad_dist)
}
