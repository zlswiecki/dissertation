get_dist_adj_set_sim = function(model_list){

  meths = t(combn(x = names(model_list),m = 2))

  distance_list = list()

  # browser()

  for(i in seq.int(length(model_list))){

    # ad_dists = map2(.x = model_list[meths[i,1]],.y = model_list[meths[i,2]],.f = adj_dist_sim)
    ad_dists = adj_dist_sim(model_list[[meths[i,1]]],model_list[[meths[i,2]]])

    distance_list[[i]] = ad_dists
  }

  ###may want to store raw distances and check the order

  names(distance_list) = paste0(meths[,1]," & ",meths[,2])
  return(distance_list)

}





