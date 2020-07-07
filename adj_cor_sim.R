#function for calculating the correlation (angle between) adjacency vectors

adj_cor_sim = function(i,j){

  lw_1 = i
  lw_2 = j
  # browser()
  adj_cor = suppressWarnings(sapply(seq.int(dim(lw_1)[1]), function(i) cor(lw_1[i,],lw_2[i,])))
  return(adj_cor)

}

get_adj_cor_real = function(model_list){

  meths = t(combn(x = names(model_list),m = 2))

  cor_list = list()

  for(i in seq.int(length(model_list))){

    # ad_dists = map2(.x = model_list[meths[i,1]],.y = model_list[meths[i,2]],.f = adj_dist_sim)
    adj_cors = adj_cor_sim(model_list[[meths[i,1]]],model_list[[meths[i,2]]])

    cor_list[[i]] = adj_cors
  }
  names(cor_list) = paste0(meths[,1]," & ",meths[,2])
  return(cor_list)
}
