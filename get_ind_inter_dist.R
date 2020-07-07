#funciton to get distance/correlation between adjacency vectors

get_ind_inter_dist = function(ind_adjs, inter_adjs){


  adj_dist = sapply(seq.int(dim(ind_adjs)[1]), function(i) dist(rbind(ind_adjs[i,], inter_adjs[i,]))[1])
}


get_ind_inter_cor = function(ind_adjs, inter_adjs){

  adj_cor = suppressWarnings(sapply(seq.int(dim(ind_adjs)[1]), function(i) cor(ind_adjs[i,],inter_adjs[i,])))
}
