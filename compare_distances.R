compare_distances = function(matrix_list,codes){
  
  results_list = list()
  for (i in 1:length(matrix_list)){
    
    dat = matrix_list[[i]]
    ind = get_ind_vectors(matrix = dat,codes = codes)
    inter = get_inter_vectors(matrix = dat,codes = codes)
    
    dist = get_ind_inter_dist(ind_adjs = ind,inter_adjs = inter)
    
    dat$dist_estimate = dist
    results_list[[i]] = dat
    
  }
  
  full_dat = bind_rows(results_list)
  plot(full_dat$distance,full_dat$dist_estimate)
  correlation = cor(full_dat$distance,full_dat$dist_estimate)
  test_formula = distance ~ dist_estimate #+ Team
  test_model = lm(formula = test_formula,data = full_dat)
  
  return(list(datset = full_dat, dist_corr = correlation, model = test_model))
  
}
