#functions for calculating metrics to predict distances

norm_vec <- function(x) sqrt(sum(x^2))

get_diag_error = function(mat,diagonal){
  # browser()
  other_diag = diag(mat)
  delt = abs(diagonal - other_diag)
  return(delt)
}


get_sna_diff = function(mat_){
  diag_mat_ = diag(mat_)
  diff = 1 - diag_mat_
}

get_dist_metrics = function(simulated_data){

  test_code_mats = pluck(simulated_data,"code_mats")
  test_sna_mats = pluck(simulated_data,"sna_mats")
  test_sna_mats = test_sna_mats$matrices
  test_sna_mats = as.matrix(test_sna_mats[[1]])
  # browser()

  mae_list = list()
  for(i in 1:length(test_code_mats)){
    diagonal = diag(test_code_mats[[i]])
    error = lapply(test_code_mats[-i],get_diag_error,diagonal = diagonal)
    error = mean(unlist(error))
    mae_list[[i]] = error
  }

  sna_diff = get_sna_diff(test_sna_mats)
  metric_df = cbind(unlist(mae_list),sna_diff)
  colnames(metric_df) = c("cog_diff","soc_diff")
  return(metric_df)
}

get_dist_metrics_real = function(code_mats,sna_mats){

  test_code_mats = code_mats
  # test_code_mats = as.matrix(test_code_mats[,colnames(test_code_mats[-1])])
  test_sna_mats = sna_mats
  # test_sna_mats = as.matrix(test_sna_mats[[1]])
  # browser()

  mae_list = list()
  for(i in 1:length(test_code_mats)){
    diagonal = diag(test_code_mats[[i]])
    error = lapply(test_code_mats[-i],get_diag_error,diagonal = diagonal)
    error = mean(unlist(error))
    mae_list[[i]] = error
  }

  sna_diff = get_sna_diff(test_sna_mats)
  metric_df = cbind(unlist(mae_list),sna_diff)
  colnames(metric_df) = c("cog_diff","soc_diff")
  return(metric_df)
}

get_upper_norm = function(code_mats){
  upper_list_all = list()
  #browser()
  for(i in 1:length(code_mats)){
    upper_list_team = list()
    for(j in seq.int(length(code_mats[[i]]))){
      upper = code_mats[[i]][[j]][upper.tri(code_mats[[i]][[j]])]
      upper_norm = norm_vec(x = upper)
      upper_list_team[[j]] = upper_norm
    }
    upper_list_all[[i]] = upper_list_team
  }
  return(unlist(upper_list_all))
}



