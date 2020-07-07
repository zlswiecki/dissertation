### Function for computing the correlation between the ena scores from different models for different groups in each model-----

compare_points_real_groups = function(model_list, group_col, group_val) {

  results = matrix(data = NA,nrow = choose(length(model_list),2),ncol = 4)

  meths = t(combn(x = names(model_list),m = 2))

  results[,1:2] = meths

  for (i in 1:length(model_list)) {

    mod_1 = model_list[results[i,1]]
    mod_2 = model_list[results[i,2]]

    mod_1 = flatten(mod_1)
    mod_2 = flatten(mod_2)

    # print(results[i,1])
    # print(results[i,2])
    
    
    points_1 = mod_1$points %>% filter(.data[[group_col]] == group_val)
    points_1 = points_1 %>% select(contains("SVD"))
    points_2 = mod_2$points %>% filter(.data[[group_col]] == group_val)
    points_2 = points_2 %>% select(contains("SVD"))

    results[i,3] = round(cor(x = points_1[,1], y = points_2[,1]),3)
    results[i,4] = round(cor(x = points_1[,2], y = points_2[,2]),3)

  }

  colnames(results) = c("model_1","model_2", "x_corr", "y_corr")
  
  res_list = list(results)
  names(res_list) = group_val
  
  return(res_list)
}



