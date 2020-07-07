### Function for computing the correlation between the ena scores from different models-----

compare_points_real = function(model_list) {

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

    results[i,3] = round(cor(x = as.matrix(mod_1$points)[,1], y = as.matrix(mod_2$points)[,1]),3)
    results[i,4] = round(cor(x = as.matrix(mod_1$points)[,2], y = as.matrix(mod_2$points)[,2]),3)

  }

  colnames(results) = c("model_1","model_2", "x_corr", "y_corr")
  return(results)
}
