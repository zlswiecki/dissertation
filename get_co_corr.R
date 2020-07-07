 # Function for calculating the correlations between co-occurence dimensions

get_co_corr =  function(model_list) {

  corr_list = list()

  results = matrix(data = NA,nrow = choose(length(model_list),2),ncol = 3)

  meths = t(combn(x = names(model_list),m = 2))

  results[,1:2] = meths

  for (i in 1:length(model_list)) {

    mod_1 = model_list[results[i,1]]
    mod_2 = model_list[results[i,2]]

    mod_1 = flatten(mod_1)
    mod_2 = flatten(mod_2)

    lw_1 = as.matrix(mod_1$line.weights)
    lw_2 = as.matrix(mod_2$line.weights)

    ad_cors = suppressWarnings(sapply(seq.int(dim(lw_1)[2]), function(i) cor(lw_1[,i], lw_2[,i]))) # swap i to col for the column cors
    ad_cors = na.omit(ad_cors)

    corr_list[[i]] = ad_cors

    results[i,3] = round(mean(ad_cors),2)

  }

  colnames(results) = c("model_1","model_2", "avg_corr")
  list_names = paste0(results[,1]," & ",results[,2])
  # print(list_names)
  names(corr_list) = list_names
  return(list(avg_cor = results, corrs = corr_list))
}
