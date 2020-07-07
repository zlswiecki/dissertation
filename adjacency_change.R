adjancency_change = function(model_list){

  results_list = list()

  results = matrix(data = NA,nrow = choose(length(model_list),2),ncol = 3)

  meths = t(combn(x = names(model_list),m = 2))

  results[,1:2] = meths

  for (i in 1:length(model_list)) {

    mod_1 = model_list[results[i,1]]
    mod_2 = model_list[results[i,2]]

    mod_1 = flatten(mod_1)
    mod_2 = flatten(mod_2)

    line_av_1 = as.matrix(mod_1$model$row.connection.counts)
    line_av_2 = as.matrix(mod_2$model$row.connection.counts)



    av_sub = line_av_1 - line_av_2

    sums = rowSums(av_sub)

    changed = which(sums != 0)

    percent_changed = round(length(changed)/nrow(line_av_1),2)

    results[i,3] = percent_changed
  }
  colnames(results) = c("model_1","model_2", "percent_changed")
  return(results)
}
