### function to calculate distances between adjacency vectors

#### UPDATE TO USE RAW CONNECTION COUNTS OR NORMED COUNTS

get_dist_adj_2 = function(model_list, uniqueuser, centered = TRUE){

  if(centered){

    results_list = list()

    results = matrix(data = NA,nrow = choose(length(model_list),2),ncol = 4)

    meths = t(combn(x = names(model_list),m = 2))

    results[,1:2] = meths

    dist_list = list()

    # get_dists = function(sequence, lw_1,lw_2){}

    for (i in 1:length(model_list)) {

      mod_1 = model_list[results[i,1]]
      mod_2 = model_list[results[i,2]]

      mod_1 = flatten(mod_1)
      mod_2 = flatten(mod_2)

      lw_1 = as.matrix(mod_1$line.weights)
      lw_2 = as.matrix(mod_2$line.weights)

      # vecs_to_compare = rbind(lw_1[i,],lw_2[i,])
      #
      # ad_dist =  dist(x = vecs_to_compare)

      ad_dist = sapply(seq.int(dim(lw_1)[1]), function(i) dist(rbind(lw_1[i,], lw_2[i,]))[1])

      # print(ad_dist)
      names_df = data.frame(mod_1$meta.data)

      # names(ad_dist) = names_df[,uniqueuser]

      dist_list[[i]] = ad_dist
      names(dist_list[[i]]) = names_df[,uniqueuser]

      results[i,3] = round(mean(ad_dist),2)

    }
    colnames(results) = c("model_1","model_2", "mean_distance","eval")
    list_names = paste0(results[,1]," & ",results[,2])
    # print(list_names)
    names(dist_list) = list_names
    up_thresh = quantile(unlist(dist_list))[4]
    low_thresh = quantile(unlist(dist_list))[2]

    for (i in 1:length(model_list)) {

      results[i,4] = ifelse(results[i,3]>= up_thresh, "different", ifelse(results[i,3]<= low_thresh, "same","indeterminate"))
    }

    return(list(avg_dist = results, distances = dist_list))
  }else{

    meths = t(combn(x = names(model_list),m = 2))

    distance_list = list()

    # browser()

    for(i in seq.int(length(model_list))){

      # ad_dists = map2(.x = model_list[meths[i,1]],.y = model_list[meths[i,2]],.f = adj_dist_sim)
      ad_dists = adj_dist_sim(model_list[[meths[i,1]]],model_list[[meths[i,2]]])

      # mod = model_list[[meths[i,1]]]
      # names_df = data.frame(mod$meta.data)

      distance_list[[i]] = ad_dists
      # names(dist_list[[i]]) = names_df[,uniqueuser]
    }
    ###may want to store raw distances and check the order
    names(distance_list) = paste0(meths[,1]," & ",meths[,2])
    return(distance_list)
  }
}
