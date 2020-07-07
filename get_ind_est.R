get_ind_est_dep = function(ind_set){
  browser()
  adj_mats = connection.matrix(ind_set$connection.counts)
  uppers = map(adj_mats,function(i) upper_mat = i[upper.tri(i)])
  uppers_mat = t(bind_rows(uppers))
  co_names= colnames(as.matrix(ind_set$connection.counts))
  colnames(uppers_mat) = co_names
  new_order = order(ind_set$meta.data$team,ind_set$meta.data$unit)
  raw = uppers_mat[new_order,]
  line_counts = table(ind_set$model$row.connection.counts$ENA_UNIT) #### CHECK ORDER HERE
  data_order = as.tbl(data.frame(rownames(raw)))
  colnames(data_order) = "Var1"
  unit_counts = as.tbl(data.frame(line_counts))
  line_counts = inner_join(data_order,unit_counts,by = "Var1")
  prob = raw/as.vector(line_counts$Freq)
  normed = fun_sphere_norm(prob)
  rownames(normed) = rownames(prob)
  colnames(normed) = colnames(prob)
  return(normed)
}

get_ind_est = function(ind_set,split_col = "team"){
  #getting adjacency prob matrices
  adj_mats = connection.matrix(ind_set$connection.counts)
  new_order = order(ind_set$meta.data$team,ind_set$meta.data$unit)
  adj_mats = adj_mats[new_order]
  line_counts = table(ind_set$model$row.connection.counts$ENA_UNIT)
  data_order = as.tbl(data.frame(names(adj_mats)))
  colnames(data_order) = "Var1"
  unit_counts = as.tbl(data.frame(line_counts))
  line_counts = inner_join(data_order,unit_counts,by = "Var1")
  line_counts = as.list(line_counts$Freq)
  probs = map2(adj_mats,line_counts,function(i,j)i/j)
  prob_names = names(probs)
  probs_list = list()
  for(i in 1:length(probs)){
    nas = is.na(probs[[i]])
    probs[[i]][nas] = 0
    probs_list[[i]] = probs[[i]]
  }
  names(probs_list) = prob_names
  cc = ind_set$connection.counts[new_order,]
  teams = unique(cc[,split_col])
  which_list = list()
  for(i in 1:length(teams)){
    which_list[[i]] = which(cc[,split_col] == teams[[i]])
  }
  names(which_list) = teams

  teams_list = list()
  for(i in 1:length(which_list)){
    teams_list[[i]] = probs[which_list[[i]]]
  }
  names(teams_list) = teams
  ###getting normed vecs
  adj_mats = connection.matrix(ind_set$connection.counts)
  uppers = map(adj_mats,function(i) upper_mat = i[upper.tri(i)])
  uppers_mat = t(bind_rows(uppers))
  co_names= colnames(as.matrix(ind_set$connection.counts))
  colnames(uppers_mat) = co_names
  # new_order = order(ind_set$meta.data$team,ind_set$meta.data$unit)
  raw = uppers_mat[new_order,]
  line_counts = table(ind_set$model$row.connection.counts$ENA_UNIT) #### CHECK ORDER HERE
  data_order = as.tbl(data.frame(rownames(raw)))
  colnames(data_order) = "Var1"
  unit_counts = as.tbl(data.frame(line_counts))
  line_counts = inner_join(data_order,unit_counts,by = "Var1")
  prob = raw/as.vector(line_counts$Freq)
  normed = fun_sphere_norm(prob)
  rownames(normed) = rownames(prob)
  colnames(normed) = colnames(prob)

  return(list(normed = normed, prob = prob, team_probs = teams_list))
}



get_ind_est_real = function(ind_set,split_col){
  adj_mats = connection.matrix(ind_set$connection.counts)
  adj_mats = adj_mats[order(names(adj_mats))]
  line_counts = table(ind_set$model$row.connection.counts$ENA_UNIT)
  line_counts = as.list(line_counts)
  probs = map2(adj_mats,line_counts,function(i,j)i/j)
  prob_names = names(probs)
  probs_list = list()
  for(i in 1:length(probs)){
    nas = is.na(probs[[i]])
    probs[[i]][nas] = 0
    probs_list[[i]] = probs[[i]]
  }
  names(probs_list) = prob_names
  cc = ind_set$connection.counts[order(ind_set$connection.counts$ENA_UNIT),]
  teams = unique(cc[,split_col])
  which_list = list()
  for(i in 1:length(teams)){
    which_list[[i]] = which(cc[,split_col] == teams[[i]])
  }
  names(which_list) = teams

  teams_list = list()
  for(i in 1:length(which_list)){
    teams_list[[i]] = probs[which_list[[i]]]
  }

  names(teams_list) = teams
  # teams_list = teams_list[sort.int(names(teams_list))]####sorting is fucked...
  uppers = map(adj_mats,function(i) upper_mat = i[upper.tri(i)])
  uppers_mat = t(bind_rows(uppers))
  line_counts = table(ind_set$model$row.connection.counts$ENA_UNIT)
  co_names= colnames(as.matrix(ind_set$connection.counts))
  colnames(uppers_mat) = co_names
  raw = uppers_mat
  raw = uppers_mat[order(rownames(raw)),]
  prob = raw/as.vector(line_counts)
  normed = fun_sphere_norm(prob)
  rownames(normed) = rownames(prob)
  colnames(normed) = colnames(prob)
  return(list(normed = normed, prob = prob,team_probs = teams_list))
}

get_ind_est_real_old = function(ind_set){
  browser()
  adj_mats = connection.matrix(ind_set$connection.counts)
  uppers = map(adj_mats,function(i) upper_mat = i[upper.tri(i)])
  uppers_mat = t(bind_rows(uppers))
  co_names= colnames(as.matrix(ind_set$connection.counts))
  colnames(uppers_mat) = co_names
  # uppers_mat = uppers_mat[sort(rownames(uppers_mat)),]
  # browser()
  #new_order = order(ind_set$meta.data$team,ind_set$meta.data$unit)
  raw = uppers_mat
  raw = uppers_mat[order(rownames(raw)),]
  line_counts = table(ind_set$model$row.connection.counts$ENA_UNIT) #### CHECK ORDER HERE
  # line_counts = data.frame(line_counts)
  # sub = which(line_counts$Var1 %in% rownames(raw))
  # line_counts = line_counts[sub,]
  # data_order = as.tbl(data.frame(rownames(raw)))
  # colnames(data_order) = "Var1"
  # unit_counts = as.tbl(data.frame(line_counts))
  # line_counts = inner_join(data_order,unit_counts,by = "Var1")
  # prob = raw/as.vector(line_counts$Freq)
  prob = raw/as.vector(line_counts)
  normed = fun_sphere_norm(prob)
  rownames(normed) = rownames(prob)
  colnames(normed) = colnames(prob)
  return(list(normed = normed, prob = prob))
}
