real_test_2 = function(dat_,split_col,norm = TRUE, window.size){



  # nav_dat$uniqueuser = paste(nav_dat$Team,nav_dat$Scenario,nav_dat$Speaker,sep = ".")
  codes_nav = names(nav_dat)[30:37]
  if(split_col == "Team"){
    units_nav = c("Team","Speaker")
    meta_nav = c("COND",
                 "MacroRole" ,
                 "AVG_ATOMTOT",
                 "uniqueuser"#,
                 # "TEAMNUM"
                 # "var_bin",
                 # "var_per_code"
                 # "ATOMTOT_GROUP"
    )
  }else{
    units_nav = c("Team","Scenario","Speaker") # collapsing over scenarios. Need to JUSTIFY AND ALSO TRY AT SCENARIO LEVEL
    meta_nav = c("COND",
                 "MacroRole" ,
                 "AVG_ATOMTOT",
                 "uniqueuser2",
                 "TEAMNUM"
                 # "var_bin",
                 # "var_per_code"
                 # "ATOMTOT_GROUP"
    )
  }
  convo_nav_inter = c("Team","Scenario")
  convo_nav_intra = c("Team","Speaker","Scenario")
  convo_nav_ind = c("Team", "Scenario") # I think I need scenario here # doesn't actually matter
  # window_size_nav = 5
  window_intra = 3


  models_nav = get_normed_adj_real(data_ = dat_,
                                   codes = codes_nav,
                                   units = units_nav,
                                   convo.ind = convo_nav_ind,
                                   convo.intra = convo_nav_intra,
                                   convo.inter = convo_nav_inter,
                                   window.size.inter = window.size,
                                   window.size.intra = window_intra,
                                   meta = meta_nav,
                                   context = "TADMUS")

  unit_names = models_nav$unit_names
  meta = models_nav$meta
  ind = models_nav$ind_set
  models_nav = models_nav[1:3]
  #meta = meta[order(meta$ENA_UNIT),]

  dist_nav = get_adj_dist_real(model_list = models_nav)
  dist_nav = dist_nav$`ind & inter`
  names(dist_nav) = unit_names   #### NEED TO CHECK ORDER HERE

  #correlations
  cor_nav = get_adj_cor_real(model_list = models_nav)
  cor_nav = cor_nav$`ind & inter`
  names(dist_nav) = unit_names
  #

  nav_split = split(nav_dat,nav_dat[,split_col])

  # get social matrices

  if(split_col == "Team"){
    nav_probs = nav_code_probs
    soc_nav = map(.x = nav_split,get_social_matrix,unit = "uniqueuser",window = window.size,units = units_nav,convo = convo_nav_inter,meta = meta_nav)
    soc_nav_2 = map(.x = nav_split,get_social_matrix,unit = "uniqueuser",window = 2,units = units_nav,convo = convo_nav_inter,meta = meta_nav)
  }else{
    nav_probs = nav_code_probs_sc
    soc_nav = map(.x = nav_split,get_social_matrix,unit = "uniqueuser2",window = window.size,units = units_nav,convo = convo_nav_inter,meta = meta_nav)
  }

  nav_probs = map(nav_probs,data.frame)
  test = nav_probs
  nav_probs = map(nav_probs, function(i) i = i[,-1])
  # nav_probs = map(nav_probs,subset,select = codes_nav)

  upper_norms_nav = get_upper_norm(code_mats = nav_mats$team_matrices)
  # browser()


  ind = ena.accumulate.data(units = nav_dat[,units_nav],
                            conversation = nav_dat[,convo_nav_inter],
                            codes = nav_dat[,codes_nav],
                            metadata = nav_dat[,meta_nav],
                            window.size.back = 1)

  ind_est_nav_all = get_ind_est_real(ind_set = ind,split_col = split_col)
  if(norm){
    ind_est_nav = ind_est_nav_all$normed
  }else{
    ind_est_nav = ind_est_nav_all$prob
  }

  nav_co_mats = ind_est_nav_all$team_probs ### NEED TO ORDER SOMEHOW TO MATCH SOC AND CODE PROBS
  nav_co_mats = nav_co_mats[names(soc_nav)]


  ### getting matrices the old_way
  # browser()
  #
  # soc_nav_ = map(.x = nav_split,get_social_matrix, unit = "uniqueuser",window = window_size_nav,units = units_nav, convo = convo_nav_inter, meta = meta_nav)
  # soc_nav_ = map(soc_nav_,pluck,2)
  # nav_cooccurences_all = get_cooccurence_prob_4(data = nav_dat,splitCol = "Team",codes = codes_nav,units = "uniqueuser")
  # nav_cooccurences = nav_cooccurences_all$team_matrices
  #
  # nav_code_probs = get_code_probs(data = nav_dat,splitCol = "Team",codes = codes_nav,units = "uniqueuser")
  # nav_code_probs = map(nav_code_probs,data.frame)
  # nav_code_probs_names = nav_code_probs
  # nav_code_probs = map(nav_code_probs,subset,select = codes_nav)
  #
  # ### getting estimates the old way
  # inter_mats_adjs_nav = pmap(list(nav_code_probs,soc_nav,nav_cooccurences),inter_by_team)
  # inter_adj_nav = map(inter_mats_adjs_nav,pluck,3)
  # inter_adj_nav = do.call("rbind",inter_adj_nav)
  #
  # ind_adj_nav = nav_cooccurences_all$normed
  # inter_ind_dist_estimate = get_ind_inter_dist(ind_adjs = ind_adj_nav,inter_adjs = inter_adj_nav)
  # names(inter_ind_dist_estimate)= rownames(ind_adj_nav)
  ####

  ###estimates the new way



  inter_est_nav = pmap(list(nav_probs,soc_nav,nav_co_mats),inter_by_team)
  inter_est_nav = map(inter_est_nav,pluck,3)
  inter_est_nav = do.call("rbind",inter_est_nav) ### preappends X to rownames...

  ind_inter_dist_est = get_ind_inter_dist(ind_adjs = ind_est_nav,inter_adjs = inter_est_nav)
  names(ind_inter_dist_est)= rownames(ind_est_nav)

  ### correlation estimate
  ind_inter_cor_est = get_ind_inter_cor(ind_adjs = ind_est_nav,inter_adjs = inter_est_nav)
  names(ind_inter_cor_est)= rownames(ind_est_nav)
  ###

  distance_metrics = map2(nav_mats$team_matrices,soc_nav_2,get_dist_metrics_real)
  distance_metrics_all = do.call("rbind",distance_metrics)

  # browser()


  sub_names = names(ind_inter_dist_est) %in% names(dist_nav)
  sub_pos = which(names(ind_inter_dist_est) %in% names(dist_nav))
  ind_inter_dist_est_sub = ind_inter_dist_est[sub_names]

  #correlation estimate sub
  ind_inter_cor_est_sub = ind_inter_cor_est[sub_names]
  ###

  meta = meta[order(meta$ENA_UNIT),]
  dist_df = meta
  dist_df$real_dist = dist_nav
  dist_df$dist_est = ind_inter_dist_est_sub
  ###adding correlation values in
  dist_df$cor = cor_nav
  dist_df$cor_est = ind_inter_cor_est_sub
  ###
  distance_metrics_all = distance_metrics_all[sub_names,]
  uppers = upper_norms_nav[sub_pos]
  dist_df = cbind(dist_df,distance_metrics_all)
  dist_df$upper_norm = uppers


  # correlation = cor(ind_inter_dist_est_sub,dist_nav)

  # distance_metrics_all = distance_metrics_all[unit_sub,]
  # distance_metrics_all = cbind(dist_nav,distance_metrics_all)
  # colnames(distance_metrics_all)[1] = "actual_dist"
  # upper_norms_nav = upper_norms_nav[unit_sub]
  # distance_metrics_all$upper_norm = upper_norms_nav
  # distance_metrics_all = cbind(meta,distance_metrics_all)
  # distance_metrics_all$dist_est = ind_inter_dist_est

  return(list(results = dist_df, models = models_nav))

}
