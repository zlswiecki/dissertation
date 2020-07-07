### distance comparison for real data

real_test = function(dat_){

  codes_nav = names(nav_dat)[30:37]
  units_nav = c("Team","Speaker") # collapsing over scenarios. Need to JUSTIFY AND ALSO TRY AT SCENARIO LEVEL
  convo_nav_inter = c("Team","Scenario")
  convo_nav_intra = c("Team","Speaker","Scenario")
  convo_nav_ind = c("Team", "Scenario") # I think I need scenario here # doesn't actually matter
  meta_nav = c("COND",
               "MacroRole" ,
               "AVG_ATOMTOT",
               "uniqueuser"#,
               # "var_bin",
               # "var_per_code"
               # "ATOMTOT_GROUP"
  )
  window_size_nav = 5
  window_intra = 3

  # browser()

  models_nav = get_normed_adj_real(data_ = dat_,
                                   codes = codes_nav,
                                   units = units_nav,
                                   convo.ind = convo_nav_ind,
                                   convo.intra = convo_nav_intra,
                                   convo.inter = convo_nav_inter,
                                   window.size.inter = window_size_nav,
                                   window.size.intra = window_intra,
                                   meta = meta_nav,
                                   context = "TADMUS")

  unit_names = models_nav$unit_names
  meta = models_nav$meta
  models_nav = models_nav[1:3]
  #meta = meta[order(meta$ENA_UNIT),]

  dist_nav = get_adj_dist_real(model_list = models_nav)
  dist_nav = dist_nav$`ind & inter`
  # names(dist_nav) = unit_names

  # nav_split = split(nav_dat,nav_dat[,"Team"])

  # get social matrices

  # soc_nav = map(.x = nav_split,get_social_matrix,unit = "uniqueuser",window_size = 1) ### update to be lag 1
  # nav_mats = get_cooccurence_prob_4(data = nav_dat,splitCol = "Team",codes = codes_nav,units = "uniqueuser") #add this as data to the package later

  # distance_metrics = map2(nav_mats$team_matrices,soc_nav,get_dist_metrics_real)
  # distance_metrics_all = do.call("rbind",distance_metrics)

  # browser()
  # unit_sub = which(meta$ENA_UNIT %in% rownames(distance_metrics_all))
  # distance_metrics_all = distance_metrics_all[unit_sub,]
  # distance_metrics_all = cbind(dist_nav,distance_metrics_all)
  # colnames(distance_metrics_all)[1] = "actual_dist"
  # distance_metrics_all = cbind(meta,distance_metrics_all)
  # reg = lm(actual_dist ~ cog_diff*soc_diff + Team,distance_metrics_all)

  return(list(dist = dist_nav,meta = meta))

}
