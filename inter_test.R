#### testing inter_estimate on navy data

inter_test = function(dat_,split_col){

  # browser()
  nav_split = split(nav_dat,nav_dat[,split_col])
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
    units_nav = c("Team","Scenario","Speaker")
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
  convo_nav_ind = c("Team", "Scenario")
  window_size_nav = 5
  window_intra = 3
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
  ind = models_nav$ind_set
  models_nav = models_nav[1:3]

  if(split_col == "Team"){
    nav_probs = nav_code_probs
    soc_nav = map(.x = nav_split,get_social_matrix,unit = "uniqueuser",window = window_size_nav,units = units_nav,convo = convo_nav_inter,meta = meta_nav)
  }else{
    nav_probs = nav_code_probs_sc
    soc_nav = map(.x = nav_split,get_social_matrix,unit = "uniqueuser2",window = window_size_nav,units = units_nav,convo = convo_nav_inter,meta = meta_nav)
  }


  nav_probs = map(nav_probs,data.frame)
  nav_probs = map(nav_probs, function(i) i = i[,-1])

  ind = ena.accumulate.data(units = nav_dat[,units_nav],
                            conversation = nav_dat[,convo_nav_inter],
                            codes = nav_dat[,codes_nav],
                            metadata = nav_dat[,meta_nav],
                            window.size.back = 1)

  ind_est_nav_all = get_ind_est_real(ind_set = ind,split_col = split_col)
  ind_est_nav = ind_est_nav_all$normed
  nav_co_mats = ind_est_nav_all$team_probs
  nav_co_mats = nav_co_mats[names(soc_nav)]

  inter_est_nav = pmap(list(nav_probs,soc_nav,nav_co_mats),inter_by_team)
  inter_est_nav = map(inter_est_nav,pluck,3)
  inter_est_nav = do.call("rbind",inter_est_nav)

  return(list(soc = soc_nav,co = nav_co_mats,prob = nav_probs,est = inter_est_nav))
}

#compare

# team_test = inter_test(dat_ = nav_dat,split_col = "Team")
# scenario_test = inter_test(dat_ = nav_dat,split_col = "TEAMNUM")




