### test for engineering data

real_test_eng = function(dat_ = eng_dat,split_col = "unique_team_final",norm = TRUE){

  # browser()

  codes_eng = names(eng_dat)[9:14]
#
#   if(rm_ones){
#     ones = which(eng_dat$unique_team_final %in% c("iowa2015a&1","ksu2015c&1","ksu2015c&2"))
#     eng_dat = eng_dat[-ones,]
#
#   }

  units_eng = c("uniqueuserid","site_x", "group_id") # added group id to units to distinguish mentors
  convo_eng_inter = c("group_id","roomName","site_x")
  convo_eng_intra = c("uniqueuserid","group_id","roomName")
  convo_eng_ind = c("group_id","roomName","site_x")
  meta_eng = c("site_x",
               "OutcomeBins",
               "unique_team_final",
               "uniqueuser"#,
               # "OutcomeBin_Char",
               # "var_bin",
               # "var_per_code"#,
               #"mean_bin",
               #"mean_score"#,
               # "ATOMTOT_GROUP"
  )
  window_size_eng = 4
  window_size_intra = 3


  models_eng = get_normed_adj_real(data_ = eng_dat,
                                   codes = codes_eng,
                                   units = units_eng,
                                   convo.ind = convo_eng_ind,
                                   convo.intra = convo_eng_intra,
                                   convo.inter = convo_eng_inter,
                                   window.size.inter = window_size_eng,
                                   window.size.intra = window_size_intra,
                                   meta = meta_eng,
                                   context = "NTX")

  unit_names_e = models_eng$unit_names
  meta = models_eng$meta
  models_eng = models_eng[1:3]

  ### get_real_dist
  dist_eng = get_adj_dist_real(model_list = models_eng)
  dist_eng = dist_eng$`ind & inter`
  names(dist_eng) = unit_names_e

  ### get_correlation
  cor_eng = get_adj_cor_real(model_list = models_eng)
  cor_eng = cor_eng$`ind & inter`
  names(cor_eng) = unit_names_e

  ### get estimated distances
  eng_split = split(eng_dat,eng_dat[,split_col])

  #get social matrices

  soc_eng = map(.x = eng_split,
                get_social_matrix,
                unit = "uniqueuser",
                window = window_size_eng,
                units = units_eng,
                convo = convo_eng_inter,
                meta = meta_eng)

  #get code matrices
  eng_code_probs = get_code_probs(data = eng_dat,
                                  splitCol = "unique_team_final",
                                  codes = codes_eng,
                                  units = "uniqueuser")
  eng_code_probs = map(eng_code_probs,data.frame)
  eng_code_probs = map(eng_code_probs, function(i) i = i[,-1])

  #get co-occurence matrices

  ind = ena.accumulate.data(units = eng_dat[,units_eng],
                           conversation = eng_dat[,convo_eng_inter],
                           codes = eng_dat[,codes_eng],
                           metadata = eng_dat[,meta_eng],
                           window.size.back = 1)

  ind_est_eng_all = get_ind_est_real(ind_set = ind,split_col = split_col)
  if(norm){
    ind_est_eng = ind_est_eng_all$normed
  }else{
    ind_est_eng = ind_est_eng_all$prob
  }
  eng_co_mats = ind_est_eng_all$team_probs
  eng_co_mats = eng_co_mats[names(soc_eng)]



  #inter est
  inter_est_eng = pmap(list(eng_code_probs,soc_eng,eng_co_mats),inter_by_team)
  inter_est_eng = map(inter_est_eng,pluck,3)
  inter_est_eng = do.call("rbind",inter_est_eng)

  #dist est
  # browser()
  # sub_names_eng = rownames(ind_est_eng) %in% rownames(inter_est_eng)
  # ind_est_eng = ind_est_eng[sub_names_eng,]
  ind_est_eng = ind_est_eng[rownames(inter_est_eng),]
  ind_inter_dist_est = get_ind_inter_dist(ind_adjs = ind_est_eng,inter_adjs = inter_est_eng)
  names(ind_inter_dist_est)= rownames(ind_est_eng)

  #cor est
  ind_inter_cor_est = get_ind_inter_cor(ind_adjs = ind_est_eng,inter_adjs = inter_est_eng)
  names(ind_inter_cor_est) = rownames(ind_est_eng)

  #real distance
  dist_eng = dist_eng[names(ind_inter_dist_est)]
  #reorder estimate

  # correlation
  cor_eng = cor_eng[names(ind_inter_dist_est)]

  ### make results df

  # browser()

  eng_cooccurences_all = get_cooccurence_prob_3(data = eng_dat,splitCol = "unique_team_final",codes = codes_eng,units = "uniqueuser")
  eng_cooccurences = eng_cooccurences_all$team_matrices
  upper_norms_eng = get_upper_norm(code_mats = eng_cooccurences)

  meta = meta[order(meta$ENA_UNIT),]
  dist_df = meta
  dist_df$real_dist = dist_eng
  dist_df$dist_est = ind_inter_dist_est

  distance_metrics = map2(eng_cooccurences,soc_eng,get_dist_metrics_real)
  distance_metrics_all = do.call("rbind",distance_metrics)
  distance_metrics_all = distance_metrics_all[dist_df$ENA_UNIT,]
  dist_df = cbind(dist_df,distance_metrics_all)
  dist_df$upper_norm = upper_norms_eng
  ### adding correlation
  dist_df$cor = cor_eng
  dist_df$cor_est = ind_inter_cor_est

  return(dist_df)
}
