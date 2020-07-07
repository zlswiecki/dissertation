### simulation pipeline function

sim_test_dep = function(n_teams,
                        lines_per_team,
                        team_size,
                        n_codes,
                        soc_type,
                        dom_num,
                        window_size,
                        comparison,
                        use_real,
                        code_probs,
                        codes_real,
                        adj_type,
                        delta,
                        soc_delta){

  team_list = c(1:n_teams)


  ### simulate data

  sim_data = map(.x = team_list,.f = sim_team_data, ### update to map 2 to go over all intervals
                 sna_mat_type = soc_type,
                 team_size = team_size,
                 n_codes = n_codes,
                 dom_num = dom_num,
                 n_lines = lines_per_team,
                 use_real = use_real,
                 code_probs = code_probs,
                 codes_real = codes_real,
                 delta = delta,
                 soc_delta = soc_delta,
                 your_probs = NULL)

  team_lines = map(sim_data,pluck,1)
  team_co_mats = map(sim_data,pluck,2)
  upper_norms = get_upper_norm(code_mats = team_co_mats)

  distance_metrics = map(sim_data,get_dist_metrics)
  distance_metrics_df = do.call("rbind",distance_metrics)

  get_diag = function(mat_list){
    diags = map(mat_list,function(i)diag = diag(i))
  }

  all_diags = map(team_co_mats,.f = get_diag)
  code_probs = map(all_diags,do.call,what = "rbind")


  #### adjacency vectors for simulated data

  #codes = month.abb[1:n_codes]
  codes = letters[1:n_codes]
  units = c("unit","team")
  meta = c("unique_unit","convo_fill","team")
  convo = c("team","convo_fill")
  sim_models = get_normed_adj_sim(sim_data_list = team_lines, ### combines all team_lines
                                  units = units,
                                  convo = convo,
                                  convo.intra = convo,
                                  codes = codes,
                                  meta = meta,
                                  window_size = window_size,
                                  adj_type)


  #### distance between adj types

  sim_distances = get_dist_adj_set_sim(sim_models[1:3])

  #### distance estimate for ENA models

  #get co_occurence probs

  # sim_cooccurences_all = map(team_lines,get_cooccurence_prob_4, splitCol = "team",codes = codes, units = "unique_unit") ### breaking here. Maybe I could use the ENA output somehow....(run window 1 model...)

  # get social matrices
  # browser()
  # soc_sim = map(team_lines,get_social_matrix_nd,unit = "unique_unit",window_size = (window_size -1))
  team_sna_mats = map(team_lines,get_social_matrix,unit = "unique_unit",units = units,convo = convo, meta = meta,window = window_size)

  #get code matrices

  # sim_code_probs = map(.x = team_lines,.f = get_code_probs, splitCol = "team", codes = codes, units = "unique_unit")


  # getting interdependent matrices for all teams

  # sim_cooccurences = map(sim_cooccurences_all,pluck,"team_matrices")
  # sim_cooccurences = flatten(sim_cooccurences)
  # sim_soc = map(soc_sim,pluck,2)
  # names(sim_soc) = team_list
  # sim_code_probs = map(flatten(sim_code_probs),data.frame)
  # sim_code_probs = map(sim_code_probs,function(x) { x[, codes]})

  #getting interdependent adj estimate

  # team_sna_mats = map(team_sna_mats,pluck,"matrices")
  # team_sna_mats = flatten(team_sna_mats)
  # names(team_sna_mats) = team_list
  # names(code_probs) = team_list

  # inter_adj_est_2 = pmap(list(sim_code_probs,soc_sim,sim_cooccurences),inter_by_team) ### errors out if team has just two people
  inter_adj_est = pmap(list(code_probs,team_sna_mats,team_co_mats),inter_by_team) #### ERRORING OUT I THINK BECAUSE OF CLASS OF THE MATRICES (SHOULD BE DFS)
  #
  inter_adj_est = map(inter_adj_est,pluck,3)

  ### get ind estimate   #### UPDATE TO BE MORE GENERAL W/ VARIABLE NAMES....INTER/INTER ETC

  ind_est = get_ind_est(sim_models$ind_set)
  # ind_est_2 = map(sim_cooccurences_all,pluck,"normed")
  #inter_adj_est = bind_rows(inter_adj_est)
  inter_adj_est = do.call("rbind",inter_adj_est)


  ### dist estimate comparison (distance for each person)

  # ind_inter_est_dist = map2(ind_est,inter_adj_est,get_ind_inter_dist)
  # #
  # ind_inter_est_dist = unlist(ind_inter_est_dist)
  ind_inter_est_dist = get_ind_inter_dist(ind_est,inter_adj_est)

  ### set real dist for ind & inter

  ind_inter_dist = sim_distances[comparison][[1]]

  ### get avg dist

  # plot = hist(ind_inter_dist)

  avg_dist = mean(ind_inter_dist)

  n = length(ind_inter_dist)

  sd_dist = sd(ind_inter_dist)

  error = qt(p = 0.975,df = n-1) * (sd_dist/sqrt(n))

  left = avg_dist - error
  right = avg_dist + error

  ci = c(left,avg_dist,right) #,sep = " ")

  distance_metrics_df = cbind(ind_inter_dist,distance_metrics_df)
  colnames(distance_metrics_df)[1] = "real_dist"
  distance_metrics_df = cbind(sim_models$meta,distance_metrics_df)
  distance_metrics_df$upper_norm = upper_norms
  distance_metrics_df$dist_est = ind_inter_est_dist

  # reg = lm(real_dist ~ cog_diff*soc_diff + team,data = distance_metrics_df)


  #compare actual and estimate

  #dist_cor = cor(ind_inter_dist,ind_inter_est_dist) ### will likely need to account for nested structure of the data

  return(list(avg_dist = avg_dist,
              #correlation = dist_cor,
              #plot = plot,
              ci = ci,
              sdev = sd_dist,
              # dist = ind_inter_dist,
              # mods = sim_models
              # dat_ = team_lines,
              dist = distance_metrics_df#,
              #reg = reg
  ))
}


