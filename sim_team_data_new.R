sim_team_data = function(sna_mat_type,
                             team_size,
                             n_codes,
                             dom_num,
                             n_lines,
                             team_name,
                             use_real,
                             real_probs,
                             codes_real,
                             delta,
                             soc_delta,
                             your_probs){

  # browser()
  team_code_mats = generate_code_matrix_interval(n_codes = n_codes,
                                                 team_size = team_size,
                                                 delta = delta,
                                                 your_probs = your_probs,
                                                 use_real = use_real,
                                                 real_probs = real_probs,
                                                 codes_real = codes_real

                                                 )

  team_sna_mats = get_sequences_wrapper(type = sna_mat_type,team_size = team_size,matrix_num = 1,seq_length = n_lines,soc_delta = soc_delta)
  counts = team_sna_mats$counts
  # print(counts)
  unit_lines = map2(.x = team_code_mats,.y = flatten(counts),.f = deflator,codes = letters[1:n_codes])
  # browser()
  zeros = map(unit_lines,pluck,"zero_count")
  zeros = sum(unlist(zeros))
  unit_lines = map(unit_lines,pluck,"unit_lines")
  if(zeros > 0){
    print(paste(zeros," ","units with no coded lines in this run"))
  }
  # browser()

  for(i in seq.int(length(unit_lines))){
    unit_lines[[i]][,"unit"] = rep(i,nrow(unit_lines[[i]]))
  }

  for(i in seq.int(length(unit_lines))){
    unit_order = sample(1:nrow(unit_lines[[i]]),replace = FALSE)
    unit_lines[[i]] = unit_lines[[i]][unit_order,]
  }

  all_lines = do.call(rbind,unit_lines)
  all_lines = all_lines[order(order(unlist(team_sna_mats$sequences))),]
  all_lines$team = rep(team_name,nrow(all_lines))
  all_lines$convo_fill = rep(0,nrow(all_lines))
  all_lines$unique_unit = paste0(all_lines$unit,".",all_lines$team)

  return(list(dat_ = all_lines,code_mats = team_code_mats, sna_mats = team_sna_mats))
}
