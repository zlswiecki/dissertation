### function for calculating normed adjacency vectors from simulated data

get_normed_adj_sim_old = function(sim_data_list,
                              units,
                              convo,
                              convo.intra,
                              codes,
                              meta,
                              window_size){


  sim_data = do.call("rbind",sim_data_list)

  dat = sim_data

  ind = ena.accumulate.data(units = dat[,units],
                            conversation = dat[,convo],
                            codes = dat[,codes],
                            metadata = dat[,meta],
                            window = "MovingStanzaWindow",
                            window.size.back = 1)

  intra = ena.accumulate.data(units = dat[,units],
                              conversation = dat[,convo.intra],
                              codes = dat[,codes],
                              metadata = dat[,meta],
                              window = "MovingStanzaWindow",
                              window.size.back = window_size)

  inter = ena.accumulate.data(units = dat[,units],
                              conversation = dat[,convo],
                              codes = dat[,codes],
                              metadata = dat[,meta],
                              window = "MovingStanzaWindow",
                              window.size.back = window_size)

  # team = "team"
  # unit = "unit"

  # browser()
  ind_raw = ind$connection.counts
  # ind_raw = ind_raw[order(team,unit)]
  ind_raw = ind_raw[order(ind_raw$team, ind_raw$unit), ]
  ind_raw = as.matrix(ind_raw)

  intra_raw = intra$connection.counts
  # intra_raw = intra_raw[order(team,unit)]
  intra_raw = intra_raw[order(intra_raw$team, intra_raw$unit), ]
  intra_raw = as.matrix(intra_raw)

  inter_raw = inter$connection.counts
  # inter_raw = inter_raw[order(team,unit)]
  inter_raw = inter_raw[order(inter_raw$team, inter_raw$unit), ]
  inter_raw = as.matrix(inter_raw)

  ind_norm = rENA:::fun_sphere_norm(ind_raw)
  intra_norm = rENA:::fun_sphere_norm(intra_raw)
  inter_norm = rENA:::fun_sphere_norm(inter_raw)


  return(list(ind = ind_norm, intra = intra_norm, inter = inter_norm,meta = ind$meta.data))

}



