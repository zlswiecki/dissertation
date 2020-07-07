### function for calculating normed adjacency vectors from simulated data

get_normed_adj_sim = function(sim_data_list,
                              units,
                              convo,
                              convo.intra,
                              codes,
                              meta,
                              window_size,
                              adj_type){

  if(is.data.frame(sim_data_list)){
    dat = sim_data_list
  }else{
    sim_data = do.call("rbind",sim_data_list)
    dat = sim_data
  }


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

  # browser()

  new_order = order(ind$connection.counts$team,ind$connection.counts$unit)
  ind_raw = as.matrix(ind$connection.counts)
  ind_raw = ind_raw[new_order,]

  new_order = order(intra$connection.counts$team,intra$connection.counts$unit)
  intra_raw = as.matrix(intra$connection.counts)
  intra_raw = intra_raw[new_order,]

  new_order = order(inter$connection.counts$team,inter$connection.counts$unit)
  inter_raw = as.matrix(inter$connection.counts)
  inter_raw = inter_raw[new_order,]

  if(adj_type == "normed"){
    ind_res = rENA:::fun_sphere_norm(ind_raw)
    intra_res = rENA:::fun_sphere_norm(intra_raw)
    inter_res = rENA:::fun_sphere_norm(inter_raw)
  }else{
    ind_res = ind_raw
    intra_res = intra_raw
    inter_res = inter_raw
  }
  return(list(ind = ind_res, intra = intra_res, inter = inter_res, meta = ind$meta.data[new_order,],ind_set = ind))
}



