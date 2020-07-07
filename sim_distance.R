# distance simulation

sim_distance = function(n_codes,runs,use_real,code_probs,codes_real){
  distances = list()
  for(i in 1:runs){
    m1 = generate_code_matrix(n_codes = n_codes,use_real = use_real,code_probs = code_probs,codes_real = codes_real)
    m2 = generate_code_matrix(n_codes = n_codes,use_real = use_real,code_probs = code_probs,codes_real = codes_real)
    m1_norm = norm(m1,"f")
    m2_norm = norm(m2,"f")
    distance = abs(m1_norm - m2_norm)
    distances[[i]] = distance
  }
  return(list(histo = hist(unlist(distances),plot = TRUE),quant = quantile(unlist(distances))))
}

dist_distribution_sim = function(code_range,runs,use_real,code_probs,codes_real){
  results = lapply(code_range,sim_distance,use_real = use_real,code_probs = code_probs, codes_real = codes_real,runs = runs)
  names(results) = code_range
  return(results)
}


