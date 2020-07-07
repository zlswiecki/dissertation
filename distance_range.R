check_distance = function(distance,interval){
  distance >= interval[1] & distance <= interval[2]
}

dist_range = function(n_codes,use_real,code_probs,codes_real){ #### set up to make arbitrary number of mats and calc avg dist between
  max_dist = n_codes
  range = seq.int(0,max_dist,by = 0.1)
  lob = range[-length(range)]
  upb = range[-1]
  intervals = rbind(lob,upb)
  interval_pos = seq.int(1:length(upb))
  matrix.list <- sapply(interval_pos, function(x) { list() })
  names(matrix.list) <- interval_pos

  while (length(na.omit(interval_pos)) > 0){
    # browser()
    m1 = generate_code_matrix(n_codes = n_codes,use_real = use_real,code_probs = code_probs,codes_real = codes_real)
    m2 = generate_code_matrix(n_codes = n_codes,use_real = use_real,code_probs = code_probs,codes_real = codes_real)
    m1_norm = norm(m1,"f")
    m2_norm = norm(m2,"f")
    distance = abs(m1_norm - m2_norm)
    z = apply(X = intervals,MARGIN = 2,FUN = check_distance,distance = distance)
    # print(z)
    if(any(z)){
      found_int = which(z)
      matrix.list[[found_int]][[length(matrix.list[[found_int]]) +1]] = list(m1,m2)
      if(length(matrix.list[[found_int]])==1){
        interval_pos[found_int] = NA
        print(interval_pos)
      }
    }
  }
  return(matrix.list)
}
