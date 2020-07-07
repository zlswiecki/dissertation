augment_matrix = function(mat,delta,test = FALSE){
  if(test){
    new_mat = mat
  }else{
    mat_diag = diag(mat)
    if(delta == 0){
      new_diag = mat_diag
    }else{
      shift = rep(sample(x = c(delta,delta*-1),prob = c(.5,.5),replace = TRUE,size = length(mat_diag)))
      new_diag = mat_diag + shift
      # new_diag = mat_diag
      # for(i in 1:length(new_diag)){
      #   valence = sample(c(0,1),1,prob = c(.5,.5))
      #   if (valence == 0){
      #     new_diag[[i]] = new_diag[[i]] - delta
      #   }else{
      #     new_diag[[i]] = new_diag[[i]] + delta
      #   }
      # }
    }
    if (any(new_diag < 0)){
      new_diag[which(new_diag <0)] = 0
    }
    if (any(new_diag > 1)){
      new_diag[which(new_diag >1)] = 1
    }
    # browser()
    combo = combn(length(new_diag),2)
    upper.list = list()
    for(i in seq.int(ncol(combo))){
      pair = new_diag[combo[,i]]
      upper.val = prod(pair)
      upper.list[[i]] = upper.val
    }
    new_mat = matrix(rep(0,length(new_diag)^2),nrow = length(new_diag),ncol = length(new_diag))
    new_mat[lower.tri(new_mat)] = unlist(upper.list)
    new_mat = t(new_mat)
    new_mat[diag(TRUE,length(new_diag))] = new_diag
  }
  return(new_mat)
}


augment_matrix_jit = function(mat,delta,test = FALSE){
  if(test){
    new_mat = mat
  }else{
    mat_diag = diag(mat)
    if(delta == 0){
      new_diag = mat_diag
    }else{
      new_diag = jitter(mat_diag,amount = delta)
      # new_diag = mat_diag
      # for(i in 1:length(new_diag)){
      #   valence = sample(c(0,1),1,prob = c(.5,.5))
      #   if (valence == 0){
      #     new_diag[[i]] = new_diag[[i]] - delta
      #   }else{
      #     new_diag[[i]] = new_diag[[i]] + delta
      #   }
      # }
    }
    if (any(new_diag < 0)){
      new_diag[which(new_diag <0)] = 0
    }
    if (any(new_diag > 1)){
      new_diag[which(new_diag >1)] = 1
    }
    # browser()
    combo = combn(length(new_diag),2)
    upper.list = list()
    for(i in seq.int(ncol(combo))){
      pair = new_diag[combo[,i]]
      upper.val = prod(pair)
      upper.list[[i]] = upper.val
    }
    new_mat = matrix(rep(0,length(new_diag)^2),nrow = length(new_diag),ncol = length(new_diag))
    new_mat[lower.tri(new_mat)] = unlist(upper.list)
    new_mat = t(new_mat)
    new_mat[diag(TRUE,length(new_diag))] = new_diag
  }
  return(new_mat)
}


augment_matrix_test = function(mat,delta,test = FALSE){
  if(test){
    new_mat = mat
  }else{
    mat_diag = diag(mat)
    if(delta == 0){
      new_diag = mat_diag
    }else{
      new_diag = jitter(mat_diag,amount = delta)
    }
    if (any(new_diag < 0)){
      new_diag[which(new_diag <0)] = 0
    }
    if (any(new_diag > 1)){
      new_diag[which(new_diag >1)] = 1
    }
    # browser()
    combo = combn(length(new_diag),2)
    upper.list = list()
    for(i in seq.int(ncol(combo))){
      pair = new_diag[combo[,i]]
      upper.val = prod(pair)
      upper.list[[i]] = upper.val
    }
    new_mat = matrix(rep(0,length(new_diag)^2),nrow = length(new_diag),ncol = length(new_diag))
    new_mat[lower.tri(new_mat)] = unlist(upper.list)
    new_mat = t(new_mat)
    new_mat[diag(TRUE,length(new_diag))] = new_diag
  }
  return(new_mat)
}

generate_code_matrix_interval = function(n_codes,
                                         team_size,
                                         delta,
                                         use_real,
                                         real_probs,
                                         codes_real,
                                         your_probs
                                         ){
  matrix_list = list()
  int_mat = generate_code_matrix(n_codes = n_codes,use_real = use_real,real_probs = real_probs,codes_real = codes_real, your_probs = your_probs)
  int_mat[lower.tri(int_mat)] = 0
  matrix_list[[1]] = int_mat
  # print(int_mat)
  # browser()
  for (i in 2:team_size ){
    new_mat = augment_matrix(int_mat,delta)
    matrix_list[[i]] = new_mat
  }
  for (i in seq.int(length(matrix_list))){
    colnames(matrix_list[[i]]) = letters[1:n_codes]
    rownames(matrix_list[[i]]) = letters[1:n_codes]
  }
  # print(matrix_list)
  return(matrix_list)
}



generate_code_matrix_interval_old = function(n_codes,
                                         team_size,
                                         delta,
                                         use_real = TRUE,
                                         code_probs = nav_code_probs,
                                         codes_real = codes_nav,
                                         norm_type){
  matrix_list = list()
  int_mat = generate_code_matrix(n_codes = n_codes,use_real = use_real,code_probs = code_probs,codes_real = codes_real)
  int_mat[lower.tri(int_mat)] = 0
  matrix_list[[1]] = int_mat
  # print(int_mat)
  # browser()
  while (length(matrix_list) < team_size){
    temp_list = matrix_list
    candidate_mat = augment_matrix(int_mat,delta) #FIX THIS LATER AND COMMENT OUT NEXT 2 LINES
    # candidate_mat = generate_code_matrix(n_codes = n_codes,use_real = TRUE,code_probs = nav_code_probs,codes_real = codes_nav)
    # candidate_mat[lower.tri(candidate_mat)] = 0
    candidate_mat = list(candidate_mat)
    temp_list = append(temp_list,candidate_mat)
    mean_dist = get_mat_dist(temp_list,norm_type)
    if(mean_dist >= interval[1] & mean_dist <= interval[2]){
      matrix_list = append(matrix_list,candidate_mat)
    }
  }
  for (i in seq.int(length(matrix_list))){
    colnames(matrix_list[[i]]) = letters[1:n_codes]
    rownames(matrix_list[[i]]) = letters[1:n_codes]
  }
  # print(matrix_list)
  return(matrix_list)
}

# test = generate_code_matrix_interval(n_codes = 5,team_size = 3,interval = r,norm_type = "f",fudge = 1/3)
# test

#either need to generate the matrices with jitter around them or make the distance intervals smaller... making distances smaller isn't helping...
