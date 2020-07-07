rand_vect_cont <- function(N, M) {
  vec <- rbeta(n = N,shape1 = 1.1,shape2 = 5)
  vec / sum(vec) * M
}

augment_soc_mat = function(team_size,soc_delta){
  # browser()
  int_mat = matrix(data = rep(0,team_size^2),nrow = team_size,ncol = team_size)
  int_mat_d = rep(1,team_size)
  sub_vec = runif(length(int_mat_d),soc_delta/1.1,soc_delta)
  int_mat_d_jit = int_mat_d - sub_vec
  new_mat = int_mat
  for(i in 1:length(sub_vec)){
    new_mat[i,] = new_mat[i,] + sub_vec[i]/(team_size - 1)
  }
  new_mat[diag(TRUE,team_size)] = int_mat_d_jit
  if(all(rowSums(new_mat) == 1)){
    return(new_mat)
  }else{
    stop("all row sums do not equal one")
  }
}


augment_soc_mat_2 = function(team_size,soc_delta){### randomly distributs leftover val
  # browser()
  int_mat = matrix(data = rep(0,team_size^2),nrow = team_size,ncol = team_size)
  int_mat_d = rep(1,team_size)
  sub_vec = runif(length(int_mat_d),soc_delta/1.1,soc_delta)
  int_mat_d_jit = int_mat_d - sub_vec
  new_mat = int_mat
  for(i in 1:length(sub_vec)){
    val = sub_vec[i]
    vec = runif(team_size-1,0,1-val)
    scaled_vec = vec*(val/sum(vec))
    new_mat[i,-i] = scaled_vec
  }
  new_mat[diag(TRUE,team_size)] = int_mat_d_jit
  if(all(round(rowSums(new_mat)) == 1)){
    return(new_mat)
  }else{
    print(rowSums(new_mat))
    print(new_mat)
    stop("all row sums do not equal one")
  }
}

augment_soc_mat_3 = function(team_size,soc_delta){### randomly distributs leftover val unevenly
  int_mat = matrix(data = rep(0,team_size^2),nrow = team_size,ncol = team_size)
  int_mat_d = rep(1,team_size)
  sub_vec = runif(length(int_mat_d),soc_delta/1.05,soc_delta) ### may need to adjust this
  int_mat_d_jit = int_mat_d - sub_vec
  new_mat = int_mat
  browser()
  for(i in 1:length(sub_vec)){
    value_list = list()
    samp_list = c(1:(length(sub_vec) -1))
    for(j in 1:(length(sub_vec)-1)){
      if(length(samp_list) == 0){
        value = 0
        value_list[[j]] = 0
      }else{
        value = sample(samp_list,1)
        value_list[[j]] = value/(length(sub_vec)-1)
        samp_list = samp_list - value
        nonpos = which(samp_list<=0)
        samp_list = samp_list[-nonpos]
      }
    }
    # print(unlist(value_list))
    row_ = unlist(value_list) * sub_vec[i]
    new_ord = sample(c(1:length(row_)),size = length(row_),replace = FALSE)
    row_ = row_[new_ord]
    new_mat[i,-i] = row_
  }
  new_mat[diag(TRUE,team_size)] = int_mat_d_jit
  if(all(round(rowSums(new_mat)) == 1)){
    return(new_mat)
  }else{
    print(rowSums(new_mat))
    print(new_mat)
    stop("all row sums do not equal one")
  }
}

augment_soc_mat_4 = function(team_size,soc_delta){### randomly distributs leftover val unevenly
  int_mat = matrix(data = rep(0,team_size^2),nrow = team_size,ncol = team_size)
  int_mat_d = rep(1,team_size)
  # sub_vec = runif(length(int_mat_d),soc_delta/1.05,soc_delta) ### may need to adjust this
  sub_vec = rep(soc_delta,length(int_mat_d))
  int_mat_d_jit = int_mat_d - sub_vec
  new_mat = int_mat
  # browser()
  for(i in 1:length(int_mat_d_jit)){
    row_list = list()
    leftover = 1 - int_mat_d_jit[[i]]
    row_list[[i]] = rand_vect_cont(N = length(int_mat_d_jit)-1,M = leftover)
    row_ = unlist(row_list)
    new_mat[i,-i] = row_
  }
  new_mat[diag(TRUE,team_size)] = int_mat_d_jit
  if(all(round(rowSums(new_mat)) == 1)){
    return(new_mat)
  }else{
    print(rowSums(new_mat))
    print(new_mat)
    stop("all row sums do not equal one")
  }
}



generate_sna_matrices = function(type,team_size,n,soc_delta){
  mat_list = list()
  if(type == "test"){
    for (i in 1:n){
      set.seed(555)
      vals = rnorm(n = team_size^2,mean = 1/3,sd = 0.01)
      mat = matrix(vals,nrow = team_size,ncol = team_size)
      mat = abs(mat)
      mat <- apply(mat, 1, function(i) i/sum(i))
      mat = t(mat)
      mat_list[[i]] = mat
    }
  }else{
    for(i in 1:n){
      mat_list[[i]] = augment_soc_mat_4(team_size = team_size,soc_delta = soc_delta)
    }
  }
  return(mat_list)
}


