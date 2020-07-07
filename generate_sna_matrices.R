generate_sna_matrices_old = function(type,team_size,n){

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
  }else if(type == "even"){
    vals = list()
    count = 0

    while(count < n){

      vals = rnorm(n = team_size^2,mean = 1/3,sd = 0.01)
      mat = matrix(vals,nrow = team_size,ncol = team_size)
      mat = abs(mat)
      mat <- apply(mat, 1, function(i) i/sum(i))
      mat = t(mat)
      if (all(mat <= 0.8)){
        mat_list[[count + 1]] = mat
        count = count + 1
      } else{
        NULL
      }
      mat_list = compact(mat_list)
    }
  }else if (type == "iso"){
    vals = list()
    vals_diag = list()
    count = 0
    while(count < n){ ### set to

      vals_diag = runif(n = team_size,min = .8,max = 1)
      vals = rnorm(n = team_size^2,mean = 0.1,sd = 0.05)
      mat = matrix(vals,nrow = team_size,ncol = team_size)
      diag(mat) = vals_diag
      mat = abs(mat)
      mat = apply(mat, 1, function(i) i/sum(i))
      mat = t(mat)
      if (all(mat <= 1)){
        mat_list[[count + 1]] = mat
        count = count + 1
      } else{
        NULL
      }
      mat_list = compact(mat_list)
    }

  }else if (type == "clique"){ ### update for more flexible cliques (e.g., cliques of 3)

    count = 0

    while(count < n){
      # vals = list()
      # cliq_mems = list()
      # cliq_list = list()

      if(team_size >= 4){

        cliq_mems = sort(sample(seq.int(team_size),size = 4,replace = FALSE))
        cliq_list =  rnorm(n = length(cliq_mems),mean = 1,sd = 0.05)
        vals = rnorm(n = team_size^2,mean = 0.1,sd = 0.05)
        mat = matrix(vals,nrow = team_size,ncol = team_size)
        mat[cliq_mems[1],cliq_mems[2]] = cliq_list[1]
        mat[cliq_mems[2],cliq_mems[1]] = cliq_list[2]
        mat[cliq_mems[3],cliq_mems[4]] = cliq_list[3]
        mat[cliq_mems[4],cliq_mems[3]] = cliq_list[4]


      } else{

        cliq_mems = sort(sample(seq.int(team_size),size = 2,replace = FALSE))
        cliq_list =  rnorm(n = length(cliq_mems),mean = 1,sd = 0.05)
        vals = rnorm(n = team_size^2,mean = 0.1,sd = 0.05)
        mat = matrix(vals,nrow = team_size,ncol = team_size)
        mat[cliq_mems[1],cliq_mems[2]] = cliq_list[1]
        mat[cliq_mems[2],cliq_mems[1]] = cliq_list[2]

      }

      mat = abs(mat)
      mat = apply(mat, 1, function(i) i/sum(i)) ### normalization makes the non clique vals pretty high ## think about how to fix this
      mat = t(mat)
      # print(c("clique: ",cliq_mems))
      if (all(mat <= 0.8)){
        mat_list[[count + 1]] = mat
        count = count + 1
      } else{
        NULL
      }
      mat_list = compact(mat_list)
    }
  }else{
    print("invalid matrix type")
  }
  # print(mat_list)
  return(mat_list)
}




