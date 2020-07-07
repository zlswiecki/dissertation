# "matrix" is in the form of the output of get_dist_var_matrices

unit_team_dist= function(matrix,codes,splitCol){
  
  matrix_list = split(matrix,matrix[,splitCol])
  
  for(i in 1:length(matrix_list)){
    
    dist_list = list()
    
    for(j in 1:nrow(matrix_list[[i]])){
      
      dat = matrix_list[[i]][,codes]
      unit_vec = dat[j,]
      team_vecs = dat[-j,]
      team_vec = colMeans(team_vecs)
      distance = dist(rbind(unit_vec,team_vec))[1]
      dist_list[[j]] = distance
      
    }
    
    matrix_list[[i]]$code_vec_dist = unlist(dist_list)

  }
  
  matrix_dist = bind_rows(matrix_list)
  return(matrix_dist)
}

