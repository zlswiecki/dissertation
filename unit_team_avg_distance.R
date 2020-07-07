### variant of unit_team_avg function to find the average distance all teammates rather than the differance for the team average

unit_team_avg_distance= function(matrix,codes,splitCol){
  
  matrix_list = split(matrix,matrix[,splitCol])
  
  for(i in 1:length(matrix_list)){
    
    dist_list = list()
    
    for(j in 1:nrow(matrix_list[[i]])){
      
      dat = matrix_list[[i]][,codes]
      unit_vec = dat[j,]
      team_vecs = dat[-j,]
      # team_vec = colMeans(team_vecs)
      distance = sapply(X = seq.int(nrow(team_vecs)), FUN = function (k) dist(rbind(unit_vec,team_vecs[k,]))[1])
      avg_dist = mean(unlist(distance))
      dist_list[[j]] = avg_dist
      
    }
    
    matrix_list[[i]]$avg_distance_from_tm = unlist(dist_list)
    
  }
  
  matrix_dist = bind_rows(matrix_list)
  return(matrix_dist)
}

