### function for calculating the averge distances between all metrices in a list

get_mat_dist = function(matrices,norm_type){
  dist_list = list()
  pairs = combn(c(1:length(matrices)),2)
  for(i in 1:ncol(pairs)){  ### could make this an apply function
    m1 = matrices[[pairs[,i][1]]]
    m2 = matrices[[pairs[,i][2]]]
    if(norm_type == "error"){ ### probably don't need this
      m1 = as.vector(m1)
      m2 = as.vector(m2)
      error = sqrt((m1 - m2)^2/length(m1))
      dist_list[[i]] = error

    }else{
      m1_norm = norm(m1,norm_type)
      m2_norm = norm(m2,norm_type)
      dist_list[[i]] = abs(m1_norm - m2_norm)
    }
  }
  return(mean(unlist(dist_list)))
}
