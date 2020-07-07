### function for unwrapping diag and upper tri of matrix into a vector

unwrap_mat = function(mat){
  combos = combn(1:ncol(mat),2)
  upper_list = list()
  for (i in seq.int(ncol(combos))){

    upper_list[[i]] = mat[combos[,i][1],combos[,i][2]]
  }
  upper_list = unlist(upper_list)
  vec = c(diag(mat),upper_list)
  return(vec)
}
