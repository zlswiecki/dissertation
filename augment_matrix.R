# take in a diaganal, generate the full matrix

augment_matrix_old = function(og_mat,type,sim_val = .02, diff_val = .3){

  mat_diag = diag(og_mat)

  if (type == "test"){
    new_diag = mat_diag
  }
  else if (type == "similar"){
    new_diag = jitter(mat_diag,amount = sim_val)
  }
  else if (type == "different"){
    new_diag = jitter(mat_diag,amount = diff_val)
  }
  else{
    stop(print("invalid type: try one of similar,different"))
  }

  ## also need to check if all zeros

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
  new_mat = matrix(rep(NA,length(new_diag)^2),nrow = length(new_diag),ncol = length(new_diag))
  new_mat[lower.tri(new_mat)] = unlist(upper.list)
  new_mat = t(new_mat)
  new_mat[diag(TRUE,length(new_diag))] = new_diag
  return(new_mat)
}
