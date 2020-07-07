get_ind_vectors = function(matrix,codes){
  
  dat = matrix[,codes]
  upper_tri_indices <- combn(ncol(dat), 2)
  adjs <- apply(dat, 1, function(row) {
    apply(upper_tri_indices, 2, function(o) {
      row[o[1]] * row[o[2]]
    })
  })
  return(fun_sphere_norm(t(adjs)))
}
