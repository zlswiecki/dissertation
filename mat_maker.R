mat_maker = function(n_codes,vals){

  mat = matrix(vals,n_codes,n_codes)
  rownames(mat) = letters[1:n_codes]
  colnames(mat) = letters[1:n_codes]
  mat[lower.tri(mat)] = NA
  mat = mat/10
  return(mat)
}
