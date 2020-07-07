#### function for generating code probability matrices for simulation

### new code prob matrix generation function

generate_code_matrix_old = function(n_codes,dom_num,use_real,code_probs = NULL,codes_real = NULL){

  codes = letters[1:n_codes]
  mat = matrix(rep(NA,n_codes^2),nrow = n_codes,ncol = n_codes)

  if(dom_num >0){

    dom_list = runif(dom_num,.5,1)
    diag_vals_remain = runif(n_codes - dom_num,min = 0,max = 0.2)

  }else{

    if(use_real == TRUE){
      diag_vals = list()
      for(i in seq.int(n_codes)){
        diag_vals[[i]] = samp_from_real(code_probs = code_probs,codes = codes_real)
        diag_vals = unlist(diag_vals)
      }
    }else{
      diag_vals = runif(n_codes,min = 0,max = 1)
    }
  }

  combo = combn(n_codes,2)
  upper.list = list()

  for(i in seq.int(ncol(combo))){
    pair = diag_vals[combo[,i]]
    #min_val = pair[1] + pair[2] - 1
    #if(min_val < 0){min_val = 0}
    upper.val = prod(pair)
    # upper.val = runif(n = 1,min = prod(pair) ,max = min(pair))
    upper.list[[i]] = upper.val
  }

  mat[lower.tri(mat)] = unlist(upper.list)
  mat = t(mat)
  mat[diag(TRUE,n_codes)] = diag_vals

  # mat = round(mat,1)

  return(mat)
}
