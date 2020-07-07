#### function for generating code probability matrices for simulation

### new code prob matrix generation function

generate_code_matrix = function(n_codes,dom_num = 0,use_real,real_probs = real_probs,codes_real = codes_real, your_probs = your_probs){

  codes = letters[1:n_codes]
  mat = matrix(rep(NA,n_codes^2),nrow = n_codes,ncol = n_codes)
  # print(your_probs)

  if(dom_num >0){

    dom_list = runif(dom_num,.5,1)
    diag_vals_remain = runif(n_codes - dom_num,min = 0,max = 0.2)

  }else if (use_real){
    # browser()
    diag_vals = list()
    real_probs = map(real_probs,data.frame)
    real_probs = map(real_probs,function(x) { x[, codes_real]})
    real_probs = bind_rows(real_probs)
    for(i in seq.int(n_codes)){
      diag_vals[[i]] = samp_from_real(real_probs = real_probs[,i])
    }
  }else if(is.null(your_probs) == FALSE){
    if(length(your_probs) != n_codes){
      stop("length of provided code probability vector does not match number of codes requested")
    }
    diag_vals = your_probs
  }else{
    print("selecting random code probs")
    diag_vals = runif(n_codes,min = 0,max = 1)
  }
  diag_vals = unlist(diag_vals)
  combo = combn(n_codes,2)
  upper.list = list()
  # print(diag_vals)

  for(i in seq.int(ncol(combo))){
    pair = diag_vals[combo[,i]]
    #min_val = pair[1] + pair[2] - 1
    #if(min_val < 0){min_val = 0}
    upper.val = prod(pair)
    # upper.val = runif(n = 1,min = prod(pair) ,max = min(pair))
    upper.list[[i]] = upper.val
  }

  # browser()

  mat[lower.tri(mat)] = unlist(upper.list)
  mat = t(mat)
  mat[diag(TRUE,n_codes)] = diag_vals
  mat[lower.tri(mat)] = t(mat)[lower.tri(mat)]

  # mat = round(mat,1)

  return(mat)
}
