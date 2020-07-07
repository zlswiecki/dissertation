### new function to calc similarity

generate_code_mats_for_team_new = function(type,
                                           team_size,
                                           n_codes,
                                           dom_num,
                                           use_real = NULL,
                                           code_probs = NULL,
                                           codes_real = NULL){
  count = 0
  while(count < 1){
    int_matrix = generate_code_matrix(n_codes,dom_num,use_real = use_real,code_probs = code_probs,codes_real = codes_real)
    if(round(sum(diag(int_matrix)),1) > .2){
      count = count + 1
    }
  }
  # int_matrix = generate_code_matrix(n_codes,dom_num,use_real = use_real,code_probs = code_probs,codes_real = codes_real)
  # browser()
  mat_list = list()
  mat_list[[1]] = int_matrix
  for (i in c(2:team_size)){
    if (type == "random"){
      mat_list[[i]] = generate_code_matrix(n_codes = n_codes,dom_num = 0,use_real = use_real,code_probs = code_probs,codes_real = codes_real)
    }
    else{
      count2 = 0
      while(count2 < 1){
        aug_mat = augment_matrix(og_mat = int_matrix,type = type)
        if(round(sum(diag(aug_mat)),1) > .2){
          mat_list[[i]] = aug_mat
          count2 = count2 + 1
        }
      }
    }
  }
  for (i in seq.int(length(mat_list))){
    colnames(mat_list[[i]]) = letters[1:n_codes]
    rownames(mat_list[[i]]) = letters[1:n_codes]
  }
  return(mat_list)
}
