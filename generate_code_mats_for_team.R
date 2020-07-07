### function for generating code probability matrices for an entire team. individuals on teams may be similar or different

### NEED TO UPDATE TO MAKE DOMINANCE A TEAM LEVEL CONSTRUCT
generate_code_mats_for_team = function(type,team_size,n_codes,dom_num,use_real = NULL,code_probs = NULL, codes_real = NULL){
  int_matrix = generate_code_matrix(n_codes,dom_num,use_real = use_real,code_probs = code_probs,codes_real = codes_real)
  int_vec = c(int_matrix[diag(TRUE,n_codes)],int_matrix[upper.tri(int_matrix)])
  sim_vec_list = list()
  sim_vec_list[[1]] = int_vec
  tot = team_size
  count = length(sim_vec_list)

  while(count < tot){

    sim_mat = generate_code_matrix(n_codes,dom_num,use_real = use_real,code_probs = code_probs,codes_real = codes_real)
    sim_vec = c(sim_mat[diag(TRUE,n_codes)],sim_mat[upper.tri(sim_mat)])

    if(type == "similar"){ ### This runs slow for higher numbers of codes. May need to go cell by cell for similar

      if(dist(rbind(sim_vec_list[[1]],sim_vec)) <= (.2 * sqrt(n_codes))){
        sim_vec_list[[count+1]] = sim_vec
        count = count + 1
      }
    }else if (type == "different"){

      if(dist(rbind(sim_vec_list[[1]],sim_vec)) >= (.6 * sqrt(n_codes))){
        sim_vec_list[[count+1]] = sim_vec
        count = count + 1
      }
    } else{(stop(print("invalid type")))}

  }
  sim_mat_list = list()

  for (i in seq.int(length(sim_vec_list))){
    mat = matrix(rep(NA,n_codes^2),nrow = n_codes,ncol = n_codes)
    mat[diag(TRUE,n_codes)] = sim_vec_list[[i]][1:n_codes]
    mat[upper.tri(mat)] = sim_vec_list[[i]][(n_codes + 1):length(sim_vec_list[[i]])]
    colnames(mat) = letters[1:n_codes]
    rownames(mat) = letters[1:n_codes]
    sim_mat_list[[i]] = mat
  }
  return(sim_mat_list)
}
