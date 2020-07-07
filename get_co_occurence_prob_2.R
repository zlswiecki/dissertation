# function to get code co-occurence *probability* matrices. Produces 1 code by code matric per person. For codei and code j, cellij is the number
# of lines with code i & code j divided by the number of lines with code i

get_cooccurence_prob_2 = function(data, splitCol, codes, units) {

  co_names_mat = combn(codes,2)

  co_names = apply(co_names_mat,2,paste,collapse = " & ")

  split_result = split(x = data, f = data[, splitCol])

  team_adj_list = list()
  team_mat_list = list()

  for (i in 1:length(split_result)) {

    co_adj_list = list()
    co_mat_list = list()

    mat = split_result[[i]]

    mat_units = split(mat,mat[,units])

    for(j in 1:length(mat_units)){

      co_mat = matrix(rep(0,length(codes)^2),length(codes),length(codes))

      for(p in 1:nrow(mat_units[[j]])){

        co_mat = (as.numeric(mat[p,codes]) %o% as.numeric(mat[p,codes])) + co_mat

      }

      code_sums = colSums(mat[,codes])
      zeroes = which(code_sums == 0)
      code_sums[zeroes] = 1

      for(k in 1:length(codes)){

        co_mat[k,] = co_mat[k,]/code_sums[k]
      }

      ut = upper.tri(co_mat)
      co_mat_prob_adj = co_mat[ut]

      rownames(co_mat) = codes
      colnames(co_mat) = codes


      co_adj_list[[j]] = co_mat_prob_adj
      co_mat_list[[j]] = co_mat
    }

    names(co_adj_list) = names(mat_units)
    names(co_mat_list) = names(mat_units)
    co_adj_list = bind_cols(co_adj_list)
    co_adj_list = t(co_adj_list)
    colnames(co_adj_list) = co_names
    team_adj_list[[i]] = co_adj_list
    team_mat_list[[i]] = co_mat_list
  }

  names(team_adj_list) = names(split_result)
  names(team_mat_list) = names(split_result)
  team_mat_adj = do.call("rbind",team_adj_list)
  # team_mat_adj = bind_rows(team_adj_list)

  normed_co = fun_sphere_norm(team_mat_adj)
  rownames(normed_co) = rownames(team_mat_adj)
  colnames(normed_co) = colnames(team_mat_adj)

  # return(list(raw = co_mat_adj, norm = normed_co))

  return(list(team_list = team_adj_list, normed = normed_co, team_matrices = team_mat_list))
}
