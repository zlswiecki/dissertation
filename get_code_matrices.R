get_code_matrices = function(data, splitCol, codes, units, exclusion) {

  # if(is.null(exclusion) == FALSE){
  #
  #   data = data[!exclusion,]
  #
  # }else{
  #   data = data
  # }

  split_result = split(x = data, f = data[, splitCol])

  frequency_list = list()

  for (i in 1:length(split_result)) {

    sub = split_result[[i]] %>%
      select(units,codes)

    agg = sub %>%
      group_by_at(units) %>%
      summarise_at(codes, sum)

    code_sums = colSums(agg[, codes])

    ## remove zeros to prevent division by zero
    zeroes = which(code_sums == 0)
    code_sums[zeroes] = 1

    agg[, codes] = mapply('/',agg[, codes], code_sums)

    agg[,codes] = round(agg[,codes],3)

    frequency_list[[i]] = agg
  }

  names(frequency_list) = names(split_result)
  return(frequency_list)
}













