# function to get code *probability* matrices. For person i and code j, cell ij is the number of times code j occurs in person i's lines
# diveded by the number of lines for person i. Each row will sum to less than 1. Would sum to 1 if a null code was included

get_code_probs = function(data, splitCol, codes, units) {

  split_result = split(x = data, f = data[, splitCol])

  frequency_list = list()

  for (i in 1:length(split_result)) {

    sub = split_result[[i]] %>%
      select(units,codes)

    sub_units = split(sub, sub[,units])

    unit_rows = map(sub_units,nrow)

    agg = sub %>%
      group_by_at(units) %>%
      summarise_at(codes, sum)

    agg_prob = agg

    agg_prob[,codes] = agg_prob[,codes]/unlist(unit_rows)

    #agg[,codes] = round(agg[,codes],3)

    frequency_list[[i]] = agg_prob
  }

  names(frequency_list) = names(split_result)
  return(frequency_list)
}
