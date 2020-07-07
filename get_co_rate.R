### calculating percentage of lines with co-occurence
get_co_rate = function(dat_,codes,users){
cor_rate = dat_ %>%
    as_tibble() %>%
    select(users,codes) %>%
    mutate(code.sums = rowSums(select(.,codes))) %>%
    mutate(bin = ifelse(select(.,code.sums)>1,1,0)) %>%
    group_by(users) %>%
    summarise(co.rate = mean(bin))
}

### there is an issue with this function I will have to attend to later. Something to do with passing
### an object to the dplyr functions
