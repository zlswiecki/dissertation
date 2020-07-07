#### function for sampling code probs given a distribution of code probs from real data

samp_from_real_old = function(code_probs,codes){

  dat_ = map(code_probs,select,all_of(codes))
  dat_ = unlist(dat_)
  bps = seq(from = 0, to = 1, by = .05)
  dat_hist = hist(dat_,bps,plot = FALSE)
  prob_vec = dat_hist$counts/sum(dat_hist$counts)
  lob = bps[-length(bps)]
  upb = bps[-1]
  prob_ind = sample(1:length(prob_vec),1,prob= prob_vec)
  val=runif(1,min=lob[prob_ind],max=upb[prob_ind])
  return(val)
}


samp_from_real = function(real_probs){ ### one code at a time
  # browser()
  # dat_ = unlist(real_probs)
  dat_ = real_probs
  bps = seq(from = 0, to = 1, by = .05)
  dat_hist = hist(dat_,bps,plot = FALSE)
  prob_vec = dat_hist$counts/sum(dat_hist$counts)
  lob = bps[-length(bps)]
  upb = bps[-1]
  prob_ind = sample(1:length(prob_vec),1,prob= prob_vec)
  val=runif(1,min=lob[prob_ind],max=upb[prob_ind])
  return(val)
}
