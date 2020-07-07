# Generate bootstrap samples from data with team structure ---------------------

generate_boot_samples = function(data, teamCol, runs, seed) {

  set.seed(seed)
  dat = data
  team.ids = unique(dat[, teamCol])
  resampled.data.list = list()

  for (run in c(1:runs)) {

    team.sample = sample(x = team.ids,size = length(team.ids),replace = T)
    dat.list = list()
    count = 1

    for (team in team.sample) {

      sample.rows = which(dat[, teamCol] == team)
      sample.data = dat[sample.rows,]
      sample.data$sample = count
      dat.list[[count]] = sample.data
      count = count + 1

    }

    resampled.data <- do.call("rbind", dat.list)
    resampled.data.list[[run]] = resampled.data

  }

  return(resampled.data = resampled.data.list)
}

