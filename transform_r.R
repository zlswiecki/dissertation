transform_r = function(r){
  z = atanh(r)
  avg_z = mean(z)
  rz_avg = tanh(avg_z)
  return(rz_avg)
}
