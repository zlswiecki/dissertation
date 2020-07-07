### ENA wrapper for NTX data. Option for a projection set----------------

make_ena_ntx = function(df,
                           codeCols,
                           unitCols,
                           convoCols,
                           metaCols,
                           windowType,
                           windowSizeFor,
                           windowSizeBack,
                           meansRotate,
                           projSet) {
  
  accum = ena.accumulate.data(
    units = df[, unitCols],
    conversation = df[, convoCols],
    metadata = df[, metaCols],
    codes = df[, codeCols],
    window = windowType,
    window.size.back = windowSizeBack,
    window.size.forward = windowSizeFor)
  
  if(is.null(projSet) == FALSE) {
    
    set = ena.make.set(
      enadata = accum,
      dimensions = choose(length(codeCols),2),
      rotation.set = projSet$rotation)
    
  }else if(meansRotate == TRUE) {
    
    set = ena.make.set(
      enadata = accum,
      dimensions = choose(length(codeCols),2), 
      rotation.by = ena.rotate.by.mean,
      rotation.params = list(accum$meta.data$OutcomeBins == 1, accum$meta.data$OutcomeBins == 0))
    
  }else{
    
    set = ena.make.set(
      enadata = accum,
      dimensions = choose(length(codeCols),2)) 
  }
  
  return(set)
}