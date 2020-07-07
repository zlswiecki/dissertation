### ENA wrapper for general data. Option for a projection set----------------

make_ena = function(df,
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
    
  }else{
    
    set = ena.make.set(
      enadata = accum,
      dimensions = choose(length(codeCols),2)) 
  }
  
  return(set)
}