### ENA wrapper for TADMUS data. Option for a projection set. Option for removing scenario personnel----------------

make_ena_tadmus = function(df,
                           codeCols,
                           unitCols,
                           convoCols,
                           metaCols,
                           windowType,
                           windowSizeFor,
                           windowSizeBack,
                           meansRotate,
                           projSet,
                           removeOther) {

  accum = ena.accumulate.data(
    units = df[, unitCols],
    conversation = df[, convoCols],
    metadata = df[, metaCols],
    codes = df[, codeCols],
    window = windowType,
    window.size.back = windowSizeBack,
    window.size.forward = windowSizeFor)

  if(removeOther == TRUE) {

    other = which(accum$meta.data$MacroRole == "Other")
    accum$meta.data = accum$meta.data[-other,]
    for (i in seq(ncol(accum$meta.data))) {
      data.table::set(accum$meta.data, j = i,
          value = rENA:::as.ena.metadata(accum$meta.data[[i]]))
    }

    accum$connection.counts = accum$connection.counts[-other,]
    for (i in seq(ncol(accum$meta.data))) {
      data.table::set(accum$connection.counts, j = i,
          value = rENA:::as.ena.metadata(accum$connection.counts[[i]]))
    }
    for (i in ((ncol(accum$meta.data)+1):ncol(accum$connection.counts))) {
      data.table::set(accum$connection.counts, j = i,
          value = rENA:::as.ena.co.occurrence(accum$connection.counts[[i]]))
    }


    accum$model$unit.labels = accum$model$unit.labels[-other]
  }else{

    accum = accum
  }

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
      rotation.params = list(accum$meta.data$COND == "Experimental", accum$meta.data$COND == "Control"))

  }else{

    set = ena.make.set(
      enadata = accum,
      dimensions = choose(length(codeCols),2))
  }

  return(set)
}
