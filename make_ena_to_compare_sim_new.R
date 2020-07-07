### Generate ind, intra, and inter ena models for simulated datasets

make_ena_to_compare_sim_new = function(resamples,
                                      codeNames,
                                      units,
                                      convo.ind,
                                      convo.intra,
                                      convo.inter,
                                      window.size.inter,
                                      window.size.intra,
                                      meta,
                                      proj_set = NULL
){

  ind.ena = list()
  intra.ena = list()
  inter.ena = list()

  for (i in 1:length(resamples)){

    dat = resamples[[i]]


    ind = make_ena_tadmus(df = dat,
                          codeCols = codeNames,
                          unitCols = units,
                          convoCols = convo.ind,
                          metaCols = meta,
                          # windowType = "Conversation",
                          windowType = "MovingStanzaWindow",
                          windowSizeFor = 0,
                          windowSizeBack = 1,
                          meansRotate = FALSE,
                          projSet = proj_set,
                          removeOther = FALSE)

    intra = make_ena_tadmus(df = dat,
                            codeCols = codeNames,
                            unitCols = units,
                            convoCols = convo.intra,
                            metaCols = meta,
                            windowType = "MovingStanzaWindow",
                            windowSizeFor = 0,
                            windowSizeBack = window.size.intra,
                            meansRotate = FALSE,
                            projSet = proj_set,
                            removeOther = FALSE)

    inter = make_ena_tadmus(df = dat,
                            codeCols = codeNames,
                            unitCols = units,
                            convoCols = convo.inter,
                            metaCols = meta,
                            windowType = "MovingStanzaWindow",
                            windowSizeFor = 0,
                            windowSizeBack = window.size.inter,
                            meansRotate = FALSE,
                            projSet = proj_set,
                            removeOther = FALSE)


    ind.ena[[i]] = ind
    intra.ena[[i]] = intra
    inter.ena[[i]] = inter

  }
  return(list(ind.ena = ind.ena, intra.ena = intra.ena, inter.ena = inter.ena))
}

