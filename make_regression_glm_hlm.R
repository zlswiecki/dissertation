
make_regressions_glm_hlm = function(ena.list, formula, reg.type) {

  reg.list = list()
  for (i in 1:length(ena.list)) {
    set = ena.list[[i]]
    dat = set$points

    if(reg.type == "glm"){

      reg = glm(formula = formula, data = dat,family = binomial(link = "logit"))

    }else if(reg.type == "hlm"){

      reg = lmer(formula = formula, data = dat)

    }else if(reg.type == "glmer"){

      reg = glmer(formula = formula,data = dat,family = "binomial")
    }else{

      reg = lm(formula = formula, data = dat)
    }

    reg.list[[i]] = reg
  }
  return(reg.list)
}
