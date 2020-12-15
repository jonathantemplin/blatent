getTable = function(vars, varsLevels){

  vars = data.frame(vars)
  for (i in 1:ncol(vars)){
    vars[,i] = factor(vars[,i], levels = varsLevels[[i]])
  }

  return(data.frame(table(vars))$Freq)

}
