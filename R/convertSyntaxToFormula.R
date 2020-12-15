convertSyntaxToFormula = function(line.simple){

  #check for multiple ~ signs on
  temp = trimws(unlist(strsplit(x = line.simple, split = "~")))

  if (length(temp) > 2){
    stop(paste0("Blatent syntax error: Model statements can only have one ~ per statement. \n Problem with statement: \n", line.simple))
  }

  if (length(temp) < 2){
    stop(paste0("Blatent syntax error: Model statements must have one = per statement. \n Problem with statement: \n", line.simple))
  }

  lhs = temp[1]
  #check variables for being part of index list
  if (length(grep(pattern = "-", x = lhs)) > 0) lhs = getVariableList(variableText = temp[1])

  rhs = temp[2]

  modelFormulas = lapply(X = paste(lhs, "~", rhs), FUN = stats::formula)

  return(modelFormulas)

}
