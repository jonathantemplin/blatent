parsePriors = function(priorText){
  # separate based on <-
  rhs = strsplit(x = priorText, split = " <- ")[[1]][2]
  p = eval(parse(text = rhs))
  # separate based on parentheses
  return(p)
}
