generateDataFromModel = function(variables, data){

  newdata = vapply(
    X = variables,
    FUN = createData,
    FUN.VALUE = numeric(nrow(data)),
    data = data
  )

  return(newdata)
}

