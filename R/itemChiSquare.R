itemChiSquare = function(observed, observedLevels, expected, expectedLevels, minObs, minExp){

  observedTable = getTable(vars = observed, varsLevels = observedLevels)

  expectedTable = getTable(vars = expected, varsLevels = expectedLevels)

  if (any(expectedTable <= minExp) | any(observedTable <= minObs)){
    return(NA)
  } else {
    obsMexp = (observedTable-expectedTable)^2/expectedTable
    return(sum(obsMexp))
  }

}

