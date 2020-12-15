makeConstraintMatrix = function(model, categoricalLatentVariables){

  # first create pattern matrix to build constraints
  nClasses = 2^length(categoricalLatentVariables)
  nAttributes = length(categoricalLatentVariables)
  attributeProfiles = matrix(data = 0, nrow = nClasses, ncol = nAttributes)
  colnames(attributeProfiles) = categoricalLatentVariables
  for (att in 1:nClasses){
    attributeProfiles[att, ] = dec2bin(decimal_number = (att-1), nattributes = nAttributes, basevector = rep(2,nAttributes))
  }

  modelSections = paste0(stats::terms(model))
  rhsFormula = paste0(modelSections[1], modelSections[3])
  masterX = stats::model.matrix(stats::formula(rhsFormula), data = as.data.frame(attributeProfiles))

  #creates a constrant matrix used for rejection sampling

  #first subset masterX with only applicable columns
  LP = masterX
  LP = LP[!duplicated(x = LP, MARGIN = 1),]
  LP_vals = apply(X = LP, MARGIN = 1, FUN = sum)
  LP_val_order = unique(LP_vals)[order(-unique(LP_vals))]

  #construct C matrix
  C = NULL
  Ccompare = NULL
  LPval = 1
  for (LPval in 1:(length(LP_val_order)-1)){

    current_num = LP_val_order[LPval]
    current_list = which(LP_vals == current_num)

    #compare with values one step away
    compare_num = LP_val_order[LPval+1]
    compare_list = which(LP_vals == compare_num)

    cur=1
    for (cur in 1:length(current_list)){
      current = current_list[cur]

      #loop through comparison and add constraints to C matrix
      comp=1
      for (comp in 1:length(compare_list)){
        compare = compare_list[comp]
        C = rbind(C, LP[current,] - LP[compare,])
        Ccompare = rbind(Ccompare, c(current, compare))
      }
    }

  }
  # remove redundant rows from constraint matrix (if any)
  C = C[!duplicated(x = C, MARGIN=1), , drop=FALSE]
  # rownames(C) = apply(X = Ccompare, MARGIN = 1, FUN = function(x) paste(x[1], x[2]))

  return(C)
}
