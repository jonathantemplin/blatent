getVariableList = function(variableText){
#split character by spaces to divide out lists from non-lists
temp = unlist(strsplit(x = variableText, split = " "))

#check if any entry is purely a "-"
dashLoc = which(temp == "-")
while (length(which(temp == "-"))>0){
  preIndex = dashLoc[1]-1
  if (preIndex < 1) if (postIndex > length(temp)) stop(paste0("error with variable list. \n Problem with: \n", variableText))
  pre = temp[preIndex]

  postIndex = dashLoc[1]+1
  if (postIndex > length(temp)) stop(paste0("error with variable list. \n Problem with: \n", variableText))
  post = temp[postIndex]
  temp = c(temp[1:(preIndex-1)], paste0(pre,"-", post), temp[(postIndex+1):length(temp)])
  dashLoc = which(temp == "-")
}

variableArray = temp
variableList = NULL
for (str in 1:length(variableArray)){
  if (grepl(pattern = "-", x = variableArray[str])){

    #ensure only one dash
    if (sapply(regmatches(variableArray[str], gregexpr("-", variableArray[str])), length) > 1 |
        sapply(regmatches(variableArray[str], gregexpr("-", variableArray[str])), length) == 0) {
      stop(paste("only one - allowed in variable list. Problem with",  variableText))
    }

    #split variables by dash
    vars = unlist(strsplit(x = variableArray[str], split = "-"))

    #ensure vars has two section otherwise not a proper array
    if (length(vars) != 2) stop(paste0("error with variable list. \n Problem with: \n", variableText))

    #determine numeric portion of each
    var1Chars = unlist(strsplit(vars,"")[[1]])
    var2Chars = unlist(strsplit(vars,"")[[2]])

    #see if each character is a numeric
    var1Num = grepl("[[:digit:]]", var1Chars)
    var2Num = grepl("[[:digit:]]", var2Chars)

    #check last character to see if numeric
    if (!var1Num[length(var1Num)]){
      stop(paste("variable list must have numeric entries at the end of variables. Problem with", variableText))
    }

    if (!var2Num[length(var2Num)]){
      stop(paste("variable list must have numeric entries at the end of variables. Problem with", variableText))
    }

    #get end numbers and variable names
    var1End = length(var1Num)
    var1Take = NULL

    while(var1Num[var1End]){
      var1Take = c(var1Chars[var1End], var1Take)
      var1End = var1End-1
    }
    var1Number = as.numeric(paste(var1Take, collapse = ""))
    var1Name = paste(var1Chars[1:var1End], collapse = "")

    #get end numbers and variable names
    var2End = length(var2Num)
    var2Take = NULL

    while(var2Num[var2End]){
      var2Take = c(var2Chars[var2End], var2Take)
      var2End = var2End-1
    }
    var2Number = as.numeric(paste(var2Take, collapse = ""))
    var2Name = paste(var2Chars[1:var2End], collapse = "")

    if (var2Number <= var1Number){
      stop(paste("variable list must have larger number on second variable than on first. Problem with", variableText))
    }

    if (var1Name != var2Name){
      stop(paste("variable list must have same name for variables. Problem with", variableText))
    }

    variableList = c(variableList, paste(var1Name, var1Number:var2Number, sep=""))

  } else {
    #list of variables separated by " "
    variableList = c(variableList, variableArray[str])
  }
}


return(variableList)
}
