# a boilerplate R script

MyFunctiom <- function(Arg1, Arg2){
  
  #statment involving Arg1 and 2:
  print(paste("Argument", as.character(Arg1), "is a", class(Arg1)))
  print(paste("Argument", as.character(Arg2), "is a", class(Arg2)))
  return(c(Arg1, Arg2))
}

MyFunctiom(1,2)
MyFunctiom("Riki", "Tiki")
MyFunctiom(1, "tiki")
