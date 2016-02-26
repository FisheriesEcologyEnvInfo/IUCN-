#' chitester
#'
#' Calculates a contingency table for two factors and reads the results of a chisquared test
#'
#' @return chitester
#'
#' @author Niko Hartline
#'
#'
chitester=function(factor1,factor2){ 
  StatisticalTestTable=read.csv("./Data/Long Format Collected Fish Information IUCN")
  testable=table(StatisticalTestTable[[factor1]],StatisticalTestTable[[factor2]])
  CHItest=suppressWarnings(chisq.test(testable))
  if(CHItest$p.value<.05){print(paste("There is a significant association between",factor1, "and",factor2,"p =",CHItest$p.value,sep=" "))}else{print(paste("There is no significant association between",factor1, "and",factor2,sep=" "))}
}