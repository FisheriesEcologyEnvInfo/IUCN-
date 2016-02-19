#' iucn_index
#'
#' Calculates a weighted average for IUCN's categories
#' @param data (vector of categories by country)
#'
#' @return iucn_index (The IUCN index)
#'
#' @author Juan, Patricia, JC
#'
#'
#'

iucn_index=function(data){

  # agregar un if que defina que tipo de dato es

  data=as.factor(data)

  S=summary(data)
  SS=data.frame(category=names(S),count=S)

  all=data.frame(c("DD", "LC", "LR/nt", "NT", "VU", "EN", "CR"))
  colnames(all)=c("category")

  S=left_join(all, SS, by="category")

  S$count[is.na(S$count)]=0


  index=(1*S$count[S$category=="DD"]+
         2*S$count[S$category=="LC"]+
         3*S$count[S$category=="LR/nt"]+
         4*S$count[S$category=="NT"]+
         5*S$count[S$category=="VU"]+
         6*S$count[S$category=="EN"]+
         7*S$count[S$category=="CR"]
  )/sum(S$count)

return(index)

}
