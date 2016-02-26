#' iucn_index
#'
#' Calculates a weighted average for IUCN's categories. It can calculate it as an absolute indes, or a relative index (0 to 1).
#'
#' @param data (vector of categories by country. Acceptable formats for categories are: "DD", "LC", "LR/nt", "NT", "VU", "EN", "CR")
#'
#' @param scores (vector of scores to weight each category. Default is c(1,2,3,4,5,6,7))
#'
#' @param type (type of index to use. String "a" uses a weighted average. String "b" uses a relative score (1 = highest possible value, when all species ar CR))
#'
#' @return iucn_index (The IUCN index)
#'
#' @author Juan, Patricia, Juan Carlos, Fish-ecol.github.io
#'
#'
#'

iucn_index=function(data, scores=c(1,2,3,4,5,6,7), type="a"){

  # agregar un if que defina que tipo de dato es

  data=as.factor(data)

  S=summary(data)
  SS=data.frame(category=names(S),count=S)

  all=data.frame(c("DD", "LC", "LR/nt", "NT", "VU", "EN", "CR"))
  colnames(all)=c("category")

  S=left_join(all, SS, by="category")

  S$count[is.na(S$count)]=0

ifelse (type=="a",
        {
          index=(scores[1]*S$count[S$category=="DD"]+
                   scores[2]*S$count[S$category=="LC"]+
                   scores[3]*S$count[S$category=="LR/nt"]+
                   scores[4]*S$count[S$category=="NT"]+
                   scores[5]*S$count[S$category=="VU"]+
                   scores[6]*S$count[S$category=="EN"]+
                   scores[7]*S$count[S$category=="CR"]
          )/sum(S$count)
          },
        {
          index=(scores[1]*S$count[S$category=="DD"]+
                   scores[2]*S$count[S$category=="LC"]+
                   scores[3]*S$count[S$category=="LR/nt"]+
                   scores[4]*S$count[S$category=="NT"]+
                   scores[5]*S$count[S$category=="VU"]+
                   scores[6]*S$count[S$category=="EN"]+
                   scores[7]*S$count[S$category=="CR"]
          )/(sum(S$count)*scores[7])
          }
        )


return(index)

}
