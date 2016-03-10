#' Extantify
#'
#' subsetcolumns and extantifies the IUCN long data table based on desired output
#'
#' @param subsetcolumns (a vector of columns to subset based off of)
#'
#' @param factors (Indicate what factors to select for the corresponding subset columns. Note this should be a vector or list of same length as subsetcolumns)
#'
#' @return extant (Extant fish species for a given subset)
#'
#' @author Niko Hartline, Fish-ecol.github.io
#'
#'
#'
Extantify=function(subsetcolumns=NA,factors=NA, dir="a"){
  
  if(length(subsetcolumns)!=length(factors)){stop("Error: subsetcolumns input is not the same length as the factors input. These should match in ")}
  
  if (dir=="a"){
  #Load in original data set- note this will take a couple seconds
  LongData=read.csv("./data/Long Format Collected Fish Information IUCN")
  
  #Load in land locked data set (ISO3 codes)
  Landlocked=read.csv("./data/Landlocked.csv")
  }
  
  if (dir=="b"){
    #Load in original data set- note this will take a couple seconds
    LongData=read.csv("../fish.ecol/data/Long Format Collected Fish Information IUCN")
    
    #Load in land locked data set (ISO3 codes)
    Landlocked=read.csv("../fish.ecol/data/Landlocked.csv")
  }
  
  #Exclude landlocked countries
  OceanNationsOnly=LongData[!LongData$CountryISO3%in%Landlocked$Landlocked_Countries,]
  
  #Find the extant classifications: 1- Introduced 2- Native 6- Reintroduced
  extantIDX=grepl("1|2|6",as.character(OceanNationsOnly$Species_Presence))
  
  extant=OceanNationsOnly[extantIDX,-c(1,5,6)]
  
  if(is.na(subsetcolumns[1])){return(extant)}else{
    for(i in 1:length(subsetcolumns)){
      IDXl=grepl(paste(factors[[i]],collapse="|"),extant[[subsetcolumns[[i]]]])
      extant=extant[IDXl,]
    }
    return(extant)
  }
}


