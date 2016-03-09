\name{Extantify}
\alias{Extantify}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
Finds all extant marine fish species in non-landlocked countries. Additionally, it subsets the output based on selected columns (if no columns are selected i.e. Extantify(), the function defaults to all extant marine fish species).
}
\usage{
Extantify(subsetcolumns,factors)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{subsetcolumns}{
Choose from the following columns for subsetting: CountryISO3, Category, Modified.Year, Class, Order, Family, or Genus. The input can be a list with multiple columns selected
}
\item{factors}{
Choose factor levels that you would like to obtain a subset of. Multiple factor levels can be chosen with c(factor1,factor2,...). This variable must be the same length as the subsetcolumns list variable defined above.
}
}

\examples{
#Useful examples to demonstrate how this function works:

Extantify()
#Calculates global extant marine fish species

Extantify("CountryISO3","USA")
#Calculates extant fish species in the United States of America

Extantify("CountryISO3",list(c("USA","RUS")))
#Calculates extant fish species in the United States of America and Russia

Extantify(list("Category","Class"),list("CR","CHONDRICHTHYES"))
#Calculates extant fish species globally that are critically endangered (CR) and belong to the class Chondrichthyes.

Extantify(list("Category","Class"),list(c("CR","EN","VU"),"CHONDRICHTHYES"))
#Calculates extant fish species globally that are categorized as critically endangered (CR), endangered (EN), or vulnerable (VU) and belong to the class Chondrichthyes.

}
