\name{chitester}
\alias{chitester}
\title{
Chi Squared Testing
}
\description{
This function creates and tests a contingency table for two factors with a chi-square test.
}
\usage{
chitester(factor1,factor2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{factor1}{
The first factor from the Statistical Test Table for generation and testing of a contigency table. Inputs include "Category", "Species_Presence", "Population.Trend", "Modified.Year", "Class", and "Order"}
  \item{factor2}{
The second factor from the Statistical Test Table for generation and testing of a contigency table. Inputs are the same as above
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{

}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
chitester("Class","Category")
}
