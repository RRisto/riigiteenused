\name{andmedPikaks}
\alias{andmedPikaks}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Andmed pikaks}
\description{
votab riigiteenuste laiad andmed, mis on alla laetud funktsiooniga andmedSisse() ning viib need pikka formaati.}
\usage{
andmedPikaks(andmedLai)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{andmedLai}{
andmedSisse() funktsioonist tulnud riigiteenuste laias formaadis dataframe/data.table'i}
}
\details{

}
\value{

- tagastab pikas formaadis data.table'i/data.frame'i
}
\references{
laia ja pika formaadi kohat loe siit: http://vita.had.co.nz/papers/tidy-data.pdf
data.table'i kohta loe siit: https://cran.r-project.org/web/packages/data.table/vignettes/datatable-intro.pdf}
\author{
ristohinno@gmail.com
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
#andmedPikaks(andmedSisse("https://www.riigiteenused.ee/api/et/all"))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
