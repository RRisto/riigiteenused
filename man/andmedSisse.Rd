\name{andmedSisse}
\alias{andmedSisse}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Riigiteenuste andmete sisselugemine}
\description{
laeb riigiteenuste andmed veebist JSON formaadist (https://www.riigiteenused.ee/api/et/all) sisse laias formaadis.
}
\usage{
andmedSisse(url)
}
\arguments{
  \item{url}{
"https://www.riigiteenused.ee/api/et/all"
}
\value{
tagastab dataframe'i
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Risto Hinno, ristohinno.com
}
\note{
rohkem infot riigiteenuste kaardistmaise kohta on siin: https://github.com/MKM-ITAO/riigiteenused. Annab üldjuhul hoiatuse "incomplete final line found", mis ei mojuta andmete sisselugemist.
}

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
#andmedLai=andmedSisse("https://www.riigiteenused.ee/api/et/all")

## The function is currently defined as
function ()
{
    library(jsonlite)
    andmed = fromJSON(readLines("https://www.riigiteenused.ee/api/et/all"),
        flatten = T)
    andmed = andmed["teenuste_kanalid_ja_moodikud" != "list()"]
    andmedMoodik <- rbindlist(lapply(andmed[["teenuste_kanalid_ja_moodikud"]],
        function(x) {
            as.list(unlist(x))
        }), fill = TRUE)
    andmed
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~riigiteenused }
\keyword{ ~JSON }% __ONLY ONE__ keyword per line
