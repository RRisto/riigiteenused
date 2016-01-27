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
andmedSisse() funktsioonist tulnud riigiteenuste laias formaadis dataframe}
}
\details{hoiab laiast formaadist alles jargnevad andmed:
}
\value{
taastab pikas formaadis dataframe'i
}
\references{
laia ja pika formaadi kohat loe siit: http://vita.had.co.nz/papers/tidy-data.pdf}
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

## The function is currently defined as
function (andmedLai)
{
    vars = names(andmedLai) \%in\% c("regulatsioon", "objectId",
        "eluarisyndmus", "kirjeldus", "eeltingimus", "jareltingimus",
        "createdAt", "updatedAt", "keel", "osakondyksus", "omanikunimi",
        "omanikutelefon", "omanikuemail", "konfinfo", "seotuddokumendid",
        "seisund", "muudatustvajav", "aegumisekpv", "funktsioon",
        "veebiaadress")
    andmedLai = andmedLai[, !vars]
    andmedLai2015 = andmedLai[, !grepl("empty.|2011.|2013.|2012.|2014.",
        names(andmedLai))]
    andmedLai2014 = andmedLai[, !grepl("empty.|2011.|2013.|2012.|2015.",
        names(andmedLai))]
    andmedLai2013 = andmedLai[, !grepl("empty.|2011.|2012.|2014.|2015.",
        names(andmedLai))]
    andmedLai2012 = andmedLai[, !grepl("empty.|2011.|2013.|2014.|2015.",
        names(andmedLai))]
    andmedLai2011 = andmedLai[, !grepl("empty.|2014.|2013.|2012.|2015.",
        names(andmedLai))]
    andmedLaiEmpty = andmedLai[, !grepl("2014.|2011.|2013.|2012.|2015.",
        names(andmedLai))]
    puhas2015 = korrastaja(andmedLai2015, "2015.", "2015")
    puhas2014 = korrastaja(andmedLai2014, "2014.", "2014")
    puhas2013 = korrastaja(andmedLai2013, "2013.", "2013")
    puhas2012 = korrastaja(andmedLai2012, "2012.", "2012")
    puhas2011 = korrastaja(andmedLai2011, "2011.", "2011")
    puhasEmpty = korrastaja(andmedLaiEmpty, "empty.", "pole moodetud")
    andmedPikk = rbind(puhas2015, puhas2014, puhas2013, puhas2012,
        puhas2011, puhasEmpty)
    andmedPikk = andmedPikk[!is.na(andmedPikk$link), ]
    andmedPikk
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
