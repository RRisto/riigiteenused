\name{korrastaja}
\alias{korrastaja}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Kanalite andmed pikaks}
\description{
Funktsiooni andmedPikaks() alamfunktsioon. Votab info, mis on funktsiooniga andmedSisse() sisse loetud ning meltimine()/meltimineDT() poolt juba pisut toodeldud ning viib iga kanali andmed pikka formaati. Korraga tootab yhe aasta andmetega}
\usage{
korrastaja(andmed, eemalda, mootmiseAasta)
korrastajaDT(andmed, eemalda, mootmiseAasta)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{andmed}{
laias formaadis riigiteenuste andmed, kus on ainult muutujad, mida on edaspidi vaja}
  \item{eemalda}{
fraasid, mida on vaja pikas formaadis olevate veergude nimedest eemaldada (tyypliselt aastaarv, nt: "2015."}
  \item{mootmiseAasta}{
mootmise aasta, mille kohta statistikat koguti, vajalik, et teada, mis aastanumber mootmise aasta veergu kirjutada}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
- korrastaja() tagastab dataframe'i

- korrastajaDT() tagastab data.table'i
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


}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
