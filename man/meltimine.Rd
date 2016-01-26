\name{meltimine}
\alias{meltimine}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Laiast pikaks formaadiks}
\description{
Funktsiooni korrastaja() alamfunktsioon: Votab funktsiooniga andmedSisse() sisse loetud andmed, mida on tootlenud funktsioon andmedPikaks() ning teeb laiast formaadist pika formaadi.}
\usage{
meltimine(kanal, data)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{kanal}{
Kanal peab olema yks neist "Veebileht / portaal.", "E-iseteenindus.""Eesti.ee.","Nutirakendus.""Digitelevisioon.","E-post.","Tekstisõnum.","Telefon.", "Faks.","Post.", "Letiteenus büroos.", "Kliendi juures."
}
  \item{data}{
andmed peavad olema laias formaadis}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
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
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
#veeb=meltimine("Veebileht / portaal.", data=andmedLai)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~riigiteenused }
