% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/alternative.R
\name{alternative}
\alias{alternative}
\title{alternative}
\usage{
alternative(data, constr, taillech, ancien)
}
\arguments{
\item{data}{un jeu de donnée}

\item{constr}{un tableau de quota de type df avec comme variable "variables", "modalitesé" et "objectifs" et des na dans objectifs pour les quotas modifiables}

\item{taillech}{taille de l'echantillon}

\item{ancien}{anciens quotas}
}
\value{
un df comme constr avec les nouveaux quotas
}
\description{
Permet d'obtenir des quotas alternatif les plus proches des anciens
}
\examples{
variables=c("Species","Species","Species")
modalites=c("setosa","versicolor","virginica")
objectifs=c(25,NA,NA)
constr=data.frame(variables=variables,modalites=modalites,
objectifs=objectifs,stringsAsFactors =FALSE)
taillech=110
ancien=c(25,60)
alternative(iris,constr,taillech,ancien)
}
