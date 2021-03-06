% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/maxdiffCombinaciones.R
\name{maxdiffCombinaciones}
\alias{maxdiffCombinaciones}
\title{Función para crear combinaciones maxdiff}
\usage{
maxdiffCombinaciones(Atributos, nAtributos = length(Atributos), nBloques,
  AlternativasPorBloque, Iteraciones = 5)
}
\arguments{
\item{Atributos}{vector de tipo caracter que contiene a los atributos}

\item{nBloques}{número de bloques en el maxdiff}

\item{AlternativasPorBloque}{número de atributos que entrarán dentro de cada bloque}

\item{Iteraciones}{número de veces que el modelo buscará la combinación óptima, se recomienda utilizar una iteración para definir el número de bloques y número de atributos por bloque. Una vez definidos estos el número de iteraciones mínimas recomendadas es de 50}
}
\description{
Crea la batería de combinaciones para realizar un maxdiff a partir de un conjunto de atributos
}
\examples{
combinaciones<-maxdiffCombinaciones(Atributos=letters[1:10], nBloques=10, AlternativasPorBloque=3, Iteraciones = 2)
combinaciones
}
\keyword{combinaciones}
\keyword{maxdiff}

