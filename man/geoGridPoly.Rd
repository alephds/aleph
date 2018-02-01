
\name{geoGridPoly}
\alias{geoGridPoly}
\title{Función para superponer un grid sobre un polígono}
\usage{
  geoGridPoly(mapa_orig, npuntos, centroides)
}
\arguments{
  \item{mapa_orig}{SpatialPolygonDataFrame que contiene al polígono sobre el cual se quiere generar el grid.}
  \item{npuntos}{Entero que da el número de puntos a generar.}
  \item{centroides}{SpatialPointsDataFrame que contiene a los centroides que contienen a la variable que se quiere predecir; opcional.}
}
\description{Genera un grid regular de puntos sobre un polígono arbitrario, usualmente usado para predecir con kriging.
  }
\examples{
  data(mapMunicipal.RData); data(mapaMunicipalCentroides.RData)
  geoGridPoly(mapMunicipal, 1000, mapaMunicipalCentroides)
}
\keyword{grid}
\keyword{kriging}

