
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
  data(mapas.RData)
  #con centroides
  con_centr <- geoGridPoly(tabasco, 1000, centroides)
  #sin centroides
  sin_centr <- geoGridPoly(tabasco, 1000)
  
  plot(con_centr)
  plot(sin_centr)
}
\keyword{grid}
\keyword{kriging}

