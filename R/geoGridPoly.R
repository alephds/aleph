
# Los centroides son opcionales -----------------------------------------

geoGridPoly<- function(mapa_orig, npuntos, centroides){
  library(raster); library(sp)
  grid <- sp::makegrid(mapa_orig, npuntos)
  puntos <- SpatialPoints(grid, proj4string = CRS(proj4string(map_orig)))
  puntosBuenos <- coordinates(puntos[map_orig,])
  puntosBuenos <- as.data.frame(puntosBuenos)
  coordinates(puntosBuenos) <- cbind(puntosBuenos$x1, puntosBuenos$x2)
  if(missing(centroides)){
    proj4string(puntosBuenos) <- proj4string(map_orig) 
  } else{
    proj4string(puntosBuenos) <- proj4string(centroides)
  }
  puntosBuenos
}
