plotKrigingMapa <- function(kriged, escala, mapa){
  library(ggplot2)
  mapa@data$id <- rownames(mapa@data)
  mexico <- fortify(mapa, region = 'id')
  mexicodf <- merge(mexico, mapa@data, by = 'id')
  
  krigeddf <- as.data.frame(kriged)
  
  ggplot(data = krigeddf, aes(x=coords.x1, y=coords.x2)) + geom_tile(aes(fill=var1.pred)) + coord_equal() +
    scale_fill_gradient(low = "red", high="yellow") + 
    theme_bw() + geom_path(data = mexicodf, size = 0.001,aes(x=mexicodf$long, y=mexicodf$lat, group=mexicodf$group), color = 'white') +
    labs(fill = escala, x = '', y = '')
}
