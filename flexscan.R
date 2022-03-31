#calculate fls
#province_shp: .shp data
flexscan_qxa = function(province_shp){
  nb <- poly2nb(province_shp, queen = T)
  coord <- data.frame(x=province_shp$lon, y=province_shp$lat)
  coordinates(coord) <- c("x", "y")
  proj4string(coord) <- proj4string(province_shp)
  coord <- spTransform(coord, CRS("+init=epsg:3415"))
  fls <- rflexscan(name = province_shp$dt_adcode,
                   x = coord$x, y = coord$y, nb = nb,
                   observed = province_shp$case_cnt, expected = province_shp$expected_case)
  list(fls,nb)
}