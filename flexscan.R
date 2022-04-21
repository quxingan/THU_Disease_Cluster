#calculate fls
#province_shp: .shp data
#method: the way to calculate coord
library(rflexscan)
library(spdep)
library("rgdal")
flexscan_qxa = function(country_shp, province_code, 
                        start_date,end_date, method = 'original'){
  province_shp = country_shp[startsWith(china$dt_adcode, as.character(province_code)),]
  nb <- poly2nb(province_shp, queen = T)
  if(method=='original'){
    fls <- rflexscan(name = province_shp$dt_adcode,
                     lon = province_shp$lon, lat = province_shp$lat, nb = nb,
                     observed = province_shp$case_cnt, expected = province_shp$expected_case)
  }
  else if(method=='coord')
  {
    coord <- data.frame(x=province_shp$lon, y=province_shp$lat)
    coordinates(coord) <- c("x", "y")
    proj4string(coord) <- proj4string(province_shp)
    coord <- spTransform(coord, CRS("+init=epsg:3415"))
    fls <- rflexscan(name = province_shp$dt_adcode,
                     x = coord$x, y = coord$y, nb = nb,
                     observed = province_shp$case_cnt, expected = province_shp$expected_case)
  }

  list('province_shp' = province_shp, 'fls' = fls,'nb' = nb)
}