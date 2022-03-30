library(sqldf)
dat = read.csv2("prepared_dat.csv",sep = ",")

province_lat_lon = unique(subset(dat,select = c("地区编码","经度","纬度")))
colnames(province_lat_lon) = c("code","lon","lat")
province_lat_lon = sqldf("select * from province_lat_lon 
                         where lon !=''")

require(geosphere)
distVincentyEllipsoid()

########################################################################
#rflexscan
#prepare data
library("rgdal")
china <- readOGR("ChinaAdminDivisonSHP-master/District/district.shp",
                      stringsAsFactors = FALSE)

head(china@data)

district_summary = sqldf("select 地区编码 as code,count(*) as case_cnt,
                              经度 as lon,
                              纬度 as lat
                              from dat
                              where 经度 !=''
                              group by 地区编码")
set.seed(1)
district_summary$expected_case = as.integer(10000*runif(dim(district_summary)[1]
                                                        ,0.51,2))

shp_dat = china@data

new_china_data = sqldf("select  a.dt_adcode, case_cnt,expected_case,
                        lon,lat
                       from shp_dat a
                       left join district_summary b 
                       on a.dt_adcode = b.code
                       where lon!=''
                       ")

china@data = new_china_data

coord <- data.frame(x=new_china_data$lon, y=new_china_data$lat)
coord$x = as.numeric(coord$x)
coord$y = as.numeric(coord$y)
coordinates(coord) <- c("x", "y")
proj4string(coord) <- proj4string(china)
coord <- spTransform(coord, CRS("+init=epsg:32618"))

library("spdep")
nb <- poly2nb(china, queen = T)
head(nb)




