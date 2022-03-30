library(sqldf)
dat = read.csv2("prepared_dat.csv",sep = ",",encoding = 'UTF-8')

province_lat_lon = unique(subset(dat,select = c("地区编码","经度","纬度")))
colnames(province_lat_lon) = c("code","lon","lat")
province_lat_lon = sqldf("select * from province_lat_lon 
                         where lon !=''")

require(geosphere)


########################################################################
#rflexscan
#prepare data
library("rgdal")
china <- readOGR("District/District/district.shp",
                      stringsAsFactors = FALSE,encoding = 'UTF-8')

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
                       ")
new_china_data$lon = as.numeric(new_china_data$lon)
new_china_data$lat = as.numeric(new_china_data$lat)
new_china_data[is.na(new_china_data$case_cnt),"case_cnt"] = 0
new_china_data[is.na(new_china_data$expected_case),"expected_case"] = 10000
new_china_data[is.na(new_china_data$lat),"lat"] = mean(new_china_data$lat,na.rm = TRUE)
new_china_data[is.na(new_china_data$lon),"lon"] = mean(new_china_data$lon,na.rm = TRUE)
china@data = new_china_data

coord <- data.frame(x=new_china_data$lon, y=new_china_data$lat)
coord$x = as.numeric(coord$x)
coord$y = as.numeric(coord$y)
coordinates(coord) <- c("x", "y")
proj4string(coord) <- proj4string(china)
coord <- spTransform(coord, CRS("+init=epsg:3415"))

library('rflexscan')
library("rgdal")
library("spdep")
library("RColorBrewer")
#china nb
nb <- poly2nb(china, queen = T)
head(nb)
#bj nb
beijing = china[startsWith(china$dt_adcode, "11"),] 
nb_bj <- poly2nb(beijing, queen = T)
head(nb_bj)
fls_bj <- rflexscan(name = beijing$dt_adcode,
                 lat = beijing$lat, lon = beijing$lon, nb = nb_bj,
                 observed = beijing$case_cnt, expected = beijing$expected_case)

fls <- rflexscan(name = china$dt_adcode,
                 lat = china$lat, lon = china$lon, nb = nb,
                 observed = china$case_cnt, expected = china$expected_case)
print(fls)
summary(fls)

plot(fls, rank = 1:7, col = brewer.pal(7, "RdYlBu"))
box()
legend("topleft", legend = 1:7, fill = brewer.pal(7, "RdYlBu"), bty="n")

choropleth(manhattan, fls, rank = 1:7, col = brewer.pal(7, "RdYlBu"))
legend("topleft", legend = 1:7, fill = brewer.pal(7, "RdYlBu"), bty="n")


