library(sqldf)
library('rflexscan')
library("rgdal")
library("spdep")
library("RColorBrewer")

dat = read.csv2("prepared_dat.csv",sep = ",",encoding = 'UTF-8')

province_lat_lon = unique(subset(dat,select = c("地区编码","经度","纬度")))
colnames(province_lat_lon) = c("code","lon","lat")
province_lat_lon = sqldf("select * from province_lat_lon 
                         where lon !=''")


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

#remove na
na.pos = is.na(new_china_data$lon)
new_china_data = new_china_data[!na.pos,]
china@data = new_china_data
##polygons
poly_list = china@polygons
china@polygons = poly_list[!na.pos]
##plotorder
o = china@plotOrder
match.pos = match(o,seq(1:2875)[na.pos])
o = o[is.na(match.pos)]
china@plotOrder=o

#run algorithm
res = flexscan_qxa(china)















