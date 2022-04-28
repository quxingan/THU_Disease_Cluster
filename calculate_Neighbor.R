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

new_china_data = sqldf("select  a.dt_adcode, b.case_cnt,b.expected_case
                        
                       from shp_dat a
                       left join district_summary b 
                       on a.dt_adcode = b.code
                       ")
new_china_data2 = sqldf('select dt_adcode, a.case_cnt, a.expected_case,
                       b.lon,b.lat
                       from
                       new_china_data a
                       left join region b
                       on a.dt_adcode= cast(b.code as varchar)')

new_china_data2$lon = as.numeric(new_china_data2$lon)
new_china_data2$lat = as.numeric(new_china_data2$lat)

na.dat = new_china_data2[is.na(new_china_data2$lon),]
#1st method fill na
na.pos = seq(1:length(new_china_data2$lon))[is.na(new_china_data2$lon)]
poly_list = china@polygons
for(j in 1:length(na.pos)){
  i = na.pos[j]
  a = poly_list[[i]]@Polygons
  b = a[[1]]@coords
  fill_lon = mean(b[,1])
  fill_lat = mean(b[,2])
  new_china_data2[i,]$lon = fill_lon
  new_china_data2[i,]$lat = fill_lat
}
new_china_data2$case_cnt[is.na(new_china_data2$case_cnt)] = 0
new_china_data2$expected_case[is.na(new_china_data2$expected_case)] = 5000
china@data = new_china_data2


#2st method remove na
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



