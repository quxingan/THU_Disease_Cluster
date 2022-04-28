library(sqldf)
library(dplyr)
my_dat = read.csv2('prepared_dat.csv',sep = ',',encoding='UTF-8')
#dat_filled_day = read.csv2('prepared_visual_dat_filled_day.csv',sep = ',')
#prepare dat for shiny app
visual_dat = sqldf("select 地区编码 as area_id, 发病日期 as date,
                  count(*) as cases
                 from my_dat
                 where 经度 != '' and
                 area_id not in (350403,350427,410306)
                 group by 地区编码,发病日期
                 order by area_id")


visual_dat$date = as.Date(visual_dat$date)
##sim population
set.seed(311)
visual_dat$population = as.integer(100*runif(nrow(visual_dat),0.5,1.5))


#turn shp to csv
library(sp)
library(maptools)
#library(mapdata)
dis <- readShapeSpatial("District/District/district.shp")

diff1 = setdiff(unique(visual_dat$area_id),as.numeric(as.character(dis$dt_adcode)))
diff2 = setdiff(as.numeric(as.character(dis$dt_adcode)),unique(visual_dat$area_id))

for (i in seq(1,388)){
  new = c(diff2[i],"2000-1-1",0,100)
  visual_dat = rbind(visual_dat,new)
}

#change data type
visual_dat$area_id = as.numeric(visual_dat$area_id)
visual_dat$cases = as.numeric(visual_dat$cases)
visual_dat$population = as.numeric(visual_dat$population)

# sample data and fill the missing date
visual_dat_sample = sqldf("select * from (
                          select *, rank () over (partition by area_id
	                        order by date desc) as date_rank
                          from visual_dat
                          ) a
                          where date_rank<5
                          order by area_id, date"
                          )
#fill the missing date
missing_date_df = data.frame()
i=1
j=2
while(i<=9919){
  start_date = as_datetime(visual_dat_sample$date[i])
  end_date = as_datetime(visual_dat_sample$date[j])
  dates_diff = as.integer(end_date-start_date)
  if(visual_dat_sample$area_id[i]==visual_dat_sample$area_id[j] & dates_diff>1){
    cur_area = visual_dat_sample$area_id[i]
   # print(start_date+86400)
    for(k in seq(1:(dates_diff-1))){
      missing_date = c(cur_area,as.character(as.Date(start_date+86400*k)),0,100,0)
      missing_date_df =  rbind(missing_date_df,missing_date)
    }
    i = j
    j = j+1
  }
  else{
    i=j
    j=j+1
  }
}
colnames(missing_date_df) = colnames(visual_dat_sample)
visual_dat_sample = rbind(visual_dat_sample,missing_date_df)
visual_dat$date = as.character(visual_dat$date)
write.csv(visual_dat,"data/prepared_visual_dat_filled_day.csv",col.names = FALSE)

#######################################################################
#get data in year
visual_dat = read.csv2('prepared_visual_dat.csv',sep = ',')
visual_dat0407 = sqldf("select area_id, substring(date,1,4) as year,
                  sum(cases) as cases,avg(population) as population
                 from visual_dat
                 where year in('2007','2005','2006','2004')
                 group by area_id,year
                 order by area_id,year")
#fill area
diff2 = setdiff(as.numeric(as.character(dis$dt_adcode)),unique(visual_dat0407$area_id))
for (i in seq(1,length(diff2))){
  new04 = c(diff2[i],"2004",0,100)
  new05 = c(diff2[i],"2005",0,100)
  new06 = c(diff2[i],"2006",0,100)
  new07 = c(diff2[i],"2007",0,100)
  visual_dat0407 = rbind(visual_dat0407,new04,new05,new06,new07)
}
diff2 = setdiff(as.numeric(as.character(dis$dt_adcode)),unique(visual_dat0407$area_id))
print(paste('diff2:',diff2))
#fill year
all_dt = as.numeric(as.character(dis$dt_adcode))
for(i in seq(1,length(all_dt))){
  if(i%%100==0){
    print(paste(i,'complete'))
  }
  cur_dt = all_dt[i]
  has_year = subset(visual_dat0407,area_id==cur_dt,select = year)
  if (!('2004' %in% has_year$year)){
    new = c(cur_dt,"2004",runif(1,0,100),100)
    visual_dat0407 = rbind(visual_dat0407,new)
  }
  if (!('2005' %in% has_year$year)){
    new = c(cur_dt,"2004",runif(1,0,100),100)
    visual_dat0407 = rbind(visual_dat0407,new)
  }
  if (!"2006" %in% has_year$year){
    new = c(cur_dt,"2006",runif(1,0,100),100)
    visual_dat0407 = rbind(visual_dat0407,new)
  }
  if (!("2007" %in% has_year$year)){
    new = c(cur_dt,"2007",runif(1,0,100),100)
    visual_dat0407 = rbind(visual_dat0407,new)
  }
}

visual_dat0407$population = as.integer(10000*runif(nrow(visual_dat0407),0.35,20))
visual_dat0407$cases = as.integer(visual_dat0407$cases)
visual_dat0407$area_id = as.integer(visual_dat0407$area_id)

write.csv(visual_dat0407,"prepared_visual_dat_0407.csv")
#######################################################################
##rfexscan

all_case = sqldf("select 地区编码 as DOHREGION, count(发病日期) as case_cnt,
                 经度 as lon, 纬度 as lat
                 from my_dat
                 where 经度 != ''
                 group by 地区编码
                 order by DOHREGION")
#get the neighbour of each district
#method: district are neighbours in the same province
i = 1
j = 1
nb = list()
while(i<=2490){
  province = substr(as.character(all_case$DOHREGION[i]),1,4)
  j = i
  cur_province = substr(as.character(all_case$DOHREGION[j]),1,4)
  while(cur_province==province & j<=2490){
    j = j+1
    cur_province = substr(as.character(all_case$DOHREGION[j]),1,4)
  }
 # print(j)
  for (k in seq(i,j-1)){
    if(k>i & k<j-1){
      cur_nb = c(seq(i,k-1),seq(k+1,j-1))
    }
    else if(k==i){
      cur_nb = seq(k+1,j-1)
     # print(seq(k+1,j-1))
    }
    else if(k==j-1){
      cur_nb = seq(i,j-2)
    }
    #print(cur_nb)
    nb = c(nb,list(cur_nb))
  }
  i = j
}

#sim ECASE
set.seed(311)
all_case$ECASE = all_case$case_cnt*runif(2490,0.1,2)


#prepare data type
all_case$lat = unlist(lapply(all_case$lat,as.numeric))
all_case$lon = unlist(lapply(all_case$lon,as.numeric))
all_case$DOHREGION = unlist(lapply(all_case$DOHREGION,as.character))
#run the algorithm

fls <- rflexscan(name = all_case$DOHREGION,
                 lat = all_case$lat, lon  = all_case$lon, nb = nb,
                 observed = all_case$case_cnt, expected = all_case$ECASE)
print(fls)

summary(fls)

library("RColorBrewer")
plot(fls, rank = 1:7, col = brewer.pal(7, "RdYlBu"))
box()
legend("topleft", legend = 1:7, fill = brewer.pal(7, "RdYlBu"), bty="n")

choropleth(manhattan, fls, rank = 1:7, col = brewer.pal(7, "RdYlBu"))
legend("topleft", legend = 1:7, fill = brewer.pal(7, "RdYlBu"), bty="n")




