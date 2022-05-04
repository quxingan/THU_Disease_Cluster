#used to select time window
#return .shp file for flexscan
library(sqldf)
library(dplyr)
library("rgdal")
# china <- readOGR("District/District/district.shp",
#                  stringsAsFactors = FALSE,encoding = 'UTF-8')
# 
# disease_data = read.csv2('disease_data.csv')
# district_lon_lat = read.csv2('district_lon_lat.csv')


#disease_data: contains two columns: district_code,date
#expected_dat: expect case in the time widow, contains two columns: district_code, expected case
#start_date: the start of time window
#end_date: the end of time window
#district_lon_lat: contains 3 columns: district_code, lon,lat
selectTimeWindow = function(disease_data,
                            expected_data,
                            start_date= '2004/12/27',
                            end_date= '2006/10/10',
                            district_lon_lat)
  {
  time_window_summary = subset(disease_data,date >= start_date, date <= end_date)
  time_window_summary = sqldf("select district_code,
                              count(*) as case_cnt
                              from time_window_summary
                              group by district_code
                              ")
  #fill expected_data with simulation
  if(missing(expected_data)){
    set.seed(123)
    time_window_summary$expected_case = as.integer(max(time_window_summary$case_cnt)*
                                                     runif(dim(time_window_summary)[1]
                                                           ,0.5,3))
  }else{
    time_window_summary = sqldf('select a.district_code,case_cnt,expected_case
                                from time_window_summary a
                                inner join expected_dat b')
  }
  
  #fill na 
  new_data = sqldf("select a.dt_adcode, case_cnt, expected_case,
                        lon,lat
                         from district_lon_lat a 
                         left join time_window_summary b
                         on a.dt_adcode = b.district_code")
  na.pos = is.na(new_data$case_cnt)
  new_data$case_cnt[na.pos] = 0
  new_data$expected_case[na.pos] = max(time_window_summary$case_cnt)
  new_data$dt_adcode = as.character(new_data$dt_adcode)
  new_data
}







# 
# disease_data = read.csv2('pos_date.csv',encoding = 'UTF-8',sep=',')
# names(disease_data) = c('district_code','date')
# 
# 
# #join .shp file
# shp_dat = china@data
# 
# prepared_shp = subset(china@data,select = c('dt_adcode','lon','lat'))
# write.csv2(prepared_shp,file = 'district_lon_lat.csv')
# 
# new_china_data = sqldf("select a.dt_adcode, case_cnt, expected_case,
#                        lon,lat
#                        from prepared_shp a 
#                        left join time_window_summary b
#                        on a.dt_adcode = b.district_code")
# na.pos = is.na(new_china_data$case_cnt)
# new_china_data$case_cnt[na.pos] = 0
# new_china_data$expected_case[na.pos] = max(time_window_summary$case_cnt)
# china1@data = new_china_data



