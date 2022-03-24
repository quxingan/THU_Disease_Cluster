library(sqldf)
all_dat = read.csv2('prepared_dat.csv',encoding = 'UTF-8',sep = ',')
province = read.csv2('province.csv',sep = ',')
colnames(all_dat) = c('x','code','date','level','district_name','lat','lon')
year_summary = sqldf("select substring(cast(code as varchar),1,2) as province_code,
                     substring(date,1,4) as year,
                     count(*) as cases
                     from all_dat
                     group by year,province_code")

province_year_summary = sqldf("select * 
                              from year_summary a
                              inner join province b
                              on a.province_code==cast(b.code as varchar)")

write.csv2(province_year_summary,"province_year_summary.csv")
