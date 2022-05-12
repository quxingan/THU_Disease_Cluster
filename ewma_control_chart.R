library(qcc)
library(sqldf)
disease_data = read.csv2('disease_data.csv')
start_date= '2004-12-27'
end_date= '2005-01-30'
region_code = '11'
#disease_data: contains two columns: district_code,date.Date format must be the same as start_date & end_date
#start_date: the start of time window
#end_date: the end of time window
#region_code: the interested region_code
#lambda,nsigmas: parameters for ewma charts
ewma_control_chart = function(disease_data,
                              start_date= '2004-12-27',
                              end_date= '2005-01-30',
                              region_code = '11',
                              lambda = 0.2, 
                              nsigmas = 3)
{
  #select time
  pos = disease_data$date >= start_date & disease_data$date <= end_date
  time_window_summary = disease_data[pos,]
  max(time_window_summary$date)
  min(time_window_summary$date)
  #select region
  
  time_window_summary = time_window_summary[startsWith(as.character(time_window_summary$district_code),
                                                       region_code),]
  #summary data according to date
  time_window_summary = sqldf('select count (*) as case_cnt, date
                              from time_window_summary
                              group by date
                              order by date desc')
  
  #create df containing all dates
  date <- as.character(seq.Date(from = as.Date(start_date,format = "%Y-%m-%d"), 
                                to = as.Date(end_date,format = "%Y-%m-%d"), by = "day"))
  all_dates = as.data.frame(date)
  
  #join time_window_summary with all_dates, fill na with 0
  all_date_summary = sqldf('select b.date, case_cnt
                           from
                           all_dates b
                           left join time_window_summary a
                           on a.date = b.date')
  all_date_summary$case_cnt[is.na(all_date_summary$case_cnt)] = 0
  
  #use ewma
  q <-  ewma(all_date_summary$case_cnt, lambda=0.2, nsigmas=3)
  summary(q)
  dev.new()
  plot(q,
       title = paste('region:',region_code,'s:',start_date,'e:',end_date))
  return(q)
}

#run the finction
ewma_res = ewma_control_chart(disease_data,
                             start_date= '2005-05-15',
                             end_date= '2005-05-30',
                             region_code = '11',
                             lambda = 0.2, 
                             nsigmas = 3)


