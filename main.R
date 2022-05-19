source('../THU_Disease_Cluster/flexscan.R')
source('../THU_Disease_Cluster/show_flexscan_result.R')
source('../THU_Disease_Cluster/select_date.R')

#prepare data
load("C:/Users/2017013184/Desktop/capstone/data/china.RData")
disease_data = read.csv2('disease_data.csv')
district_lon_lat = read.csv2('district_lon_lat.csv')


#select time window and change china data
china@data = selectTimeWindow(disease_data ,start_date= '2004-12-20',
                              end_date= '2005-01-15-10',district_lon_lat = district_lon_lat)
#run flexscan algorithm

res = flexscan_qxa(china,province_code = 11, method = 'coord')
summary = summary_plot(res$fls,res$province_shp)
