source('../THU_Disease_Cluster/flexscan.R')
source('../THU_Disease_Cluster/show_flexscan_result.R')

load("C:/Users/2017013184/Desktop/capstone/data/china.RData")
#run algorithm
#shanghai <- china[startsWith(china$dt_adcode, "31"),]

res = flexscan_qxa(china,province_code = 11, method = 'coord')

summary = summary_plot(res$fls,res$province_shp)

