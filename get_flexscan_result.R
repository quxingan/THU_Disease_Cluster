#used for get result of fls
#fls: flexscan result
#data_shp: data read from .shp file
#return plot
summary_plot  = function(fls,data_shp){
  print(fls)
  summary(fls)
  dev.new()
  plot(fls, rank = 1:7, col = brewer.pal(7, "RdYlBu"))
  box()
  legend("topleft", legend = 1:7, fill = brewer.pal(7, "RdYlBu"), bty="n")
  dev.new()
  choropleth(data_shp, fls, rank = 1:7, col = brewer.pal(7, "RdYlBu"))
  legend("topleft", legend = 1:7, fill = brewer.pal(7, "RdYlBu"), bty="n")
}