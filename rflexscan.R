library('rflexscan')
library("rgdal")
nys <- readOGR("data/NYS_Cancer/NYSCancer_region.shp",
                    stringsAsFactors = FALSE)

head(nys@data[c("DOHREGION", "LATITUDE", "LONGITUDE",
                    "OBREAST", "EBREAST")])
manhattan <- nys[startsWith(nys$DOHREGION, "36061"),]

coord <- data.frame(x=manhattan$LONGITUDE, y=manhattan$LATITUDE)
coordinates(coord) <- c("x", "y")
proj4string(coord) <- proj4string(manhattan)
coord <- spTransform(coord, CRS("+init=epsg:32618"))

library("spdep")
nb <- poly2nb(manhattan, queen = T)
print(nb)

head(nb)


fls <- rflexscan(name = manhattan$DOHREGION,
                     x = coord$x, y = coord$y, nb = nb,
                    observed = manhattan$OBREAST, expected = manhattan$EBREAST)
print(fls)

summary(fls)

library("RColorBrewer")
plot(fls, rank = 1:7, col = brewer.pal(7, "RdYlBu"))
box()
legend("topleft", legend = 1:7, fill = brewer.pal(7, "RdYlBu"), bty="n")

choropleth(manhattan, fls, rank = 1:7, col = brewer.pal(7, "RdYlBu"))
legend("topleft", legend = 1:7, fill = brewer.pal(7, "RdYlBu"), bty="n")

