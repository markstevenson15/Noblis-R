

plotdistance = function(IP, city){
  library(leaflet)
  library(ggmap)
  library(rgeolocate)
  library(geosphere)
  df = data.frame(matrix(NA, nrow = 2, ncol = 3))

  address = db_ip(ip_addresses = IP, key = "346abd4989475c9b12c373c461d631071238537f")
  output = c(address[[1]][4],address[[1]][3])
  IPlocation = geocode(paste(output))[1,]
  TINlocation = geocode(city)
  df = data.frame(matrix(NA, nrow = 2, ncol = 2))
  df[1,1:2] = as.numeric(IPlocation)
  df[2,1:2] = TINlocation
  df[1,3]= "IP"
  df[2,3]= "TIN Address"

  distance = distm (c(df[1,1], df[1,2]), c(df[2,1], df[2,2]), fun = distHaversine)*0.000621371

  mapout = leaflet(data = df[1:2,]) %>% addTiles() %>%
    addPolylines(data = df[1:2], lng = ~X1, lat = ~X2)  %>%
    addCircleMarkers(lng = df[1,1], lat = df[1,2], label = "IP",labelOptions = labelOptions(noHide = T))%>%
    addCircleMarkers(lng = df[2,1], lat = df[2,2], label = "TIN Address",labelOptions = labelOptions(noHide = T))
  
  print(distance)
  print(mapout)
}