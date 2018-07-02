pal <- colorNumeric("Reds", 
                    domain=(meteors %>% 
                              filter(year>1780))$year)

meteors_mass1 <- meteors %>% 
  filter((meteors$kg) < 0.01)
meteors_mass2 <- meteors %>% 
  filter(between((meteors$kg), 0.01, 0.03))
meteors_mass3 <- meteors %>% 
  filter(between((meteors$kg), 0.03, 0.2))
meteors_mass4 <- meteors %>% 
  filter(between((meteors$kg), 0.2, 10))
meteors_mass5 <- meteors %>% 
  filter((meteors$kg) > 10)

magnification <- 10000

meteors %>% 
  filter(year > 1800) %>% 
  leaflet() %>% 
  addProviderTiles(provider = "Esri") %>% 
  setView(lng=-5, lat=50, zoom=2) %>% 
  addCircles(data=meteors_mass1, 
                   lat=~reclat,
                   lng=~reclong,
                   popup = ~paste(name,"<br>",  
                                  "Mass", kg, "kg", "<br>",
                                  "Year", year),
                   color = ~pal(year),
                   radius=~(((kg)^0.25)*magnification),
                   group = "Mass under 0.01kg") %>% 
  addCircles(data=meteors_mass2, 
                   lat=~reclat,
                   lng=~reclong,
                   popup = ~paste(name,"<br>",  
                                  "Mass", kg, "kg", "<br>",
                                  "Year", year),
                   color = ~pal(year),
                   radius=~(((kg)^0.25)*magnification),
                   group = "Mass: 0.01kg - 0.03kg") %>% 
  addCircles(data=meteors_mass3, 
                   lat=~reclat,
                   lng=~reclong,
                   popup = ~paste(name,"<br>",  
                                  "Mass", kg, "kg", "<br>",
                                  "Year", year),
                   color = ~pal(year),
                   radius=~(((kg)^0.25)*magnification),
                   group = "Mass: 0.03kg - 0.2kg") %>% 
  addCircles(data=meteors_mass4, 
                   lat=~reclat,
                   lng=~reclong,
                   popup = ~paste(name,"<br>",  
                                  "Mass", kg, "kg", "<br>",
                                  "Year", year),
                   color = ~pal(year),
                   radius=~(((kg)^0.25)*magnification),
                   group = "Mass: 0.2kg - 10kg") %>% 
  addCircles(data=meteors_mass5, 
             lat=~reclat,
             lng=~reclong,
             popup = ~paste(name,"<br>",  
                            "Mass", kg, "kg", "<br>",
                            "Year", year),
             color = ~pal(year),
             radius=~(((kg)^0.25)*magnification),
             group = "Mass over 10kg") %>% 
  addLegend("bottomright", pal = pal, 
            values = ~year,
            title = "Year",
            labFormat =  labelFormat(big.mark = ""),
            opacity = 1) %>% 
  addLayersControl(overlayGroups = c("Mass under 0.01kg", 
                                     "Mass: 0.01kg - 0.03kg", 
                                     "Mass: 0.03kg - 0.2kg", 
                                     "Mass: 0.2kg - 10kg",
                                     "Mass over 10kg"),
                   options=layersControlOptions(collapsed = F)) %>% 
  hideGroup(c("Mass under 0.01kg", "Mass: 0.01kg - 0.03kg", 
              "Mass: 0.03kg - 0.2kg", "Mass: 0.2kg - 10kg"))


factor_pal <- colorFactor(
  palette = c("#00ffff", "#111151", "#4379ac", "#00aba6", 
              "#FFFF00", "#FF0000", "#00FF00"),
  domain = levels(new_meteors$chond))

meteors_chond1 <- new_meteors %>% 
  filter(chond == "chondrite low iron")
meteors_chond2 <- new_meteors %>% 
  filter(chond == "chondrite high iron")
meteors_chond3 <- new_meteors %>% 
  filter(chond == "chondrite")
meteors_chond4 <- new_meteors %>% 
  filter(chond == "achondrite")
meteors_chond5 <- new_meteors %>% 
  filter(chond == "chondrite vlow iron")
meteors_chond6 <- new_meteors %>% 
  filter(chond == "irons")
meteors_chond7 <- new_meteors %>% 
  filter(chond == "Stony_Iron")


new_meteors %>% 
  leaflet() %>% 
  addProviderTiles(provider = "Esri") %>%
  setView(lng=-5, lat=50, zoom=4) %>% 
  addCircles(data=meteors_chond1, 
             lat=~reclat,
             lng=~reclong,
             popup = ~paste("<b>",name,"</b>","<br>",  
                            "Mass", kg, "kg", "<br>",
                            "Year", year, "<br>",
                            "Type", recclass),
             color= ~factor_pal(chond),
             radius=100,
             group = "chondrite low iron") %>% 
  addCircles(data=meteors_chond2, 
             lat=~reclat,
             lng=~reclong,
             popup = ~paste("<b>",name,"</b>","<br>",  
                            "Mass", kg, "kg", "<br>",
                            "Year", year, "<br>",
                            "Type", recclass),
             color= ~factor_pal(chond),
             radius=100,
             group = "chondrite high iron") %>% 
  addCircles(data=meteors_chond3, 
             lat=~reclat,
             lng=~reclong,
             popup = ~paste("<b>",name,"</b>","<br>",  
                            "Mass", kg, "kg", "<br>",
                            "Year", year, "<br>",
                            "Type", recclass),
             color= ~factor_pal(chond),
             radius=100,
             group = "chondrite") %>% 
  addCircles(data=meteors_chond4, 
             lat=~reclat,
             lng=~reclong,
             popup = ~paste("<b>",name,"</b>","<br>",  
                            "Mass", kg, "kg", "<br>",
                            "Year", year, "<br>",
                            "Type", recclass),
             color= ~factor_pal(chond),
             radius=100,
             group = "achondrite") %>% 
  addCircles(data=meteors_chond5, 
             lat=~reclat,
             lng=~reclong,
             popup = ~paste("<b>",name,"</b>","<br>",  
                            "Mass", kg, "kg", "<br>",
                            "Year", year, "<br>",
                            "Type", recclass),
             color= ~factor_pal(chond),
             radius=100,
             group = "chondrite vlow iron") %>% 
  addCircles(data=meteors_chond6, 
             lat=~reclat,
             lng=~reclong,
             popup = ~paste("<b>",name,"</b>","<br>",  
                            "Mass", kg, "kg", "<br>",
                            "Year", year, "<br>",
                            "Type", recclass),
             color= ~factor_pal(chond),
             radius=100,
             group = "irons") %>% 
  addCircles(data=meteors_chond7, 
             lat=~reclat,
             lng=~reclong,
             popup = ~paste("<b>",name,"</b>","<br>",  
                            "Mass", kg, "kg", "<br>",
                            "Year", year, "<br>",
                            "Type", recclass),
             color= ~factor_pal(chond),
             radius=100,
             group = "Stony_Iron") %>% 
  addLegend("bottomright", pal = factor_pal, 
            values = ~chond,
            title = "Type",
            labFormat =  labelFormat(big.mark = ""),
            opacity = 1
  ) %>% 
  addLayersControl(overlayGroups = c("chondrite", 
                                     "chondrite high iron",
                                     "chondrite low iron", 
                                     "chondrite vlow iron",                                   
                                     "irons",
                                     "Stony_Iron",
                                     "achondrite"),
                   options=layersControlOptions(collapsed = F)) %>% 
  hideGroup(group = c("chondrite low iron", 
                      "chondrite high iron", 
                      "chondrite", 
                      "achondrite",
                      "chondrite vlow iron",
                      "Stony_Iron"))
