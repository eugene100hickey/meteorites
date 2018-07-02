setwd("~/Desktop/Academic/Research/Blogs/Meteorites")

library(ggplot2)
library(magrittr)
library(dplyr)
library(leaflet)
library(lubridate)
library(sp)
library(rworldmap)
library(RColorBrewer)
library(png)
library(grid)

coords2continent = function(points)
{  
  countriesSP <- getMap(resolution='low')
  pointsSP = SpatialPoints(points, proj4string=CRS(proj4string(countriesSP)))  
  indices = over(pointsSP, countriesSP)
  indices$REGION   # returns the continent (7 continent model)
}


meteors <- read.csv("Meteorite_Landings.csv", sep=",")
meteors <- meteors %>% select(-GeoLocation)

meteors[grepl(x=meteors$name, pattern="Larkman Nunatak*"),]$reclat <- -85.7667
meteors[grepl(x=meteors$name, pattern="Larkman Nunatak*"),]$reclong<- 179.3833
meteors[grepl(x=meteors$name, pattern="Graves Nunatak*"),]$reclat <- -86.7167
meteors[grepl(x=meteors$name, pattern="Graves Nunatak*"),]$reclong<- 141.5
meteors[grepl(x=meteors$name, pattern="Yamato*") & is.na(meteors$reclong),]$reclat <- -71.5
meteors[grepl(x=meteors$name, pattern="Yamato*") & is.na(meteors$reclong),]$reclong<- 35.66667



empties <- sum(!complete.cases(meteors))

meteors <- meteors %>% 
  mutate(year=year(dmy_hms(year))) %>% 
  mutate(kg=mass..g./1000) %>% 
  na.omit %>% 
  mutate(continent=coords2continent(data.frame(reclong, reclat)))

meteors$category <- meteors$recclass
meteors$category <- gsub("OC", "chondrite", meteors$category)
meteors$category <- gsub("C.*", "carbonaceous", meteors$category)
meteors$category <- gsub("Diogenite.*", "diogenite", meteors$category)
meteors$category <- gsub("Eucrite.*", "eucrite", meteors$category)
meteors$category <- gsub("Iron.*", "irons", meteors$category)
meteors$category <- gsub("LL.*", "chondrite vlow iron", meteors$category)
meteors$category <- gsub("Lunar.*", "lunar", meteors$category)
meteors$category <- gsub("EL.*", "enstatite", meteors$category)
meteors$category <- gsub("L.*", "chondrite low iron", meteors$category)
meteors$category <- gsub("En.*", "enstatite", meteors$category)
meteors$category <- gsub("EH.*", "enstatite", meteors$category)
meteors$category <- gsub("E-.*", "enstatite", meteors$category)
meteors$category <- gsub("E3.*", "enstatite", meteors$category)
meteors$category <- gsub("E4.*", "enstatite", meteors$category)
meteors$category <- gsub("E5.*", "enstatite", meteors$category)
meteors$category <- gsub("E6.*", "enstatite", meteors$category)
meteors$category <- gsub("Howardite.*", "howardite", meteors$category)
meteors$category <- gsub("H.*", "chondrite high iron", meteors$category)
meteors$category <- gsub("Martian.*", "martian", meteors$category)
meteors$category <- gsub("Mesosiderite.*", "mesosiderite", meteors$category)
meteors$category <- gsub("Pallasite.*", "pallasite", meteors$category)
meteors$category <- gsub("Relict.*", "relict", meteors$category)
meteors$category <- gsub("R.*", "rumuruti", meteors$category)
meteors$category <- gsub("Ureilite.*", "ureilite", meteors$category)
meteors$category <- gsub("E", "enstatite", meteors$category)

meteors$chond <- meteors$category
meteors$chond <- gsub("Achondrite.*", "achondrite", meteors$chond)
meteors$chond <- gsub("Chondrite.*", "chondrite", meteors$chond)
#meteors$chond <- gsub("Irons.*", "Irons", meteors$chond)
meteors$chond <- gsub("rumuruti", "chondrite", meteors$chond)
meteors$chond <- gsub("enstatite", "chondrite", meteors$chond)
meteors$chond <- gsub("mesosiderite", "Stony_Iron", meteors$chond)
meteors$chond <- gsub("pallasite", "Stony_Iron", meteors$chond)
meteors$chond <- gsub("ureilite", "Stony_Iron", meteors$chond)
meteors$chond <- gsub("eucrite", "achondrite", meteors$chond)
meteors$chond <- gsub("howardite", "achondrite", meteors$chond)
meteors$chond <- gsub("Aubrite.*", "achondrite", meteors$chond)
meteors$chond <- gsub("Acapulcoite.*", "achondrite", meteors$chond)
meteors$chond <- gsub("Angrite.*", "achondrite", meteors$chond)
meteors$chond <- gsub("Brachinite", "achondrite", meteors$chond)
meteors$chond <- gsub("diogenite", "achondrite", meteors$chond)
meteors$chond <- gsub("Stone.*", "achondrite", meteors$chond)
meteors$chond <- gsub("carbonaceous", "chondrite", meteors$chond)
meteors$chond <- gsub("OChondrite", "chondrite", meteors$chond)
meteors$chond <- gsub("E3.*", "chondrite", meteors$chond)
meteors$chond <- gsub("Winonaite", "achondrite", meteors$chond)
meteors$chond <- gsub("Achondrite-.*", "achondrite", meteors$chond)
meteors$chond <- gsub("Chondrite.*", "chondrite", meteors$chond)

# z <- meteors %>% 
#   select(category) %>% 
#   table %>% 
#   as.data.frame() %>% 
#   filter(Freq>100)
# 
# z <- factor(z$., levels = z$.[order(z$Freq, decreasing = T)])

new_meteors <- meteors %>% 
  filter(chond %in% c("chondrite", "chondrite high iron", 
                     "chondrite low iron","chondrite vlow iron",
                      "irons", "Stony_Iron","achondrite")) %>% 
  mutate(chond = factor(chond, levels=c("chondrite", "chondrite high iron", 
                                        "chondrite low iron","chondrite vlow iron",
                                        "irons", "Stony_Iron","achondrite")))

factor_pal <- colorFactor(
  palette = c("#03396c", "#6497b1", "#005b96", "#b3cde0",
              "#FF0000", "#00FF00", "#00ffff"),
  domain = levels(new_meteors$chond)
)

new_meteors %>% 
  leaflet() %>% 
  addTiles() %>% 
  addCircleMarkers(lat=~reclat,
                   lng=~reclong,
                   popup = ~paste(name,"<br>",  
                                  "Mass", kg, "kg", "<br>",
                                  "Year", year, "<br>",
                                  "Type", recclass),
                   color= ~factor_pal(chond),
                   radius=0.2) %>% 
  addLegend("bottomright", pal = factor_pal, 
            values = ~chond,
            title = "Type",
            labFormat =  labelFormat(big.mark = ""),
            opacity = 1
  )

img <- png::readPNG("Bovedy800.png")
g_pic <- rasterGrob(img, interpolate=TRUE)


z <- meteors %>% 
  select(category) %>% 
  table %>% 
  as.data.frame() %>% 
  filter(Freq>100)

z <- factor(z$., levels = z$.[order(z$Freq, decreasing = T)])

arrows <- data.frame(x=c(1979, 1988, 1997, 1999), xend=c(1979, 1988, 1997, 1999), 
                     y=c(1e2, 1e2, 50, 50), yend=c(5, 5, 1, 1))
cbPalette <- c("#03396c", "#6497b1", "#005b96", "#FFFF00",
               "#FF0000", "#00FF00", "#00ffff")

new_meteors %>% 
  filter(chond %in% levels(chond)) %>% 
  mutate(chond = factor(chond, levels = levels(chond))) %>% 
  ggplot(aes(x=year, y=kg, col=chond)) + 
  geom_jitter(aes(), alpha=0.1, size=0.5) +  
  theme_minimal() + 
  #theme(legend.position = "none") +
  scale_y_log10(labels=scales::comma, name="Mass (kg)") + 
  xlim(1950, 2010) +
  guides(colour = guide_legend(override.aes = list(size=2, alpha=1))) +
  scale_colour_manual(values=cbPalette) +
  geom_segment(data=arrows, aes(x=x, xend=xend, y=y, yend=yend), 
               col="firebrick4", size=0.5, arrow=arrow(type="open", angle=10)) +
  annotate(geom="text", x=1979, y=140, label="Yamato", col="firebrick4", cex=4) + 
  annotate(geom="text", x=1988, y=140, label="Asuka", col="firebrick4", cex=4) +
  annotate(geom="text", x=1998, y=200, label="Queen\nAlexandra\nRange ", col="firebrick4", cex=4)


meteors %>% 
  filter(grepl("Asuka", name)) %>% 
  summarise(n())

all_proportions <- meteors %>% 
  group_by(recclass) %>% 
  summarise(count=n()) %>%
  mutate(all = round(count/sum(count), 3)) %>% 
  arrange(desc(all))%>% 
  select(-count)

Asuka_proportions <- meteors %>% 
  filter(grepl("Asuka", name)) %>% 
  group_by(recclass) %>% 
  summarise(count=n()) %>%
  mutate(Asuka = round(count/sum(count), 3)) %>% 
  arrange(desc(Asuka))%>% 
  select(-count)

QAR_proportions <- meteors %>% 
  filter(grepl("Queen Alexandra Range", name)) %>% 
  group_by(recclass) %>% 
  summarise(count=n()) %>%
  mutate(QAR = round(count/sum(count), 3)) %>% 
  arrange(desc(QAR))%>% 
  select(-count)

Yamato_proportions <- meteors %>% 
  filter(grepl("Yamato", name)) %>% 
  group_by(recclass) %>% 
  summarise(count=n()) %>%
  mutate(Yamato = round(count/sum(count), 3)) %>% 
  arrange(desc(Yamato)) %>% 
  select(-count)

Yamato_table <- left_join(Yamato_proportions, all_proportions) %>% 
  left_join(., Asuka_proportions) %>% 
  left_join(., QAR_proportions)

  