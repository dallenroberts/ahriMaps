#############################################################################
## Allen Roberts
## April 2020
## Mapping AHRI bounded structures
#############################################################################

rm(list = ls())

library(sp)
library(rgdal)
library(readxl)
library(tidyverse)
library(mapproj)
library(ggmap)
library(maptools)
library(rmapshaper)
library(ggrepel)

theme_set(theme_classic())

datapath <- "~/Documents/research/data/ahri/"

## Viral load data
vlData <- read_excel(paste(datapath, 
                           "ppdv/PPDV_values (Diego Cuadros's conflicted copy 2018-06-20).xls", 
                           sep = "/"))
vlData <- vlData %>%
  rename("long" = "Longitude_",
         "lat" = "Latitude_y")

## Distinct bounded structure IDs with coordinates
bsCoords <- vlData %>%
  select(c(BSIntID, long, lat)) %>%
  distinct()

## Create provincial map
zaf1 <- readOGR(dsn = paste(datapath,
                           "shp",
                           "zaf_adm_2016SADB_OCHA_SHP",
                           sep = "/"),
               layer ="zaf_admbnda_adm1_2016SADB_OCHA")
proj4string(zaf1)

zaf1 <- ms_simplify(zaf1, keep = 0.15, keep_shapes = T)
zaf1@data$id <- rownames(zaf1@data)
zaf1Points <- fortify(zaf1, region = "id")
zaf1DF <- left_join(zaf1Points, zaf1@data, by = "id")

centroids <- as.data.frame(coordinates(zaf1))
names(centroids) <- c("long", "lat")
centroids$id <- rownames(zaf1@data)
centroids$name <- zaf1@data$ADM1_EN
centroids$abbrev <- c("WC", "EC", "NC", "FS", "KZN", "NW", "GT", "MP", "LP")

zaf1DF$isKZN <- factor(ifelse(zaf1DF$ADM1_EN == "KwaZulu-Natal", 1, 0))

provinceMap <- ggplot(data = zaf1DF) +
  aes(x = long, y = lat, group = group) + 
  geom_polygon(aes(fill = isKZN)) +
  scale_fill_manual(values = c(colors()[222], colors()[600]), guide = FALSE) +
  geom_path(color = "black", size = 0.1) +
  geom_text_repel(data = centroids, aes(label = abbrev, group = id), 
                  fontface = "bold", 
                  seed = 104,
                  size = 1) +
  coord_map(projection = "mercator") +
  theme_void()

## Delineate AHRI wards
zaf4 <- readOGR(dsn = paste(datapath,
                            "shp",
                            "zaf_adm_2016SADB_OCHA_SHP",
                            sep = "/"),
                layer ="zaf_admbnda_adm4_2016SADB_OCHA")

ahriWards <- subset(zaf4, ADM3_EN == "Mtubatuba" & as.numeric(ADM4_EN) %in% c(1:2, 6, 13:20))

ahriWardsCoarse <- ms_simplify(ahriWards, keep = 0.15, keep_shapes = T)
ahriWardsCoarse@data$id <- rownames(ahriWardsCoarse@data)
ahriWardsCoarsePoints <- fortify(ahriWardsCoarse, region = "id")
ahriWardsCoarseDF <- left_join(ahriWardsCoarsePoints, ahriWardsCoarse@data, by = "id")

provinceMapAHRI <- provinceMap +
  geom_polygon(data = ahriWardsCoarseDF, fill = colors()[490]) +
  geom_path(data = ahriWardsCoarseDF, color = colors()[490])

## Zoomed-in AHRI map
ahriWardsFine <- ms_simplify(ahriWards, keep = 0.50, keep_shapes = T)
ahriWardsFine@data$id <- rownames(ahriWardsFine@data)
ahriWardsFinePoints <- fortify(ahriWardsFine, region = "id")
ahriWardsFineDF <- left_join(ahriWardsFinePoints, ahriWardsFine@data, by = "id")

ahriMap <- ggplot(data = ahriWardsFineDF) +
  aes(x = long, y = lat, group = group) + 
  geom_polygon(fill = "white") +
  # geom_polygon(fill = colors()[16]) +
  geom_path(color = "black") +
  coord_map(projection = "mercator") +
  theme_void() +
  theme(plot.background = element_rect(fill = "lightgray"))

ahriMapBS <- ahriMap + 
  geom_point(data = bsCoords, aes(x = long, y = lat, group = BSIntID), size = 0.05, alpha = 0.3)

## Plot of bounded structures with no admin boundaries
bsCoordsF <- fortify(bsCoords)
bsPlotMerc <- ggplot(data = bsCoordsF, aes(x = long, y = lat)) +
  coord_map(projection = "mercator") +
  geom_point(alpha = 0.5, size = 0.01) +
  theme_void()

## Save maps
ggsave("provinceMapAHRI.pdf", provinceMapAHRI, "pdf", path = "maps", width = 3, height = 3, units = 'in')
ggsave("ahriMapBS.pdf", ahriMapBS, "pdf", path = "maps", width = 3, height = 3, units = 'in')
ggsave("BSMapNoBoundary.pdf", bsPlotMerc, "pdf", path = "maps", width = 3, height = 3, units = 'in')

###############################################################################
## Notes
###############################################################################
## Admin 2 boundaries
# subset(zaf3, ADM2_EN %in% c("Umkhanyakude",
#                                    "Uthungulu"))

# Admin 3 boundaries - most are in Mtubatuba
# subset(zaf4, ADM3_EN %in% c("Big Five Hlabisa",
#                                    "Mtubatuba",
#                                    "Mfolozi"))
