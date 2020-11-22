
#mapa de calor segun proporción de mujeres que obtienen puestos ministeriales. 

library(polAr)
library(tidyverse)
library(geofaceteAR)
library(ggplot2)
library(hrbrthemes)
library(sf)
library(leaflet)

#traigo el mapa de polar
geo_provincias <- get_geo(geo = "ARGENTINA", level = "provincia") %>% 
  print()
#selecciono variables
arg_deptos <- get_geo(geo = "ARGENTINA") %>% 
  as_tibble() %>% 
  select(codprov, provincia) %>% 
  unique()

geo_test <- geo_provincias %>% 
  as_tibble() %>% 
  left_join(arg_deptos) %>% 
  select(provincia, codprov, geometry) %>% 
  st_as_sf %>% 
  print()

#tengo que agregar la base con las proporciones por provincia y unirla a la base geo_test


por_provincia <-gabinetes_5 %>%
  select(cod_prov, prct_f, gobernadora) %>%
  rename(codprov = cod_prov)


geo_test2 <- geo_test %>%
  left_join(por_provincia) %>%
  print()

#traigo una tabla con coordenadas de provincias que tuvieron/tienen gobernadoras para hacer un geom_point

geo_puntos <- read_excel("gabinetes_5.xlsx") 

geo_puntos %>%
st_as_sf(geo_puntos$lat, geo_puntos$long) 

#hago el data frame manualmente

geo_puntos2 <- data.frame(longitude = c(-57.9881898,
                                        -65.8101795,
                                        -63.0351591,
                                        -66.3797061,
                                        -69.3417889,
                                        -64.3370889,
                                        -68.3728429
                          ), latitude = c(-34.9205233,
                                          -28.4644541,
                                          -40.8249538,
                                          -33.2974938,
                                          -51.6263212,
                                          -27.8015453,
                                          -54.8067441
                          )) %>%
  print()


ggplot() +
  geom_sf(data = geo_test2, aes(fill= geo_test2$prct_f)) +
  geom_point(aes(x = geo_puntos2$longitude, y = geo_puntos2$latitude), size = 4, 
             shape = 21, fill = "violet") + 
  labs(x = "", y = "") +
  scale_fill_viridis_c(trans = "sqrt", alpha = .4) +
  theme_minimal()+
  theme(axis.text.y = element_blank(), axis.text.x=element_blank(),
        panel.grid = element_blank(),  plot.title = element_text("Porcentaje de mujeres por provincia"), plot.subtitle = element_text(2008-2020))  
