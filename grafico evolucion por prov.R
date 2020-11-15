#Geofacet

library(polAr)
library(tidyverse)
library(geofaceteAR)
library(ggplot2)
library(hrbrthemes)

get_grid("ARGENTINA")



data_geo <-
  select(data_para_geofacet, "anio", "name_es", "prct_f") %>%
  print() %>%
  mutate(prct_f = prct_f*100) %>%
  print()

argentina_grid2

evol_por_provincia <-ggplot(data_geo) +
  geom_point(mapping = aes(x = anio, y = prct_f), shape = 16, size = 0.5, color="#69b3a2") +
  geom_line(aes(x=anio, y= prct_f), color="#69b3a2")+
  scale_x_continuous(aes(x = data_geo$anio), breaks = c(2008, 2010, 2012, 2014, 2016, 2018, 2020), 
                     labels =c("2008", "2010", "2012",
                               "2014", "2016", "2018", "2020")) + 
  facet_wrap(. ~ name_es) +
  xlab("Año") + 
  ylab("Porcentaje de mujeres")


