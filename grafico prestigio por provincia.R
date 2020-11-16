library(polAr)
library(tidyverse)
library(geofaceteAR)
library(ggplot2)
library(hrbrthemes)

prestigio_f <- data_para_geofacet
prestigio_f %>% as_tibble() %>%
  select("prestigio", "name_es", "prct_mujeres")

prueba_bars <-prestigio_f %>%
  filter(name_es == "Buenos.Aires")

ggplot(prueba_bars, aes(x = prct_mujeres, y = prestigio)) +
  geom_bar(stat = "identity")

           
ggplot(prestigio_f, aes(x = prct_mujeres, y = prestigio)) +
  geom_bar(stat = "identity", fill= "#69b3a2", alpha=.6) +
  facet_wrap(. ~ name_es) +
  xlab("porcentaje de mujeres") +
  ylab("Prestigio") +
  labs(title = "Distribución según nivel de prestigio", caption = "Porcentaje según prestigio sobre total de mujeres asignadas por provincia")
  
    

    