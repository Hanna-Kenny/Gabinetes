#prueba de gráficos con distribució por tipo de ministerio. 

library("tidyverse")
library(ggplot2)
library(ggthemes)
library(hrbrthemes)
library(polAr)
library(hrbrthemes)

(gabinetes_5)

prestigio <-gabinetes_5 %>%
  select(cod_provincia, anio, genero, categoria, Prestigio) %>%
  print()

prestigio_f <- prestigio %>%
  filter(genero == "F") %>%
  group_by(Prestigio) %>% 
  summarise(nuevaf = n())

prestigio_m <- prestigio %>%
  filter(genero == "M") %>%
  group_by(Prestigio) %>% 
  summarise(nuevam = n()) %>%
  print()
    
prestigio_2 <- prestigio_m %>% left_join(prestigio_f) %>%
  mutate(total = nuevaf+nuevam)%>%
  mutate(prct_f = (nuevaf/total)*100) %>%
  mutate(prct_m = (nuevam/total)*100) %>%
  print()
  

ggplot(prestigio_2, aes(x= prestigio_2$Prestigio, y= prestigio_2$prct_f )) + 
  geom_bar(position="dodge", stat="identity", color="#69b3a2") +
  theme_ipsum()
  
ggplot(prestigio_2, aes(x= prestigio_2$Prestigio, y= prestigio_2$prct_m )) + 
  geom_bar(position="dodge", stat="identity", color="#69b3a2") +
  theme_ipsum()

  
