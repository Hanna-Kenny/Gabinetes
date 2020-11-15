#pruebo el mismo gráfico de barras de prestigio pero con la cat feminizado

library("tidyverse")
library(ggplot2)
library(ggthemes)
library(hrbrthemes)
library(polAr)
library(hrbrthemes)
install.packages("patchwork")
library(patchwork)

(gabinetes_5)

femi <-gabinetes_5 %>%
  select(cod_provincia, anio, genero, categoria, Feminizado) %>%
  print()

femi_f <- femi %>%
  filter(genero == "F") %>%
  group_by(Feminizado) %>% 
  summarise(nuevaf = n())

femi_m <- femi %>%
  filter(genero == "M") %>%
  group_by(Feminizado) %>% 
  summarise(nuevam = n()) %>%
  print()

femi_2 <- femi_m %>% left_join(femi_f) %>%
  mutate(total = nuevaf+nuevam)%>%
  mutate(prct_f = (nuevaf/total)*100) %>%
  mutate(prct_m = (nuevam/total)*100) %>%
  print()


plot_ff <- ggplot(femi_2, aes(x= femi_2$Feminizado, y= femi_2$prct_f )) + 
  geom_bar(position="dodge", stat="identity", color="#69b3a2") +
  ylab("Porcentaje") + 
  xlab("Categoría") +
  labs(title = "% mujeres") +
  theme_ipsum() 

plot_fm <- ggplot(femi_2, aes(x= femi_2$Feminizado, y= femi_2$prct_m )) + 
  geom_bar(position="dodge", stat="identity") +
  ylab("Porcentaje") + 
  xlab("Categoría") +
  labs(title = "% hombres")+
  theme_ipsum()

unificado_prestigio <- plot_ff + plot_fm #atención a la escala!

