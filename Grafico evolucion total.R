library("tidyverse")
library(ggplot2)
library(ggthemes)
library(hrbrthemes)
library(polAr)
library(geofaceteAR)

#Gráfico de la evolución por año de la participación. 

evolucion1 <-
  gabinetes_5 %>%
  select("anio", "genero")%>%
  print()

mujeres <- evolucion1 %>%
  filter(genero == "F") %>%
  group_by(anio) %>% 
     summarise(f = n())

hombres <- evolucion1 %>%
  filter(genero == "M") %>%
  group_by(anio) %>%
    summarise(m = n())
#uno la cantidad de mujeres y hombres por año. 
evolucion2 <- mujeres %>% left_join(hombres) %>%
  mutate(total = f+m)%>%
  mutate(prct_f = (f/total)*100) %>%
  mutate(prct_m = (m/total)*100)

#grafico de la evolucion total de las mujeres
evolucion_total_mujeres <-
ggplot(evolucion2) +
  geom_point(mapping = aes(x = anio, y = prct_f), shape = 16, size = 2, color="#69b3a2") +
  geom_line(aes(x=anio, y= prct_f), color="#69b3a2")+
  scale_x_continuous(aes(x= evolucion2$anio), breaks = c(2008, 2009, 2010, 2011, 2012,
                     2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020), 
                     labels =c("2008", "2009", "2010", "2011", "2012",
                               "2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020")) +
  ylab("% de mujeres") +
  xlab("año") +
  theme_ipsum_rc()

#geofacet de la evolucion de mujeres por prov

show_arg_codes()
get_grid(distrito = "ARGENTINA")

#tengo que separar los datos por provincias
 
##pruebo solo con PBA

pba_f <- gabinetes_5 %>%
  filter(cod_provincia == 1) %>%
  group_by(anio) %>%
  filter(genero == "F") %>%
  summarise(f = n())  

pba_m <-gabinetes_5 %>%
  filter(cod_provincia == 1) %>%
  group_by(anio) %>%
  filter(genero == "M") %>%
  summarise(m= n())

#este es el elemento que tengo que crear por provincia para poder hacer el geofacet
pba_02 <- pba_f %>% left_join(pba_m) %>%
  mutate(total = f+m)%>%
  mutate(prct_f = (f/total)*100) %>%
  mutate(prct_m = (m/total)*100) %>%
  mutate(pba_02, cod_provincia = 02) %>% 
   print()

  ggplot(pba_02) +
  geom_point(mapping = aes(x = anio, y = prct_f), shape = 16, size = 2, color="#69b3a2") +
  geom_line(aes(x=anio, y= prct_f), color="#69b3a2")+
  scale_x_continuous(aes(x= evolucion2$anio), breaks = c(2008, 2009, 2010, 2011, 2012,
                                                         2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020), 
                     labels =c("2008", "2009", "2010", "2011", "2012",
                               "2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020")) +
  ylab("% de mujeres") +
  xlab("año") +
  labs(title = "Evolución en PBA") +
  theme_ipsum_rc()

#2 CABA
  caba_f <- gabinetes_5 %>%
    filter(cod_provincia == 2) %>%
    group_by(anio) %>%
    filter(genero == "F") %>%
    summarise(f = n())  
  
  caba_m <-gabinetes_5 %>%
    filter(cod_provincia == 2) %>%
    group_by(anio) %>%
    filter(genero == "M") %>%
    summarise(m= n())
  
  caba_01 <- caba_m %>% left_join(caba_f) %>%
    replace(caba_01$f, is.na(.), 0)) 
    mutate(total = f+ m)%>%
    mutate(prct_f = (f/total)*100) %>%
    mutate(prct_m = (m/total)*100) %>%
    mutate(caba_01, cod_provincia = 01) %>% 
    print()

get_grid("ARGENTINA")

ggplot(caba_01) +
    geom_point(mapping = aes(x = anio, y = prct_f), shape = 16, size = 2, color="#69b3a2") +
    geom_line(aes(x=anio, y= prct_f), color="#69b3a2")+
    scale_x_continuous(aes(x= evolucion2$anio), breaks = c(2008, 2009, 2010, 2011, 2012,
                                                           2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020), 
                       labels =c("2008", "2009", "2010", "2011", "2012",
                                 "2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020")) +
    ylab("% de mujeres") +
    xlab("año") +
    labs(title = "Evolución en CABA") +
    theme_ipsum_rc()


#3 Catamarca

cat_f <- gabinetes_5 %>%
  filter(cod_provincia == 3) %>%
  group_by(anio) %>%
  filter(genero == "F") %>%
  summarise(f = n()) %>%
  print()
  

cat_m <-gabinetes_5 %>%
  filter(cod_provincia == 3) %>%
  group_by(anio) %>%
  filter(genero == "M") %>%
  summarise(m= n()) %>%
  print()

cat_03 <- cat_m %>% left_join(cat_f) %>%
  mutate(total = f+m)%>%
  mutate(prct_f = (f/total)*100) %>%
  mutate(prct_m = (m/total)*100) %>%
  replace(is.na(.), 0) %>% 
  mutate(cat_03, cod_provincia = 03) %>% 
  print()


ggplot(cat_03) +
  geom_point(mapping = aes(x = anio, y = prct_f), shape = 16, size = 2, color="#69b3a2") +
  geom_line(aes(x=anio, y= prct_f), color="#69b3a2")+
  scale_x_continuous(aes(x= evolucion2$anio), breaks = c(2008, 2009, 2010, 2011, 2012,
                                                         2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020), 
                     labels =c("2008", "2009", "2010", "2011", "2012",
                               "2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020")) +
  ylab("% de mujeres") +
  xlab("año") +
  labs(title = "Evolución en Catamarca") +
  theme_ipsum_rc()
