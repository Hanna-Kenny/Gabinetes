install.packages("tidyverse")
library(tidyverse)
install.packages("janitor")
library(janitor)

gabi2014 <- clean_names(LAQUEVA_2008_2014)

gabinetes2014 <-select(gabi2014, "year", "province", "provincecode", "gobernadore", "partido", "division", "sex", "name1_9")


#tengo que separar la columna names1_9 en partes. Puedo separar por espacios y dejar primer nombre y apellido por separado y las restantes borrarlas.   

gabinetes2014 %>% str_split(name1_9, regex([:punct:]), n = Inf, simplify = FALSE)

#Con este saqué los nombres
gabinetes2014$name2 = str_extract(gabinetes2014$name1_9, "(?<=\\s)[[:alpha:]]+" )

#Estos no funcionaron
#gabinetes2014$surname = str_extract(gabinetes2014$name1_9,"(?<=\\s)[:upper:]+")
#gabinetes2014$surname2 = str_extract_all(gabinetes2014$name1_9, "\\w+", simplify = FALSE)

#Con este saqué el apellido
gabinetes2014$surname3 = word(gabinetes2014$name1_9, -1)

gabinetes_1 <-
  select(gabinetes2014, "year", "province", "provincecode", "gobernadore", "partido", "division", "sex", "name2", "surname3")
