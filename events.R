# Code by yabellini (Yanina Bellini Saibene) for IWD2020

# API KEY de Meetup
Sys.setenv(MEETUP_KEY = "your meetup key")

# Paquetes necesarios para trabajar
# Packages
library(meetupr)
library(purrr)
library(dplyr)
library(tidyr)
library(lubridate)

library(ggplot2)

# Función para que no de error de time out para la llamadas al método de meetupr (por Jenny Biran)
# Function so that no time out error for calls to meetupr method (by Jenny Brian)

slowly <- function(f, delay = 0.5) {
  
  function(...) {
    
    Sys.sleep(delay)
    
    f(...)
    
  }
  
}


# Obteniendo todos los grupos de R-Ladies
# Getting all the R-Ladies groups
all_rladies_groups <- find_groups(text = "r-ladies")

# Cleanup
rladies_groups <- all_rladies_groups[grep(pattern = "rladies|r-ladies", 
                                          x = all_rladies_groups$name,
                                          ignore.case = TRUE), ]

# Obtengo todos los eventos ya realizados. 
# I get all the events already done.
eventos <- rladies_groups$urlname %>%
  map(slowly(safely(get_events)), event_status='past') %>% transpose()

# En eventos me queda una lista con todos los datos de los eventos: nombre, fecha, lugar, descripción, 
# y varios datos más.  
# Por el momento la lista tiene dos elementos: una lista con los resultados correctos y otra con los errores.  
# Da error cuando no hay eventos pasados en el grupo.

# Sólo me interesa la información que tenemos en resultados. Asi que me quedo con esa información
# La lista de resultados tiene un tibble por grupo con los eventos realizados para ese grupo
# Voy a armar un solo tibble con todos los eventos juntos

# In events I have a list with all the event data: name, date, place, description,
# and several more data.
# At the moment the list has two elements: a list with the correct results and another with the errors.
# It gives error when there are no past events in the group.

# I am only interested in the information we have in results. So I keep that information
# The results list has one tibble per group with the events held for that group
# I'm going to put together a single tibble with all the events together

# Creo un vector lógico con los eventos donde hay error
# I create a logical vector with events where there is error

eventos_con_datos <- eventos$result %>% 
  map_lgl(is_null)

# Filtro los eventos correctos con el vector lógico anterior y luego uno todos los tibble
# por sus filas en uno solo

# Filter the correct events with the previous logical vector and then bind all the tibbles
# by their rows in one data set

eventos_todos_juntos <- eventos$result[!eventos_con_datos] %>% 
  map_dfr(~ .) 

#Saco el campo resources
#Remove the resources field

eventos <- eventos_todos_juntos %>%
  select(-resource) %>%
  as_tibble()

# Exporto los datos
# Write the data
write.csv(eventos, "eventsRLadiesUntilJanuary2020.csv")
saveRDS(eventos_todos_juntos, "eventsJanuary2019.rds")

# Cuento y grafico la cantidad de eventos realizados por año
# I count and plot the number of events held per year

eventos_todos_juntos %>%
  group_by(year(time)) %>%
  summarise(cantidad=n()) %>%
  ggplot(aes(y=cantidad, x=`year(time)`)) +
  geom_col(aes(fill=`year(time)`))

# Cantidad de RSVP positivos por año
# Amount of positive RSVP per year

eventos_todos_juntos %>%
  group_by(year(time)) %>%
  summarise(asistentes=sum(yes_rsvp_count)) %>%
  ggplot(aes(y=asistentes, x=`year(time)`)) +
  geom_col(aes(fill=`year(time)`))

