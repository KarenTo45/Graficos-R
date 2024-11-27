# Librerias ====================================================================
install.packages("ggExtra")
install.packages("DescTools")
install.packages("ggspatial")
install.packages("viridis")
library(tidyverse)
library(ggExtra)
library(sf)
library(readr)
library(DescTools)
library(ggspatial)
library(viridis)
library(lubridate)
# Datos ========================================================================

setwd("C:/Users/RICARDO/Downloads/Datos-Curso-VizualizacionDatos")
dir()


# lectura de shapefile y bases de datos 
Medellin <- read_sf("BarrioVereda_2014.shp")
Bogota <- read_sf("Loca.shp") |> filter(!(LocNombre %in% c("USME", "SUMAPAZ")))
co_properties <- read.csv("co_properties.csv", fileEncoding = "latin1") 
View(co_properties)



# Boxplot =====================================================================
ggplot(data = co_properties, aes(x = Zona, y = log(Precio), color = Zona)) +
  geom_boxplot() +
  facet_wrap(~ Ciudad, scales = "free_x") + 
  labs(title = "Distribuci?n del Logaritmo del Precio por Zona y Ciudad",
       x = "Zona",
       y = "Log(Precio)") +
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")

# Grafico de barras ============================================================
ggplot(data = co_properties, aes(x = Zona, y = Precio, fill = Zona)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ Ciudad, scales = "free_x") + 
  labs(title = "Distribución del Precio por Zona y Ciudad",
       x = "Zona",
       y = "Precio") +
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")
# Grafico de torta =============================================================

co_properties_s <- co_properties |>
  group_by(Ciudad, Zona) |>
  summarise(Total_Precio = sum(Precio, na.rm = TRUE))

ggplot(data = co_properties_s |> 
         filter(Ciudad == "Bogotá D.C"), aes(x = "", y = Total_Precio, fill = Zona)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +  
  labs(title = "Distribución del Precio por Zona en Bogotá",
       x = NULL, y = NULL) +
  theme_bw()

# Serie de tiempo ==============================================================
meds <- co_properties |>
  filter(Ciudad == "Medellín") |>
  group_by(Año,Mes, Zona) |>
  summarise(Media = mean(Precio, na.rm = TRUE))


ggplot(data = meds, aes(x = Mes, y = Media, color = Zona)) +
  geom_line() +  
  labs(title = "Serie Temporal del Precio Promedio por Zona",
       x = "Fecha",
       y = "Precio Promedio") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  



bogs <- co_properties |>
  filter(Ciudad == "Bogotá D.C") |>
  group_by(Año,Mes, Zona) |>
  summarise(Media = mean(Precio, na.rm = TRUE))


ggplot(data = bogs, aes(x = Mes, y = Media, color = Zona)) +
  geom_line() +  # Dibuja la l?nea para cada zona
  labs(title = "Serie Temporal del Precio Promedio por Zona",
       x = "Fecha",
       y = "Precio Promedio") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  



# Plot Density =================================================================
ggplot(data = co_properties , aes(x = Precio, fill = Ciudad, color = Ciudad)) +
  geom_density(alpha = 0.5) +  
  labs(title = "Distribuci?n de la Densidad del Precio por Zonz",
       x = "Precio",
       y = "Densidad") +
  theme_minimal() +
  theme(legend.position = "top") 

ggplot(data = co_properties |> filter(Ciudad == "Bogotá D.C"), aes(x = Precio, fill = Zona, color = Zona)) +
  geom_density(alpha = 0.5) + 
  labs(title = "Distribuci?n de la Densidad del Precio por Zona",
       x = "Precio",
       y = "Densidad") +
  theme_minimal() +
  theme(legend.position = "top") 

ggplot(data = co_properties |> filter(Ciudad == "Bogot? D.C"), aes(x = log(Precio), fill = Tipo_Propiedad,
                                                                   color = Tipo_Propiedad)) +
  geom_density(alpha = 0.5) + 
  labs(title = "Distribuci?n de la Densidad del Precio por tipo de propiedad",
       x = "Precio",
       y = "Densidad") +
  theme_minimal() +
  theme(legend.position = "top") 


ggplot(co_properties |>
         filter(Ciudad == "Bogotá D.C"), aes(log(Precio))) + 
  geom_histogram(aes(fill = Zona), binwidth = 0.1, position = "fill",
                 na.rm = TRUE) +
  xlim(15, 24) +
  theme_bw()

# Grafico de puntos ===========================================================

ggplot(data = co_properties |>
         filter(Ciudad == "Medellín"), aes(x = log(Precio), y = log(Superficie), color = Tipo_Propiedad)) +
  geom_point() +
  theme_bw()


# Espacial =====================================================================
ggplot(data = Medellin) +
  geom_sf() +
  geom_point(data = co_properties |> filter(Ciudad == "Medellín"), aes(x = lon, y = lat), size = .4,
             col = "red") + theme_bw() +
  annotation_scale(location = "bl", width_hint = 0.4) +
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(0.0, "in"), pad_y = unit(0.2, "in"),
                         style = north_arrow_fancy_orienteering)

ggplot(data = Medellin) +
  geom_sf() +
  geom_point(data = co_properties |> filter(Ciudad == "Medellín"),
             aes(x = lon, y = lat, color = Precio),  
             size = 1, alpha = 0.7) + 
  theme_bw() +
  annotation_scale(location = "bl", width_hint = 0.4) +
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(0.0, "in"), pad_y = unit(0.2, "in"),
                         style = north_arrow_fancy_orienteering) +
  scale_color_viridis(option = "magma", trans = "log")  


ggplot(data = Bogota) +
  geom_sf() +
  geom_point(data = co_properties |> filter(Ciudad == "Bogotá D.C"), aes(x = lon, y = lat), size = .4,
             col = "red") + theme_bw() +
  annotation_scale(location = "bl", width_hint = 0.4) +
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(0.0, "in"), pad_y = unit(0.2, "in"),
                         style = north_arrow_fancy_orienteering)


ggplot(data = Bogota) +
  geom_sf() +
  geom_point(data = co_properties |> filter(Ciudad == "Bogot? D.C"),
             aes(x = lon, y = lat, color = Precio),  
             size = 1, alpha = 0.7) + 
  theme_bw() +
  annotation_scale(location = "bl", width_hint = 0.4) +
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(0.0, "in"), pad_y = unit(0.2, "in"),
                         style = north_arrow_fancy_orienteering) +
  scale_color_viridis(option = "magma", trans = "log")  



