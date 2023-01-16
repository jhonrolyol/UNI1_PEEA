#=================================================
#     TRABAJO FINAL MÓDULO 1: Visualización de datos
#=================================================
#Instalando librerías
install.packages("ggplot2") #paquete dedicado exclusivamente a la visualización de datos
install.packages("dplyr") #tratamiento de datos
install.packages("gganimate") #paquete integrado de animaciones
install.packages("tidyverse")  #nos ayuda a hacer filtros explorar dataset
install.packages("plotly") #crea gráficos interactivos
install.packages("gifski") #soporte gráfico animación
install.packages("png") #soporte gráfico animación
install.packages("gapminder") 
install.packages("dslabs") 
#----------------------------------------------------------------------
#Cargando librerías
library(gapminder)
library(tidyverse)
library(ggplot2)


names(gapminder) # ver las variables 
head(gapminder) # ver información adicional
df <- gapminder #asignando
#=================================================
#      Introducción a la paqueteria de "ggplot2"
#=================================================
#1.Scatterplot
ggplot(filter(df, year == 2007), aes(x = gdpPercap, y = lifeExp)) +
  geom_point(colour = "#fc6721") +
  scale_x_log10(labels = scales::dollar) +
  labs(title = "Gráfico de dispersión",
       subtitle = "Relación entre la esperanza de vida e ingresos, 2007",
       caption = "Fuente: Gapminder.org" ,
       x = "PBI per capita ($)",
       y = "Años (years)") 


#2.Scatterplot
ggplot(filter(df, year == 2007), aes(x = gdpPercap, y = lifeExp)) +
  geom_point(aes(colour=continent)) +
  scale_x_log10(labels = scales::dollar) +
  labs(title = "Gráfico de dispersión",
       subtitle = "Relación entre la esperanza de vida e ingresos, 2007",
       caption = "Fuente: Gapminder.org" ,
       x = "PBI per capita ($)",
       y = "Años (years)") +
  theme_bw()
  

#Varios gráficos en una figura (comparaciones)--> facet_grap
df %>% 
  filter(continent != "Oceania") %>% 
  ggplot( aes(x = gdpPercap, y = lifeExp, color = continent)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~ continent)

#Gráfica de series de tiempo con lineas 
#a.¿Qué países han crecido más en en el tiempo? 
top5_countries <- df %>% 
  select(continent,year, country, gdpPercap) %>%
  filter(year %in% c("1997", "2007"))%>%
  pivot_wider(names_from=year, values_from = gdpPercap) %>% 
  mutate(gdp_difference = `2007` - `1997`) %>% 
  top_n(5,gdp_difference)

top5_countries 

top_countries <- top5_countries$country
top_countries

df %>% filter(country %in% top_countries) %>% 
  ggplot(aes(x = year, y = gdpPercap, col = country))+
  geom_line(size = 1) +
  labs(title = "Top 5 de paises con mayor crecimiento poblacional",
       caption = "Fuente: Gapminder.org" ,
       x = "Población (mll)",
       y = "Años")
  

# Gráfica de series de tiempo con puntos
df %>% 
  filter(country == "Peru") %>% 
  ggplot(aes(year, gdpPercap)) +
  geom_point(aes(size = gdpPercap)) +
  labs(title = "Gráfico de dispersión",
       subtitle = "PBI per capita, Perú",
       caption = "Fuente: Gapminder.org" ,
       x = "Años",
       y = "PBI per capita ($)")
 

#Gráfica de series de tiempo con lineas
df %>% 
  filter(country == "Peru" ) %>% 
  ggplot(aes(year, gdpPercap)) +
  geom_line(size=1) +
  labs(title = "Grafico de lineas",
       subtitle = "PBI per capita, Perú",
       caption = "Fuente: Gapminder.org" ,
       x = "Años",
       y = "PBI per capita ($)") +
      theme_classic()


#Gráfico de barras
ggplot(df, aes(x=continent, fill=continent)) + 
  geom_bar(aes(y=..count../12)) +
  guides(fill=FALSE) +
  labs(title = "Diagrama de barras",
       subtitle = "Número de paises según continente",
       caption = "Fuente: Gapminder.org" ,
       x = "Continente",
       y = "Cantidad de paises") +
  theme_minimal()


#(horizontal)
b <- df %>%
  filter(year == 2007) %>%
  group_by(continent) %>%
  summarise(median = median(gdpPercap))

ggplot(b, aes(reorder(continent, -median,sum), median)) +
  geom_col(fill = "#fc6721", alpha = 0.8) +
  scale_y_continuous(expand = c(0, 0), labels = scales::dollar) +
  coord_flip() +
  labs(title = "PBI Per Capita: promedio por continente, 2007",
       caption = "Fuente: Gapminder.org",
       x = NULL,
       y = "PBI per capita",
       fill = NULL) +
  theme(panel.grid.major.y = element_blank())


#Grafico de densidad  (geom_density)
ggplot(filter(df, year == 2007 & continent != "Oceania"), aes(x = lifeExp)) + 
  geom_density(aes(fill = continent), size = 0.1, alpha = 0.5) +
  scale_fill_brewer(palette = "Set2") +
  labs(title =  "Distribución esperanza de vida, 2007",
       x = "Años (years)",
       y = "",
       fill = NULL) +
  theme(panel.grid.major.x = element_blank())

#boxplot
gap1 <- ggplot(df, aes(x=continent, y=lifeExp, fill=continent))
gap1 +
  geom_boxplot(outlier.size=2)+
labs(title = "Boxplot",
    subtitle = "Distribución esperanza de vida, 2007",
    caption = "Fuente: Gapminder.org" ,
    x = "Continente",
    y = "Años (years)")

#--------------------------------------------

#Extra:

cg <- gapminder %>% 
  filter(year == 2007 & continent == "Europe") %>% 
  arrange(gdpPercap) %>% 
  mutate(country = factor(country, levels = country))

ggplot(cg, aes(x = gdpPercap, y = country)) +
  geom_segment(aes(x = 0, xend = gdpPercap, y = country, yend = country), colour = "#f0f0f0") + 
  geom_point(colour = "#fc6721", size = 3, alpha = 0.8) +
  scale_x_continuous(expand = c(0, 0), limits = c(0, max(df$gdpPercap) * 1.1),
                     labels = scales::dollar) +
  labs(title = "Lollipop Chart",
       subtitle = "PBI per capita en paises de Europa, 2007",
       caption = "Source: Gapminder.org ",
       x = NULL, 
       y = NULL, 
       fill = NULL) +
  theme(panel.grid.major = element_blank(),
        axis.text.y = element_text(hjust = 0))

#=================================================
#         gráfico espacial
#=================================================
install.packages("rnaturalearth")
install.packages("RColorBrewer")
install.packages("sf")

jf <- gapminder %>%
  filter(year == 2007) %>%
  left_join(country_codes) %>% 
  rename("iso_a3" = "iso_alpha")

library(rnaturalearth)
world <- ne_countries(type = "countries",  returnclass = 'sf')
sf <- ne_countries(type = "countries",  returnclass = 'sf') %>% 
  left_join(., jf, by = "iso_a3", sort = FALSE) %>% 
  filter(!is.na(country)) %>% 
  select("country", "continent" = "continent.y", "year", "lifeExp", "pop", "gdpPercap", "geometry")

library(sf) ; library(RColorBrewer)
ggplot(sf, aes(fill = lifeExp)) +
  geom_sf(data = world, fill = "#f0f0f0", colour = "white") +
  geom_sf(alpha = 0.8, colour = "white", size = 0.1) +
  scale_fill_gradientn(colours = brewer.pal(5, "Oranges"),
                       name = "Age (Years)",
                       guide = guide_colourbar(
                         direction = "horizontal",
                         barheight = unit(2, units = "mm"),
                         barwidth = unit(50, units = "mm"),
                         title.position = 'top',
                         title.hjust = 0.5,
                         label.hjust = 0.5)) +
  labs(title = "Gráfico Espacial",
       subtitle = "Esperanza de vida, 2007",
       caption = "Source: Gapminder.org",
       x = NULL, 
       y = NULL) +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = "bottom") +
  coord_sf(crs = "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000
           +datum=WGS84 +units=m +no_defs",
           datum = NA)

#=================================================
#         Gráficos animados con  "gganimate"
#=================================================
library(gifski)
library(png)
library(gganimate)

gap2 <- ggplot(data=gapminder, aes(x=continent, y=lifeExp, fill=continent))
anim <- gap2 +
  geom_boxplot(outlier.size=2) +
  transition_time(year)


animate(anim,renderer = gifski_renderer(),
        height=500,width=800)
anim_save("C:/Users/gimen/OneDrive - Universidad Nacional Federico Villareal/Documentos/animacion.gif")
# Se pone ruta de la carpeta y archivo terminado en ".gif"
