
library(tidyverse)
library(lubridate)


library(ggthemes)
library(rgeos) ## needed for tidying with regions
library(broom) ## needed for tidy

library(sf)
library(viridis)

source("r/003_xy_to_latlong.R")

### Getting density for der_plots

# Get density of points in 2 dimensions.
# @param x A numeric vector.
# @param y A numeric vector.
# @param n Create a square n by n grid to compute density.
# @return The density within each square.
get_density <- function(x, y, n = 100) {
  dens <- MASS::kde2d(x = x, y = y, n = n)
  ix <- findInterval(x, dens$x)
  iy <- findInterval(y, dens$y)
  ii <- cbind(ix, iy)
  return(dens$z[ii])
}





# Getting the map shapefiles and generating the basemap -------------------

area_sf <- sf::st_read("shp/maakond_20180402.shp")
area <- rgdal::readOGR("shp/maakond_20180402.shp")

area.points <- broom::tidy(area, region = "MKOOD")


# Defining colors
cus_grey <- "#969696"
cus_dblue <- "#002c77"


# base plot with sf
base_map_sf <- ggplot() +
  geom_sf(data = area_sf,
          color = cus_dblue,
          fill = cus_grey)+
  labs(caption = "Allikad: (1) Kaardi info Eesti Maa-ametist (02.04.2018) \n (2) Hajatootjate paiknemine Elektrilevi andmestikust")+
  coord_sf()+
  theme_map()+
  theme(text = element_text(size = 32),
        plot.caption =  element_text(size= 16, color = cus_grey),
        panel.grid = element_blank())

base_map_sf

# base plot with rgdal

base_map <- ggplot() +
  geom_polygon(aes(x = long,
                   y = lat,
                   group = group),
               data = area.points,
               color = cus_dblue,
               fill = cus_grey,
               alpha = 0.5) +
  labs(caption = "Allikad: (1) Kaardi info Eesti Maa-ametist (02.04.2018) \n (2) Hajatootjate paiknemine Elektrilevi andmestikust")+
  theme_map()+
  coord_fixed()+
  theme(text = element_text(size = 32),
        plot.caption =  element_text(size= 16, color = cus_grey))

base_map


# Importing the DER station geo data --------------------------------------

# ## In case of new data, run this
# nis_der <- read_csv2(file = "data/...tootjad_15082018.csv")
# 
# nis_der_selection <- nis_der %>%
#   select(1,41,32:33,25,6:7) %>%
#   rename(operational = `Käigus alates`,
#          prod_type = `Jaama liik`,
#          voltage = `Nimipinge (V)`,
#          current = `Nimivool (A)`) %>%
#   mutate(date_length = nchar(operational)) %>%
#   mutate(operational = ifelse(date_length <8, paste0(0,operational),operational)) %>%
#   mutate(operational = as.POSIXct(strptime(operational, format = "%d%m%Y"))) %>%
#   mutate(op_year = year(operational),
#          prod_type = ifelse(is.na(prod_type) == TRUE, "Teadmata",prod_type),
#          power = voltage*current) %>%
#   filter(is.na(op_year) != TRUE) %>%
#   select(2,5:7) %>% 
#   mutate(lat = as.numeric(map2(x,y,lest_geo_lat)),
#          lng = as.numeric(map2(x,y,lest_geo_lng)))
# 
# saveRDS(nis_der_selection, "data/der_plot_data.RDS")

der_selection = readRDS("data/der_plot_data.RDS") %>% 
  mutate(op_year = lubridate::year(operational))

### plotting with NIS data

# base_map +
  
base_map+
  geom_point(data = der_selection, aes(x = y, y = x), size = 3, alpha = 0.5, color = cus_dblue)+
  # scale_color_brewer(name = "Tootja liik", palette = "Set1")+
  # annotate("text", x= 400000, y = 6610000 ,label = max_year, size = 8)+
  # labs(title = paste(max_year))+
  theme(legend.position = "bottom",
        legend.box.background = element_blank(),
        legend.key = element_blank())+
  guides(color = guide_legend(override.aes = list(size=6)))


der_plot <- der_selection %>% 
  filter(der_selection$op_year <= 2018)

## growth gif plot with gifski
library(gifski)

for (max_year in 2012:lubridate::year(Sys.Date())){
  plot <- base_map +
    geom_point(data = subset(der_selection, der_selection$op_year <= max_year),aes(x = y, y = x), size = 3, alpha = 0.5, color = cus_dblue)+
    # scale_color_brewer(name = "Tootja liik", palette = "Set1")+
    # annotate("text", x= 400000, y = 6610000 ,label = max_year, size = 8)+
    labs(title = paste(max_year))+
    theme(legend.position = "bottom",
          legend.box.background = element_blank(),
          legend.key = element_blank())+
    guides(color = guide_legend(override.aes = list(size=6)))
  
  print(plot)
  ggsave(paste0("output/der_",max_year,".png"), width = 16, height = 9)
  
}

gif_files <- list.files(path = "output/", pattern = ".png")

gifski(png_files = paste0("output/",gif_files), gif_file = "output/der.gif",
       width = 1600,
       height = 900, 
       delay = 1,
       loop = TRUE)



## density tests
base_map+
  geom_density2d(data = der_plot, aes(x = y, y = x), size = 1)+
  stat_density2d(data = der_plot, aes(x= y, y = x, fill = ..level.., alpha = ..level..), bins = 25, geom = "polygon")+
  scale_fill_gradient(low = "green", high = "red")+
  scale_alpha(range = c(0.2, 0.5), guide = FALSE)+
  theme(legend.position = "none")


## prod type gif
for (max_year in 2012:2018){
  
  der_plot <- der_selection %>% 
    filter(der_selection$op_year <= max_year)
  # calculating density for points
  
  
  density_limit <- 7*10^-11
  
  der_plot$density <- get_density(x = der_plot$x,y = der_plot$y, n = 1000)
  
  der_plot <- der_plot %>% 
    mutate(density = case_when(density > density_limit ~ density_limit,
                               TRUE ~ density))
  
  #ploting the map
  plot <- base_map +
    geom_point(data = der_plot, aes(x = y, y = x, color = density), size = 2, alpha = 0.7)+
    # scale_color_brewer(name = "Tootja liik", palette = "Set1")+
    labs(title = paste(max_year))+
    theme(legend.position = "none",
          legend.box.background = element_blank(),
          legend.key = element_blank())+
    guides(color = guide_legend(override.aes = list(size=8)))+
    scale_color_viridis()
  
  print(plot)
  ggsave(paste0("output/prod_type/der_",max_year,".png"), width = 16, height = 9)
}



gif_files <- list.files(path = "output/prod_type/", pattern = ".png")

gifski(png_files = paste0("output/prod_type/",gif_files), gif_file = "output/prod_type/der_prod.gif",
       width = 1600,
       height = 900, 
       delay = 1,
       loop = TRUE)
