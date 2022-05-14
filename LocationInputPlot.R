##libraries needed
library(ggplot2)
library(sf)
library(rnaturalearth)

input_read <- readline(prompt = "Enter Name: ") ##Need to execute this line first

##Then only run these statements, so like two separate runtimes are needed
if (is.character(input_read)) {

worldmap <- ne_countries(scale = 'medium', type = 'map_units',
                         returnclass = 'sf')
# have a look at these two columns only
head(worldmap[c('name', 'continent')])

ggplot() + geom_sf(data = worldmap) + theme_bw()

location <- worldmap[worldmap$name == input_read,]
ggplot() + geom_sf(data = location) + theme_bw()
} else {
  print("Invalid keyword!")
}
