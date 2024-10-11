library(tidyverse)
#Con data(starwards) crea el data directamente, ya que está dentro de la libreria y no necesita usar<-
data(starwars) 
#Para seleccionar todas las columnas excepto el nombre, $el primer data
starwars %>% select(-name)
#Para seleccionar las columnas que contengan _
starwars %>% select(contains("_"))
#Para seleccionar las columnas que empiezan con "s"
starwars %>% select(starts_with("s"))
#Crear un data frame con los nombres y planeta de origen (homeworld), con esto se guarda todo bajo el nombre de homeworld
#para el data homeworld (segundo data)
homeworld <- starwars %>% select(name, homeworld)
#Filtrar datos 
#Filtrar por especies: sólo humanos;  == es igual que
human <- starwars %>% filter(species == "Human")
#Filtrar por especies: sólo humanos del planeta Tatooine --> aparece en la consola pero no el dataframe, ya que no tiene <-
stawars_tatooine <- starwars %>% filter(species == "Human", homeworld == "Tatooine")
#Crear un nuevo datframe con todas las especies menos los Droides
starwars_nodroids <- starwars %>% filter(species != "Droid")
#Usamos group_by y tally; todas las filas que tienen el mismo valor en la columna species serán tratadas como un grupo.
#La función tally() cuenta el número de observaciones (filas) en cada grupo creado por group_by().
#El resultado será un nuevo data frame que muestra cuántos personajes hay en cada grupo de especies.
starwars %>% group_by(species) %>% tally()
#Añadiendo otra variable
starwars %>% group_by(species, gender) %>% tally()
#Si lo quieres guardar en el environment recuerda asignarle un nombre
table_gender <- starwars %>% group_by(species, gender) %>% tally()
#na.rm=T quiere decir que elima los NA (valores No Asignados o sin datos)
starwars %>% group_by(species) %>% summarise(mean_height = mean(height, na.rm = T),mean_mass = mean(mass,na.rm = T))
#PREGUNTARtable_gender <- starwars %>% group_by(species, gender, ¿na.rm = T?) %>% tally()
#sd_y el parámetro que se quiera medir
sd_gender <- table_gender <- starwars %>% group_by(species, gender) %>% tally()
sd_height <- starwars %>% group_by(species) %>% summarise(mean_height = mean(height, na.rm = T),mean_mass = mean(mass,na.rm = T))
# con sd_gender da el mismo resultado, ya que la variable estaba already created
#Para crear graficos y modificar algunos elementos: 

#Hacer un gráfico de la altura vs. la masa de los personajes
ggplot(starwars, aes(height, mass)) + geom_point()
#Puedes modificar el color 
ggplot(starwars, aes(height, mass)) + geom_point(colour = "red")
#Modificando el color y el punto
ggplot(starwars, aes(height, mass)) + geom_point(colour = "purple", pch = 3)
#Modificando el color y el fondo 
ggplot(starwars, aes(height, mass)) + geom_point(colour = "red") + theme_light()

# Inspeccionar los personajes ordenados por masa (de mayor a menor mirando sd_height)
#es muy importante usar filter antes que summarise, sino no funcionará
starwars_wthoutheavyone <- starwars %>% group_by(species) %>% filter(mass < 1000) %>% summarise(mean_height = mean(height, na.rm = T),mean_mass = mean(mass,na.rm = T))
ggplot(starwars_wthoutheavyone, aes(mean_height, mean_mass)) + geom_point()
#Para el archivo toy, cambiar como ruta de acceso manteniendo las comillas boton derecho en descargas y cambiar orientacion barras
#tambien se puede hacer desde immport dataset
toy <- read_csv( "C:/Users/migur/Downloads/toy.csv" )
#IMC es Indice de Masa Corporal; IAS significa Indice de Alimentación Saludable; primero group by t luego summarise 
sd_toy <- toy %>% group_by(Sex) %>% summarise(mean(Height_cm),mean(Weight_Kg), mean(IMC) , mean(IAS), mean(Ccintura))
#select() se usa para seleccionar columnas, mientras que filter() se usa para seleccionar filas con base en condiciones lógicas
sd_toy_women <- toy %>% filter(Sex == "Women") %>% filter(IMC_clas == "Overweight")
#hacer un grafico
ggplot(toy, aes(IMC, Weight_Kg)) + geom_point()
#Pacientes Overweight y obesity
overw_and_obesity <- toy %>% filter(IMC > 25)
#grafico

ggplot(overw_and_obesity, aes(x = IMC, y = Weight_Kg)) + 
  geom_point()
install.packages("ape")
install.packages("phangorn")
install.packages("phytools")