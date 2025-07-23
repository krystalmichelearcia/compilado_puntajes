rm(list = ls())

# Direccionar
setwd("C:/Users/KrystalMicheleOcampo/Documents/R/PLS/Compilación de puntajes")

# ---- Cargar las librerías necesarias ----

if(!require(pacman)){
  install.packages('pacman')
};
library(pacman)
#library(readr)
pacman::p_load(here,tidyverse, openxlsx, readr)

# ---- Crear directorios ----
##crea los directorios para base de datos y productos
#directorio de insumos
dir_insumos<- paste0(here(),'/INSUMOS')
if (!dir.exists(dir_insumos)) {
  dir.create(dir_insumos)
}
#directorio productos
dir_productos<- paste0(here(),'/PRODUCTOS')
if (!dir.exists(dir_productos)) {
  dir.create(dir_productos)
}
#Analiza los archivos en el directorio y los clasifica segun su tipo
#DF de archivos
archivos<-data.frame(archivos = list.files(here()))
#busca dentro de los archivos
for (archivo in archivos[,1]) {
  if (grepl('\\.(xlsx)$', archivo,ignore.case = TRUE)) {
    file.rename(paste0(here(), "/", archivo), paste0(dir_insumos, "/", archivo))
  }
}

# ---- Leer bases de datos ----
###Lee las bases de datos#####
bases<- data.frame(nombre = list.files(dir_insumos))  #
# data.frame vacío para acumular los datos
Concentrado_de_puntajes  <- data.frame()
for (base in bases[,1]) {
  if (grepl('\\.(xlsx)$',base, ignore.case = TRUE)) {
    nombre<-paste0(dir_insumos,'/', base)
    base1<-readxl::read_xlsx(paste0(nombre)) 
    #assign(paste0(base),base1)
    Concentrado_de_puntajes <- rbind(Concentrado_de_puntajes, base1)  # Combina los datos #bind_rows(M, base1)
  };
}

# ---- Limpieza para crear concentrado de puntajes ----
Concentrado_de_puntajes <- Concentrado_de_puntajes %>%
  select(-1) %>%  # Seleccionar todas las columnas excepto la primera
  mutate(across(c( # Convertir columnas a numérico para poder hacer cálculos
    "% Cumplimiento", "% Mejora",
    "Didáctica en los campos disciplinares...12",
    "Evaluación y retroalimentación del proceso de aprendizaje...13",
    "Intervención educativa...14"
  ), as.numeric)) %>%
  mutate(`% Global` = `% Cumplimiento` + `% Mejora`) %>%  # Sumar % de Cumplimiento y Mejora
  drop_na() %>% #Eliminar filas con valos de NA
  add_count(`Id AV`, name = "Número de asignaciones") %>% # Añadir núm. de asignaciones por ID AV
  select(`Id AV`, `Nombre AV`, `Número de asignaciones`, everything()) #Ordenar

# ---- Calcular promedios y organizarlos en una tabla ----
# Agrupamos por Id AV y Nombre AV, y luego calculamos el promedio de las columnas seleccionadas
Concentrado_de_promedio_de_puntajes  <- Concentrado_de_puntajes %>%
  group_by(`Id AV`, `Nombre AV`, `Número de asignaciones`) %>% # Agrupar por Nombre AV
  summarise(across( # y calcular promedio de estas variables
    c(`% Cumplimiento`, 
      `Didáctica en los campos disciplinares...8`,
      `Evaluación y retroalimentación del proceso de aprendizaje...9`,
      `Intervención educativa...10`, `% Mejora`,
      `Didáctica en los campos disciplinares...12`,
      `Evaluación y retroalimentación del proceso de aprendizaje...13`,
      `Intervención educativa...14`, `% Global`),
    ~ round(mean(.x, na.rm = TRUE), 2), # Redondear
    .names = "Promedio de {col}" #Asignar nombres
  )) %>%
  ungroup()  # Desagrupar para evitar problemas en próximos cálculos

# ---- Guardar archivos ----
archivos_to_save <- list(`Concentrado de promedio de puntajes` = Concentrado_de_promedio_de_puntajes, `Concentrado de puntajes` = Concentrado_de_puntajes)

for (archivo_to_save in names(archivos_to_save)) {
  write.xlsx(archivos_to_save[[archivo_to_save]], here(dir_productos, paste0(archivo_to_save, ".xlsx")))
}
