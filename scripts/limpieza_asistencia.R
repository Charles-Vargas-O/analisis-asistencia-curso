# =================================================
# Limpieza y análisis de asistencia a sesiones
# Evaluación técnica - Analista de Datos
# =================================================

# -------------------------------------------------
# Cargar librerías necesarias
# -------------------------------------------------

library(tidyverse)
library(readxl)
library(stringi)
library(stringr)

# -------------------------------------------------
# Selección y carga del archivo de datos
# Se utiliza file.choose() para permitir seleccionar
# manualmente el archivo Excel que contiene la base
# de datos proporcionada para la evaluación.
# -------------------------------------------------

datos <- read_excel(file.choose())

# -------------------------------------------------
# Limpieza inicial
# Reemplazar valores NA en las columnas de asistencia
# por 0 para facilitar el cálculo posterior
# -------------------------------------------------

datos <- datos %>%
  mutate(across(starts_with("asistencia"), ~replace_na(., 0)))

# -------------------------------------------------
# Normalización de nombres y correos
# Se estandarizan los correos en minúsculas y se
# construye una clave basada en el nombre completo
# para facilitar la identificación de duplicados
# -------------------------------------------------

datos <- datos %>%
  mutate(
    email_clean = str_to_lower(str_trim(Email)),
    
    nombre_key = paste(Nombre, `Primer apellido`, `Segundo apellido`) %>%
      str_to_lower() %>%
      str_squish()
  )

# -------------------------------------------------
# Creación de identificador único
# Si el correo no está disponible se utiliza la
# clave generada a partir del nombre
# -------------------------------------------------

datos <- datos %>%
  mutate(
    id_participante = if_else(
      email_clean == "" | is.na(email_clean),
      nombre_key,
      email_clean
    )
  )

# -------------------------------------------------
# Consolidación de participantes únicos
# Se agrupan los registros por participante y se
# toma el valor máximo de asistencia por sesión
# para considerar asistencia en cualquiera de las
# bases originales
# -------------------------------------------------

participantes <- datos %>%
  group_by(id_participante) %>%
  summarise(
    Nombre = first(Nombre),
    apellido1 = first(`Primer apellido`),
    apellido2 = first(`Segundo apellido`),
    Email = first(Email),
    
    ses1 = max(asistencia_ses1),
    ses2 = max(asistencia_ses2),
    ses3 = max(asistencia_ses3),
    ses4 = max(asistencia_ses4),
    ses5 = max(asistencia_ses5),
    
    .groups = "drop"
  )

# -------------------------------------------------
# Creación de nombre completo limpio
# Se genera una columna con el nombre completo en
# formato Title Case y sin acentos
# -------------------------------------------------

participantes <- participantes %>%
  mutate(
    nombre_completo = paste(Nombre, apellido1, apellido2),
    nombre_completo = str_to_title(nombre_completo),
    nombre_completo = stringi::stri_trans_general(nombre_completo, "Latin-ASCII")
  )

# -------------------------------------------------
# Cálculo del desempeño
# Se suma el número total de sesiones asistidas
# por cada participante
# -------------------------------------------------

participantes <- participantes %>%
  mutate(
    total_asistencia = ses1 + ses2 + ses3 + ses4 + ses5
  )

# -------------------------------------------------
# Identificación de participantes con menor desempeño
# -------------------------------------------------

menor_desempeno <- participantes %>%
  filter(total_asistencia == min(total_asistencia))

menor_desempeno

# -------------------------------------------------
# Tabla resumen de asistencia
# Muestra cuántos participantes asistieron a cada
# número de sesiones del curso
# -------------------------------------------------

tabla_asistencia <- participantes %>%
  count(total_asistencia) %>%
  arrange(total_asistencia)

tabla_asistencia

# -------------------------------------------------
# Estadísticas generales del curso
# -------------------------------------------------

resumen_curso <- participantes %>%
  summarise(
    total_participantes = n(),
    promedio_asistencia = mean(total_asistencia),
    completaron_curso = sum(total_asistencia == 5),
    tasa_finalizacion = completaron_curso / total_participantes
  )

resumen_curso

# -------------------------------------------------
# Exportar base de participantes limpia
# -------------------------------------------------

write_csv(participantes, "participantes_limpios.csv")

# -------------------------------------------------
# Visualización de distribución de asistencia
# Permite identificar patrones de participación
# en el curso
# -------------------------------------------------

grafico_asistencia <- participantes %>%
  count(total_asistencia) %>%
  ggplot(aes(x = total_asistencia, y = n)) +
  geom_col() +
  labs(
    title = "Distribución de asistencia al curso",
    x = "Número de sesiones asistidas",
    y = "Número de participantes"
  ) +
  theme_minimal()

grafico_asistencia

# Guardar gráfico generado
ggsave("distribucion_asistencia.png", grafico_asistencia)
