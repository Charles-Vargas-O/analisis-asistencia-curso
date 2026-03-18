library(tidyverse)

participantes %>%
  count(total_asistencia) %>%
  ggplot(aes(x = total_asistencia, y = n)) +
  geom_col() +
  labs(
    title = "Distribución de asistencia al curso",
    x = "Número de sesiones asistidas",
    y = "Número de participantes"
  ) +
  theme_minimal()
