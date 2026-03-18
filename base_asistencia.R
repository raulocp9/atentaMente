```r
## ---- leer_excel
library(readxl)
library(dplyr)
library(stringr)
library(stringi)
library(tidyr)
library(ggplot2)


df <- read_excel("Base_prueba.xlsx")


## ---- limpiar_datos
df_clean <- df %>%
  
  rename(
    primer_apellido = `Primer apellido`,
    segundo_apellido = `Segundo apellido`
  ) %>%
  mutate(
    Email = str_to_lower(str_trim(Email)),
    Nombre = str_trim(Nombre),
    primer_apellido = str_trim(primer_apellido),
    segundo_apellido = str_trim(segundo_apellido)
  ) %>%

  mutate(across(c(Nombre, primer_apellido, segundo_apellido), ~ replace_na(., ""))) %>%
 
  mutate(
    nombre_completo =
      str_squish(paste(Nombre, primer_apellido, segundo_apellido))
  ) %>%
  mutate(
    nombre_completo = stri_trans_general(nombre_completo, "Latin-ASCII") %>%
      str_to_lower() %>%
      str_squish()
  ) %>%
  mutate(
    nombre_completo = str_to_title(nombre_completo)
  ) %>%
  mutate(
    key = ifelse(Email == "" | is.na(Email), nombre_completo, Email)
  )

#View(df_clean)


## ---- agrupar_clave
df_final <- df_clean %>%
  group_by(key) %>%
  summarise(
    # Tomar cualquier nombre disponible
    nombre_completo = first(nombre_completo),

    # Si asistió en alguna base cuenta como asistencia 
    across(
      starts_with("asistencia"),
      ~ max(as.numeric(replace_na(.x, 0)), na.rm = TRUE)
    ),
    .groups = "drop"
  ) %>%
  mutate(
    total_asistencia = rowSums(across(starts_with("asistencia_ses")))
  )

View(df_final)

## ---- cero_asistencias
cero_asistencia <- df_final %>%
  filter(total_asistencia == 0)

## ----- cinco_asistencias
cinco_asistencia <- df_final %>%
  filter(total_asistencia == 5)
# View(cero_asistencia)
# View(cinco_asistencia)
media <- mean(df_final$total_asistencia, na.rm = TRUE)

## ---- Graficar
df_plot <- df_final %>%
  count(total_asistencia) %>%
  mutate(
    porcentaje = n / sum(n)
  )

ggplot(df_plot, aes(x = factor(total_asistencia), y = n)) +
  geom_bar(stat = "identity", fill = "steelblue", color = "black") +

  
  geom_text(
    aes(label = scales::percent(porcentaje, accuracy = 1)),
    vjust = -0.5
  ) +
  labs(
    title = "Distribución de asistencia",
    x = "Número de sesiones asistidas",
    y = "Número de participantes"
  ) +
  theme_minimal()

count(df)
count(df_final)
```