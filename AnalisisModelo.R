# Autora: Yanet Karina González Arellano
# Script(guión) para el modelo de regresión lineal múltiple

# Instalar paquetes necesarios
install.packages("readxl","lmtest","car","leaps")

# Cargar los paquetes necesarios 
library(readxl)
library(lmtest)
library(car)
library(leaps)

# Cargar el archivo en excel con las calificaciones finales 
libro <- read_excel("/Tesis/AnalisisEstadistico.xlsx", sheet = "Notas")

# Crear dataset limpio con nombres simples
datos <- data.frame(
  y  = libro$final,
  x1 = as.factor(libro$tratamiento), 
  # x2 = libro$asesorias      
  #x3 = libro$asistencias           
  #x4 = libro$proyecto,           
  #x5 = libro$quizes,             
  x6 = libro$tareas            
  #x7 = libro$diagnostico,          
  #x8 = libro$parciales           
)

# Convertir calificación final a numérica
datos$y <- as.numeric(as.character(datos$y))

# Llamar el comando lm (linear model) para el modelo lineal, análisis y resumen 
modelo <- lm(y~x1+x6,data=datos)
summary(modelo)

# Prueba de Normalidad
shapiro.test(residuals(modelo))

# Prueba de Homocedasticidad
leveneTest(y ~ x1, data = datos)
  
# Prueba de Independencia
durbinWatsonTest(modelo)
  
# Prueba de Multicolinealidad
vif(modelo)

# Para analizar variables relevantes
  # regfit <- regsubsets(y~x1+x2+x3+x4+x5+x6+x7+x8 , data = datos, nvmax = 8)
  #  
  # summary(regfit)
  # res <- summary(regfit)
  #  
  # res$adjr2   # R² ajustado de cada modelo
  # res$cp      # Cp de Mallows
  # res$bic     # BIC de cada modelo
