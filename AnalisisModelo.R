#install.packages("readxl")
library(readxl)
library(lmtest)
library(car)
library(leaps)
libro <- read_excel("/home/juanlps/Dropbox/YanetYJlps/YanetProject/Tesis/AnalisisEstadistico3.xlsx", sheet = "Notas")
# Crea dataset limpio con nombres simples
datos <- data.frame(
  y  = libro$final,
  x1 = as.factor(libro$tratamiento), 
  # x2 = libro$asesorias      
  #x3 = libro$asistencias           
  #x4 = libro$proyecto,           
  #x5 = libro$quizes,             
  x6 = libro$tareas            
  #x7 = libro$diagnostico          
  #x8 = libro$parciales           
)
datos$y <- as.numeric(as.character(datos$y))
modelo <- lm(y~x1+x6,data=datos)
summary(modelo)
  
 
  # Normalidad
  shapiro.test(residuals(modelo))
  #lillie.test(residuals(modelo))
  
  # Homogeneidad de varianzas
  leveneTest(y ~ x1, data = datos)
  
  
  
  # Independencia de errores
  durbinWatsonTest(modelo)
  
  # Multicolinealidad
  vif(modelo)
  
  # Para anlizar variables relevantes
  # regfit <- regsubsets(final ~ tratamiento + asesorias + asistencias +
  #                      proyecto + quizes + tareas + diagnostico
  #                      + parciales, data = datos, nvmax = 8)
  #  
  # summary(regfit)
  # res <- summary(regfit)
  #  
  # res$adjr2   # R² ajustado de cada modelo
  # res$cp      # Cp de Mallows
  # res$bic     # BIC de cada modelo
  #  
  # # Método 1: Usando la función pcr() del paquete pls (recomendado)
  # modelo_pcr <- pcr(final ~ tratamiento + asesorias + asistencias + 
  #                     proyecto + quizes + tareas + diagnostico + parciales, 
  #                   data = datos, 
  #                   scale = TRUE,  # Estandarizar predictores
  #                   validation = "CV")  # Validación cruzada
  # 
  # # Resumen del modelo
  # summary(modelo_pcr)
  # 
