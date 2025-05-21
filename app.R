library(shiny)
library(forecast)
library(prophet)
library(dplyr)
library(httr)
library(jsonlite)
#esperar un rato para que lleguen metricas
prometheus_url <- "http://localhost:9090/api/v1/query"
query <- 'go_gc_duration_seconds_sum'
response <- GET(prometheus_url, query = list(query = query))
content_text <- content(response, "text", encoding = "UTF-8")
content_json <- fromJSON(content_text, flatten = TRUE)
results <- content_json$data$result
value_vec <- results$value[[1]]
value_str <- value_vec[2]
value_parseados <- as.numeric(value_str)
#serie temporal de ejemplo
vector <- c(
  2498560, 4055040, 4169728, 4636672, 4284416, 3661824, 4005888, 3768320, 4448256,
  3900000, 4100000, 7000000,  
  4050000, 4087808, 4317184, 4554752, 3842048, 3866624, 4268032,
  4481024, 4235264, 3989504, 8000000,  
  4759552, 4554752, 3555328, 4784128, 4390912, 3973120,
  4251648, 4030464, 4276224, 3956736, 8500000, 
  4505600, 4153344, 4595712, 4000000, 3800000, 3700000, 3600000,
  4571136, 4145152, 4792320, 3579904, 7000000,
  3883008, 3700000, 3600000,
  3400000, 3300000, 3100000, 3000000
)

create_fourier <- function(n, K) {
  time <- 1:n
  X <- matrix(1, nrow = n, ncol = 1 + 2 * K)
  colnames(X) <- c("intercept", paste0("cos_", 1:K), paste0("sin_", 1:K))
  for (k in 1:K) {
    X[, 2*k] <- cos(2 * pi * k * time / 12)
    X[, 2*k+1] <- sin(2 * pi * k * time / 12)
  }
  return(as.data.frame(X))
}

ui <- fluidPage(
  titlePanel("📊 Modelado, Predicción y Prevención "),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Selecciona un modelo de predicción y el horizonte deseado."),
      
      selectInput("modelo", "Modelo de predicción:",
                  choices = c("ARIMA", "Prophet", "Armónico (Fourier + LM)", 
                              "Red Neuronal (nnetar)", "Regresión a la Media")),
      
      numericInput("horizonte", "Horizonte de predicción:", value = 10, min = 1),
      
      actionButton("predecir", "Ejecutar Modelo")
    ),
    
    mainPanel(
      plotOutput("grafico"),
      verbatimTextOutput("infoModelo")
    )
  )
)

server <- function(input, output) {
  
  datos <- reactive({ vector })
  
  prediccion <- eventReactive(input$predecir, {
    ts_data <- ts(datos(), frequency = 12)
    n <- length(ts_data)
    h <- input$horizonte
    
    switch(input$modelo,
           
           "ARIMA" = {
             modelo <- auto.arima(ts_data)
             pred <- forecast(modelo, h = h)
             list(modelo = modelo, pred = pred)
           },
           
           "Prophet" = {
             df <- data.frame(
               ds = seq.Date(from = as.Date("2020-01-01"), by = "month", length.out = n),
               y = as.numeric(ts_data)
             )
             modelo <- prophet(df, yearly.seasonality = TRUE, daily.seasonality = FALSE, weekly.seasonality = FALSE, verbose = FALSE)
             futuro <- make_future_dataframe(modelo, periods = h, freq = "month")
             pred <- predict(modelo, futuro)
             list(modelo = modelo, pred = pred, df = df)
           },
           
           "Armónico (Fourier + LM)" = {
             K <- 3
             X <- create_fourier(n, K)
             modelo <- lm(ts_data ~ . - 1, data = X)
             X_future <- create_fourier(n + h, K)[(n + 1):(n + h), ]
             pred_values <- predict(modelo, newdata = X_future)
             list(modelo = modelo, pred = pred_values)
           },
           
           "Red Neuronal (nnetar)" = {
             modelo <- nnetar(ts_data)
             pred <- forecast(modelo, h = h)
             list(modelo = modelo, pred = pred)
           },
           
           "Regresión a la Media" = {
             serie <- datos()
             media_global <- mean(serie)
             sd_global <- sd(serie)
             
             # Variables de entrada y salida
             X_t <- serie[1:(length(serie) - 1)]
             X_t1 <- serie[2:length(serie)]
             
             retorno_logico <- as.numeric(X_t1 < X_t)  # 1 si hay retorno, 0 si no
             
             # Entrenamiento del modelo logístico
             modelo_logit <- glm(retorno_logico ~ X_t, family = binomial)
             
             # Predicción con el último valor observado
             x_actual <- tail(serie, 1)
             prob_retorno_actual <- predict(modelo_logit, newdata = data.frame(X_t = x_actual), type = "response")
             
             list(
               xt = X_t,
               xt1 = X_t1,
               retorno = retorno_logico,
               modelo_logit = modelo_logit,
               x_actual = x_actual,
               prob_retorno_actual = prob_retorno_actual
             )
           }
           
    )
  })
  
  output$grafico <- renderPlot({
    req(prediccion())
    modelo_sel <- input$modelo
    datos_orig <- datos()
    ts_data <- ts(datos_orig, frequency = 12)
    h <- input$horizonte
    n <- length(ts_data)
    
    if (modelo_sel == "Prophet") {
      pred <- prediccion()$pred
      total_length <- n + h
      ylim_range <- range(c(datos_orig, pred$yhat[(n + 1):total_length],
                            pred$yhat_upper[(n + 1):total_length],
                            pred$yhat_lower[(n + 1):total_length]), na.rm = TRUE)
      
      plot(1:total_length, c(datos_orig, rep(NA, h)), type = "l", col = "black", lwd = 2,
           ylim = ylim_range, xlab = "Tiempo", ylab = "Valor", main = "Predicción con Prophet")
      
      lines((n + 1):total_length, pred$yhat[(n + 1):total_length], col = "blue", lwd = 2)
      lines((n + 1):total_length, pred$yhat_upper[(n + 1):total_length], col = "lightblue", lty = 2)
      lines((n + 1):total_length, pred$yhat_lower[(n + 1):total_length], col = "lightblue", lty = 2)
      
      legend("topleft", legend = c("Serie original", "Predicción", "Intervalo confianza"), 
             col = c("black", "blue", "lightblue"), lty = c(1,1,2), bty = "n")
      
    } else if (modelo_sel == "Armónico (Fourier + LM)") {
      pred <- prediccion()$pred
      max_x <- max(60, n + h)
      valores_plot <- c(as.numeric(ts_data), rep(NA, max_x - n))
      plot(1:max_x, valores_plot, col = "black", lwd = 2, type = "l",
           main = "Predicción Modelo Armónico", ylab = "Valor", xlab = "Tiempo")
      lines((n + 1):(n + h), pred, col = "blue", lwd = 2)
      legend("topleft", legend = c("Serie original", "Predicción futura"), col = c("black", "blue"), lty = 1, bty = "n")
      
    } else if (modelo_sel == "Regresión a la Media") {
      res <- prediccion()
      media_global <- mean(datos())  # Calculamos la media global
      
      plot(res$xt, type = "l", col = "black", lwd = 2,
           main = "Regresión a la media",
           xlab = "Tiempo", ylab = expression(X[t]))
      

      abline(h = media_global, col = "darkgreen", lty = 3, lwd = 2)  # Línea de la media
      

    }
    else {
      pred <- prediccion()$pred
      plot(pred, main = paste("Predicción con modelo", modelo_sel),
           ylab = "Valor", xlab = "Tiempo", col = "blue", lwd = 2)
      lines(ts_data, col = "black", lwd = 1)
      legend("topleft", legend = c("Datos reales", "Predicción"), col = c("black", "blue"), lty = 1, bty = "n")
    }
  })
  
  output$infoModelo <- renderPrint({
    req(prediccion())
    modelo_sel <- input$modelo
    
    if (modelo_sel == "Prophet") {
      cat("Modelo Prophet entrenado. No se muestra resumen en consola.\n")
      
    } else if (modelo_sel == "Armónico (Fourier + LM)") {
      print(summary(prediccion()$modelo))
      
    } else if (modelo_sel == "Regresión a la Media") {
      res <- prediccion()
      cat("Modelo de Regresión a la Media (logístico):\n")
      cat(sprintf("Valor actual observado (X_t): %.2f\n", res$x_actual))
      cat(sprintf("Media global actual: %.2f\n", mean(datos())))
      cat(sprintf("Probabilidad de que el siguiente valor sea menor al valor actual: %.2f%%\n",
                  100 * res$prob_retorno_actual))
    }
    
    else {
      print(prediccion()$modelo)
    }
  })
}

shinyApp(ui = ui, server = server)
