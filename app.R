library(shiny)
library(forecast)
library(prophet)
library(dplyr)
library(httr)
library(jsonlite)
#esperar un rato para que lleguen metricas
#prometheus_url <- "http://localhost:9090/api/v1/query"
#query <- 'go_gc_duration_seconds_sum'
#response <- GET(prometheus_url, query = list(query = query))
#content_text <- content(response, "text", encoding = "UTF-8")
#content_json <- fromJSON(content_text, flatten = TRUE)
#results <- content_json$data$result
#value_vec <- results$value[[1]]
#value_str <- value_vec[2]
#value_parseados <- as.numeric(value_str)
#serie temporal de ejemplo
# Simulación de métrica Prometheus (puedes sustituir esto por extracción real)

run_forecasting_and_send_metrics <- function(env_url, token) {
  # --- Librerías necesarias ---
  library(prophet)
  library(forecast)
  library(tibble)
  library(dplyr)
  #datos de prueba
  set.seed(123)
  n <- 60
  base_values <- rnorm(n, mean = 5e6, sd = 7e5)
  spike_positions <- sample(1:n, 5)
  base_values[spike_positions] <- base_values[spike_positions] + 4e6  
  
  values_originales <- round(pmin(base_values, 9e6))  
  
  ts_data <- ts(values_originales, frequency = 12)
  
  df_prophet <- tibble(ds = seq.Date(Sys.Date() - n + 1, by = "day", length.out = n),
                       y = as.numeric(values_originales))
  m_prophet <- prophet(df_prophet, verbose = FALSE)
  future_prophet <- make_future_dataframe(m_prophet, periods = 10)
  forecast_prophet <- predict(m_prophet, future_prophet)
  forecast_prophet_vector <- round(tail(forecast_prophet$yhat, 10))
  
  fit_sarima <- auto.arima(ts_data)
  forecast_sarima <- forecast(fit_sarima, h = 10)
  forecast_sarima_vector <- round(forecast_sarima$mean)
  
  t <- 1:length(ts_data)
  harmonic_model <- lm(ts_data ~ sin(2 * pi * t / 12) + cos(2 * pi * t / 12))
  future_t <- (length(t) + 1):(length(t) + 10)
  harmonic_forecast <- predict(harmonic_model, newdata = data.frame(
    t = future_t,
    sin = sin(2 * pi * future_t / 12),
    cos = cos(2 * pi * future_t / 12)
  ))
  forecast_harmonic_vector <- round(harmonic_forecast)
  
  send_metrics <- function(values, metric_name) {
    cpu <- 1
    start_ts <- as.numeric(Sys.time()) * 1000
    start_ts <- start_ts - length(values) * 60000  
    
    lines <- c()
    for (i in seq_along(values)) {
      options(scipen = 999)
      ts <- start_ts + (i) * 60000
      print(ts)
      line <- sprintf("%s,cpu=%d %.1f %.0f", metric_name, cpu, values[i], ts)
      lines <- c(lines, line)
    }
    
    body <- paste(lines, collapse = "\n")
    
    curl_cmd <- sprintf(
      'curl -X POST "%s" -H "Authorization: Api-Token %s" -H "Content-Type: text/plain; charset=utf-8" --data-binary "%s"',
      env_url, token, body
    )
    
    cat("Líneas que se van a enviar para", metric_name, ":\n", body, "\n")
    system(curl_cmd)
  }
  
  cat("Valores originales:\n")
  print(values_originales)
  
  cat("\nForecast Prophet:\n")
  print(forecast_prophet_vector)
  send_metrics(forecast_prophet_vector, "custom.prophet")
  
  cat("\nForecast SARIMA:\n")
  print(forecast_sarima_vector)
  send_metrics(forecast_sarima_vector, "custom.sarima")
  
  cat("\nForecast Armónico:\n")
  print(forecast_harmonic_vector)
  send_metrics(forecast_harmonic_vector, "custom.armonico")
  
  send_metrics(values_originales, "custom.originales")
}

#datos de prueba
vector <- c(
  2498560, 4055040, 4169728, 4636672, 4284416, 3661824, 4005888, 3768320, 4448256,
  3900000, 4100000, 7000000, 4050000, 4087808, 4317184, 4554752, 3842048, 3866624, 4268032,
  4481024, 4235264, 3989504, 8000000, 4759552, 4554752, 3555328, 4784128, 4390912, 3973120,
  4251648, 4030464, 4276224, 3956736, 8500000, 4505600, 4153344, 4595712, 4000000, 3800000, 
  3700000, 3600000, 4571136, 4145152, 4792320, 3579904, 7000000, 3883008, 3700000, 3600000, 
  3400000, 3300000, 3100000, 3000000
)
#credenciales de dyantrace
run_forecasting_and_send_metrics(
  env_url = "TU URL ",
  token = "TU TOKEN"
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
  titlePanel("📊 Modelado, Predicción y Alerta Automática"),
  
  sidebarLayout(
    sidebarPanel(
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
    ultimo_valor <- tail(ts_data, 1)
    umbral_alarma <- 1.2
    
    switch(input$modelo,
           
           "ARIMA" = {
             modelo <- auto.arima(ts_data)
             pred <- forecast(modelo, h = h)
             alarma <- pred$mean[1] > ultimo_valor * umbral_alarma
             list(modelo = modelo, pred = pred, alarma = alarma)
           },
           
           "Prophet" = {
             df <- data.frame(
               ds = seq.Date(from = as.Date("2020-01-01"), by = "month", length.out = n),
               y = as.numeric(ts_data)
             )
             modelo <- prophet(df, yearly.seasonality = TRUE, daily.seasonality = FALSE, weekly.seasonality = FALSE)
             futuro <- make_future_dataframe(modelo, periods = h, freq = "month")
             pred <- predict(modelo, futuro)
             alarma <- pred$yhat[n + 1] > ultimo_valor * umbral_alarma
             list(modelo = modelo, pred = pred, df = df, alarma = alarma)
           },
           
           "Armónico (Fourier + LM)" = {
             K <- 3
             X <- create_fourier(n, K)
             modelo <- lm(ts_data ~ . - 1, data = X)
             X_future <- create_fourier(n + h, K)[(n + 1):(n + h), ]
             pred_values <- predict(modelo, newdata = X_future)
             alarma <- pred_values[1] > ultimo_valor * umbral_alarma
             list(modelo = modelo, pred = pred_values, alarma = alarma)
           },
           
           "Red Neuronal (nnetar)" = {
             modelo <- nnetar(ts_data)
             pred <- forecast(modelo, h = h)
             alarma <- pred$mean[1] > ultimo_valor * umbral_alarma
             list(modelo = modelo, pred = pred, alarma = alarma)
           },
           
           "Regresión a la Media" = {
             serie <- datos()
             X_t <- serie[1:(length(serie) - 1)]
             X_t1 <- serie[2:length(serie)]
             retorno_logico <- as.numeric(X_t1 < X_t)
             modelo_logit <- glm(retorno_logico ~ X_t, family = binomial)
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
      
    } else if (modelo_sel == "Armónico (Fourier + LM)") {
      pred <- prediccion()$pred
      max_x <- max(60, n + h)
      plot(1:max_x, c(as.numeric(ts_data), rep(NA, max_x - n)), col = "black", lwd = 2, type = "l",
           main = "Modelo Armónico", ylab = "Valor", xlab = "Tiempo")
      lines((n + 1):(n + h), pred, col = "blue", lwd = 2)
      
    } else if (modelo_sel == "Regresión a la Media") {
      res <- prediccion()
      plot(res$xt, type = "l", col = "black", lwd = 2,
           main = "Regresión a la Media", xlab = "Tiempo", ylab = expression(X[t]))
      abline(h = mean(datos()), col = "darkgreen", lty = 3, lwd = 2)
      
    } else {
      pred <- prediccion()$pred
      plot(pred, main = paste("Predicción -", modelo_sel), ylab = "Valor", xlab = "Tiempo", col = "blue", lwd = 2)
      lines(ts_data, col = "black", lwd = 1)
    }
  })
  
  output$infoModelo <- renderPrint({
    req(prediccion())
    modelo_sel <- input$modelo
    
    if (modelo_sel == "Prophet") {
      cat("Modelo Prophet entrenado.\n")
      if (prediccion()$alarma) cat("🚨 ALARMA: Se predice un aumento significativo.\n")
      
    } else if (modelo_sel == "Armónico (Fourier + LM)") {
      print(summary(prediccion()$modelo))
      if (prediccion()$alarma) cat("🚨 ALARMA: Se predice un aumento significativo.\n")
      
    } else if (modelo_sel == "Regresión a la Media") {
      res <- prediccion()
      cat("Modelo Regresión a la Media:\n")
      cat(sprintf("Valor actual: %.2f\n", res$x_actual))
      cat(sprintf("Media global: %.2f\n", mean(datos())))
      cat(sprintf("Probabilidad de retorno a la baja: %.2f%%\n", 100 * res$prob_retorno_actual))
      
    } else {
      print(prediccion()$modelo)
      if (prediccion()$alarma) cat("🚨 ALARMA: Se predice un aumento significativo.\n")
    }
  })
}

shinyApp(ui = ui, server = server)


