# 🔍 Monitoreo y Predicción con Prometheus, R y Ansible. Visualización en Dynatrace 

Este proyecto automatiza la recolección, análisis y predicción de métricas de [Prometheus](https://prometheus.io/) utilizando R y técnicas de modelado estadístico y de machine learning. La automatización completa del flujo de trabajo se realiza con [Ansible](https://www.ansible.com/). Además se implementa una alarma cuando la app detecta un aumento significativo en la serie temporal. Una vez se ha modelado las series temporales y se extraen las predicciones que ofrece cada modelo, se envía la información a Dynatrace via POST para su visualización. Se crea una alerta en Dynatrace para que te notifique por correo si la serie temporal supera cierto umbral. A continuaciónn se muestra un diagrama con las herramientas utilizadas. 

![Predicción ARIMA](Captura.JPG)


![Predicción ARIMA](arima.JPG)
![Predicción ARIMA](armonico.JPG)
![Predicción ARIMA](prophet.JPG)
![Predicción ARIMA](alarmas.JPG)
---

