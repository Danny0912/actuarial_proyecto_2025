suppressPackageStartupMessages({
  library(shiny)
  library(dplyr)
  library(shinydashboard)
  library(readxl)
  library(lubridate)
  library(lifecontingencies)
  library(plotly)
  library(ggplot2)
  library(future)
  library(furrr)
  library(purrr)
})

options(dplyr.summarise.inform = FALSE, scipen = 999)

# --- CARGA DE DATOS Y PREPROCESAMIENTO ---

dir_proyecto <- getwd()
load(file.path(dir_proyecto, '.RData'))

#Lectura de datos
base_polizas <- read_excel(file.path(dir_proyecto, 'Datos_SCR.xlsx')) %>%
  mutate(
    FECHA_DE_NACIMIENTO = as.Date(FECHA_DE_NACIMIENTO),
    DURACION = ifelse(is.na(DURACION), 0, DURACION),
    Edad_actual = as.integer(time_length(difftime(Sys.Date(), FECHA_DE_NACIMIENTO), "years")),
    k_ajustada = ifelse(PRIMA == "UNICA", 1, FLUJOS_ANUALES)
  ) %>%
  filter(Edad_actual <= 99 & Edad_actual > 0)

# --- FUNCIONES DE TABLAS DE MORTALIDAD ---
TablaSegurosF <- function(shock) {
  infladas <- pmin(1, probs * (1 + shock))
  probs2lifetable(infladas, radix = 100000, type = "qx")
}
TablaSegurosM <- function(shock) {
  infladas <- pmin(1, probsM * (1 + shock))
  probs2lifetable(infladas, radix = 100000, type = "qx")
}
TablaRentasF <- function(shock) {
  reducidas <- pmax(0, probs * (1 - shock))
  probs2lifetable(reducidas, radix = 100000, type = "qx")
}
TablaRentasM <- function(shock) {
  reducidas <- pmax(0, probsM * (1 - shock))
  probs2lifetable(reducidas, radix = 100000, type = "qx")
}

TH_base <- TablaRentasF(0)
TM_base <- TablaRentasM(0)
TS_F_base <- TablaSegurosF(0)
TS_M_base <- TablaSegurosM(0)

# --- FUNCIONES DE VALORACIÓN ACTUARIAL 

#  RENTAS VITALICIAS
calcular_valor_renta <- function(sexo, edad, indem, tasa_dif, interes, tabla_f, tabla_m, k = 1) {
  numerador <- sum(sapply(0:(100 - edad), function(j) {
    indem * ((1 + tasa_dif)^j) * axn(if (sexo == "M") tabla_m else tabla_f, x = edad, n = 1, k = k, m = j, i = interes, payment = "immediate") * k
  }))
  
  if (is.na(k) || k == 1) {
    return(numerador)
  } else {
    denom <- axn(if (sexo == "M") tabla_m else tabla_f, x = edad, n = 100 - edad, i = interes, k = k, payment = "due")
    return(numerador / denom)
  }
}

# SEGURO VIDA ENTERA (Producto 2)
calcular_valor_seguro_entera <- function(sexo, edad, indem, tasa_dif, interes, tabla_seg_f, tabla_seg_m, tabla_base_f, tabla_base_m, k = 1) {
  tabla_seguros <- if (sexo == "M") tabla_seg_m else tabla_seg_f
  tabla_base <- if (sexo == "M") tabla_base_m else tabla_base_f
  
  numerador <- sum(sapply(0:(100 - edad), function(j) {
    indem * ((1 + tasa_dif)^j) * Axn(tabla_seguros, x = edad + j, n = 1, i = interes) * Exn(tabla_base, x = edad, n = j, i = interes)
  }))
  
  if (is.na(k) || k == 1) return(numerador)
  denom <- axn(tabla_seguros, x = edad, n = 100 - edad, i = interes, k = k, payment = "due")
  return(numerador / denom)
}

# Función para SEGURO TEMPORAL (Producto 1)
calcular_valor_seguro_temporal <- function(sexo, edad, duracion, indem, tasa_dif, interes, tabla_seg_f, tabla_seg_m, tabla_base_f, tabla_base_m, k = 1) {
  tabla_seguros <- if (sexo == "M") tabla_seg_m else tabla_seg_f
  tabla_base <- if (sexo == "M") tabla_base_m else tabla_base_f
  
  numerador <- sum(sapply(0:(duracion - 1), function(j) {
    (indem + tasa_dif * j) * Axn(tabla_seguros, x = edad + j, n = 1, i = interes) * Exn(tabla_base, x = edad, n = j, i = interes)
  }))
  
  if (is.na(k) || k == 1) return(numerador)
  denom <- axn(tabla_seguros, x = edad, n = duracion, i = interes, k = k, payment = "due")
  return(numerador / denom)
}

# Función  unificada
calcular_valor_unificado <- function(producto, sexo, edad, duracion, indem, tasa_dif, interes, tabla_f, tabla_m, tabla_seg_f, tabla_seg_m, k = 1) {
  
  tabla_base_f <- TH_base 
  tabla_base_m <- TM_base
  
  if (producto == 0) {
    calcular_valor_renta(sexo, edad, indem, tasa_dif, interes, tabla_f, tabla_m, k)
  } else if (producto == 2) {
    calcular_valor_seguro_entera(sexo, edad, indem, tasa_dif, interes, tabla_seg_f, tabla_seg_m, tabla_base_f, tabla_base_m, k)
  } else if (producto == 1) {
    calcular_valor_seguro_temporal(sexo, edad, duracion, indem, tasa_dif, interes, tabla_seg_f, tabla_seg_m, tabla_base_f, tabla_base_m, k)
  } else {
    return(0)
  }
}


# --- SERVIDOR SHINY ---
shinyServer(function(input, output, session) {
  
  polizas_agrupadas <- reactive({
    base_polizas %>%
      group_by(PRODUCTO_CODIGO, SEXO, Edad_actual, DURACION,
               INDEMNIZACION_INICIAL, TASA_DIFERENCIA_ANUAL, k_ajustada) %>%
      summarise(n_polizas = n(), .groups = 'drop')
  })
  
  resultados_scr <- eventReactive(input$run_sim, {
    start_time <- Sys.time()
    showModal(modalDialog(
      title = "Simulando SCR",
      tagList(tags$p("Preparando y ejecutando simulación Monte Carlo..."),
              tags$div(style="text-align:center;", icon("cogs", class = "fa-spin", style = "font-size: 40px; color: #2c3e50;"))),
      footer = NULL))
    
    plan(multisession, workers = max(1, availableCores() - 1))
    
    n_sim <- input$n_sims
    mu_mort <- input$mort_mean / 100
    sd_mort <- input$mort_sd / 100
    mu_long <- input$long_mean / 100
    sd_long <- input$long_sd / 100
    interes <- input$Interes
    
    datos_agrupados <- polizas_agrupadas()
    
    valores_base_agrupados <- datos_agrupados %>%
      mutate(vp_unitario_base = pmap_dbl(
        .l = list(
          producto = PRODUCTO_CODIGO,
          sexo = SEXO,
          edad = Edad_actual,
          duracion = DURACION,
          indem = INDEMNIZACION_INICIAL,
          tasa_dif = TASA_DIFERENCIA_ANUAL,
          k = k_ajustada
        ),
        .f = ~calcular_valor_unificado(
          producto = ..1, sexo = ..2, edad = ..3, duracion = ..4, indem = ..5, tasa_dif = ..6, k = ..7,
          interes = interes, tabla_f = TH_base, tabla_m = TM_base, 
          tabla_seg_f = TS_F_base, tabla_seg_m = TS_M_base
        )
      )) %>%
      mutate(vp_total_base = vp_unitario_base * n_polizas)
    
    total_base_long <- sum(valores_base_agrupados$vp_total_base[valores_base_agrupados$PRODUCTO_CODIGO == 0], na.rm = TRUE)
    total_base_mort <- sum(valores_base_agrupados$vp_total_base[valores_base_agrupados$PRODUCTO_CODIGO %in% c(1, 2)], na.rm = TRUE)
    
    withProgress(message = 'Simulando SCR Monte Carlo...', value = 0, {
      res <- future_map_dbl(1:n_sim, function(i) {
        shock_m <- rnorm(1, mu_mort, sd_mort)
        shock_l <- rnorm(1, mu_long, sd_long)
        
        tablaF_mort_sim <- TablaSegurosF(shock_m)
        tablaM_mort_sim <- TablaSegurosM(shock_m)
        tablaF_long_sim <- TablaRentasF(shock_l)
        tablaM_long_sim <- TablaRentasM(shock_l)
        
        valores_sim_agrupados <- valores_base_agrupados %>%
          mutate(vp_unitario_sim = pmap_dbl(
            # CORRECCIÓN 3: Usar la misma lista nombrada aquí
            .l = list(
              producto = PRODUCTO_CODIGO,
              sexo = SEXO,
              edad = Edad_actual,
              duracion = DURACION,
              indem = INDEMNIZACION_INICIAL,
              tasa_dif = TASA_DIFERENCIA_ANUAL,
              k = k_ajustada
            ),
            .f = ~calcular_valor_unificado(
              producto = ..1, sexo = ..2, edad = ..3, duracion = ..4, indem = ..5, tasa_dif = ..6, k = ..7,
              interes = interes,
              tabla_f = if(..1 == 0) tablaF_long_sim else TH_base,
              tabla_m = if(..1 == 0) tablaM_long_sim else TM_base,
              tabla_seg_f = if(..1 != 0) tablaF_mort_sim else TS_F_base,
              tabla_seg_m = if(..1 != 0) tablaM_mort_sim else TS_M_base
            )
          )) %>%
          mutate(vp_total_sim = vp_unitario_sim * n_polizas)
        
        total_sim_long <- sum(valores_sim_agrupados$vp_total_sim[valores_sim_agrupados$PRODUCTO_CODIGO == 0], na.rm = TRUE)
        total_sim_mort <- sum(valores_sim_agrupados$vp_total_sim[valores_sim_agrupados$PRODUCTO_CODIGO %in% c(1, 2)], na.rm = TRUE)
        
        loss_long <- total_base_long - total_sim_long
        loss_mort <- total_base_mort - total_sim_mort
        
        sqrt(loss_long^2 + loss_mort^2 + 2 * (-0.25) * loss_long * loss_mort)
        
      }, .options = furrr_options(seed = TRUE))
      
      incProgress(1)
    })
    
    end_time <- Sys.time()
    tiempo_total <- round(as.numeric(difftime(end_time, start_time, units = "secs")), 1)
    removeModal()
    showModal(modalDialog(
      title = "Simulación completada",
      paste("Tiempo total de ejecución:", tiempo_total, "segundos."),
      easyClose = TRUE,
      footer = modalButton("Cerrar")
    ))
    
    return(res)
  })
  
  output$scr_var <- renderValueBox({
    req(resultados_scr())
    val <- quantile(resultados_scr(), 0.995)
    valueBox(
      paste0("$", format(round(val / 1e6, 2), nsmall = 2), "M"),
      "Capital Requerido (VaR 99.5%)",
      icon = icon("shield-alt"),
      color = "green"
    )
  })
  
  output$scr_medio <- renderValueBox({
    req(resultados_scr())
    val <- mean(resultados_scr())
    valueBox(
      paste0("$", format(round(val / 1e6, 2), nsmall = 2), "M"),
      "SCR Promedio",
      icon = icon("balance-scale"),
      color = "blue"
    )
  })
  
  output$scr_sd <- renderValueBox({
    req(resultados_scr())
    val <- sd(resultados_scr())
    valueBox(
      paste0("$", format(round(val / 1e6, 2), nsmall = 2), "M"),
      "Volatilidad del SCR",
      icon = icon("chart-line"),
      color = "red"
    )
  })
  
  output$scr_histogram <- renderPlotly({
    req(resultados_scr())
    scr_data <- resultados_scr()
    mean_scr <- mean(scr_data)
    var_995 <- quantile(scr_data, 0.995)
    
    plot_ly(x = ~scr_data, type = "histogram", nbinsx = 50,
            name = "Frecuencia",
            marker = list(color = "#2c3e50")) %>%
      layout(
        title = "Distribución del Capital de Solvencia Requerido (SCR)",
        xaxis = list(title = "SCR ($)"),
        yaxis = list(title = "Frecuencia"),
        shapes = list(
          list(type = "line", x0 = mean_scr, x1 = mean_scr, y0 = 0, y1 = 1, yref = "paper", line = list(color = "blue", dash = "dash")),
          list(type = "line", x0 = var_995, x1 = var_995, y0 = 0, y1 = 1, yref = "paper", line = list(color = "red", dash = "solid"))
        )
      )
  })
})

