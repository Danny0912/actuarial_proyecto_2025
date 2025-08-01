# ui.R

suppressPackageStartupMessages({
  library(shiny)
  library(shinydashboard)
  library(shinythemes)
  library(plotly)
})

shinyUI(
  dashboardPage(
    skin = "blue",
    dashboardHeader(title = "Simulación Actuarial SCR"),
    
    dashboardSidebar(
      width = 300,
      sidebarMenu(
        menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
        menuItem("Parámetros", tabName = "params", icon = icon("sliders-h"),
                 numericInput("n_sims", "Número de Simulaciones:", value = 10, min = 100, max = 10000, step = 100),
                 sliderInput("mort_mean", "Mortalidad - Shock Promedio:", min = 0, max = 50, value = 15, step = 1, post = "%"),
                 sliderInput("mort_sd", "Mortalidad - Volatilidad:", min = 0, max = 20, value = 5, step = 1, post = "%"),
                 sliderInput("long_mean", "Longevidad - Reducción Promedio:", min = 0, max = 50, value = 20, step = 1, post = "%"),
                 sliderInput("long_sd", "Longevidad - Volatilidad:", min = 0, max = 20, value = 5, step = 1, post = "%"),
                 sliderInput("Interes", "Tasa de Interés Técnica:", min = 0.01, max = 0.15, value = 0.04, step = 0.005),
                 actionButton("run_sim", "Ejecutar Simulación", icon = icon("play-circle"), width = '80%', style="color: #fff; background-color: #2ecc71; border-color: #27ad60; margin-left: 10%;")
        )
      )
    ),
    
    dashboardBody(
      h2("Dashboard de Simulación de Riesgo Actuarial (Monte Carlo)", align = "center"),
      div(style = "text-align: right; font-size: 12px; color: #7f8c8d; margin-bottom: 10px; margin-right: 15px;",
          "Grupo - Danny Reina, Diana Paspuel, Jarol Reyes, Samantha Moya"),
      
      fluidRow(
        valueBoxOutput("scr_var", width = 4),
        valueBoxOutput("scr_medio", width = 4),
        valueBoxOutput("scr_sd", width = 4)
      ),
      
      fluidRow(
        box(
          width = 12,
          title = "Distribución del Capital de Solvencia Requerido (SCR)",
          status = "primary",
          solidHeader = TRUE,
          plotlyOutput("scr_histogram", height = "500px")
        )
      )
    )
  )
)