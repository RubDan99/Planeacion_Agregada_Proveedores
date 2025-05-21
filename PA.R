library(shiny)
library(lpSolve)
library(dplyr)
library(ggplot2)
library(plotly)
library(shinydashboard)
library(DT)
library(shinyjs)

# UI del aplicativo
ui <- dashboardPage(
  dashboardHeader(title = "Optimización de Producción"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Parámetros", tabName = "parameters", icon = icon("sliders")),
      menuItem("Proveedores", tabName = "suppliers", icon = icon("truck")),
      menuItem("Resultados", tabName = "results", icon = icon("chart-line")),
      menuItem("Análisis de Proveedores", tabName = "supplier_analysis", icon = icon("balance-scale")),
      menuItem("Acerca del Modelo", tabName = "about", icon = icon("info-circle"))
    )
  ),
  dashboardBody(
    useShinyjs(),
    tabItems(
      # Pestaña de Parámetros
      tabItem(
        tabName = "parameters",
        fluidRow(
          box(
            title = "Parámetros Generales", status = "primary", width = 6,
            numericInput("trabajadores", "Número de Trabajadores:", value = 1250, min = 100),
            numericInput("horasNormales", "Horas Normales Mensuales:", value = 160, min = 1),
            numericInput("prodPorHora", "Producción por Hora (galones):", value = 6, min = 1),
            numericInput("costoHoraNormal", "Costo Hora Normal (€):", value = 20, min = 1),
            numericInput("costoHoraExtra", "Costo Hora Extra (€):", value = 30, min = 1),
            numericInput("costoAlmacenamiento", "Costo Almacenamiento (€/galón):", value = 3, min = 0),
            numericInput("costoMateriales", "Costo Materiales (€/galón):", value = 20, min = 1),
            numericInput("stockInicial", "Stock Inicial (miles de galones):", value = 0, min = 0),
            numericInput("stockFinal", "Stock Final Requerido (miles de galones):", value = 50, min = 0)
          ),
          box(
            title = "Demanda Mensual", status = "primary", width = 6,
            numericInput("dem1", "Demanda Mes 1 (miles de galones):", value = 800, min = 0),
            numericInput("dem2", "Demanda Mes 2 (miles de galones):", value = 900, min = 0),
            numericInput("dem3", "Demanda Mes 3 (miles de galones):", value = 1000, min = 0),
            numericInput("dem4", "Demanda Mes 4 (miles de galones):", value = 950, min = 0),
            numericInput("dem5", "Demanda Mes 5 (miles de galones):", value = 850, min = 0),
            numericInput("dem6", "Demanda Mes 6 (miles de galones):", value = 900, min = 0),
            numericInput("dem7", "Demanda Mes 7 (miles de galones):", value = 1000, min = 0),
            numericInput("dem8", "Demanda Mes 8 (miles de galones):", value = 1100, min = 0),
            numericInput("dem9", "Demanda Mes 9 (miles de galones):", value = 950, min = 0),
            numericInput("dem10", "Demanda Mes 10 (miles de galones):", value = 900, min = 0),
            numericInput("dem11", "Demanda Mes 11 (miles de galones):", value = 850, min = 0),
            numericInput("dem12", "Demanda Mes 12 (miles de galones):", value = 900, min = 0)
          )
        ),
        fluidRow(
          box(
            width = 12, 
            actionButton("solve", "Resolver Modelo", 
                         icon = icon("calculator"), 
                         class = "btn-primary btn-lg")
          )
        )
      ),
      
      # Pestaña de Proveedores
      tabItem(
        tabName = "suppliers",
        fluidRow(
          box(
            title = "Selección automática de proveedores", status = "primary", width = 12,
            p("El modelo de programación lineal seleccionará automáticamente el proveedor o combinación de proveedores 
              que minimice los costos totales, considerando las capacidades y restricciones de cada uno."),
            p(strong("Características clave de la optimización:")),
            tags$ul(
              tags$li("No es obligatorio usar todos los proveedores - el modelo puede seleccionar solo los más convenientes"),
              tags$li("No es obligatorio usar toda la capacidad de cada proveedor - se utilizará solo lo necesario"),
              tags$li("La selección puede variar cada mes según la demanda y costos")
            ),
            checkboxInput("useProveedores", "Habilitar uso de proveedores externos", value = TRUE)
          )
        ),
        conditionalPanel(
          condition = "input.useProveedores == true",
          fluidRow(
            column(4,
                   box(
                     title = "Proveedor 1", status = "info", width = 12,
                     numericInput("p1_capacidad", "Capacidad mensual (galones):", value = 300000, min = 0),
                     numericInput("p1_costo", "Costo por galón (€):", value = 30, min = 1),
                     numericInput("p1_tiempoEntrega", "Tiempo de entrega (días):", value = 7, min = 1),
                     checkboxInput("p1_habilitado", "Habilitado", value = TRUE)
                   )
            ),
            column(4,
                   box(
                     title = "Proveedor 2", status = "info", width = 12,
                     numericInput("p2_capacidad", "Capacidad mensual (galones):", value = 500000, min = 0),
                     numericInput("p2_costo", "Costo por galón (€):", value = 35, min = 1),
                     numericInput("p2_tiempoEntrega", "Tiempo de entrega (días):", value = 5, min = 1),
                     checkboxInput("p2_habilitado", "Habilitado", value = TRUE)
                   )
            ),
            column(4,
                   box(
                     title = "Proveedor 3", status = "info", width = 12,
                     numericInput("p3_capacidad", "Capacidad mensual (galones):", value = 200000, min = 0),
                     numericInput("p3_costo", "Costo por galón (€):", value = 25, min = 1),
                     numericInput("p3_tiempoEntrega", "Tiempo de entrega (días):", value = 10, min = 1),
                     checkboxInput("p3_habilitado", "Habilitado", value = TRUE)
                   )
            )
          ),
          fluidRow(
            box(
              title = "Penalización por tiempo de entrega", status = "warning", width = 6,
              p("Costo adicional por día de tiempo de entrega (se suma al costo base por unidad):"),
              numericInput("penalizacion_tiempo", "€ por día de tiempo de entrega:", value = 0.5, min = 0, step = 0.1),
              helpText("Este valor se integra directamente en la función objetivo como parte del costo total a minimizar.")
            ),
            box(
              title = "Uso mínimo de proveedores", status = "warning", width = 6,
              p("Nota: La capacidad de los proveedores se expresa en galones, mientras que la demanda se expresa en miles de galones."),
              numericInput("uso_minimo_proveedor", "Uso mínimo mensual (galones):", value = 0, min = 0),
              helpText("Si se establece en 0, no hay mínimo requerido. El modelo podrá seleccionar libremente los proveedores según costos.")
            )
          ),
          fluidRow(
            box(
              title = "Características del modelo de optimización", status = "warning", width = 12,
              p("El modelo de programación lineal seleccionará automáticamente qué proveedores utilizar en cada mes para minimizar el costo total."),
              p(strong("Todos los costos son parte integral de la función objetivo a minimizar:")),
              tags$ul(
                tags$li(strong("Costo por galón"), " - Precio base de cada proveedor"),
                tags$li(strong("Penalización por tiempo de entrega"), " - Costo adicional según días de entrega"),
                tags$li(strong("Costo total del proveedor"), " = Costo por galón + Penalización por tiempo"),
                tags$li(strong("Capacidad disponible"), " - Límite máximo mensual de cada proveedor"),
                tags$li(strong("Uso mínimo"), " - Cantidad mínima a comprar (si se especifica)")
              ),
              p(strong("Flexibilidad de la solución:")),
              tags$ul(
                tags$li("Si un proveedor tiene un costo excesivo, el modelo puede decidir no utilizarlo en absoluto"),
                tags$li("El modelo puede variar entre proveedores cada mes según las necesidades y costos"),
                tags$li("La cantidad adquirida a cada proveedor siempre será la óptima, no necesariamente la total")
              )
            )
          )
        )
      ),
      
      # Pestaña de Resultados
      tabItem(
        tabName = "results",
        fluidRow(
          tabBox(
            width = 12,
            tabPanel("Resumen", 
                     valueBoxOutput("costoTotal", width = 4),
                     valueBoxOutput("produccionTotal", width = 4),
                     valueBoxOutput("horasExtraTotal", width = 4),
                     hr(),
                     conditionalPanel(
                       condition = "input.useProveedores == true",
                       fluidRow(
                         valueBoxOutput("comprasProveedores", width = 3),
                         valueBoxOutput("costoProveedores", width = 3),
                         valueBoxOutput("costoPenalizacionTiempo", width = 3),
                         valueBoxOutput("proveedoresSeleccionados", width = 3)
                       )
                     ),
                     dataTableOutput("resultTable")
            ),
            tabPanel("Gráficos", 
                     fluidRow(
                       column(12, plotlyOutput("demandaVsProduccion")),
                       column(12, plotlyOutput("stockChart")),
                       column(12, plotlyOutput("horasExtraChart")),
                       conditionalPanel(
                         condition = "input.useProveedores == true",
                         column(12, plotlyOutput("proveedoresChart")),
                         column(6, plotlyOutput("utilizacionProveedores")),
                         column(6, plotlyOutput("proveedoresPorMes"))
                       )
                     )
            )
          )
        )
      ),
      
      # Pestaña de Análisis de Proveedores
      tabItem(
        tabName = "supplier_analysis",
        fluidRow(
          box(
            title = "Análisis Comparativo de Proveedores", status = "primary", width = 12,
            p("Analice y compare los proveedores disponibles para determinar la mejor estrategia de abastecimiento externo."),
            conditionalPanel(
              condition = "input.useProveedores == false",
              div(class = "alert alert-warning",
                  "Para utilizar esta funcionalidad, active la opción de proveedores externos en la pestaña 'Proveedores'.")
            ),
            conditionalPanel(
              condition = "input.useProveedores == true",
              fluidRow(
                column(6,
                       selectInput("criterio_analisis", "Criterio principal de análisis:",
                                   choices = c("Costo Total" = "costo",
                                               "Tiempo de Entrega" = "tiempo",
                                               "Balance Costo-Tiempo" = "balance"),
                                   selected = "balance")
                ),
                column(6,
                       sliderInput("importancia_costo", "Importancia del costo vs. tiempo:",
                                   min = 0, max = 100, value = 50,
                                   post = "%")
                )
              ),
              hr(),
              actionButton("analizar_proveedores", "Ejecutar Análisis", 
                           icon = icon("search"), 
                           class = "btn-primary")
            )
          )
        ),
        conditionalPanel(
          condition = "input.useProveedores == true",
          fluidRow(
            tabBox(
              width = 12,
              tabPanel("Comparativa", 
                       plotlyOutput("grafico_comparativo_proveedores"),
                       dataTableOutput("tabla_comparativa_proveedores")
              ),
              tabPanel("Escenarios de Demanda", 
                       fluidRow(
                         column(4, 
                                selectInput("nivel_demanda", "Nivel de demanda:",
                                            choices = c("Baja" = "baja",
                                                        "Normal" = "normal",
                                                        "Alta" = "alta",
                                                        "Pico" = "pico"),
                                            selected = "normal")
                         ),
                         column(8,
                                numericInput("demanda_personalizada", "Demanda mensual personalizada (miles de galones):", 
                                             value = 1000, min = 0)
                         )
                       ),
                       hr(),
                       plotlyOutput("grafico_escenario_demanda"),
                       verbatimTextOutput("recomendacion_proveedores")
              ),
              tabPanel("Análisis de Sensibilidad", 
                       selectInput("variable_sensibilidad", "Variable para análisis:",
                                   choices = c("Costo por galón" = "costo",
                                               "Tiempo de entrega" = "tiempo",
                                               "Capacidad" = "capacidad"),
                                   selected = "costo"),
                       sliderInput("rango_sensibilidad", "Rango de variación:", 
                                   min = -50, max = 50, value = c(-20, 20),
                                   post = "%"),
                       plotlyOutput("grafico_sensibilidad")
              )
            )
          )
        )
      ),
      
      # Pestaña de Acerca del Modelo
      tabItem(
        tabName = "about",
        box(
          title = "Acerca del Modelo de Optimización", width = 12,
          p("Este modelo de programación lineal busca optimizar la planificación de producción anual,
             minimizando los costos totales (función objetivo) que incluyen:"),
          tags$ul(
            tags$li("Costos de producción normal (€/galón × cantidad)"),
            tags$li("Costos de horas extras (€/hora × horas extras)"),
            tags$li("Costos de almacenamiento de inventario (€/galón × stock)"),
            tags$li(strong("Costos de adquisición a proveedores externos:"),
                    tags$ul(
                      tags$li("Costo base por galón (precio de compra)"),
                      tags$li("Penalización por tiempo de entrega (días × €/día)")
                    )
            )
          ),
          p("El modelo considera las siguientes restricciones:"),
          tags$ul(
            tags$li("Balance de inventario mensual"),
            tags$li("Capacidad de producción en horas normales"),
            tags$li("Límite máximo de horas extras"),
            tags$li("Stock inicial y final definidos"),
            tags$li("Capacidad limitada de los proveedores externos"),
            tags$li("Posibles requisitos mínimos de compra a proveedores")
          ),
          p("Las variables de decisión son:"),
          tags$ul(
            tags$li("Producción en horas normales por mes (en miles de galones)"),
            tags$li("Horas extras utilizadas por mes"),
            tags$li("Nivel de inventario al final de cada mes"),
            tags$li(strong("Cantidad comprada a cada proveedor por mes"))
          ),
          p(strong("Nota sobre la optimización de proveedores:")),
          p("El modelo determina automáticamente cuál es la combinación óptima de producción interna y proveedores que minimiza el costo total. Todos los costos, incluyendo los costos de los proveedores (precio base más penalización por tiempo de entrega), son parte integral de la función objetivo que el algoritmo de programación lineal minimiza.")
        )
      )
    )
  )
)

# Server logic
server <- function(input, output, session) {
  
  # Función para resolver el modelo
  solveModel <- eventReactive(input$solve, {
    # Extraer los parámetros del modelo
    trabajadores <- input$trabajadores
    horasNormales <- input$horasNormales
    prodPorHora <- input$prodPorHora
    costoHoraNormal <- input$costoHoraNormal
    costoHoraExtra <- input$costoHoraExtra
    costoAlmacenamiento <- input$costoAlmacenamiento
    costoMateriales <- input$costoMateriales
    stockInicial <- input$stockInicial
    stockFinal <- input$stockFinal
    
    # Calcular parámetros derivados
    capacidadProd <- (trabajadores * horasNormales * prodPorHora) / 1000
    maxHorasExt <- 20 * trabajadores
    
    # Vector de demanda mensual
    demanda <- c(
      input$dem1, input$dem2, input$dem3, input$dem4, 
      input$dem5, input$dem6, input$dem7, input$dem8,
      input$dem9, input$dem10, input$dem11, input$dem12
    )
    
    # Costos
    costoProd <- (costoMateriales + costoHoraNormal / prodPorHora) * 1000
    costoExtra <- costoHoraExtra / prodPorHora * 1000
    costoStock <- costoAlmacenamiento
    
    # Parámetros de proveedores
    useProveedores <- input$useProveedores
    n_proveedores <- 3
    
    # Recopilar información de los proveedores
    proveedores <- list()
    if (useProveedores) {
      # Proveedor 1
      if (input$p1_habilitado) {
        proveedores[[1]] <- list(
          capacidad = input$p1_capacidad / 1000, # Convertir galones a miles de galones
          costo = input$p1_costo,
          tiempoEntrega = input$p1_tiempoEntrega,
          costo_tiempo = input$p1_tiempoEntrega * input$penalizacion_tiempo, # Costo asociado al tiempo de entrega
          habilitado = TRUE
        )
      } else {
        proveedores[[1]] <- list(
          capacidad = 0,
          costo = 999999, # Costo muy alto para que no se use
          tiempoEntrega = input$p1_tiempoEntrega,
          costo_tiempo = 0,
          habilitado = FALSE
        )
      }
      
      # Proveedor 2
      if (input$p2_habilitado) {
        proveedores[[2]] <- list(
          capacidad = input$p2_capacidad / 1000, # Convertir galones a miles de galones
          costo = input$p2_costo,
          tiempoEntrega = input$p2_tiempoEntrega,
          costo_tiempo = input$p2_tiempoEntrega * input$penalizacion_tiempo,
          habilitado = TRUE
        )
      } else {
        proveedores[[2]] <- list(
          capacidad = 0,
          costo = 999999,
          tiempoEntrega = input$p2_tiempoEntrega,
          costo_tiempo = 0,
          habilitado = FALSE
        )
      }
      
      # Proveedor 3
      if (input$p3_habilitado) {
        proveedores[[3]] <- list(
          capacidad = input$p3_capacidad / 1000, # Convertir galones a miles de galones
          costo = input$p3_costo,
          tiempoEntrega = input$p3_tiempoEntrega,
          costo_tiempo = input$p3_tiempoEntrega * input$penalizacion_tiempo,
          habilitado = TRUE
        )
      } else {
        proveedores[[3]] <- list(
          capacidad = 0,
          costo = 999999,
          tiempoEntrega = input$p3_tiempoEntrega,
          costo_tiempo = 0,
          habilitado = FALSE
        )
      }
    } else {
      # Inicializar proveedores vacíos si no se usan
      for (i in 1:n_proveedores) {
        proveedores[[i]] <- list(
          capacidad = 0,
          costo = 999999,
          tiempoEntrega = 0,
          costo_tiempo = 0,
          habilitado = FALSE
        )
      }
    }
    
    # Calcular el costo total por unidad para cada proveedor (base + penalización por tiempo)
    # Esto hace que el tiempo de entrega sea parte del costo a optimizar
    for (i in 1:n_proveedores) {
      if (proveedores[[i]]$habilitado) {
        proveedores[[i]]$costo_total = proveedores[[i]]$costo + proveedores[[i]]$costo_tiempo
      } else {
        proveedores[[i]]$costo_total = 999999
      }
    }
    
    # Uso mínimo de proveedores (en miles de galones)
    # Si un proveedor se usa, debe usarse al menos esta cantidad, pero no es obligatorio usar todos los proveedores
    uso_minimo_proveedor <- if(useProveedores) input$uso_minimo_proveedor / 1000 else 0
    
    # Configurar el problema de programación lineal
    n_months <- 12
    
    if (!useProveedores) {
      n_vars <- 3 * n_months  # Prod, HorasExt, Stock para cada mes
    } else {
      n_vars <- (3 + n_proveedores) * n_months  # Prod, HorasExt, Stock, Prov1, Prov2, Prov3 para cada mes
    }
    
    # Función objetivo (minimizar costos)
    obj <- numeric(n_vars)
    for (t in 1:n_months) {
      # Costos de producción interna
      obj[t] <- costoProd                  # Costo de producción normal
      obj[n_months + t] <- costoExtra      # Costo de horas extras
      obj[2*n_months + t] <- costoStock    # Costo de almacenamiento
      
      # Costos de proveedores - PARTE INTEGRAL DE LA FUNCIÓN OBJETIVO
      # Cada unidad comprada a un proveedor contribuye directamente al costo total
      if (useProveedores) {
        for (p in 1:n_proveedores) {
          # El costo_total incluye el precio base y la penalización por tiempo de entrega
          obj[(3 + p - 1) * n_months + t] <- proveedores[[p]]$costo_total * 1000  # Multiplicamos por 1000 para mantener la escala
        }
      }
    }
    
    # Matriz de restricciones
    if (!useProveedores) {
      n_constraints <- n_months + 2*n_months + 1  # Balance, capacidad prod, max horas ext, stock final
    } else {
      # Balance, capacidad prod, max horas ext, stock final, capacidad proveedores
      n_constraints <- n_months + 2*n_months + 1 + n_proveedores*n_months
      
      # Si hay uso mínimo y es > 0, no agregamos restricciones aquí - lo manejaremos de otra forma
    }
    
    A <- matrix(0, nrow = n_constraints, ncol = n_vars)
    
    # Restricciones de balance de inventario
    for (t in 1:n_months) {
      # Si es el primer mes, consideramos el stock inicial
      if (t == 1) {
        A[t, t] <- 1  # Producción normal
        A[t, n_months + t] <- prodPorHora / 1000  # Producción en horas extras
        A[t, 2*n_months + t] <- -1  # Stock final
        
        # Contribución de proveedores
        if (useProveedores) {
          for (p in 1:n_proveedores) {
            A[t, (3 + p - 1) * n_months + t] <- 1  # Unidades de proveedor p
          }
        }
      } else {
        A[t, 2*n_months + t - 1] <- 1  # Stock inicial (del mes anterior)
        A[t, t] <- 1  # Producción normal
        A[t, n_months + t] <- prodPorHora / 1000  # Producción en horas extras
        A[t, 2*n_months + t] <- -1  # Stock final
        
        # Contribución de proveedores
        if (useProveedores) {
          for (p in 1:n_proveedores) {
            A[t, (3 + p - 1) * n_months + t] <- 1  # Unidades de proveedor p
          }
        }
      }
    }
    
    # Restricciones de capacidad de producción
    for (t in 1:n_months) {
      A[n_months + t, t] <- 1
    }
    
    # Restricciones de horas extras máximas
    for (t in 1:n_months) {
      A[2*n_months + t, n_months + t] <- 1
    }
    
    # Restricción de stock final
    A[3*n_months + 1, 3*n_months] <- 1
    
    # Si usamos proveedores, agregar restricciones para ellos
    if (useProveedores) {
      base_idx <- 3*n_months + 1
      
      # Restricciones de capacidad de proveedores
      for (p in 1:n_proveedores) {
        for (t in 1:n_months) {
          A[base_idx + (p-1)*n_months + t, (3 + p - 1)*n_months + t] <- 1
        }
      }
    }
    
    # Vector del lado derecho (RHS)
    b <- numeric(n_constraints)
    
    # Para restricciones de balance
    for (t in 1:n_months) {
      if (t == 1) {
        b[t] <- demanda[t] - stockInicial
      } else {
        b[t] <- demanda[t]
      }
    }
    
    # Para restricciones de capacidad de producción
    for (t in 1:n_months) {
      b[n_months + t] <- capacidadProd
    }
    
    # Para restricciones de horas extras máximas
    for (t in 1:n_months) {
      b[2*n_months + t] <- maxHorasExt
    }
    
    # Para restricción de stock final
    b[3*n_months + 1] <- stockFinal
    
    # Si usamos proveedores, agregar valores RHS para ellos
    if (useProveedores) {
      base_idx <- 3*n_months + 1
      
      # Límites de capacidad de proveedores
      for (p in 1:n_proveedores) {
        for (t in 1:n_months) {
          b[base_idx + (p-1)*n_months + t] <- proveedores[[p]]$capacidad
        }
      }
    }
    
    # Dirección de las restricciones
    if (!useProveedores) {
      dir <- c(rep("=", n_months),        # Balance de inventario
               rep("<=", 2*n_months),     # Capacidad prod y horas extras
               "=")                       # Stock final
    } else {
      dir <- c(rep("=", n_months),        # Balance de inventario
               rep("<=", 2*n_months),     # Capacidad prod y horas extras
               "=",                       # Stock final
               rep("<=", n_proveedores*n_months))  # Capacidad de proveedores
    }
    
    # Resolver el problema
    result <- lp(
      direction = "min",
      objective.in = obj,
      const.mat = A,
      const.dir = dir,
      const.rhs = b,
      all.int = TRUE  # Variables enteras
    )
    
    # Extraer resultados
    if (result$status == 0) {
      solution <- result$solution
      
      # Separar las variables
      prod <- solution[1:n_months]
      horasExt <- solution[(n_months+1):(2*n_months)]
      stock <- solution[(2*n_months+1):(3*n_months)]
      
      # Inicializar arrays para proveedores
      compra_prov1 <- rep(0, n_months)
      compra_prov2 <- rep(0, n_months)
      compra_prov3 <- rep(0, n_months)
      
      # Si usamos proveedores, extraer esos resultados
      if (useProveedores) {
        compra_prov1 <- solution[(3*n_months+1):(4*n_months)]
        compra_prov2 <- solution[(4*n_months+1):(5*n_months)]
        compra_prov3 <- solution[(5*n_months+1):(6*n_months)]
      }
      
      # Calcular stock efectivo (considerando el inicial para el primer mes)
      stock_efectivo <- stock
      if (stockInicial > 0) {
        # Ajustar los resultados para considerar el stock inicial
        stock_efectivo <- c(stockInicial, stock[-n_months])
      }
      
      # Calcular costos desglosados
      costos_prod <- prod * costoProd
      costos_horas_ext <- horasExt * costoExtra / 1000
      costos_stock <- stock * costoStock
      
      # Costos de proveedores
      costo_prov1 <- compra_prov1 * proveedores[[1]]$costo_total  # Incluye precio base y penalización por tiempo
      costo_prov2 <- compra_prov2 * proveedores[[2]]$costo_total
      costo_prov3 <- compra_prov3 * proveedores[[3]]$costo_total
      
      # Desglose de costos de proveedores para análisis
      costo_base_prov1 <- compra_prov1 * proveedores[[1]]$costo
      costo_base_prov2 <- compra_prov2 * proveedores[[2]]$costo
      costo_base_prov3 <- compra_prov3 * proveedores[[3]]$costo
      
      costo_tiempo_prov1 <- compra_prov1 * proveedores[[1]]$costo_tiempo
      costo_tiempo_prov2 <- compra_prov2 * proveedores[[2]]$costo_tiempo
      costo_tiempo_prov3 <- compra_prov3 * proveedores[[3]]$costo_tiempo
      
      # Crear un dataframe con los resultados
      resultados <- data.frame(
        Mes = 1:n_months,
        Demanda = demanda,
        ProduccionNormal = prod * 1000,  # Convertir a miles de galones
        ProduccionHorasExtra = (horasExt * prodPorHora),
        ProduccionTotal = (prod * 1000) + (horasExt * prodPorHora),
        CompraProveedor1 = compra_prov1 * 1000, # Convertir a galones
        CompraProveedor2 = compra_prov2 * 1000, # Convertir a galones
        CompraProveedor3 = compra_prov3 * 1000, # Convertir a galones
        CompraProveedoresTotales = (compra_prov1 + compra_prov2 + compra_prov3) * 1000, # Convertir a galones
        AbastecimientoTotal = (prod * 1000) + (horasExt * prodPorHora) + (compra_prov1 + compra_prov2 + compra_prov3) * 1000,
        HorasExtras = horasExt,
        StockFinalMes = stock,
        CostoProduccion = costos_prod,
        CostoHorasExtras = costos_horas_ext,
        CostoStock = costos_stock,
        CostoProveedor1 = costo_prov1 * 1000, # Costo total para galones
        CostoProveedor2 = costo_prov2 * 1000, # Costo total para galones
        CostoProveedor3 = costo_prov3 * 1000, # Costo total para galones
        CostoProveedores = (costo_prov1 + costo_prov2 + costo_prov3) * 1000,
        CostoBaseProv = (costo_base_prov1 + costo_base_prov2 + costo_base_prov3) * 1000,
        CostoPenalizacionTiempo = (costo_tiempo_prov1 + costo_tiempo_prov2 + costo_tiempo_prov3) * 1000,
        CostoTotal = costos_prod + costos_horas_ext + costos_stock + (costo_prov1 + costo_prov2 + costo_prov3) * 1000
      )
      
      # Calcular información adicional sobre el uso de proveedores
      resultados$UsoProveedor1 <- ifelse(resultados$CompraProveedor1 > 0, "Sí", "No")
      resultados$UsoProveedor2 <- ifelse(resultados$CompraProveedor2 > 0, "Sí", "No")
      resultados$UsoProveedor3 <- ifelse(resultados$CompraProveedor3 > 0, "Sí", "No")
      resultados$NumProveedoresUsados <- rowSums(resultados[, c("CompraProveedor1", "CompraProveedor2", "CompraProveedor3")] > 0)
      
      return(list(
        status = "success",
        resultados = resultados,
        objective = result$objval,
        useProveedores = useProveedores,
        parameters = list(
          trabajadores = trabajadores,
          horasNormales = horasNormales,
          prodPorHora = prodPorHora,
          capacidadProd = capacidadProd,
          maxHorasExt = maxHorasExt,
          costoProd = costoProd,
          costoExtra = costoExtra,
          costoStock = costoStock,
          proveedores = proveedores
        )
      ))
    } else {
      return(list(
        status = "error",
        message = "No se pudo encontrar una solución factible. Revise los parámetros del modelo."
      ))
    }
  })
  
  # Análisis de proveedores
  analizarProveedores <- eventReactive(input$analizar_proveedores, {
    # Obtener parámetros de proveedores
    proveedores <- list()
    
    # Proveedor 1
    if (input$p1_habilitado) {
      proveedores[[1]] <- list(
        nombre = "Proveedor 1",
        capacidad = input$p1_capacidad,
        costo_base = input$p1_costo,
        tiempo = input$p1_tiempoEntrega,
        costo_total = input$p1_costo + input$p1_tiempoEntrega * input$penalizacion_tiempo,
        habilitado = TRUE,
        color = 'rgba(114, 78, 145, 0.7)'
      )
    } else {
      proveedores[[1]] <- list(
        nombre = "Proveedor 1",
        capacidad = 0,
        costo_base = input$p1_costo,
        tiempo = input$p1_tiempoEntrega,
        costo_total = 999999,
        habilitado = FALSE,
        color = 'rgba(114, 78, 145, 0.7)'
      )
    }
    
    # Proveedor 2
    if (input$p2_habilitado) {
      proveedores[[2]] <- list(
        nombre = "Proveedor 2",
        capacidad = input$p2_capacidad,
        costo_base = input$p2_costo,
        tiempo = input$p2_tiempoEntrega,
        costo_total = input$p2_costo + input$p2_tiempoEntrega * input$penalizacion_tiempo,
        habilitado = TRUE,
        color = 'rgba(211, 94, 96, 0.7)'
      )
    } else {
      proveedores[[2]] <- list(
        nombre = "Proveedor 2",
        capacidad = 0,
        costo_base = input$p2_costo,
        tiempo = input$p2_tiempoEntrega,
        costo_total = 999999,
        habilitado = FALSE,
        color = 'rgba(211, 94, 96, 0.7)'
      )
    }
    
    # Proveedor 3
    if (input$p3_habilitado) {
      proveedores[[3]] <- list(
        nombre = "Proveedor 3",
        capacidad = input$p3_capacidad,
        costo_base = input$p3_costo,
        tiempo = input$p3_tiempoEntrega,
        costo_total = input$p3_costo + input$p3_tiempoEntrega * input$penalizacion_tiempo,
        habilitado = TRUE,
        color = 'rgba(128, 133, 133, 0.7)'
      )
    } else {
      proveedores[[3]] <- list(
        nombre = "Proveedor 3",
        capacidad = 0,
        costo_base = input$p3_costo,
        tiempo = input$p3_tiempoEntrega,
        costo_total = 999999,
        habilitado = FALSE,
        color = 'rgba(128, 133, 133, 0.7)'
      )
    }
    
    # Filtrar proveedores habilitados
    proveedores_habilitados <- proveedores[sapply(proveedores, function(p) p$habilitado)]
    
    if (length(proveedores_habilitados) == 0) {
      return(list(
        status = "error",
        message = "No hay proveedores habilitados para analizar."
      ))
    }
    
    # Normalizar puntuaciones para comparación
    max_costo <- max(sapply(proveedores_habilitados, function(p) p$costo_total))
    min_costo <- min(sapply(proveedores_habilitados, function(p) p$costo_total))
    max_tiempo <- max(sapply(proveedores_habilitados, function(p) p$tiempo))
    min_tiempo <- min(sapply(proveedores_habilitados, function(p) p$tiempo))
    max_capacidad <- max(sapply(proveedores_habilitados, function(p) p$capacidad))
    
    for (i in seq_along(proveedores_habilitados)) {
      # Puntuación inversa para costo (menor es mejor)
      if (max_costo > min_costo) {
        proveedores_habilitados[[i]]$puntuacion_costo <- 
          1 - ((proveedores_habilitados[[i]]$costo_total - min_costo) / (max_costo - min_costo))
      } else {
        proveedores_habilitados[[i]]$puntuacion_costo <- 1
      }
      
      # Puntuación inversa para tiempo (menor es mejor)
      if (max_tiempo > min_tiempo) {
        proveedores_habilitados[[i]]$puntuacion_tiempo <-
          1 - ((proveedores_habilitados[[i]]$tiempo - min_tiempo) / (max_tiempo - min_tiempo))
      } else {
        proveedores_habilitados[[i]]$puntuacion_tiempo <- 1
      }
      
      # Puntuación para capacidad (mayor es mejor)
      proveedores_habilitados[[i]]$puntuacion_capacidad <-
        proveedores_habilitados[[i]]$capacidad / max_capacidad
      
      # Puntuación balanceada según el criterio seleccionado
      peso_costo <- input$importancia_costo / 100
      peso_tiempo <- 1 - peso_costo
      
      proveedores_habilitados[[i]]$puntuacion_balanceada <-
        (peso_costo * proveedores_habilitados[[i]]$puntuacion_costo) +
        (peso_tiempo * proveedores_habilitados[[i]]$puntuacion_tiempo)
    }
    
    # Generar tabla comparativa
    tabla_comparativa <- data.frame(
      Proveedor = sapply(proveedores_habilitados, function(p) p$nombre),
      Capacidad = sapply(proveedores_habilitados, function(p) p$capacidad),
      CostoBase = sapply(proveedores_habilitados, function(p) p$costo_base),
      TiempoEntrega = sapply(proveedores_habilitados, function(p) p$tiempo),
      CostoTotal = sapply(proveedores_habilitados, function(p) p$costo_total),
      PuntuacionBalanceada = sapply(proveedores_habilitados, function(p) p$puntuacion_balanceada * 10)
    )
    
    # Análisis para diferentes niveles de demanda
    niveles_demanda <- list(
      baja = 600,
      normal = 1000,
      alta = 1400,
      pico = 1800
    )
    
    # Texto explicativo sobre la optimización
    texto <- "ANÁLISIS DE OPTIMIZACIÓN POR PROGRAMACIÓN LINEAL\n\n"
    texto <- paste0(texto, "El modelo matemático determina la combinación óptima de proveedores que minimiza los costos totales. ")
    texto <- paste0(texto, "Esta optimización no requiere usar todos los proveedores ni toda su capacidad, sino que selecciona ")
    texto <- paste0(texto, "la combinación más rentable para cada mes.\n\n")
    
    # Preparar análisis de escenarios
    escenarios_demanda <- list()
    
    for (nivel in names(niveles_demanda)) {
      demanda <- niveles_demanda[[nivel]]
      escenario <- list(
        nivel = nivel,
        demanda = demanda,
        proveedores = list()
      )
      
      # Para cada proveedor, evaluar si puede cubrir la demanda solo
      for (i in seq_along(proveedores_habilitados)) {
        p <- proveedores_habilitados[[i]]
        if (p$capacidad / 1000 >= demanda) {
          cobertura <- 1
        } else {
          cobertura <- p$capacidad / (1000 * demanda)
        }
        
        escenario$proveedores[[i]] <- list(
          nombre = p$nombre,
          capacidad = p$capacidad / 1000,
          cobertura = cobertura,
          costo_total = p$costo_total,
          puntuacion_balanceada = p$puntuacion_balanceada
        )
      }
      
      # Evaluar combinaciones de proveedores para cubrir demanda
      escenario$combinaciones <- list()
      
      # Combinaciones de 2 proveedores
      if (length(proveedores_habilitados) >= 2) {
        for (i in 1:(length(proveedores_habilitados)-1)) {
          for (j in (i+1):length(proveedores_habilitados)) {
            p1 <- proveedores_habilitados[[i]]
            p2 <- proveedores_habilitados[[j]]
            
            capacidad_total <- p1$capacidad / 1000 + p2$capacidad / 1000
            
            if (capacidad_total >= demanda) {
              # Si entre ambos pueden cubrir la demanda
              # Asignar proporcionalmente basado en costos
              if (p1$costo_total <= p2$costo_total) {
                # Priorizar el proveedor más barato
                unidades_p1 <- min(demanda, p1$capacidad / 1000)
                unidades_p2 <- demanda - unidades_p1
              } else {
                # Priorizar el proveedor más barato
                unidades_p2 <- min(demanda, p2$capacidad / 1000)
                unidades_p1 <- demanda - unidades_p2
              }
              
              costo_combinado <- (unidades_p1 * p1$costo_total + unidades_p2 * p2$costo_total) / demanda
              tiempo_combinado <- (unidades_p1 * p1$tiempo + unidades_p2 * p2$tiempo) / demanda
              
              escenario$combinaciones[[length(escenario$combinaciones) + 1]] <- list(
                proveedores = c(p1$nombre, p2$nombre),
                unidades = c(unidades_p1, unidades_p2),
                capacidad_total = capacidad_total,
                costo_combinado = costo_combinado,
                tiempo_combinado = tiempo_combinado,
                puntuacion = (peso_costo * (1 - (costo_combinado - min_costo) / (max_costo - min_costo))) +
                  (peso_tiempo * (1 - (tiempo_combinado - min_tiempo) / (max_tiempo - min_tiempo)))
              )
            }
          }
        }
      }
      
      # Combinación de 3 proveedores (si hay 3)
      if (length(proveedores_habilitados) >= 3) {
        p1 <- proveedores_habilitados[[1]]
        p2 <- proveedores_habilitados[[2]]
        p3 <- proveedores_habilitados[[3]]
        
        capacidad_total <- p1$capacidad / 1000 + p2$capacidad / 1000 + p3$capacidad / 1000
        
        if (capacidad_total >= demanda) {
          # Ordenar por costo ascendente
          providers <- proveedores_habilitados[order(sapply(proveedores_habilitados, function(p) p$costo_total))]
          
          # Asignar en orden de menor a mayor costo
          demanda_restante <- demanda
          unidades <- c(0, 0, 0)
          
          for (i in seq_along(providers)) {
            p <- providers[[i]]
            # Buscar el índice original de este proveedor
            idx <- which(sapply(proveedores_habilitados, function(x) x$nombre) == p$nombre)
            
            asignado <- min(demanda_restante, p$capacidad / 1000)
            unidades[idx] <- asignado
            demanda_restante <- demanda_restante - asignado
            
            if (demanda_restante <= 0) break
          }
          
          costo_combinado <- sum(unidades * sapply(proveedores_habilitados, function(p) p$costo_total)) / demanda
          tiempo_combinado <- sum(unidades * sapply(proveedores_habilitados, function(p) p$tiempo)) / demanda
          
          escenario$combinaciones[[length(escenario$combinaciones) + 1]] <- list(
            proveedores = sapply(proveedores_habilitados, function(p) p$nombre),
            unidades = unidades,
            capacidad_total = capacidad_total,
            costo_combinado = costo_combinado,
            tiempo_combinado = tiempo_combinado,
            puntuacion = (peso_costo * (1 - (costo_combinado - min_costo) / (max_costo - min_costo))) +
              (peso_tiempo * (1 - (tiempo_combinado - min_tiempo) / (max_tiempo - min_tiempo)))
          )
        }
      }
      
      escenarios_demanda[[nivel]] <- escenario
    }
    
    # Análisis de sensibilidad
    analisis_sensibilidad <- list()
    
    # Variación en costos
    rangos <- seq(-50, 50, by = 10)
    datos_sensibilidad_costo <- list()
    
    for (i in seq_along(proveedores_habilitados)) {
      p <- proveedores_habilitados[[i]]
      variaciones <- c()
      
      for (r in rangos) {
        nuevo_costo <- p$costo_base * (1 + r/100)
        # Calcular el nuevo costo total considerando tiempo de entrega y penalización
        costo_total <- nuevo_costo + p$tiempo * input$penalizacion_tiempo
        variaciones <- c(variaciones, costo_total)
      }
      
      datos_sensibilidad_costo[[p$nombre]] <- variaciones
    }
    
    analisis_sensibilidad$costo <- datos_sensibilidad_costo
    
    # Variación en tiempos
    datos_sensibilidad_tiempo <- list()
    
    for (i in seq_along(proveedores_habilitados)) {
      p <- proveedores_habilitados[[i]]
      variaciones <- c()
      
      for (r in rangos) {
        nuevo_tiempo <- p$tiempo * (1 + r/100)
        costo_total <- p$costo_base + nuevo_tiempo * input$penalizacion_tiempo
        variaciones <- c(variaciones, costo_total)
      }
      
      datos_sensibilidad_tiempo[[p$nombre]] <- variaciones
    }
    
    analisis_sensibilidad$tiempo <- datos_sensibilidad_tiempo
    
    # Variación en capacidad
    datos_sensibilidad_capacidad <- list()
    
    for (i in seq_along(proveedores_habilitados)) {
      p <- proveedores_habilitados[[i]]
      variaciones <- c()
      
      for (r in rangos) {
        nueva_capacidad <- p$capacidad * (1 + r/100)
        variaciones <- c(variaciones, nueva_capacidad)
      }
      
      datos_sensibilidad_capacidad[[p$nombre]] <- variaciones
    }
    
    analisis_sensibilidad$capacidad <- datos_sensibilidad_capacidad
    analisis_sensibilidad$rangos <- rangos
    
    return(list(
      status = "success",
      proveedores = proveedores_habilitados,
      tabla_comparativa = tabla_comparativa,
      escenarios_demanda = escenarios_demanda,
      sensibilidad = analisis_sensibilidad,
      criterio = input$criterio_analisis,
      importancia_costo = input$importancia_costo
    ))
  })
  
  # Mostrar resultados
  observe({
    result <- solveModel()
    
    if (result$status == "success") {
      # Tabla de resultados
      output$resultTable <- renderDT({
        df <- result$resultados
        
        if (!result$useProveedores) {
          datatable(
            df %>% 
              select(Mes, Demanda, ProduccionNormal, ProduccionHorasExtra, 
                     ProduccionTotal, HorasExtras, StockFinalMes),
            options = list(pageLength = 12),
            rownames = FALSE,
            caption = "Resultados mensuales del plan de producción"
          )
        } else {
          datatable(
            df %>% 
              select(Mes, Demanda, ProduccionNormal, ProduccionHorasExtra, 
                     ProduccionTotal, CompraProveedor1, CompraProveedor2, CompraProveedor3,
                     NumProveedoresUsados, AbastecimientoTotal, StockFinalMes,
                     CostoBaseProv, CostoPenalizacionTiempo, CostoProveedores),
            options = list(pageLength = 12),
            rownames = FALSE,
            caption = "Resultados mensuales del plan de producción con proveedores"
          ) %>%
            formatStyle(
              'CompraProveedor1',
              backgroundColor = styleEqual(c(0), c('#f2f2f2'))
            ) %>%
            formatStyle(
              'CompraProveedor2',
              backgroundColor = styleEqual(c(0), c('#f2f2f2'))
            ) %>%
            formatStyle(
              'CompraProveedor3',
              backgroundColor = styleEqual(c(0), c('#f2f2f2'))
            ) %>%
            formatStyle(
              'NumProveedoresUsados',
              backgroundColor = styleInterval(c(1, 2), c('#f2dede', '#fcf8e3', '#dff0d8'))
            )
        }
      })
      
      # Value boxes
      output$costoTotal <- renderValueBox({
        valueBox(
          paste0("€", format(round(result$objective), big.mark = ".", decimal.mark = ",")),
          "Costo Total Optimizado",
          icon = icon("euro-sign"),
          color = "green"
        )
      })
      
      output$produccionTotal <- renderValueBox({
        total_prod <- sum(result$resultados$ProduccionTotal)
        valueBox(
          format(round(total_prod), big.mark = ".", decimal.mark = ","),
          "Producción Total (galones)",
          icon = icon("industry"),
          color = "blue"
        )
      })
      
      output$horasExtraTotal <- renderValueBox({
        total_he <- sum(result$resultados$HorasExtras)
        valueBox(
          format(round(total_he), big.mark = ".", decimal.mark = ","),
          "Horas Extras Totales",
          icon = icon("clock"),
          color = "orange"
        )
      })
      
      # Value boxes para proveedores (solo si están habilitados)
      if (result$useProveedores) {
        output$comprasProveedores <- renderValueBox({
          total_compras <- sum(result$resultados$CompraProveedoresTotales)
          valueBox(
            format(round(total_compras), big.mark = ".", decimal.mark = ","),
            "Compras a Proveedores (galones)",
            icon = icon("truck"),
            color = "purple"
          )
        })
        
        output$costoProveedores <- renderValueBox({
          total_costo_prov <- sum(result$resultados$CostoProveedores)
          valueBox(
            paste0("€", format(round(total_costo_prov), big.mark = ".", decimal.mark = ",")),
            "Costo de Proveedores",
            icon = icon("money-bill"),
            color = "red"
          )
        })
        
        output$costoPenalizacionTiempo <- renderValueBox({
          total_costo_tiempo <- sum(result$resultados$CostoPenalizacionTiempo)
          valueBox(
            paste0("€", format(round(total_costo_tiempo), big.mark = ".", decimal.mark = ",")),
            "Costo por Tiempo de Entrega",
            icon = icon("clock"),
            color = "yellow"
          )
        })
        
        output$proveedoresSeleccionados <- renderValueBox({
          # Calcular número total de meses-proveedor utilizados
          meses_p1 <- sum(result$resultados$CompraProveedor1 > 0)
          meses_p2 <- sum(result$resultados$CompraProveedor2 > 0)
          meses_p3 <- sum(result$resultados$CompraProveedor3 > 0)
          prov_usados <- meses_p1 + meses_p2 + meses_p3
          
          # Calcular cuántos proveedores diferentes se utilizaron
          provs_diferentes <- sum(c(any(result$resultados$CompraProveedor1 > 0),
                                    any(result$resultados$CompraProveedor2 > 0),
                                    any(result$resultados$CompraProveedor3 > 0)))
          
          valueBox(
            paste0(provs_diferentes, " de 3 (", prov_usados, " meses-proveedor)"),
            "Proveedores Utilizados",
            icon = icon("check-circle"),
            color = "purple"
          )
        })
      }
      
      # Gráficos
      output$demandaVsProduccion <- renderPlotly({
        df <- result$resultados
        
        if (!result$useProveedores) {
          p <- plot_ly(data = df) %>%
            add_trace(x = ~Mes, y = ~Demanda, name = 'Demanda',
                      type = 'scatter', mode = 'lines+markers', line = list(color = 'red')) %>%
            add_trace(x = ~Mes, y = ~ProduccionTotal, name = 'Producción Total',
                      type = 'scatter', mode = 'lines+markers', line = list(color = 'blue')) %>%
            layout(title = "Demanda vs Producción Total",
                   xaxis = list(title = "Mes", tickvals = 1:12),
                   yaxis = list(title = "Miles de Galones"),
                   legend = list(x = 0.1, y = 0.9))
        } else {
          p <- plot_ly(data = df) %>%
            add_trace(x = ~Mes, y = ~Demanda, name = 'Demanda',
                      type = 'scatter', mode = 'lines+markers', line = list(color = 'red')) %>%
            add_trace(x = ~Mes, y = ~ProduccionTotal, name = 'Producción Interna',
                      type = 'scatter', mode = 'lines+markers', line = list(color = 'blue')) %>%
            add_trace(x = ~Mes, y = ~AbastecimientoTotal, name = 'Abastecimiento Total',
                      type = 'scatter', mode = 'lines+markers', line = list(color = 'green')) %>%
            layout(title = "Demanda vs Abastecimiento Total (Optimizado por PL)",
                   xaxis = list(title = "Mes", tickvals = 1:12),
                   yaxis = list(title = "Miles de Galones"),
                   legend = list(x = 0.1, y = 0.9))
        }
        
        return(p)
      })
      
      output$stockChart <- renderPlotly({
        df <- result$resultados
        
        p <- plot_ly(data = df, x = ~Mes, y = ~StockFinalMes, name = 'Stock Final',
                     type = 'bar', marker = list(color = 'green')) %>%
          layout(title = "Nivel de Inventario al Final de Cada Mes (Optimizado por PL)",
                 xaxis = list(title = "Mes", tickvals = 1:12),
                 yaxis = list(title = "Miles de Galones en Stock"))
        
        return(p)
      })
      
      output$horasExtraChart <- renderPlotly({
        df <- result$resultados
        
        p <- plot_ly(data = df, x = ~Mes, y = ~HorasExtras, name = 'Horas Extras',
                     type = 'bar', marker = list(color = 'orange')) %>%
          layout(title = "Horas Extras Utilizadas por Mes",
                 xaxis = list(title = "Mes", tickvals = 1:12),
                 yaxis = list(title = "Horas"))
        
        return(p)
      })
      
      # Gráfico de proveedores (solo si están habilitados)
      if (result$useProveedores) {
        output$proveedoresChart <- renderPlotly({
          df <- result$resultados
          
          p <- plot_ly(data = df) %>%
            add_trace(x = ~Mes, y = ~CompraProveedor1/1000, name = 'Proveedor 1',
                      type = 'bar', marker = list(color = 'rgba(114, 78, 145, 0.7)')) %>%
            add_trace(x = ~Mes, y = ~CompraProveedor2/1000, name = 'Proveedor 2',
                      type = 'bar', marker = list(color = 'rgba(211, 94, 96, 0.7)')) %>%
            add_trace(x = ~Mes, y = ~CompraProveedor3/1000, name = 'Proveedor 3',
                      type = 'bar', marker = list(color = 'rgba(128, 133, 133, 0.7)')) %>%
            layout(title = "Distribución Óptima de Compras por Proveedor",
                   subtitle = "El modelo selecciona automáticamente los proveedores más rentables",
                   xaxis = list(title = "Mes", tickvals = 1:12),
                   yaxis = list(title = "Miles de Galones"),
                   barmode = 'stack')
          
          return(p)
        })
        
        # Gráfico de utilización de proveedores
        output$utilizacionProveedores <- renderPlotly({
          df <- result$resultados
          
          # Capacidades de los proveedores
          p1_capacidad <- ifelse(input$p1_habilitado, input$p1_capacidad, 0)
          p2_capacidad <- ifelse(input$p2_habilitado, input$p2_capacidad, 0)
          p3_capacidad <- ifelse(input$p3_habilitado, input$p3_capacidad, 0)
          
          # Calcular utilización
          p1_utilizado <- sum(df$CompraProveedor1)
          p2_utilizado <- sum(df$CompraProveedor2)
          p3_utilizado <- sum(df$CompraProveedor3)
          
          # Calcular porcentajes
          if (p1_capacidad > 0) {
            p1_porcentaje <- (p1_utilizado / (p1_capacidad * 12)) * 100
          } else {
            p1_porcentaje <- 0
          }
          
          if (p2_capacidad > 0) {
            p2_porcentaje <- (p2_utilizado / (p2_capacidad * 12)) * 100
          } else {
            p2_porcentaje <- 0
          }
          
          if (p3_capacidad > 0) {
            p3_porcentaje <- (p3_utilizado / (p3_capacidad * 12)) * 100
          } else {
            p3_porcentaje <- 0
          }
          
          # Datos para el gráfico
          datos <- data.frame(
            Proveedor = c("Proveedor 1", "Proveedor 2", "Proveedor 3"),
            Porcentaje = c(p1_porcentaje, p2_porcentaje, p3_porcentaje)
          )
          
          # Crear el gráfico
          p <- plot_ly(
            x = datos$Proveedor,
            y = datos$Porcentaje,
            type = 'bar',
            marker = list(
              color = c(
                ifelse(p1_porcentaje == 0, 'lightgrey', 'rgba(114, 78, 145, 0.7)'),
                ifelse(p2_porcentaje == 0, 'lightgrey', 'rgba(211, 94, 96, 0.7)'),
                ifelse(p3_porcentaje == 0, 'lightgrey', 'rgba(128, 133, 133, 0.7)')
              )
            ),
            text = paste0(round(datos$Porcentaje, 1), "%"),
            textposition = 'auto'
          ) %>%
            layout(
              title = "Utilización de Proveedores (% de Capacidad Total)",
              xaxis = list(title = ""),
              yaxis = list(title = "Porcentaje de Utilización (%)", range = c(0, 100))
            )
          
          return(p)
        })
        
        # Heatmap de proveedores utilizados por mes
        output$proveedoresPorMes <- renderPlotly({
          df <- result$resultados
          
          # Crear matriz para el heatmap
          matriz <- matrix(c(
            as.numeric(df$CompraProveedor1 > 0),
            as.numeric(df$CompraProveedor2 > 0),
            as.numeric(df$CompraProveedor3 > 0)
          ), nrow = 3, byrow = TRUE)
          
          # Configurar textos
          textos <- matrix(c(
            ifelse(df$CompraProveedor1 > 0, "Sí", "No"),
            ifelse(df$CompraProveedor2 > 0, "Sí", "No"),
            ifelse(df$CompraProveedor3 > 0, "Sí", "No")
          ), nrow = 3, byrow = TRUE)
          
          # Datos para el eje X (meses)
          meses <- 1:12
          
          # Crear heatmap
          p <- plot_ly(
            z = matriz,
            x = meses,
            y = c("Proveedor 1", "Proveedor 2", "Proveedor 3"),
            type = "heatmap",
            colors = c("lightgrey", "#5cb85c"),
            text = textos,
            hovertemplate = "Mes: %{x}<br>%{y}: %{text}<extra></extra>"
          ) %>%
            layout(
              title = "Proveedores Seleccionados por Mes (Optimización PL)",
              xaxis = list(title = "Mes", tickvals = 1:12),
              yaxis = list(title = "", autorange = "reversed")
            )
          
          return(p)
        })
      }
      
    } else {
      # Mostrar mensaje de error
      output$resultTable <- renderDT({
        datatable(
          data.frame(Mensaje = result$message),
          options = list(dom = 't'),
          rownames = FALSE
        )
      })
    }
  })
  
  # Salidas para el análisis de proveedores
  observe({
    if (!input$useProveedores) return()
    
    # Solo ejecutar cuando se presiona el botón de análisis
    analisis <- analizarProveedores()
    
    if (analisis$status == "success") {
      # Gráfico comparativo de proveedores
      output$grafico_comparativo_proveedores <- renderPlotly({
        df <- analisis$tabla_comparativa
        
        # Normalizar para mostrar en un radar chart
        df_radar <- data.frame(
          Proveedor = df$Proveedor,
          Costo = sapply(analisis$proveedores, function(p) p$puntuacion_costo * 10),
          Tiempo = sapply(analisis$proveedores, function(p) p$puntuacion_tiempo * 10),
          Capacidad = sapply(analisis$proveedores, function(p) p$puntuacion_capacidad * 10),
          Seleccionado = c("Auto-optimizado", "Auto-optimizado", "Auto-optimizado")
        )
        
        categorias <- c("Costo", "Tiempo", "Capacidad")
        
        # Crear datos para el radar chart
        plot_ly() %>%
          add_trace(
            type = 'scatterpolar',
            r = c(df_radar$Costo[1], df_radar$Tiempo[1], df_radar$Capacidad[1], df_radar$Costo[1]),
            theta = c(categorias, categorias[1]),
            name = df_radar$Proveedor[1],
            fill = 'toself',
            fillcolor = analisis$proveedores[[1]]$color,
            line = list(color = analisis$proveedores[[1]]$color)
          ) %>%
          add_trace(
            type = 'scatterpolar',
            r = c(df_radar$Costo[2], df_radar$Tiempo[2], df_radar$Capacidad[2], df_radar$Costo[2]),
            theta = c(categorias, categorias[1]),
            name = df_radar$Proveedor[2],
            fill = 'toself',
            fillcolor = analisis$proveedores[[2]]$color,
            line = list(color = analisis$proveedores[[2]]$color)
          ) %>%
          add_trace(
            type = 'scatterpolar',
            r = c(df_radar$Costo[3], df_radar$Tiempo[3], df_radar$Capacidad[3], df_radar$Costo[3]),
            theta = c(categorias, categorias[1]),
            name = df_radar$Proveedor[3],
            fill = 'toself',
            fillcolor = analisis$proveedores[[3]]$color,
            line = list(color = analisis$proveedores[[3]]$color)
          ) %>%
          layout(
            polar = list(
              radialaxis = list(
                visible = TRUE,
                range = c(0, 10)
              )
            ),
            title = "Comparativa de Proveedores (Puntuación de 0-10)",
            showlegend = TRUE
          )
      })
      
      # Tabla comparativa
      output$tabla_comparativa_proveedores <- renderDT({
        df <- analisis$tabla_comparativa
        
        # Añadir columna de optimización
        df$Optimizado <- "Auto-seleccionado por el modelo"
        
        datatable(
          df,
          options = list(pageLength = 3),
          rownames = FALSE,
          caption = "Comparativa de Proveedores"
        ) %>%
          formatCurrency(c('CostoBase', 'CostoTotal'), '€', digits = 2) %>%
          formatRound(c('TiempoEntrega', 'PuntuacionBalanceada'), digits = 1)
      })
      
      # Gráfico de escenarios de demanda
      output$grafico_escenario_demanda <- renderPlotly({
        nivel <- input$nivel_demanda
        escenario <- analisis$escenarios_demanda[[nivel]]
        demanda_personalizada <- input$demanda_personalizada
        
        if (nivel == "personalizada") {
          demanda <- demanda_personalizada
        } else {
          demanda <- escenario$demanda
        }
        
        # Preparar datos para el gráfico
        proveedores <- sapply(analisis$proveedores, function(p) p$nombre)
        capacidades <- sapply(analisis$proveedores, function(p) p$capacidad)
        
        # Crear un data frame para el gráfico
        df_grafico <- data.frame(
          Proveedor = proveedores,
          Capacidad = capacidades / 1000, # Convertir a miles de galones para comparar con demanda
          Deficit = pmax(0, demanda - capacidades / 1000),
          Exceso = pmax(0, capacidades / 1000 - demanda),
          Optimizado = c("Modelo PL", "Modelo PL", "Modelo PL")
        )
        
        # Añadir posibles combinaciones si no pueden cubrir individualmente
        p <- plot_ly(data = df_grafico) %>%
          add_trace(
            x = ~Proveedor,
            y = ~Capacidad,
            type = 'bar',
            name = 'Capacidad',
            marker = list(color = 'rgba(55, 128, 191, 0.7)')
          ) %>%
          add_trace(
            x = ~Proveedor,
            y = ~Deficit,
            type = 'bar',
            name = 'Déficit',
            marker = list(color = 'rgba(219, 64, 82, 0.7)')
          ) %>%
          add_trace(
            x = ~Proveedor,
            y = rep(demanda, length(proveedores)),
            type = 'scatter',
            mode = 'lines',
            name = 'Demanda',
            line = list(color = 'red', width = 2, dash = 'dash')
          ) %>%
          layout(
            title = paste0("Capacidad vs Demanda - Nivel: ", 
                           ifelse(nivel == "personalizada", 
                                  paste0("Personalizado (", demanda_personalizada, " miles de galones)"),
                                  paste0(toupper(substr(nivel, 1, 1)), substr(nivel, 2, nchar(nivel)), 
                                         " (", escenario$demanda, " miles de galones)"))),
            xaxis = list(title = ""),
            yaxis = list(title = "Miles de Galones"),
            barmode = 'stack'
          )
        
        return(p)
      })
      
      # Recomendación de proveedores
      output$recomendacion_proveedores <- renderText({
        nivel <- input$nivel_demanda
        escenario <- analisis$escenarios_demanda[[nivel]]
        
        if (nivel == "personalizada") {
          demanda <- input$demanda_personalizada
        } else {
          demanda <- escenario$demanda
        }
        
        texto <- paste0("ANÁLISIS DE ESCENARIO: DEMANDA ", toupper(nivel), " (", demanda, " miles de galones)\n\n")
        
        # Verificar proveedores individuales
        for (i in seq_along(escenario$proveedores)) {
          p <- escenario$proveedores[[i]]
          if (p$capacidad >= demanda) {
            texto <- paste0(texto, "* ", p$nombre, " puede cubrir el 100% de la demanda con un costo de €", 
                            round(p$costo_total * demanda * 1000, 2), ".\n")
          } else {
            porcentaje <- round(p$cobertura * 100, 1)
            texto <- paste0(texto, "* ", p$nombre, " solo puede cubrir el ", porcentaje, 
                            "% de la demanda (", p$capacidad, " de ", demanda, " miles de galones).\n")
          }
        }
        
        texto <- paste0(texto, "\nESTRATEGIAS DE ABASTECIMIENTO RECOMENDADAS:\n\n")
        
        # Analizar combinaciones
        if (length(escenario$combinaciones) > 0) {
          # Ordenar combinaciones por puntuación
          combinaciones_ordenadas <- escenario$combinaciones[order(sapply(escenario$combinaciones, 
                                                                          function(c) -c$puntuacion))]
          
          # Mostrar las mejores 3 combinaciones o todas si hay menos
          n_mostrar <- min(3, length(combinaciones_ordenadas))
          
          for (i in 1:n_mostrar) {
            c <- combinaciones_ordenadas[[i]]
            texto <- paste0(texto, i, ". Combinar ")
            
            for (j in 1:length(c$proveedores)) {
              if (c$unidades[j] > 0) {
                texto <- paste0(texto, c$proveedores[j], " (", c$unidades[j], " miles de galones)")
                if (j < length(c$proveedores) && sum(c$unidades[(j+1):length(c$unidades)]) > 0) {
                  texto <- paste0(texto, " + ")
                }
              }
            }
            
            texto <- paste0(texto, "\n   Costo promedio: €", round(c$costo_combinado, 2), 
                            " por galón\n   Tiempo promedio de entrega: ", round(c$tiempo_combinado, 1), 
                            " días\n   Puntuación: ", round(c$puntuacion * 10, 1), "/10\n\n")
          }
        } else {
          texto <- paste0(texto, "No hay combinaciones de proveedores que puedan cubrir completamente ",
                          "la demanda de ", demanda, " miles de galones.\n\nSe recomienda:\n",
                          "1. Aumentar capacidad de proveedores\n",
                          "2. Incorporar nuevos proveedores\n",
                          "3. Distribuir la demanda en múltiples períodos\n")
        }
        
        texto <- paste0(texto, "\nRECOMENDACIÓN FINAL (Optimización por Programación Lineal - ", 
                        ifelse(analisis$criterio == "costo", "Minimizando Costo", 
                               ifelse(analisis$criterio == "tiempo", "Minimizando Tiempo de Entrega", 
                                      "Balanceando Costo-Tiempo")), "):\n\n")
        
        if (length(escenario$combinaciones) > 0) {
          if (analisis$criterio == "costo") {
            # Encontrar la opción de menor costo
            idx_mejor <- which.min(sapply(escenario$combinaciones, function(c) c$costo_combinado))
            mejor <- escenario$combinaciones[[idx_mejor]]
          } else if (analisis$criterio == "tiempo") {
            # Encontrar la opción de menor tiempo
            idx_mejor <- which.min(sapply(escenario$combinaciones, function(c) c$tiempo_combinado))
            mejor <- escenario$combinaciones[[idx_mejor]]
          } else {
            # Encontrar la opción con mejor puntuación balanceada
            idx_mejor <- which.max(sapply(escenario$combinaciones, function(c) c$puntuacion))
            mejor <- escenario$combinaciones[[idx_mejor]]
          }
          
          texto <- paste0(texto, "La mejor estrategia es combinar ")
          
          for (j in 1:length(mejor$proveedores)) {
            if (mejor$unidades[j] > 0) {
              texto <- paste0(texto, mejor$proveedores[j], " (", mejor$unidades[j], " miles de galones)")
              if (j < length(mejor$proveedores) && sum(mejor$unidades[(j+1):length(mejor$unidades)]) > 0) {
                texto <- paste0(texto, " + ")
              }
            }
          }
          
          texto <- paste0(texto, "\nCosto total estimado: €", round(mejor$costo_combinado * demanda * 1000, 2),
                          "\nTiempo promedio de entrega: ", round(mejor$tiempo_combinado, 1), " días")
        } else {
          # Si ningún proveedor o combinación puede cubrir la demanda completamente
          # Recomendar el de mayor capacidad para minimizar el déficit
          capacidades <- sapply(escenario$proveedores, function(p) p$capacidad)
          idx_mayor_cap <- which.max(capacidades)
          
          texto <- paste0(texto, "Ninguna combinación puede cubrir la demanda completa.\n",
                          "Usar al máximo ", escenario$proveedores[[idx_mayor_cap]]$nombre, 
                          " para minimizar el déficit (", capacidades[idx_mayor_cap], " de ", 
                          demanda, " miles de galones).")
        }
        
        return(texto)
      })
      
      # Gráfico de análisis de sensibilidad
      output$grafico_sensibilidad <- renderPlotly({
        variable <- input$variable_sensibilidad
        datos <- analisis$sensibilidad[[variable]]
        rangos <- analisis$sensibilidad$rangos
        
        # Crear datos para el gráfico
        df_sensibilidad <- data.frame(
          Rango = rep(rangos, length(datos)),
          Valor = unlist(datos),
          Proveedor = rep(names(datos), each = length(rangos))
        )
        
        # Crear trazas para cada proveedor
        p <- plot_ly(data = df_sensibilidad) %>%
          add_trace(
            x = ~Rango,
            y = ~Valor,
            color = ~Proveedor,
            type = 'scatter',
            mode = 'lines+markers',
            colors = c('rgba(114, 78, 145, 0.7)', 'rgba(211, 94, 96, 0.7)', 'rgba(128, 133, 133, 0.7)')
          )
        
        # Configurar el layout
        titulo <- paste0("Análisis de Sensibilidad - Variación de ", 
                         ifelse(variable == "costo", "Costo por Galón",
                                ifelse(variable == "tiempo", "Tiempo de Entrega", "Capacidad")))
        
        p <- p %>% layout(
          title = titulo,
          xaxis = list(title = "Variación (%)", tickvals = rangos),
          yaxis = list(title = ifelse(variable == "capacidad", 
                                      "Capacidad (galones)", 
                                      "Costo Total (€/galón)")),
          showlegend = TRUE
        )
        
        return(p)
      })
      
    } else {
      # Mostrar mensaje de error si no hay proveedores habilitados
      output$grafico_comparativo_proveedores <- renderPlotly({
        plot_ly() %>%
          layout(
            title = "No hay proveedores habilitados para analizar",
            xaxis = list(showticklabels = FALSE),
            yaxis = list(showticklabels = FALSE)
          )
      })
      
      output$tabla_comparativa_proveedores <- renderDT({
        datatable(
          data.frame(Mensaje = analisis$message),
          options = list(dom = 't'),
          rownames = FALSE
        )
      })
    }
  })
  
  # Mostrar/ocultar formulario de proveedores
  observe({
    if (input$useProveedores) {
      shinyjs::show("p1_capacidad")
      shinyjs::show("p1_costo")
      shinyjs::show("p1_tiempoEntrega")
      shinyjs::show("p1_habilitado")
      
      shinyjs::show("p2_capacidad")
      shinyjs::show("p2_costo")
      shinyjs::show("p2_tiempoEntrega")
      shinyjs::show("p2_habilitado")
      
      shinyjs::show("p3_capacidad")
      shinyjs::show("p3_costo")
      shinyjs::show("p3_tiempoEntrega")
      shinyjs::show("p3_habilitado")
      
      shinyjs::show("penalizacion_tiempo")
      shinyjs::show("uso_minimo_proveedor")
    } else {
      shinyjs::hide("p1_capacidad")
      shinyjs::hide("p1_costo")
      shinyjs::hide("p1_tiempoEntrega")
      shinyjs::hide("p1_habilitado")
      
      shinyjs::hide("p2_capacidad")
      shinyjs::hide("p2_costo")
      shinyjs::hide("p2_tiempoEntrega")
      shinyjs::hide("p2_habilitado")
      
      shinyjs::hide("p3_capacidad")
      shinyjs::hide("p3_costo")
      shinyjs::hide("p3_tiempoEntrega")
      shinyjs::hide("p3_habilitado")
      
      shinyjs::hide("penalizacion_tiempo")
      shinyjs::hide("uso_minimo_proveedor")
    }
  })
}

# Ejecutar la aplicación
shinyApp(ui = ui, server = server)