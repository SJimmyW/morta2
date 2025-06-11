
library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(readr)
library(writexl)
library(readxl)
library(purrr)
library(httr)
library(jsonlite)

#install.packages("httr")
#install.packages("jsonlite")
SUPABASE_URL <- "https://tgegtucbuecasgtbhpfo.supabase.co"
SUPABASE_API_KEY <- "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJpc3MiOiJzdXBhYmFzZSIsInJlZiI6InRnZWd0dWNidWVjYXNndGJocGZvIiwicm9sZSI6ImFub24iLCJpYXQiOjE3NDk1OTkxNTAsImV4cCI6MjA2NTE3NTE1MH0.OPeAyhKj_GwUkWRM_Oc1Vcl5Lg2AP3-7fleoLVA4Whk"
SUPABASE_TABLE <- "eventos"

ui <- fluidPage(
  titlePanel("Carga de Datos (imaginación titulística = 0)"),
  
  tabsetPanel(
    
    tabPanel("Carga manual",
             sidebarLayout(
               sidebarPanel(
                 textInput("filtro_usuario",
                           "Usuario:",
                           placeholder = "Escribí tu nombre o ID"),
                 
                 selectInput("evento", "Evento a cargar:",
                             choices = c("Nacimiento", 
                                         "Tratamiento", 
                                         "Muerte", 
                                         "Desleche",
                                         "Alerta de mortalidad", 
                                         "Alerta de morbilidad")
                 ),
                 
                 conditionalPanel(
                   condition = "input.evento == 'Nacimiento'",
                   textInput("caravana_nac", "Caravana:"),
                   dateInput("fecha_nac", "Fecha de nacimiento:"),
                   selectInput("sexo", "Sexo del ternero/a:", 
                               choices = c("Macho", "Hembra")),
                   numericInput("peso", "Peso del Ternero/a:", value = NULL, min = 0, step = 0.5),
                   selectInput("calostrado", "Método de evaluación:", choices = c("Óptico", "Briks")),
                   sliderInput("nivel_calostrado", "Nivel de calostrado (1 a 5):", min = 1, max = 5, value = 3),
                   actionButton("agregar_nacimiento", "Agregar Nacimiento")
                 ),
                 
                 conditionalPanel(
                   condition = "input.evento == 'Tratamiento'",
                   selectInput("caravana_trat", "Caravana:", choices = NULL),
                   dateInput("fecha_trat", "Fecha del tratamiento:", value = Sys.Date()),
                   textInput("motivo_trat", "¿Motivo de tratamiento?:"),
                   actionButton("agregar_tratamiento", "Agregar")
                 ),
                 
                 conditionalPanel(
                   condition = "input.evento == 'Muerte'",
                   selectInput("caravana_muerte", "Caravana:", choices = NULL),
                   dateInput("fecha_muerte", "Fecha de muerte:", value = Sys.Date()),
                   textInput("motivo_muerte", "Causa de muerte:"),
                   actionButton("agregar_muerte", "Agregar")
                 ),
                 
                 conditionalPanel(
                   condition = "input.evento == 'Desleche'",
                   selectInput("caravana_desleche", "Caravana:", choices = NULL),
                   dateInput("fecha_desleche", "Fecha de desleche:", value = Sys.Date()),
                   numericInput("peso_desleche", "Peso del Ternero/a:", value = NULL, min = 0, step = 0.5),
                   actionButton("agregar_desleche", "Agregar")
                 ),
                 
                 conditionalPanel(
                   condition = "input.evento == 'Alerta de mortalidad'",
                   numericInput("alerta_mortalidad", "Valor (entre 0 a 100):", value = 5, min = 0, max = 100, step = 0.5),
                   actionButton("registrar_alerta_mort", "Registrar alerta")
                 ),
                 
                 conditionalPanel(
                   condition = "input.evento == 'Alerta de morbilidad'",
                   numericInput("alerta_morbilidad", "Valor (0 a 100):", value = 5, min = 0, max = 100, step = 0.5),
                   actionButton("registrar_alerta_morb", "Registrar alerta")
                 )
               ),
               mainPanel(
                 h4("Resumen de Eventos cargados"),
                 tableOutput("resumen_corto"),
                 downloadButton("descargar_datos", "Descargar todos los datos en Excel")
               )
             )
    ),
    
    tabPanel("Carga desde archivo",
             sidebarLayout(
               sidebarPanel(
                 fileInput("archivo_datos", "Seleccioná tu archivo base (xlsx)", accept = ".xlsx"),
                 helpText("El archivo debe cumplir con las especificaciones detalladas en el enlace a continuación 
                         (https://docs.google.com/spreadsheets/d/1ZW-wP_XXhVXDzzJWlfoos203AxnPJlY_/edit?usp=sharing)"),
                 textInput("ruta_archivo", "Ruta al archivo base (.xlsx)", value = "eventos.xlsx"),
                 actionButton("cargar_archivo", "Cargar Datos")
               ),
               mainPanel(
                 h4("Datos cargados desde archivo"),
                 tableOutput("resumen_archivo")
               )
             )
    ),
    
    tabPanel("Gráficos",
             fluidRow(
               column(6,
                      h4("Mortalidad mensual"),
                      plotOutput("plot_mortalidad")
               ),
               column(6,
                      h4("Morbilidad mensual"),
                      plotOutput("plot_morbilidad")
               )
             )
    ),
    
    tabPanel("Tabla de resumen",
             fluidRow(
               column(12,
                      h4("Resumen completo de eventos"),
                      tableOutput("resumen")
               )
             )
    )
  )
)


server <- function(input, output, session) {
  
  ######## almacenar en supabase ###########
  datos_cache <- reactiveVal(NULL)
  
  leer_supabase <- function(tabla, usuario) {
    sb_url <- Sys.getenv("SUPABASE_URL")
    sb_key <- Sys.getenv("SUPABASE_KEY")
    
    supabase_query(
      url = sb_url,
      key = sb_key
    ) %>%
      from(tabla) %>%
      eq("usuario", usuario) %>%
      select()
  }
  
  insert_evento <- function(usuario, fecha, tipo_evento, caravana, detalles) {
    url <- paste0(SUPABASE_URL, "/rest/v1/", SUPABASE_TABLE)
    
    body <- toJSON(list(
      usuario = usuario,
      fecha = as.character(fecha),
      tipo_evento = tipo_evento,
      caravana = caravana,
      detalles = detalles
    ), auto_unbox = TRUE)
    
    res <- POST(
      url,
      add_headers(
        `apikey` = SUPABASE_API_KEY,
        `Authorization` = paste("Bearer", SUPABASE_API_KEY),
        `Content-Type` = "application/json",
        `Prefer` = "return=representation"
      ),
      body = body
    )
    
    if (status_code(res) == 201) {
      message("Evento insertado correctamente.")
    } else {
      warning("Error al insertar evento: ", content(res, "text"))
    }
  }
  
  ########################################
  
  max_deleche <- 90 # máximo tiempo posible en guachera
  eventos <- reactiveValues(
    nacimientos = data.frame(caravana = character(), 
                             fecha = as.Date(character()),
                             sexo     = character(),
                             peso     = numeric() 
                             ),
    tratamientos = data.frame(caravana = character(), 
                              fecha = as.Date(character()),
                              motivo = character()),
    muertes = data.frame(caravana = character(), 
                         fecha = as.Date(character()),
                         motivo = character() ),
    desleches = data.frame(caravana = character(), 
                           fecha = as.Date(character()),
                           peso_desleche = numeric()),
    
    mortalidad = data.frame(fecha = as.Date(character()), 
                            valor = numeric()),
    
    morbilidad = data.frame(fecha = as.Date(character()), 
                            valor = numeric())
  )
  #######  conecta con supabase   #####
  observeEvent(input$guardar_evento, {
    req(input$usuario, input$fecha, input$tipo, input$caravana)
    
    detalles <- list(
      peso = input$peso,
      comentario = input$comentario
    )
    
    insert_evento(
      usuario = input$usuario,
      fecha = input$fecha,
      tipo_evento = input$tipo,
      caravana = input$caravana,
      detalles = detalles
    )
  })
  ########

  # Escribe en Supabase 
  escribir_supabase <- function(tabla, datos) {
    sb_url <- Sys.getenv("SUPABASE_URL")
    sb_key <- Sys.getenv("SUPABASE_KEY")
    
    supabase_query(
      url = sb_url,
      key = sb_key
    ) %>%
      from(tabla) %>%
      insert(datos)
  }
  
  ##### traer solo lo necesario y filtrar
  datos_usuario <- reactive({
    req(input$usuario)
    
    supabase_query(
      url = Sys.getenv("SUPABASE_URL"),
      key = Sys.getenv("SUPABASE_KEY")
    ) %>%
      from("eventos") %>%
      eq("usuario", input$usuario) %>%
      select()
  })
  
  observeEvent(input$usuario, {
    
    datos <- leer_supabase("eventos", input$usuario) #datos <- datos_usuario()
    
    datos_cache(datos)
    
    eventos$nacimientos <- datos |> 
      filter(tipo_evento == "Nacimiento") |> 
      parse_tu_formato()  
    #eventos$nacimientos <- datos |> filter(tipo_evento == "Nacimiento") |> 
      mutate(fecha = as.Date(fecha),
             peso = map_dbl(detalles, "peso"),
             sexo = map_chr(detalles, "sexo")) |> 
      select(caravana, fecha, sexo, peso)
    
    eventos$tratamientos <- datos |> 
      filter(tipo_evento == "Tratamiento") |> 
      parse_tu_formato()  
      mutate(fecha = as.Date(fecha),
             motivo = map_chr(detalles, "comentario")) |> 
      select(caravana, fecha, motivo)
    
    eventos$muertes <- datos |> 
      filter(tipo_evento == "Muerte") |> 
      parse_tu_formato()  
      mutate(fecha = as.Date(fecha),
             motivo = map_chr(detalles, "comentario")) |> 
      select(caravana, fecha, motivo)
    
    eventos$desleches <- datos |> 
      filter(tipo_evento == "Desleche") |> 
      parse_tu_formato()  
    mutate(fecha = as.Date(fecha),
             peso_desleche = map_dbl(detalles, "peso")) |> 
      select(caravana, fecha, peso_desleche)
  }), ignoreNULL = TRUE, ignoreInit = TRUE)
  ############
  caravanas_activas <- reactive({
    
    nacidas <- unique(eventos$nacimientos$caravana)
    muertas <- unique(eventos$muertes$caravana)
    deslechadas <- unique(eventos$desleches$caravana)
    
    base::setdiff(nacidas, union(muertas, deslechadas))
  })
  # Carga manual
  observeEvent(input$agregar_nacimiento, {
   
    req(input$usuario, input$fecha_nac, input$caravana_nac, input$sexo)
    
    nuevo <- tibble(
      usuario     = input$usuario,
      fecha       = as.character(input$fecha_nac),
      tipo_evento = "Nacimiento",
      caravana    = input$caravana_nac,
      detalles    = list(list(
        peso       = input$peso,
        comentario = input$sexo
      ))
    )
    
    datos_act <- datos_cache()
    
    nacidos <- datos_act %>%
      filter(
        usuario     == input$usuario,
        tipo_evento == "Nacimiento",
        caravana    == input$caravana_nac,
        fecha       == as.character(input$fecha_nac)
      ) %>%
      nrow() > 0
    
   # nacidos <- any(  eventos$nacimientos$caravana == input$caravana_nac &      eventos$nacimientos$fecha    == input$fecha_nac    )
    
    if (nacidos) {
      showNotification( paste0(
        "⚠️ Ya se registró el nacimiento para la caravana ", input$caravana_nac,
        " hoy.", type = "warning", duration = 5
        )
        )
      return()
      
    } 
    
    insert_evento(
      usuario = input$usuario,
      fecha = input$fecha_nac,
      tipo_evento = "Nacimiento",
      caravana = input$caravana_nac,
      detalles = list(
        peso = input$peso,
        comentario = input$sexo
      )
    )
    
    datos_cache(bind_rows(datos_act, nuevo))
    
    eventos$nacimientos <- bind_rows(
      eventos$nacimientos,
      data.frame(
        caravana = input$caravana_nac,
        fecha    = input$fecha_nac,
        sexo     = input$sexo,
        peso     = input$peso,
        stringsAsFactors = FALSE
      )
    )
    
    showNotification(
      "✅ Nacimiento registrado correctamente",
      type = "message", duration = 3
    )

  })
  
  observeEvent(input$agregar_tratamiento, {
    
    req(input$usuario, input$fecha_trat, input$caravana_trat)
    
    nuevo <- tibble(
      usuario     = input$usuario,
      fecha       = as.character(input$fecha_trat),
      tipo_evento = "Tratamiento",
      caravana    = input$caravana_trat,
      detalles    = list(list(
        peso       = input$motivo
        ))
    )
    
    datos_act <- datos_cache()
    
    #tratados <- any(eventos$tratamientos$caravana == input$caravana_trat &  eventos$tratamientos$fecha    == input$fecha_trat   )
    
    tratados <- datos_act %>%
      filter(
        usuario     == input$usuario,
        tipo_evento == "Tratamiento",
        caravana    == input$caravana_trar,
        fecha       == as.character(input$fecha_trat)
      ) %>%
      nrow() > 0
    
    if (tratados) {
      showNotification( paste0(
        "⚠️ Ya se registró un tratamiento para la caravana ", input$caravana_trat, 
        " hoy", type = "warning", duration = 5
        )
        )
      
      return()
      
    } 
    
    insert_evento(
      usuario = input$usuario,
      fecha = input$fecha_trat,
      tipo_evento = "Tratamiento",
      caravana = input$caravana_trat,
      detalles = list(
         comentario = input$motivo_trat
      )
    )
    
    datos_cache(bind_rows(datos_act, nuevo))
    
    eventos$tratamientos <- bind_rows(
      eventos$nacimientos,
      data.frame(
        caravana = input$caravana_trat,
        fecha    = input$fecha_trat,
        motivo     = input$motivo_trat,
        stringsAsFactors = FALSE
      )
    )
    
    showNotification("✅ Tratamiento registrado correctamente", 
                     type = "message", duration = 3)
    
    
  })
  
  observeEvent(input$agregar_muerte, {
    
    req(input$usuario, input$fecha_muerte, input$caravana_muerte)
    
    nuevo <- tibble(
      usuario     = input$usuario,
      fecha       = as.character(input$fecha_muerte),
      tipo_evento = "Muerte",
      caravana    = input$caravana_muerte,
      detalles    = list(list(
        comentario = input$motivo_muerte
      ))
    )
    
    datos_act <- datos_cache()
    
    insert_evento(
      usuario = input$usuario,
      fecha = input$fecha_muerte,
      tipo_evento = "Muerte",
      caravana = input$caravana_muerte,
      detalles = list(
        comentario = input$motivo_muerte
      )
    )
    
    datos_cache(bind_rows(datos_act, nuevo))
    
    eventos$muertes <- bind_rows(
      eventos$muertes,
      data.frame(
        caravana = input$caravana_muerte,
        fecha    = input$fecha_muerte,
        motivo   = input$motivo_muerte,
        stringsAsFactors = FALSE
      )
    )
    
    showNotification(
      "✅ Muerte registrada correctamente",
      type = "message", duration = 3
    )  
  })
  
  observeEvent(input$agregar_desleche, {
    
    req(input$usuario, input$fecha_desleche, input$caravana_desleche)
    
    nacido <- eventos$nacimientos[eventos$nacimientos$caravana == input$caravana_desleche, ]
    
    if (nrow(nacido) == 0) {
      showNotification(
        "⚠️ No se encontró un nacimiento registrado para esta caravana.",
        type = "error", duration = 5
      )
      return()
    }
    
    fecha_nac <- nacido$fecha[1]
    
    if (input$fecha_desleche < fecha_nac) {
      showNotification(
        "❌ La fecha de desleche no puede ser anterior a la de nacimiento.",
        type = "error", duration = 5
      )
      return()
    }
    
    datos_act <- datos_cache()
    
    ya_deslechado <- datos_act %>%
      filter(
        usuario     == input$usuario,
        tipo_evento == "Desleche",
        caravana    == input$caravana_desleche,
        fecha       == as.character(input$fecha_desleche)
      ) %>%
      nrow() > 0
    
    if (ya_deslechado) {
      showNotification(
        paste0("⚠️ Ya se registró un desleche para la caravana ", input$caravana_desleche, " en esa fecha."),
        type = "warning", duration = 5
      )
      return()
    }
    
    nuevo <- tibble(
      usuario     = input$usuario,
      fecha       = as.character(input$fecha_desleche),
      tipo_evento = "Desleche",
      caravana    = input$caravana_desleche,
      detalles    = list(list(
        peso = input$peso_desleche
      ))
    )
    
    insert_evento(
      usuario = input$usuario,
      fecha = input$fecha_desleche,
      tipo_evento = "Desleche",
      caravana = input$caravana_desleche,
      detalles = list(
        peso = input$peso_desleche
      )
    )
    
    datos_cache(bind_rows(datos_act, nuevo))
    
    eventos$desleches <- bind_rows(
      eventos$desleches,
      data.frame(
        caravana       = input$caravana_desleche,
        fecha          = input$fecha_desleche,
        peso_desleche  = input$peso_desleche,
        stringsAsFactors = FALSE
      )
    )
    
    showNotification(
      "✅ Desleche registrado correctamente",
      type = "message", duration = 3
    )
  })
  
  ###  controla que ningún ternero permanezca más de max_desleche#####################
  control_calidad <- function() {
    df <- eventos$nacimientos
    mu <- eventos$muertes
    ds <- eventos$desleches
    
    df <- df %>% 
      left_join(mu, by = "caravana", suffix = c("", ".mu")) %>%
      left_join(ds, by = "caravana", suffix = c("", ".ds")) %>%
      mutate(
        fecha_fin = pmap_dbl(
          list(fecha, fecha.mu, fecha.ds),
          ~ min(
            ..1 + days(max_deleche),
            coalesce(..2, ..1 + days(max_deleche)),
            coalesce(..3, ..1 + days(max_deleche)),
            na.rm = TRUE
          )
        ),
        excede = fecha_fin < today()
      ) %>% filter(excede)
    
    if (nrow(df) > 0) {
      nuevos <- tibble(
        caravana = df$caravana,
        fecha = today(),
        peso_desleche = NA_real_
      )
      eventos$desleches <- bind_rows(eventos$desleches, nuevos)
      eventos$tratamientos <- bind_rows(
        eventos$tratamientos,
        tibble(caravana = df$caravana, fecha = today(), motivo = "Desleche automático")
      )
    }
  }
  # Carga desde archivo
  observeEvent(input$cargar_archivo, {
    req(input$archivo_datos)
    datos <- read_excel(input$archivo_datos$datapath)
    
    if ("fecha" %in% names(datos)) {
      datos$fecha <- as.Date(datos$fecha)
    }
    
    if ("evento" %in% names(datos)) {
      
      if ("Nacimiento" %in% datos$evento) {
        nac <- filter(datos, evento == "Nacimiento") %>%
          select(caravana, fecha, sexo, peso)
        eventos$nacimientos <- bind_rows(eventos$nacimientos, nac)
      }
      if ("Tratamiento" %in% datos$evento) {
        trat <- filter(datos, evento == "Tratamiento") %>%
          select(caravana, fecha, motivo)
        eventos$tratamientos <- bind_rows(eventos$tratamientos, trat)
      }
      if ("Muerte" %in% datos$evento) {
        muerte <- filter(datos, evento == "Muerte") %>%
          select(caravana, fecha)
        eventos$muertes <- bind_rows(eventos$muertes, muerte)
      }
      if ("Desleche" %in% datos$evento) {
        des <- filter(datos, evento == "Desleche") %>%
          select(caravana, fecha)
        eventos$desleches <- bind_rows(eventos$desleches, des)
      }
      
    }
    
    control_calidad()
  })
  # vector reactivo, actualiza las disponibles
  observe({
    caravanas <- caravanas_activas()
   # caravanas <- unique(eventos$nacimientos$caravana)
    
    updateSelectInput(session, "caravana_trat", 
                      choices = caravanas_activas())
    updateSelectInput(session, "caravana_muerte", 
                      choices = caravanas_activas())
    updateSelectInput(session, "caravana_desleche", 
                      choices = caravanas_activas())
    
    })
  # Tabla resumen
  calculos <- reactive({ 
    
    datos <- datos_cache()
    req(!is.null(datos), nrow(datos) > 0)
    
    nacidos   <- datos |> filter(tipo_evento == "Nacimiento") |>  # nacidos <- eventos$nacimientos
    muertes   <- datos |> filter(tipo_evento == "Muerte")      |> # muertes <- eventos$muertes
    desleches <- datos |> filter(tipo_evento == "Desleche")    |> # desleches <- eventos$desleches
    tratados  <- datos |> filter(tipo_evento == "Tratamiento") |> # 
      
      ternero_guachera_hoy <- nrow(nacidos) - 
      (nrow(muertes) + nrow(desleches))
    # ternero_guachera_hoy <- nrow(eventos$nacimientos) -       (nrow(eventos$muertes) + nrow(eventos$desleches))
    # muertos <-  nrow(eventos$muertes)
    
    ternero_dia <- ternero_guachera_hoy / days_in_month(today())

    mortalidad <- muertes / ternero_dia
    
    hoy <- Sys.Date()
    
    deslechados <- desleches %>%
      inner_join(nacidos, by="caravana") %>%
      transmute(
        caravana,
        sexo,
        fecha_nac  = fecha.y,
        fecha_des  = fecha.x,
        edad_desleche = as.integer(fecha.x - fecha.y),
        peso_desleche
      )
    
    edad_desleche <- mean(deslechados$edad_desleche,na.rm = TRUE)
    
    # por sexo
    desleche_machos <- deslechados %>%
      filter(sexo == "Macho") %>%
      summarise(prom = mean(edad_desleche, na.rm = TRUE)) %>% 
      pull(prom)
    
    desleche_hembras <- deslechados %>%
      filter(sexo == "Hembra") %>%
      summarise(prom = mean(edad_desleche, na.rm = TRUE)) %>% 
      pull(prom)
    
    pesos <- desleches %>%
      inner_join(nacidos, by = "caravana") %>%
      transmute(
        caravana,
        sexo,
        fecha_nac      = fecha.y,
        peso_nac       = peso,
        fecha_desleche = fecha.x,
        peso_desleche,
        dias           = as.numeric(fecha.x - fecha.y),
        gpv_diaria     = ifelse( is.na((peso_desleche - peso) / as.numeric(fecha.x - fecha.y)), 
                                 0,
                                 (peso_desleche - peso) / as.numeric(fecha.x - fecha.y)
        ) 
        
      )
    
    gpv <- mean(pesos$gpv_diaria, na.rm = TRUE)  
    
    gpv_hembras <- pesos %>%
      filter(sexo == "Hembra") %>%
      summarise(prom = mean(gpv_diaria, na.rm = TRUE)) %>% 
      pull(prom)
    
    gpv_machos <- pesos %>%
      filter(sexo == "Macho") %>%
      summarise(prom = mean(gpv_diaria, na.rm = TRUE)) %>% 
      pull(prom)
    
    
    list(
      nacidos_hoy    = nrow(filter(eventos$nacimientos,  fecha == hoy)),
      tratados_hoy   = nrow(filter(eventos$tratamientos, fecha == hoy)),
      muertos_hoy    = nrow(filter(eventos$muertes,      fecha == hoy)),
      deslechado_hoy = nrow(filter(eventos$desleches,    fecha == hoy)),
      ternero_guachera = ternero_guachera_hoy,
      ternero_dia    = ternero_dia,
      mortalidad     = ifelse(ternero_dia > 0 & muertos > 0,
                              mortalidad, "No disponible"),
      edad_desleche  = ifelse( is.na(edad_desleche), 
                               "No disponible",
                               edad_desleche
                               ) ,
      edad_desleche_machos = ifelse( is.na(desleche_machos), 
                                    "No disponible",
                                    desleche_machos
                                    ) ,
      edad_desleche_hembras = ifelse(is.na(desleche_hembras),
                                     "No disponible",
                                      desleche_hembras
                                      ),
      gpv = ifelse(is.nan(gpv), "No disponible", round(gpv, 3))
      
    )
  })
  
  output$resumen <- renderTable({
    
    data <- calculos()
    
    data.frame(
      Evento = c("Nacimientos", "Tratamientos", "Muertes", "Desleches", 
                 "Terneros en guachera hoy",
                 "Promedio de terneros dia", "% de mortalidad",
                 "Edad desleche promedio (días)", 
                 "Edad desleche en machos (días promedio)",
                 "Edad desleche en hembras (días promedio)",
                 "Ganancia de peso vivo promedio",
                 "ganancia de peso vivo promedio hembras",
                 "ganancia de peso vivo promedio machos"),
      
      Cantidad = c(
        data$nacidos_hoy,         # nrow(filter(eventos$nacimientos,  fecha == hoy)),    #  nrow(eventos$nacimientos),
        data$tratados_hoy,        # nrow(filter(eventos$tratamientos, fecha == hoy)),   # nrow(eventos$tratamientos),
        data$muertos_hoy,         # nrow(filter(eventos$muertes,      fecha == hoy)),        # muertos,
        data$deslechado_hoy,      # nrow(filter(eventos$desleches,    fecha == hoy)),                      #nrow(eventos$desleches),
        data$ternero_guachera,    # ternero_guachera_hoy,
        data$ternero_dia,         # ternero_dia ,
        data$mortalidad,
        data$edad_desleche,       # edad_desleche, 
        data$edad_desleche_machos, # (deleche_machos),
        data$edad_desleche_hembras,     # (desleche_hembras)
        data$gpv,
        data$gpv_hembras,
        data$gpv_machos# ganancia de peso vivo
        
      )
    )
  })
  
  output$resumen_corto <- renderTable({
    
    data <- calculos()
    
    data.frame(
      Evento = c("Terneros en guachera hoy",
                 "Promedio de terneros dia", "% de mortalidad",
                 "Edad desleche promedio (días)"),
      
      Cantidad = c(
        data$ternero_guachera,    # ternero_guachera_hoy,
        data$ternero_dia,         # ternero_dia ,
        data$mortalidad,
        data$edad_desleche       # edad_desleche, 
                        # ganancia de peso vivo
        
      )
    )
  })
  
  calculo_riesgo <- reactive({
    
    datos <- datos_cache()
    req(!is.null(datos), nrow(datos) > 0)
    
    nacidos   <- datos |> filter(tipo_evento == "Nacimiento") |>  # nacidos <- eventos$nacimientos
    muertes   <- datos |> filter(tipo_evento == "Muerte")      |> # muertes <- eventos$muertes
    desleches <- datos |> filter(tipo_evento == "Desleche")    |> # desleches <- eventos$desleches
    tratados  <- datos |> filter(tipo_evento == "Tratamiento") |> # tratados <- eventos$tratamientos
    
    umbral_mortalidad <- input$alerta_mortalidad
    umbral_morbilidad <- input$alerta_morbilidad
    
    guachera_dias <- nacidos %>%
      mutate(
        fecha_fin = pmin(
          fecha + days(max_deleche),
          coalesce(muertes$fecha[match(caravana, muertes$caravana)],
                   fecha + days(max_deleche)),
          coalesce(desleches$fecha[match(caravana, desleches$caravana)],
                   fecha + days(max_deleche))
        ),
        dias = map2(fecha, fecha_fin, ~ seq(.x, .y, by = "1 day"))
      ) %>%
      tidyr::unnest(dias)
    
    terneros_por_dia <- guachera_dias %>%
      group_by(dia = dias) %>%
      summarise(terneros = n(), .groups = "drop") %>%
      mutate(mes = floor_date(dia, "month"))
    
    promedio_mes <- terneros_por_dia %>%
      group_by(mes) %>%
      summarise(promedio_diario = mean(terneros), .groups = "drop")
    
    muertos_mes <- eventos$muertes %>%
      mutate(mes = floor_date(fecha, "month")) %>%
      group_by(mes) %>%
      summarise(muertes = n(), .groups = "drop")
    
    datos_mortalidad <- left_join(promedio_mes, muertos_mes,
                                  by = "mes") %>%
      replace_na(list(muertes = 0)) %>%
      mutate(
        mortalidad = ifelse(promedio_diario > 0, 
                            (muertes / promedio_diario) * 100, NA),
        color = ifelse(mortalidad >= umbral_mortalidad, "supera", "normal")
      )
    
    morb <- eventos$tratamientos %>%
      mutate(mes = floor_date(fecha, "month")) %>%
      group_by(mes) %>%
      summarise(caravanas_tratadas = n_distinct(caravana), .groups = "drop")
    
    datos_morbilidad <- left_join(promedio_mes, morb, by = "mes") %>%
      replace_na(list(caravanas_tratadas = 0)) %>%
      mutate(
        morbilidad = ifelse(promedio_diario > 0,
                            (caravanas_tratadas / promedio_diario) * 100,
                            NA),
        color = ifelse(morbilidad >= umbral_morbilidad, "supera", "normal")
      )
    list(
      nacidos = eventos$nacimientos,
      muertes = eventos$muertes,
      tratados = eventos$tratamientos,
      datos_mortalidad = datos_mortalidad,
      datos_morbilidad = datos_morbilidad
    )

    })
  
  output$plot_mortalidad <- renderPlot({
    
    data <- calculo_riesgo()
    
    datos_mortalidad <- data$datos_mortalidad
   
    req(nrow(data$nacidos) > 0, nrow(data$muertes) > 0)
    
    umbral <- input$alerta_mortalidad

    ggplot(datos_mortalidad, aes(x = mes, y = mortalidad, fill = color)) +
      geom_col() +
      scale_fill_manual(values = c("normal" = "skyblue", 
                                   "supera" = "red")) +
      labs( x = "Mes", y = "Mortalidad (%)") +
      guides(fill = "none") +
      theme_minimal()
    
    
  })
  
  output$plot_morbilidad <- renderPlot({
    
    data <- calculo_riesgo()
    datos_morbilidad <- data$datos_morbilidad
    
    req(nrow(data$nacidos) > 0, nrow(data$muertes) > 0)
    
    umbral <- input$alerta_morbilidad
    
    
    ggplot(datos_morbilidad, aes(x = mes, y = morbilidad)) +
      geom_line(color = "black") +
      #geom_point(aes(color = color), size = 3) +
      geom_smooth(method = "loess", se = FALSE,
                  color = "darkgreen", linewidth = 1) +
      scale_color_manual(values = c("normal" = "skyblue",
                                    "supera" = "red")) +
      labs(title = "", x = "Mes", y = "Morbilidad (%)") +
      theme_minimal()
  })
  
  # Descarga  en Excel
  output$descargar_datos <- downloadHandler(
    
    filename = function() {
      if (!is.null(input$archivo_datos)) {
        basename(input$archivo_datos$name)
      } else {
        paste0("eventos-", Sys.Date(), ".xlsx")
      }
    },
    
    content = function(file) {
      # Crear un único data frame
      df_novedades <- bind_rows(
        eventos$nacimientos %>% mutate(evento = "Nacimiento", 
                                       motivo = NA_character_),
        eventos$tratamientos %>% mutate(evento = "Tratamiento"),
        eventos$muertes %>% mutate(evento = "Muerte", 
                                   motivo = NA_character_),
        eventos$desleches %>% mutate(evento = "Desleche",
                                     motivo = NA_character_)
      ) %>%
        select(caravana, fecha, evento, motivo)
      
      if (!is.null(input$archivo_datos)) {
        datos_existentes <- read_excel(input$archivo_datos$datapath)
        
        datos_actualizados <- bind_rows(datos_existentes, df_novedades)
        
        write_xlsx(datos_actualizados, path = file)
        
      } else {
        write_xlsx(df_novedades, path = file)
      }
    }
  )
  session$onSessionEnded(function() {

    df_novedades <- bind_rows(
      isolate(eventos$nacimientos) %>% mutate(evento = "Nacimiento", 
                                     motivo = NA_character_),
      isolate(eventos$tratamientos) %>% mutate(evento = "Tratamiento"),
      isolate(eventos$muertes) %>% mutate(evento = "Muerte", 
                                 motivo = NA_character_),
      isolate(eventos$desleches) %>% mutate(evento = "Desleche", 
                                   motivo = NA_character_)
    ) %>%
      select(caravana, fecha, evento, motivo)
    
    # Definir la ruta donde guardar
    ruta_guardado <- paste0("www/eventos-guardados-", Sys.Date(), ".xlsx")
    
    if (!dir.exists("www")) {
      dir.create("www")  # Crear carpeta www si no existe
    }
    
    archivo <- isolate(input$archivo_datos)
    ruta <- isolate(input$archivo_datos$datapath)
    
    #if (!is.null(input$archivo_datos)) {
     if (!is.null(archivo)) {
      # Si hubo archivo cargado
      datos_existentes <- read_excel(ruta)
      datos_actualizados <- bind_rows(datos_existentes, df_novedades)
      write_xlsx(datos_actualizados, path = ruta_guardado)
    } else {
      write_xlsx(df_novedades, path = ruta_guardado)
    }
    
    cat("Archivo guardado automáticamente en", ruta_guardado, "\n")
  })
  
}

shinyApp(ui = ui, server = server)
