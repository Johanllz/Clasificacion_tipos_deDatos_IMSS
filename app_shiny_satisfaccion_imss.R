# Shiny app para probar el modelo 12_modelo_satisfaccion.rds
# Instrucciones:
# 1) Coloca los archivos siguientes en la misma carpeta que este app.R:
#    - 12_modelo_satisfaccion.rds
#    - 11_datos_procesados.csv   (opcional, para cargar ejemplos)
#    - 09_matriz_confusion.csv   (opcional, para mostrar matriz ya calculada)
# 2) Abre este archivo en RStudio y presiona "Run App" o usa: shiny::runApp("path/to/folder")
# 3) La app intentará usar predict(..., type = "prob") si está disponible; si no, usará predict(..., type = "raw").

library(shiny)
library(readr)
library(caret)    # usado si el modelo es un objeto caret
library(DT)

# Helper: convierte inputs lógicos (checkbox) en columnas que el modelo espera
make_newdata <- function(inputs){
  # Ajusta esto según las variables que usaste en tu modelo.
  # Aquí definimos un conjunto de features típico basado en el reporte del usuario.
  df <- data.frame(
    amable_Si = as.integer(inputs$amable),
    responder_Si = as.integer(inputs$responder),
    escuchar_Si = as.integer(inputs$escuchar),
    citapre_Si = as.integer(inputs$citapre),
    btratou_num = as.numeric(inputs$btratou),
    edad = as.numeric(inputs$edad),
    sexo_M = ifelse(inputs$sexo == "M", 1, 0)
  )
  # Asegurar nombres de columnas y tipos coincidan con lo que espera el modelo
  return(df)
}

ui <- fluidPage(
  titlePanel("Probador rápido - Modelo satisfacción IMSS"),
  sidebarLayout(
    sidebarPanel(
      h4("Cargar modelo y datos"),
      fileInput("model_file", "Modelo (.rds)", accept = c('.rds')),
      fileInput("data_file", "Datos procesados (11_datos_procesados.csv) - opcional", accept = c('.csv')),
      actionButton("load_example", "Cargar ejemplo de la tabla (si hay datos)", icon = icon('table')),
      hr(),
      h4("Entradas para predicción"),
      numericInput("edad", "Edad:", value = 49, min = 0, max = 120),
      selectInput("sexo", "Sexo:", choices = c("M", "F"), selected = "M"),
      checkboxInput("amable", "Amable: Sí", value = TRUE),
      checkboxInput("responder", "Responde dudas: Sí", value = TRUE),
      checkboxInput("escuchar", "Escucha: Sí", value = TRUE),
      checkboxInput("citapre", "Cita previa: Sí", value = FALSE),
      numericInput("btratou", "Calidad del trato (numérico):", value = 4, min = 0, max = 10, step = 1),
      actionButton("predict_btn", "Predecir", class = "btn-primary"),
      width = 3
    ),
    mainPanel(
      fluidRow(
        column(6, h4("Estado del modelo"), verbatimTextOutput("modelo_info")),
        column(6, h4("Predicción"), verbatimTextOutput("pred_text"))
      ),
      fluidRow(
        column(6, h4("Probabilidades"), tableOutput("prob_tab")),
        column(6, h4("Importancia de variables (si disponible)"), plotOutput("imp_plot"))
      ),
      hr(),
      h4("Ejemplos de datos (si cargaste CSV)"),
      DTOutput("tabla_muestras"),
      hr(),
      h4("Matriz de confusión (si se provee o puede calcularse)"),
      tableOutput("matriz_conf")
    )
  )
)

server <- function(input, output, session){
  rv <- reactiveValues(model = NULL, data = NULL, model_loaded = FALSE)
  
  observeEvent(input$model_file, {
    req(input$model_file)
    tryCatch({
      rv$model <- readRDS(input$model_file$datapath)
      rv$model_loaded <- TRUE
      output$modelo_info <- renderText({
        paste0("Modelo cargado: ", class(rv$model)[1], "\nRuta: ", input$model_file$name)
      })
    }, error = function(e){
      rv$model <- NULL
      rv$model_loaded <- FALSE
      output$modelo_info <- renderText({paste("Error al cargar modelo:", e$message)})
    })
  })
  
  observeEvent(input$data_file, {
    req(input$data_file)
    tryCatch({
      df <- read_csv(input$data_file$datapath, show_col_types = FALSE)
      rv$data <- df
      output$tabla_muestras <- renderDT({
        datatable(head(df, 50), selection = 'single', options = list(scrollX = TRUE))
      })
    }, error = function(e){
      rv$data <- NULL
      output$tabla_muestras <- renderDT({datatable(data.frame(error = e$message))})
    })
  })
  
  # Llenar inputs con una fila seleccionada del CSV
  observeEvent(input$load_example, {
    if(!is.null(rv$data)){
      row <- rv$data[1, , drop = FALSE]
      # Intentamos mapear columnas si existen
      if("edad" %in% names(row)) updateNumericInput(session, "edad", value = as.numeric(row$edad))
      if("sexo" %in% names(row)) updateSelectInput(session, "sexo", selected = as.character(row$sexo))
      if("amable_Si" %in% names(row)) updateCheckboxInput(session, "amable", value = as.integer(row$amable_Si) == 1)
      if("responder_Si" %in% names(row)) updateCheckboxInput(session, "responder", value = as.integer(row$responder_Si) == 1)
      if("escuchar_Si" %in% names(row)) updateCheckboxInput(session, "escuchar", value = as.integer(row$escuchar_Si) == 1)
      if("citapre_Si" %in% names(row)) updateCheckboxInput(session, "citapre", value = as.integer(row$citapre_Si) == 1)
      if("btratou_num" %in% names(row)) updateNumericInput(session, "btratou", value = as.numeric(row$btratou_num))
    } else {
      showNotification("No hay datos cargados. Carga 11_datos_procesados.csv para usar ejemplos.", type = "warning")
    }
  })
  
  observeEvent(input$predict_btn, {
    newd_raw <- make_newdata(input)
    
    if(!rv$model_loaded){
      showNotification("No hay modelo cargado. Carga 12_modelo_satisfaccion.rds.", type = "error")
      return()
    }
    
    mod <- rv$model
    
    # Helper: intenta alinear newdata con las columnas que el modelo espera
    align_newdata <- function(mod, newd){
      msgs <- character()
      expected <- character()
      
      # 1) intentar obtener trainingData (util para caret/rpart con trainingData)
      if(!is.null(mod$trainingData)){
        td_names <- setdiff(names(mod$trainingData), ".outcome")
        expected <- unique(c(expected, td_names))
        msgs <- c(msgs, paste0("Se detectó trainingData en el modelo (columns: ", paste(head(td_names, 10), collapse = ", "), if(length(td_names)>10) ", ..." else "", ")"))
      }
      
      # 2) intentar extraer términos/fórmula
      terms_obj <- tryCatch(terms(mod), error = function(e) NULL)
      if(!is.null(terms_obj)){
        pred_terms <- tryCatch(stats::delete.response(terms_obj), error = function(e) NULL)
        if(!is.null(pred_terms)){
          tlabels <- attr(pred_terms, "term.labels")
          expected <- unique(c(expected, tlabels))
          msgs <- c(msgs, paste0("Términos detectados desde terms(): ", paste(head(tlabels, 10), collapse = ", "), if(length(tlabels)>10) ", ..." else ""))
        }
      } else if(!is.null(mod$call) && !is.null(mod$call$formula)){
        try({
          f <- as.formula(mod$call$formula)
          pred_terms2 <- attr(stats::delete.response(terms(f)), "term.labels")
          expected <- unique(c(expected, pred_terms2))
          msgs <- c(msgs, paste0("Términos detectados desde mod$call$formula: ", paste(head(pred_terms2, 10), collapse = ", ")))
        }, silent = TRUE)
      }
      
      # 3) si no encontramos nada, intentar inferir nombres desde variable.importance o names(mod)
      if(length(expected)==0){
        if(!is.null(mod$variable.importance)) expected <- unique(c(expected, names(mod$variable.importance)))
        if(length(expected)>0) msgs <- c(msgs, "Se infirieron variables desde variable.importance.")
      }
      
      expected <- unique(expected)
      msgs <- c(msgs, paste0("Columnas esperadas (candidatas): ", paste(head(expected, 30), collapse = ", "), if(length(expected)>30) ", ..." else ""))
      
      # Crear copia para modificar
      nd <- as.data.frame(newd, stringsAsFactors = FALSE)
      
      # Para cada expected que no esté en nd, añadir con tipo adecuado (si trainingData disponible, usar sus clases/levels)
      for(col in expected){
        if(!(col %in% names(nd))){
          if(!is.null(mod$trainingData) && col %in% names(mod$trainingData)){
            prototype <- mod$trainingData[[col]]
            if(is.factor(prototype)){
              nd[[col]] <- factor(NA, levels = levels(prototype))
              msgs <- c(msgs, paste0("Añadida columna factor '", col, "' con niveles del trainingData"))
            } else if(is.integer(prototype)){
              nd[[col]] <- as.integer(0)
              msgs <- c(msgs, paste0("Añadida columna integer '", col, "' (0)"))
            } else if(is.numeric(prototype)){
              nd[[col]] <- as.numeric(0)
              msgs <- c(msgs, paste0("Añadida columna numeric '", col, "' (0)"))
            } else if(is.character(prototype)){
              nd[[col]] <- as.character(NA)
              msgs <- c(msgs, paste0("Añadida columna character '", col, "' (NA)"))
            } else {
              nd[[col]] <- NA
              msgs <- c(msgs, paste0("Añadida columna '", col, "' con NA (tipo desconocido)"))
            }
          } else {
            # heurística: si el nombre contiene '_Si' o empieza por 'is_' o contiene 'sexo_' -> asumir dummy 0
            if(grepl("_Si$", col) || grepl("^is_|^has_|_flag$", col) || grepl("sexo|genero|male|female|M|F", col, ignore.case = TRUE)){
              nd[[col]] <- as.integer(0)
              msgs <- c(msgs, paste0("Añadida columna dummy '", col, "' (0) por heurística)"))
            } else {
              nd[[col]] <- as.numeric(0)
              msgs <- c(msgs, paste0("Añadida columna numeric '", col, "' (0) por heurística)"))
            }
          }
        } else {
          # si existe en nd y en trainingData, forzar clases/levels a los de trainingData
          if(!is.null(mod$trainingData) && col %in% names(mod$trainingData)){
            prototype <- mod$trainingData[[col]]
            if(is.factor(prototype)){
              nd[[col]] <- factor(nd[[col]], levels = levels(prototype))
              msgs <- c(msgs, paste0("Forzado factor '", col, "' a niveles del trainingData"))
            } else if(is.integer(prototype)){
              nd[[col]] <- as.integer(as.numeric(nd[[col]]))
            } else if(is.numeric(prototype)){
              nd[[col]] <- as.numeric(nd[[col]])
            }
          }
        }
      }
      
      # También, si newd tiene columnas extras que no están en expected, mantenerlas pero advertir
      extras <- setdiff(names(nd), expected)
      if(length(expected)>0 && length(extras)>0) msgs <- c(msgs, paste0("Columnas extra en newdata (se mantendrán): ", paste(head(extras,20), collapse = ", ")))
      
      # Reordenar columnas para poner primero las esperadas si hay lista
      if(length(expected)>0){
        present_expected <- intersect(expected, names(nd))
        rest <- setdiff(names(nd), present_expected)
        nd <- nd[, c(present_expected, rest), drop = FALSE]
      }
      
      return(list(newdata = nd, msgs = msgs, expected = expected))
    }
    
    # Ejecutar alineamiento automático
    aligned <- align_newdata(mod, newd_raw)
    diag_msgs <- aligned$msgs
    newd <- aligned$newdata
    
    # Mostrar diagnóstico preliminar en UI
    output$modelo_info <- renderText({ paste(diag_msgs, collapse = "\n") })
    
    # Intentar predecir con varios modos
    pred_result <- NULL
    attempts <- list(
      list(type = "prob", args = list(type = "prob")),
      list(type = "class", args = list(type = "class")),
      list(type = "raw", args = list())
    )
    
    for(at in attempts){
      try_val <- try(do.call(predict, c(list(object = mod, newdata = newd), at$args)), silent = TRUE)
      if(!inherits(try_val, "try-error")){
        pred_result <- list(which = at$type, value = try_val)
        break
      } else {
        diag_msgs <- c(diag_msgs, paste0("Intento predict(type='", at$type, "') falló: ", as.character(try_val)))
      }
    }
    
    # Si sigue NULL, intentar model.frame para forzar factores/tipos
    if(is.null(pred_result)){
      try({
        terms_obj <- tryCatch(terms(mod), error = function(e) NULL)
        if(!is.null(terms_obj)){
          pred_terms <- tryCatch(stats::delete.response(terms_obj), error = function(e) NULL)
          if(!is.null(pred_terms)){
            mf <- try(model.frame(pred_terms, data = newd, na.action = na.pass), silent = TRUE)
            if(!inherits(mf, "try-error")){
              diag_msgs <- c(diag_msgs, "model.frame aplicado con éxito. Intentando predict con model.frame...")
              for(at in attempts){
                try_val2 <- try(do.call(predict, c(list(object = mod, newdata = mf), at$args)), silent = TRUE)
                if(!inherits(try_val2, "try-error")){
                  pred_result <- list(which = at$type, value = try_val2)
                  break
                } else {
                  diag_msgs <- c(diag_msgs, paste0("Predict con model.frame (type='", at$type, "') falló: ", as.character(try_val2)))
                }
              }
            } else {
              diag_msgs <- c(diag_msgs, paste0("model.frame falló: ", as.character(mf)))
            }
          }
        }
      }, silent = TRUE)
    }
    
    # Mostrar resultado o error en UI y consola
    if(!is.null(pred_result)){
      if(pred_result$which == "prob"){
        prob_df <- as.data.frame(pred_result$value)
        output$prob_tab <- renderTable({ prob_df }, rownames = TRUE)
        predicted <- names(prob_df)[which.max(prob_df[1, ])]
        output$pred_text <- renderText({ paste0("Predicción: ", predicted) })
        diag_msgs <- c(diag_msgs, paste0("Predict(type=prob) OK. Predicción: ", predicted))
      } else if(pred_result$which == "class"){
        predc <- as.character(pred_result$value[1])
        output$prob_tab <- renderTable({ data.frame(Predicted = predc) })
        output$pred_text <- renderText({ paste0("Predicción: ", predc) })
        diag_msgs <- c(diag_msgs, paste0("Predict(type=class) OK. Predicción: ", predc))
      } else {
        predc <- as.character(pred_result$value[1])
        output$prob_tab <- renderTable({ data.frame(Predicted = predc) })
        output$pred_text <- renderText({ paste0("Predicción (raw): ", predc) })
        diag_msgs <- c(diag_msgs, paste0("Predict raw OK. Resultado: ", predc))
      }
    } else {
      msg <- paste("No se pudo obtener una predicción. Diagnóstico:\n", paste(diag_msgs, collapse = "\n"))
      output$pred_text <- renderText({ paste("No se pudo obtener una predicción. Revisa 'Estado del modelo' para diagnóstico.") })
      output$prob_tab <- renderTable({ data.frame(Error = "No se pudo predecir. Ver estado del modelo para detalles.") })
      cat(msg, "\n")
    }
    
    # Actualizar diagnóstico final en UI
    output$modelo_info <- renderText({ paste(diag_msgs, collapse = "\n") })
  })
  
  # Importancia de variables si es posible
  output$imp_plot <- renderPlot({
    if(!is.null(rv$model)){
      imp <- NULL
      try({
        if("varImp" %in% ls("package:caret") || exists("varImp")){
          vi <- varImp(rv$model)
          if(!is.null(vi)){
            vi_df <- as.data.frame(vi$importance)
            # seleccionar top 10 si aplica
            if(nrow(vi_df) > 0){
              topn <- head(vi_df[order(-rowSums(vi_df)), , drop = FALSE], 10)
              barplot(as.matrix(topn)[,1], las = 2, main = "Importancia (top)")
            }
          }
        }
      }, silent = TRUE)
    }
  })
  
  # Mostrar matriz de confusion si CSV disponible
  output$matriz_conf <- renderTable({
    if(!is.null(rv$data) && "nivel_satisfaccion" %in% names(rv$data)){
      # intentar recrear matriz si en datos vienen reference & prediction
      if(all(c("prediccion") %in% names(rv$data))){
        tab <- table(rv$data$prediccion, rv$data$nivel_satisfaccion)
        as.data.frame.matrix(tab)
      } else {
        # si no, retornar vacio o intentar leer 09_matriz_confusion.csv si está en el mismo folder que el model
        mfile <- NULL
        if(!is.null(input$model_file)){
          folder <- dirname(input$model_file$datapath)
          candidate <- file.path(folder, "09_matriz_confusion.csv")
          if(file.exists(candidate)) mfile <- candidate
        }
        if(!is.null(mfile)){
          mm <- try(read_csv(mfile, show_col_types = FALSE), silent = TRUE)
          if(!inherits(mm, "try-error")) return(mm)
        }
        return(data.frame(info = "No hay matriz disponible. Carga 11_datos_procesados.csv con columnas 'nivel_satisfaccion' y 'prediccion', o coloca 09_matriz_confusion.csv en la carpeta del modelo."))
      }
    } else {
      return(data.frame(info = "No hay datos cargados ni matriz disponible."))
    }
  }, rownames = TRUE)
}

shinyApp(ui, server)