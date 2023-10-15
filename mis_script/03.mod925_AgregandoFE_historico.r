# limpio la memoria
rm(list = ls()) # remove all objects
gc() # garbage collection

require("data.table")
require("Rcpp")

#require("ranger")
#require("randomForest")  #solo se usa para imputar nulos

#require("lightgbm")

#--FUNCION fhistC base para la Tendencia por MinCuadrados------------------------------------------------------------------------
#se calculan para los X meses previos: el minimo, maximo y tendencia calculada con cuadrados minimos
#la formula de calculo de la tendencia puede verse en https://stats.libretexts.org/Bookshelves/Introductory_Statistics/Book%3A_Introductory_Statistics_(Shafer_and_Zhang)/10%3A_Correlation_and_Regression/10.04%3A_The_Least_Squares_Regression_Line
#para la maxíma velocidad esta funcion esta escrita en lenguaje C

cppFunction('NumericVector fhistC(NumericVector pcolumna, IntegerVector pdesde ) {
  /* Aqui se cargan los valores para la regresion */
  double x[100];
  double y[100];
  NumericVector out(pcolumna.size() * 5);
  int n = pcolumna.size();

  for(int i = 0; i < n; i++) {
    //lag
    if (pdesde[i] - 1 < i) {
      out[i + 4 * n] = NA_REAL;
      int libre = 0;
      int xvalor = 1;

      for (int j = pdesde[i] - 1; j <= i; j++) {
        double a = pcolumna[j];
        if (!R_IsNA(a)) {
          y[libre] = a;
          libre++;
        }
        xvalor++;
      }

      if (libre > 1) {
        double xsum = x[0];
        double ysum = y[0];
        double xysum = xsum * ysum;
        double xxsum = xsum * xsum;
        double vmin = y[0];
        double vmax = y[0];

        for (int h = 1; h < libre; h++) {
          xsum += x[h];
          ysum += y[h];
          xysum += x[h] * y[h];

          if (y[h] < vmin)
            vmin = y[h];
          if (y[h] > vmax)
            vmax = y[h];
        }

        out[i] = (libre * xysum - xsum * ysum) / (libre * xxsum - xsum * xsum);
        out[i + 2 * n] = vmax;
      } else {
        out[i] = NA_REAL;
        out[i + 2 * n] = NA_REAL;
      }
    }
  }

  return out;
}')


#--FUNCION PARA TENDENCIA en N-Ventanas temporal------------------------------------------------------------------------
#calcula la tendencia de las variables cols de los ultimos N meses definidos en ventanas
TendenciaYmuchomas <- function(dataset, cols, ventanas = c(3, 6, 9), tendencia = TRUE, 
                               minimo = FALSE, maximo = FALSE, promedio = FALSE, ratioavg = TRUE, ratiomax = FALSE) 
{
  gc()
  
  for (ventana_regresion in ventanas) {
    last <- nrow(dataset)
    
    vector_ids <- dataset$numero_de_cliente
    vector_desde <- seq(-ventana_regresion + 2, nrow(dataset) - ventana_regresion + 1)
    vector_desde[1:ventana_regresion] <- 1
    
    for (i in 2:last)
      if (vector_ids[i - 1] != vector_ids[i]) {
        vector_desde[i] <- i
      }
    for (i in 2:last)
      if (vector_desde[i] < vector_desde[i - 1]) {
        vector_desde[i] <- vector_desde[i - 1]
      }
    
    for (campo in cols) {
      nueva_col <- fhistC(dataset[, get(campo)], vector_desde)
      
      if (tendencia)
        dataset[, paste0("tend_", ventana_regresion, campo) := nueva_col[(0 * last + 1):(1 * last)]]
      if (minimo)
        dataset[, paste0("min_", ventana_regresion, campo) := nueva_col[(1 * last + 1):(2 * last)]]
      if (maximo)
        dataset[, paste0("max_", ventana_regresion, campo) := nueva_col[(2 * last + 1):(3 * last)]]
      if (promedio)
        dataset[, paste0("avg_", ventana_regresion, campo) := nueva_col[(3 * last + 1):(4 * last)]]
      if (ratioavg)
        dataset[, paste0("ratioavg_", ventana_regresion, campo) := get(campo) / nueva_col[(3 * last + 1):(4 * last)]]
      if (ratiomax)
        dataset[, paste0("ratiomax_", ventana_regresion, campo) := get(campo) / nueva_col[(2 * last + 1):(3 * last)]]
    }
  }
}


#--FUNCION para GENERAR LAGS y DELTA_LAGS----------------------------------------------------
generarlags <- function(dataset, cols_lagueables, lag1 = TRUE, lag2 = TRUE, lag3 = TRUE, lag4 = TRUE,
                        lag6 = TRUE, lag9 = TRUE, lag12 = TRUE) {
  
  if (lag1) {
    dataset[ , paste0(cols_lagueables, "_lag1") := shift(.SD, 1, NA, "lag"), 
             by = numero_de_cliente, 
             .SDcols = cols_lagueables ]
    for (vcol in cols_lagueables) {
      dataset[ , paste0(vcol, "_delta1") := get(vcol) - get(paste0(vcol, "_lag1"))]
    }
  }
  
  if (lag2) {
    dataset[ , paste0(cols_lagueables, "_lag2") := shift(.SD, 2, NA, "lag"), 
             by = numero_de_cliente, 
             .SDcols = cols_lagueables ]
    for (vcol in cols_lagueables) {
      dataset[ , paste0(vcol, "_delta2") := get(vcol) - get(paste0(vcol, "_lag2"))]
    }
  }
  
  if (lag3) {
    dataset[ , paste0(cols_lagueables, "_lag3") := shift(.SD, 3, NA, "lag"), 
             by = numero_de_cliente, 
             .SDcols = cols_lagueables ]
    for (vcol in cols_lagueables) {
      dataset[ , paste0(vcol, "_delta3") := get(vcol) - get(paste0(vcol, "_lag3"))]
    }
  }
  
  if (lag4) {
    dataset[ , paste0(cols_lagueables, "_lag4") := shift(.SD, 4, NA, "lag"), 
             by = numero_de_cliente, 
             .SDcols = cols_lagueables ]
    for (vcol in cols_lagueables) {
      dataset[ , paste0(vcol, "_delta4") := get(vcol) - get(paste0(vcol, "_lag4"))]
    }
  }
  
  if (lag6) {
    dataset[ , paste0(cols_lagueables, "_lag6") := shift(.SD, 6, NA, "lag"), 
             by = numero_de_cliente, 
             .SDcols = cols_lagueables ]
    for (vcol in cols_lagueables) {
      dataset[ , paste0(vcol, "_delta6") := get(vcol) - get(paste0(vcol, "_lag6"))]
    }
  }
  
  if (lag9) {
    dataset[ , paste0(cols_lagueables, "_lag9") := shift(.SD, 9, NA, "lag"), 
             by = numero_de_cliente, 
             .SDcols = cols_lagueables ]
    for (vcol in cols_lagueables) {
      dataset[ , paste0(vcol, "_delta9") := get(vcol) - get(paste0(vcol, "_lag9"))]
    }
  }
  
  if (lag12) {
    dataset[ , paste0(cols_lagueables, "_lag12") := shift(.SD, 12, NA, "lag"), 
             by = numero_de_cliente, 
             .SDcols = cols_lagueables ]
    for (vcol in cols_lagueables) {
      dataset[ , paste0(vcol, "_delta12") := get(vcol) - get(paste0(vcol, "_lag12"))]
    }
  }
  
  return(dataset)
}


#---INICIO: Carga dataset y verifica--------------------------------------------------------  
#Aqui se debe poner la carpeta de la materia de SU computadora local (el Working Directory)
setwd("~/buckets/b1")

#cargo el dataset
dataset <- fread("./datasets/competencia02_FE1_drift.csv.gz") 

# minima exploración por fecha y target
table(dataset$foto_mes, dataset$clase_ternaria )

#EXTRA: CORREGIR clase_ternaria: quedó "Continua" en 202106 y 202107 para todos los desconocidos:
#check inicial:
table(dataset$foto_mes, dataset$clase_ternaria)[28:31,] #Inicio: 202106    910      0   163510 || 202107      0      0   164682

#Marca como "" los valores "Continua" para los meses 202106 y 202107:
dataset[foto_mes %in% c("202106", "202107") & clase_ternaria == "Continua", clase_ternaria := ""]

#check final:
table(dataset$foto_mes, dataset$clase_ternaria)[28:31,] 

#EXTRA2:
#check ini:
dim(dataset)

#Columnas que quitamos y las que dejamos:
campos_a_quitar <- colnames(dataset)
campos_a_quitar <- campos_a_quitar[campos_a_quitar %like% "^(add)"]
cols_finales <- setdiff(colnames(dataset), campos_a_quitar)

# Filtrar el dataset para conservar solo las columnas deseadas
dataset <- dataset[, ..cols_finales]

# M las primeras filas del dataset actualizado
dim(dataset)


#-----PROCESO: Applico Funciones para: +FE y fix_Drifting ------------------------------------------------------
#ordeno el dataset para aplicar luego los fixes
setorder( dataset, foto_mes, numero_de_cliente )

dim(dataset) #4562810     378

#estas son las columnas a las que se puede agregar lags y tendencia
campos_a_quitar  <- colnames(dataset)
campos_a_quitar  <- campos_a_quitar[campos_a_quitar %like% "^(add|clase)"]
cols_lagueables  <- copy(  setdiff( colnames(dataset), c("numero_de_cliente", "foto_mes", campos_a_quitar)  ) )

length(cols_lagueables) #370


#FUNCION1: Genera TENDENCIA y ratioAvg, para ventana de 3, 6 y 9 meses
TendenciaYmuchomas(dataset, cols_lagueables, ventanas = c(3, 6, 9), tendencia = TRUE, 
                   minimo = FALSE, maximo = FALSE, promedio = FALSE, ratioavg = TRUE, ratiomax = FALSE)
dim(dataset) #4562810    1402 -> 1399 (sin add_)


#FUNCION2: Genera LAGs y DELTA_LAGs, para 1,2,3,4,6,9 y 12. -> cuelga la maquina
#generarlags(dataset, cols_lagueables, lag1 = TRUE, lag2 = TRUE, lag3 = TRUE, lag4 = FALSE,
#            lag6 = TRUE, lag9 = FALSE, lag12 = FALSE)



#LAGs Individuales:
lag1 <- TRUE
lag2 <- TRUE
lag3 <- TRUE
lag4 <- FALSE
lag6 <- TRUE
lag9 <- FALSE
lag12 <- FALSE
dim(dataset) #4562810    4359


if (lag1) {
  dataset[ , paste0(cols_lagueables, "_lag1") := shift(.SD, 1, NA, "lag"), 
           by = numero_de_cliente, 
           .SDcols = cols_lagueables ]
  for (vcol in cols_lagueables) {
    dataset[ , paste0(vcol, "_delta1") := get(vcol) - get(paste0(vcol, "_lag1"))]
  }
}

if (lag2) {
  dataset[ , paste0(cols_lagueables, "_lag2") := shift(.SD, 2, NA, "lag"), 
           by = numero_de_cliente, 
           .SDcols = cols_lagueables ]
  for (vcol in cols_lagueables) {
    dataset[ , paste0(vcol, "_delta2") := get(vcol) - get(paste0(vcol, "_lag2"))]
  }
}

if (lag3) {
  dataset[ , paste0(cols_lagueables, "_lag3") := shift(.SD, 3, NA, "lag"), 
           by = numero_de_cliente, 
           .SDcols = cols_lagueables ]
  for (vcol in cols_lagueables) {
    dataset[ , paste0(vcol, "_delta3") := get(vcol) - get(paste0(vcol, "_lag3"))]
  }
}

if (lag4) {
  dataset[ , paste0(cols_lagueables, "_lag4") := shift(.SD, 4, NA, "lag"), 
           by = numero_de_cliente, 
           .SDcols = cols_lagueables ]
  for (vcol in cols_lagueables) {
    dataset[ , paste0(vcol, "_delta4") := get(vcol) - get(paste0(vcol, "_lag4"))]
  }
}

if (lag6) {
  dataset[ , paste0(cols_lagueables, "_lag6") := shift(.SD, 6, NA, "lag"), 
           by = numero_de_cliente, 
           .SDcols = cols_lagueables ]
  for (vcol in cols_lagueables) {
    dataset[ , paste0(vcol, "_delta6") := get(vcol) - get(paste0(vcol, "_lag6"))]
  }
}

if (lag9) {
  dataset[ , paste0(cols_lagueables, "_lag9") := shift(.SD, 9, NA, "lag"), 
           by = numero_de_cliente, 
           .SDcols = cols_lagueables ]
  for (vcol in cols_lagueables) {
    dataset[ , paste0(vcol, "_delta9") := get(vcol) - get(paste0(vcol, "_lag9"))]
  }
}

if (lag12) {
  dataset[ , paste0(cols_lagueables, "_lag12") := shift(.SD, 12, NA, "lag"), 
           by = numero_de_cliente, 
           .SDcols = cols_lagueables ]
  for (vcol in cols_lagueables) {
    dataset[ , paste0(vcol, "_delta12") := get(vcol) - get(paste0(vcol, "_lag12"))]
  }
}





#---EXPORTA DATABSE GENERADA A .CSV---------
setwd("~/buckets/b1/datasets/")

#EXPORTAMOS a un .CSV
fwrite( dataset,
        file= "competencia02_FEhist.csv.gz",
        sep= "," )

