#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")

#------------------------------------------------------------------------------
#Funcion para corregir Zeros reemplazando con el VALOR DEL MES SIGUIENTE del mes anterior y previo

fixzerosavg <- function(pcampo, pmeses, dataset) {
  #Creamos tbl obteniendo el valor del mes previo (lag) y siguiente (lead) de la variable (pcampo) para cada cliente.
  tbl <- dataset[  ,  list( "v1" = shift( get(pcampo), 1, type="lag" ),
                            "v2" = shift( get(pcampo), 1, type="lead" )
  ), 
  by=numero_de_cliente ]
  
  #eliminamos el numero de cliente (lag y lead unicas 2 columnas previas en tbl
  tbl[ , numero_de_cliente := NULL ]
  
  #calculamos el promedio del lag y lead (+3ra col: promedio)
  tbl[ , promedio := rowMeans( tbl,  na.rm=TRUE ) ]
  
  # Agregamos una variable para conservar los valores originales (4ta col en tbl)
  tbl[ , original := dataset[[pcampo]] ]
  
  # Generamos la columna final en tbl, que tendra el promedio para los zeros y el valor original en el resto
  tbl[ , final := ifelse( original == 0,
                          tbl$promedio ,
                          get( "original") )]
  
  #reemplazo la variable en el dataset
  dataset[ ,paste0(pcampo) := ifelse( !(foto_mes %in% pmeses),
                                      get( pcampo),
                                      tbl$final ) ]
}

# Aplicar la corrección con el PROMEDIO
apply_fixzerosavg <- function(dataset) {
  fixzerosavg( "active_quarter", c(202006), dataset)
  # internet -> rota desde 202010 a 202107 (demasiados meses) -> eliminarmos variable
  
  fixzerosavg( "mrentabilidad", c(201905, 201910, 202006), dataset)
  fixzerosavg( "mrentabilidad_annual", c(201905, 201910, 202006), dataset)
  fixzerosavg( "mcomisiones", c(201905, 201910, 202006), dataset)
  fixzerosavg( "mpasivos_margen", c(201905, 201910, 202006), dataset)
  fixzerosavg( "mactivos_margen", c(201905, 201910, 202006), dataset)
  
  fixzerosavg( "mcuentas_saldo", c(202006), dataset)
  
  fixzerosavg( "ctarjeta_debito_transacciones", c(202006), dataset)
  fixzerosavg( "mautoservicio", c(202006), dataset)
  
  fixzerosavg( "ctarjeta_visa_transacciones", c(202006), dataset)
  fixzerosavg( "mtarjeta_visa_consumo", c(202006), dataset)
  
  fixzerosavg( "ctarjeta_master_transacciones", c(202006), dataset)
  fixzerosavg( "mtarjeta_master_consumo", c(202006), dataset)  
  
  fixzerosavg( "cprestamos_prendarios", c(202006), dataset)
  fixzerosavg( "mprestamos_prendarios", c(202006), dataset)
  fixzerosavg( "cprestamos_hipotecarios", c(202006), dataset)
  fixzerosavg( "mprestamos_hipotecarios", c(202006), dataset)
  
  fixzerosavg( "ctarjeta_visa_debitos_automaticos", c(201904), dataset)
  fixzerosavg( "mttarjeta_visa_debitos_automaticos", c(201904), dataset)
  
  fixzerosavg( "ccajeros_propios_descuentos", c(201910, 202002,202006, 202009, 202010, 202102), dataset)
  fixzerosavg( "mcajeros_propios_descuentos", c(201910, 202002,202006, 202009, 202010, 202102), dataset)
  fixzerosavg( "ctarjeta_visa_descuentos", c(201910, 202002,202006, 202009, 202010, 202102), dataset)
  fixzerosavg( "mtarjeta_visa_descuentos", c(201910, 202002,202006, 202009, 202010, 202102), dataset)
  fixzerosavg( "ctarjeta_master_descuentos", c(201910, 202002,202006, 202009, 202010, 202102), dataset)
  fixzerosavg( "mtarjeta_master_descuentos", c(201910, 202002,202006, 202009, 202010, 202102), dataset)
  
  fixzerosavg( "ccomisiones_otras", c(201905, 201910, 202006), dataset)
  fixzerosavg( "mcomisiones_otras", c(201905, 201910, 202006), dataset)
  
  fixzerosavg( "cextraccion_autoservicio", c(202006), dataset)
  fixzerosavg( "mextraccion_autoservicio", c(202006), dataset)
  fixzerosavg( "ccheques_depositados", c(202006), dataset)
  fixzerosavg( "mcheques_depositados", c(202006), dataset)
  fixzerosavg( "mcheques_emitidos", c(202006), dataset)
  fixzerosavg( "mcheques_emitidos", c(202006), dataset)
  
  fixzerosavg( "tcallcenter", c(202006), dataset)
  fixzerosavg( "ccallcenter_transacciones", c(202006), dataset)
  
  fixzerosavg( "thomebanking", c(202006), dataset)
  fixzerosavg( "chomebanking_transacciones", c(202006), dataset)
  
  fixzerosavg( "ccajas_transacciones", c(202006), dataset)
  fixzerosavg( "ccajas_consultas", c(202006), dataset)
  fixzerosavg( "ccajas_depositos", c(202006, 202105), dataset)
  fixzerosavg( "ccajas_extracciones", c(202006), dataset)
  fixzerosavg( "ccajas_otras", c(202006), dataset)
  
  fixzerosavg( "catm_trx", c(202006), dataset)
  fixzerosavg( "matm", c(202006), dataset)
  fixzerosavg( "catm_trx_other", c(202006), dataset)
  fixzerosavg( "matm_other", c(202006), dataset)
  
  fixzerosavg( "ctrx_quarter", c(202006), dataset)
  fixzerosavg( "tmobile_app", c(202006), dataset)
  fixzerosavg( "cmobile_app_trx", c(202006), dataset)
  
  fixzerosavg( "Master_mfinanciacion_limite", c(201904, 202103), dataset)
  fixzerosavg( "Master_fultimo_cierre", c(201905, 201910, 202006), dataset)
  fixzerosavg( "Master_mpagado", c(201906), dataset)
  
  fixzerosavg( "Visa_mfinanciacion_limite", c(201906), dataset)
  
  fixzerosavg( "Visa_msaldototal", c(201906), dataset)
  fixzerosavg( "Visa_msaldopesos", c(201906), dataset)
  
  fixzerosavg( "Visa_fultimo_cierre", c(201905, 201910, 202006), dataset)
  fixzerosavg( "Visa_mpagado", c(202011), dataset)
}

#---------------------------------------------------------------------------
#Funcion para corregir Zeros reemplazando con el VALOR DEL MES SIGUIENTE del mes anterior y previo
fixzerosnext <- function(pcampo, pmeses, dataset) {
  #Creamos tbl obteniendo el valor del mes siguiente (lead) de la variable (pcampo) para cada cliente.
  tbl <- dataset[, list("lead" = shift(get(pcampo), 1, type = "lead")),
                 by = numero_de_cliente]
  
  # Agregamos una variable para conservar los valores originales (3ra col en tbl)
  tbl[ , original := dataset[[pcampo]] ]
  
  # Generamos la columna final en tbl, que tendra el promedio para los zeros y el valor original en el resto
  tbl[ , final := ifelse( original == 0,
                          tbl$lead ,
                          get( "original") )]
  
  # Reemplaza los valores cero con el valor siguiente (lead)
  dataset[ ,paste0(pcampo) := ifelse( !(foto_mes %in% pmeses),
                                      get( pcampo),
                                      tbl$final ) ]
}

# Aplicar la corrección CON EL MES SIGUIENTE
apply_fixzerosnext <- function( dataset )
{
  fixzerosnext( "chomebanking_transacciones", c(202006),dataset )
  
  fixzerosnext( "ccheques_depositados_rechazados", c(202006),dataset )
  fixzerosnext( "mcheques_depositados_rechazados", c(202006),dataset )
  fixzerosnext( "ccheques_emitidos_rechazados", c(202006),dataset )
  fixzerosnext( "mcheques_emitidos_rechazados", c(202006),dataset )
}

#------------------------------------------------------------------------------
#Aqui empieza el programa
PARAM  <- list()

## PARAMETROS A COMPLETAR:
#PARAM$experimento  <- "01FixData"
PARAM$dataset  <- "./datasets/competencia_02_adds.csv.gz"
setwd("~/buckets/b1")

#------------------------------------------------------------------------------
#cargo el dataset
dataset  <- fread( PARAM$dataset )

#creo la carpeta donde va el experimento
#dir.create( paste0( "./exp/", PARAM$experimento, "/"), showWarnings = FALSE )
#setwd(paste0( "./exp/", PARAM$experimento, "/"))   #Establezco el Working Directory DEL EXPERIMENTO



#ordeno el dataset para aplicar luego los fixes
setorder( dataset, numero_de_cliente, foto_mes )

#FIX(1): corrijo los ceros CON EL PROMEDIO mes_next.y.prev:
apply_fixzerosavg( dataset )

#FIX(2): corrijo los ceros CON EL MES SIGUINTE
apply_fixzerosnext( dataset )

#FIX(3):Elimino los campos problematicos
#Internet se daño a partir de 202010
dataset[  , internet := NULL ]


#------------------------------------------------------------------------------
#EXPORTO el dataset a un .csv
setwd("~/buckets/b1/datasets/")
fwrite( dataset,
        file=  "competencia02_fix.csv.gz",
        logical01= TRUE,
        sep= "," )






