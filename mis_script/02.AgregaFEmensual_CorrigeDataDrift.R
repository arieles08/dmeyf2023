# limpio la memoria
rm(list = ls()) # remove all objects
gc() # garbage collection

require("data.table")
require(stats)

#--FE------------------------------------------------------------------------
#Agregar el FE mensual
Add_FE  <- function( dataset )
{
  gc()
  # Corrige clientes con menos de 90 dias de antiguedad (al ser una medida trimestral)
  #ctrx_quarter_fix
  dataset[  , ctrx_quarter_fix := ctrx_quarter ]
  dataset[ cliente_antiguedad==1 , ctrx_quarter_fix := ctrx_quarter * 3 ]
  dataset[ cliente_antiguedad==2 , ctrx_quarter_fix := ctrx_quarter * 2 ]
  
  
  #Var del Top 20 que puedan servir en relacion a la edad y edad^2
  # ctrx_quarter_fixed
  dataset[ , ctrx_quarter_fix_edad := ctrx_quarter_fix / cliente_edad ]
  dataset[ , ctrx_quarter_fix_edad2 := ctrx_quarter / cliente_edad^2]
  
  # mprestamos_personales
  dataset[ ,mprestamos_personales_edad := mprestamos_personales  / cliente_edad ]
  dataset[ ,mprestamos_personales_edad2 := mprestamos_personales  / cliente_edad^2 ]
  
  # mactivos_margen
  dataset[ ,mactivos_margen_edad := mactivos_margen  / cliente_edad ]
  dataset[ ,mactivos_margen_edad2 := mactivos_margen  / cliente_edad^2 ]
  
  # mcomisiones
  dataset[ ,mcomisiones_edad := mcomisiones  / cliente_edad ]
  dataset[ ,mcomisiones_edad2 := mcomisiones  / cliente_edad^2 ]
  
  # mpayroll
  dataset[ ,mpayroll_edad := mpayroll  / cliente_edad ]
  dataset[ ,mpayroll_edad2 := mpayroll  / cliente_edad^2 ]
  
  # ACTIVIDAD: diferencia a la media al cuadrado
  avg_ctrx_quarter_fix <- mean(dataset$ctrx_quarter_fix, na.rm = TRUE)
  dataset[, ctrx_quarter_fix_square := (ctrx_quarter_fix - avg_ctrx_quarter_fix)^2]
  
  
  #SALDO CUENTAS: Distancias
  # mcuentas_saldo - mcaja_ahorro
  dataset[, dist_msaldo_mcaja := mcuentas_saldo - mcaja_ahorro]
  
  # mcuentas_saldo - mcuenta_corriente
  dataset[, dist_msaldo_mcorriente := mcuentas_saldo - mcuenta_corriente]
  
  # mcaja_ahorro - mcuenta_corriente
  dataset[, dist_mcaja_mcorriente := mcaja_ahorro - mcuenta_corriente]
  
  # mcuentas_saldo - mpayroll
  dataset[, dist_msaldo_mpayroll := mcuentas_saldo - mpayroll]
  
  
  #PRESTAMOS: Ratios y Distancias
  # mprestamos_personales / cprestamos_personales
  dataset[, mprestamo_avg := ifelse(cprestamos_personales == 0, 100, mprestamos_personales / cprestamos_personales)]
  
  # mprestamos_personales / mcuentas_saldo
  dataset[, prestamos_saldo_ratio := ifelse(mcuentas_saldo <= 0, 100, mprestamos_personales / mcuentas_saldo)]
  
  # mprestamos_personales - mcuentas_saldo
  dataset[, dist_mprestamos_msaldo := mprestamos_personales - mcuentas_saldo]
  
  # mprestamos_personales - max(0, mcuentas_saldo)
  dataset[, dist_mprestamos_msaldo2 := mprestamos_personales - pmax(0, mcuentas_saldo)]
  
  # mprestamos_personales / mpayroll
  dataset[, prestamos_payroll_ratio := ifelse(mpayroll <= 0, 100, mprestamos_personales / mpayroll)]
  
  # mprestamos_personales / cliente_antiguedad
  dataset[, prestamos_antiguedad_ratio := ifelse(cliente_antiguedad <= 0, 100, mprestamos_personales / cliente_antiguedad)]
  
  #RENTABILIDAD: Ratios y Distancias
  # mrentabilidad - mcomisiones_mantenimiento
  dataset[, dist_mrentabilidad_mcomisiones := mrentabilidad - mcomisiones_mantenimiento]
  
  # mrentabilidad / mcomisiones_mantenimiento
  dataset[, rentabilidad_comisiones_ratio := ifelse(mcomisiones_mantenimiento == 0, 100, mrentabilidad / mcomisiones_mantenimiento)]
  
  # mrentabilidad - mactivos_margen
  dataset[, dist_mrentabilidad_mactivos := mrentabilidad - mactivos_margen]
  
  # mrentabilidad / mactivos_margen
  dataset[, rentabilidad_activos_ratio := ifelse(mactivos_margen <= 0, 100, mrentabilidad / mactivos_margen)]
  
  # mrentabilidad - mpasivos_margen
  dataset[, dist_mrentabilidad_mpasivos := mrentabilidad - mpasivos_margen]
  
  # mrentabilidad / mpasivos_margen
  dataset[, rentabilidad_pasivos_ratio := ifelse(mpasivos_margen <= 0, 100, mrentabilidad / mpasivos_margen)]
  
  # mrentabilidad / ctrx_quarter_fix
  dataset[, rentabilidad_ctrx_ratio := ifelse(ctrx_quarter_fix == 0, 100, mrentabilidad / ctrx_quarter_fix)]
  
  # mrentabilidad / (ctrx_quarter_fix - avg_ctrx_quarter_fix)^2
  dataset[, rentabilidad_ctrx2_ratio := ifelse(ctrx_quarter_fix_square == 0, 100, mrentabilidad / (ctrx_quarter_fix - avg_ctrx_quarter_fix)^2)]
  
  # mcomisiones_mantenimiento / ctrx_quarter_fix
  dataset[, rentabilidad_ctrx_ratio := ifelse(ctrx_quarter_fix == 0, 100, mcomisiones_mantenimiento / ctrx_quarter_fix)]
  
  # mcomisiones_mantenimiento / (ctrx_quarter_fix - avg_ctrx_quarter_fix)^2
  dataset[, rentabilidad_ctrx2_ratio := ifelse(ctrx_quarter_fix_square == 0, 100, mcomisiones_mantenimiento / (ctrx_quarter_fix - avg_ctrx_quarter_fix)^2)]
  
  # mrentabilidad_annual / mrentabilidad
  dataset[, rentannual_rent_ratio := ifelse(mrentabilidad == 0, 100, mrentabilidad_annual / mrentabilidad)]
  
  # mrentabilidad_annual / mcomisiones_mantenimiento
  dataset[, rentannual_comisiones_ratio := ifelse(mcomisiones_mantenimiento == 0, 100, mrentabilidad_annual / mcomisiones_mantenimiento)]
  
  
  # TARJETAS:
  # Cantidad de tarjetas total sum(ctarjeta_visa, ctarjeta_master)
  dataset[, ctarjetas := rowSums(.SD, na.rm = TRUE), .SDcols = c("ctarjeta_visa", "ctarjeta_master")]
  
  # sum(Visa_mlimitecompra, Master_mlimitecompra)
  dataset[, suma_mlimites := Visa_mlimitecompra + Master_mlimitecompra]
  
  # sum(Visa_msaldototal,Master_msaldototal)
  dataset[, vm_msaldototal := Visa_msaldototal + Master_msaldototal]
  # sum(Visa_msaldototal,Master_msaldototal) / mpayroll
  dataset[, ratio_vm_msaldototal_payroll := ifelse(mpayroll <= 0, 100, vm_msaldototal / mpayroll)]
  # sum(Visa_msaldototal,Master_msaldototal) / mcuentas_saldo
  dataset[, ratio_vm_msaldototal_saldo := ifelse(mcuentas_saldo <= 0, 100, vm_msaldototal / mcuentas_saldo)]
  # sum(Visa_msaldototal,Master_msaldototal) / sum(Visa_mlimitecompra, Master_mlimitecompra)
  dataset[, ratio_vm_msaldototal_limites := ifelse(suma_mlimites == 0, 100, vm_msaldototal / suma_mlimites)]
  
  # sum(Visa_msaldopesos,Master_msaldopesos)
  dataset[, vm_msaldopesos := Visa_msaldopesos + Master_msaldopesos]
  # sum(Visa_msaldopesos,Master_msaldopesos) / mpayroll
  dataset[, ratio_vm_msaldopesos_payroll := ifelse(mpayroll <= 0, 100, vm_msaldopesos / mpayroll)]
  # sum(Visa_msaldopesos,Master_msaldopesos) / mcuentas_saldo
  dataset[, ratio_vm_msaldopesos_saldo := ifelse(mcuentas_saldo <= 0, 100, vm_msaldopesos / mcuentas_saldo)]
  # sum(Visa_msaldopesos,Master_msaldopesos) / sum(Visa_mlimitecompra, Master_mlimitecompra)
  dataset[, ratio_vm_msaldopesos_limites := ifelse(suma_mlimites == 0, 100, vm_msaldopesos / suma_mlimites)]
  
  # sum(Visa_mpagominimo,Master_mpagominimo)
  dataset[, vm_mpagominimo := Visa_mpagominimo + Master_mpagominimo]
  # sum(Visa_mpagominimo,Master_mpagominimo) / mpayroll
  dataset[, ratio_vm_mpagominimo_payroll := ifelse(mpayroll <= 0, 100, vm_mpagominimo / mpayroll)]
  # sum(Visa_mpagominimo,Master_mpagominimo) / mcuentas_saldo
  dataset[, ratio_vm_mpagominimo_saldo := ifelse(mcuentas_saldo <= 0, 100, vm_mpagominimo / mcuentas_saldo)]
  
  # sum(mtarjeta_visa_consumo, mtarjeta_master_consumo)
  dataset[, vmtarjeta_consumos := mtarjeta_visa_consumo + mtarjeta_master_consumo]
  # sum(mtarjeta_visa_consumo, mtarjeta_master_consumo) / sum(ctarjeta_visa, ctarjeta_master)
  dataset[, ratio_vmtarjeta_consumos_tarjetas := ifelse(ctarjetas == 0, 100, vmtarjeta_consumos / ctarjetas)]
  # sum(mtarjeta_visa_consumo, mtarjeta_master_consumo) / mpayroll
  dataset[, ratio_vmtarjeta_consumos_payroll := ifelse(mpayroll <= 0, 100, vmtarjeta_consumos / mpayroll)]
  # sum(mtarjeta_visa_consumo, mtarjeta_master_consumo) / mcuentas_saldo
  dataset[, ratio_vmtarjeta_consumos_saldo := ifelse(mcuentas_saldo <= 0, 100, vmtarjeta_consumos / mcuentas_saldo)]
  # sum(mtarjeta_visa_consumo, mtarjeta_master_consumo) / sum(Visa_mlimitecompra, Master_mlimitecompra)
  dataset[, ratio_vmtarjeta_consumos_limites := ifelse(suma_mlimites == 0, 100, vmtarjeta_consumos / suma_mlimites)]
  
  # sum(Visa_mpagado , Master_mpagado)
  dataset[, vm_mpagado := Visa_mpagado + Master_mpagado]
  # sum(Visa_mpagado , Master_mpagado) - sum(Visa_msaldototal,Master_msaldototal)
  dataset[, dist_vm_mpagado_saldos := vm_mpagado - vm_msaldototal]
  # sum(Visa_mpagado , Master_mpagado) - sum(Visa_msaldopesos,Master_msaldopesos)
  dataset[, dist_vm_mpagado_saldos_pesos := vm_mpagado - vm_msaldopesos]
  # sum(Visa_mpagado , Master_mpagado) - sum(mtarjeta_visa_consumo, mtarjeta_master_consumo)
  dataset[, dist_vm_mpagado_consumos := vm_mpagado - vmtarjeta_consumos]
  # sum(Visa_mpagado , Master_mpagado) - mpayroll 
  dataset[, dist_vm_mpagado_consumos_pesos := vm_mpagado - mpayroll ]
  
  # RATIOS DE PAGO tarjetas
  # Pago / SALDO TOTAL : sum(Visa_mpagado , Master_mpagado) / sum(Visa_msaldototal,Master_msaldototal)
  dataset[, ratio_pagado_saldos := ifelse(vm_msaldototal == 0, 100, vm_mpagado / vm_msaldototal)]
  # Pago / SALDO $ : sum(Visa_mpagado , Master_mpagado) / sum(Visa_msaldopesos,Master_msaldopesos)
  dataset[, ratio_pagado_saldos_pesos := ifelse(vm_msaldopesos == 0, 100, vm_mpagado / vm_msaldopesos)]
  
  # Pago / CONSUMO :sum(Visa_mpagado , Master_mpagado) / sum(mtarjeta_visa_consumo, mtarjeta_master_consumo)
  dataset[, ratio_pagado_consumos := ifelse(vmtarjeta_consumos == 0, 100, vm_mpagado / vmtarjeta_consumos)]
  # Pago / Ingreso : sum(Visa_mpagado , Master_mpagado) / mpayroll
  dataset[, ratio_pagado_payroll := ifelse(mpayroll <= 0, 100, vm_mpagado / mpayroll)]
  # sum(Visa_mpagado , Master_mpagado) / mcuentas_saldo
  dataset[, ratio_pagado_saldo := ifelse(mcuentas_saldo <= 0, 100, vm_mpagado / mcuentas_saldo)]
  
  # Deuda Total: sum(Visa_msaldototal,Master_msaldototal, mprestamos_personales)
  dataset[, msaldovs_mprest := vm_msaldototal + mprestamos_personales]
  # sum(Visa_msaldototal,Master_msaldototal, mprestamos_personales) / mpayroll
  dataset[, ratio_msaldovs_mprest_payroll := ifelse(mpayroll <= 0, 100, msaldovs_mprest / mpayroll)]
  # sum(Visa_msaldototal,Master_msaldototal, mprestamos_personales) / mcuentas_saldo
  dataset[, ratio_msaldovs_mprest_saldo := ifelse(mcuentas_saldo <= 0, 100, msaldovs_mprest / mcuentas_saldo)]
  
  
  #FE2: Operaciones Varias:
  ## Tiempo de vida en el banco 
  dataset[, vida_banco := (cliente_antiguedad/12) / (cliente_edad)]
  
  ## Total Pasivos
  dataset[, total_mdeuda := rowSums(.SD, na.rm = TRUE), .SDcols = c("mprestamos_personales", "mprestamos_prendarios", "mprestamos_hipotecarios", "Visa_msaldototal", "Master_msaldototal")]
  
  ## Total Activos
  dataset[, total_mactivos := rowSums(.SD, na.rm = TRUE), .SDcols = c("mplazo_fijo_dolares", "mplazo_fijo_pesos", "minversion1_pesos", "minversion1_dolares", "minversion2", "mcuentas_saldo")]
  
  ## Tiene cuenta homebanking
  dataset[, has_internet := ifelse(dataset$internet > 0, 1, 0) ]
  
  ## Tiene movimientos/tarjetas
  dataset[, has_debito_transacciones := ifelse(dataset$ctarjeta_debito_transacciones > 0, 1, 0) ]
  dataset[, has_visa := ifelse(dataset$ctarjeta_visa > 0, 1, 0) ]
  dataset[, has_visa_transacciones := ifelse(dataset$ctarjeta_visa_transacciones > 0, 1, 0) ]
  dataset[, has_master := ifelse(dataset$ctarjeta_master > 0, 1, 0) ]
  dataset[, has_master_transacciones := ifelse(dataset$ctarjeta_master_transacciones > 0, 1, 0) ]
  
  ## Recibo pago de sueldo?
  dataset[, has_payroll := ifelse(dataset$cpayroll_trx + dataset$cpayroll2_trx  > 0, 1, 0) ]
  
  ## Tiene débitos automáticos?
  dataset[, has_da := ifelse(dataset$ccuenta_debitos_automaticos + dataset$ctarjeta_visa_debitos_automaticos + dataset$ctarjeta_master_debitos_automaticos  > 0, 1, 0) ]
  
  ## ¿hace más transacciones en cajeros de otros bancos?
  dataset[, cajeros_ajenos := ifelse(dataset$matm < dataset$matm_other, 1, 0)]
}

#--SELECCION COLUMNAS A CORREGIR------------------------------------------------------------------------
cols_to_fix <- c("mrentabilidad",
                 "mrentabilidad_annual",
                 "mcomisiones",
                 "mactivos_margen",
                 "mpasivos_margen",
                 
                 "mcuenta_corriente",
                 "mcaja_ahorro",
                 "mcaja_ahorro_dolares",
                 "mcuentas_saldo",
                 
                 "mtarjeta_visa_consumo",
                 
                 "mtarjeta_master_consumo",
                 
                 "mprestamos_personales",
                 "mprestamos_prendarios",
                 
                 "mplazo_fijo_dolares",
                 "mplazo_fijo_pesos",
                 
                 "mpayroll",
                 
                 "mcuenta_debitos_automaticos",
                 "mttarjeta_visa_debitos_automaticos",
                 "mpagodeservicios",
                 "mpagomiscuentas",
                 
                 "mcajeros_propios_descuentos",
                 "mtarjeta_visa_descuentos",
                 
                 "mcomisiones_mantenimiento",
                 "mcomisiones_otras",
                 
                 "mforex_buy",
                 
                 "mtransferencias_recibidas",
                 "ccheques_depositados",
                 "mcheques_depositados",
                 "mcheques_emitidos",
                 
                 "ccajas_otras",
                 
                 "matm",
                 "matm_other",
                 
                 "Master_msaldototal",
                 "Master_msaldopesos",
                 "Master_msaldodolares",
                 "Master_mconsumospesos",
                 "Master_mconsumosdolares",
                 "Master_mlimitecompra",
                 "Master_mpagado",
                 "Master_mpagospesos",
                 "Master_mpagosdolares",
                 "Master_mconsumototal",
                 "Master_mpagominimo",
                 
                 "Visa_msaldototal",
                 "Visa_msaldopesos",
                 "Visa_mconsumospesos",
                 "Visa_mconsumosdolares",
                 "Visa_mlimitecompra",
                 "Visa_madelantopesos",
                 "Visa_mpagado",
                 "Visa_mpagospesos",
                 "Visa_mpagosdolares",
                 "Visa_mconsumototal",
                 "Visa_mpagominimo"
)

colsFE_to_fix <- c("dist_msaldo_mcaja",
                   "dist_msaldo_mcorriente",
                   "dist_mcaja_mcorriente",
                   "dist_msaldo_mpayroll",
                   
                   "mprestamo_avg",
                   
                   "dist_mprestamos_msaldo",
                   "dist_mprestamos_msaldo2",
                   
                   "dist_mrentabilidad_mcomisiones",
                   "dist_mrentabilidad_mactivos",
                   "dist_mrentabilidad_mpasivos",
                   
                   "suma_mlimites",
                   "vm_msaldototal",
                   "vm_msaldopesos",
                   "vm_mpagominimo",
                   "vmtarjeta_consumos",
                   "vm_mpagado",
                   "msaldovs_mprest",
                   
                   "total_mdeuda",
                   "total_mactivos")

cols_to_fix_tot <- c(cols_to_fix, colsFE_to_fix)

#por como armé los nombres de campos, estos son los campos que expresan variables monetarias
campos_monetarios  <- cols_to_fix
campos_monetarios  <- campos_monetarios[campos_monetarios %like% "^(m|Visa_m|Master_m|vm_m)"]
campos_monetarios <- c(campos_monetarios, colsFE_to_fix)


#--FUNCIONES para corregir el Data-DRIFTING------------------------------------------------------------------------

#FUNCION1: FIX Drifting con "rank_cero_fijo" (los positivos se rankean por su lado y los negativos por otro)
drift_rank_cero_fijo  <- function( campos_drift )
{
  for( campo in campos_drift )
  {
    cat( campo, " " )
    dataset[ get(campo) ==0, paste0("rank_",campo) := 0 ]
    dataset[ get(campo) > 0, paste0("rank_",campo) :=   frank(  get(campo))  / .N, by= foto_mes ] 
    dataset[ get(campo) < 0, paste0("rank_",campo) :=  -frank( -get(campo))  / .N, by= foto_mes ]
    #dataset[ , (campo) := NULL ] #NO eliminamos la columna original -> es una especie de FE: agregamos su version Rankeada
  }
}


#FUNCION2: FIX Drifting con "Scalado z-score" (Xi - avg / desvio)
drift_zscale  <- function( campos_drift )
{
  for( campo in campos_drift )
  {
    cat( campo, " " )
    dataset[, paste0("z_",campo) := scale(get(campo),center = TRUE, scale = TRUE), by= foto_mes]
  }
} 


#FUNCION3: FE hacerle a toda la variable un "escalado z-score"
escalar_variable  <- function( campos_drift )
{
  for( campo in campos_drift )
  {
    cat( campo, " " )
    dataset[, paste0("scale_",campo) := scale(get(campo),center = TRUE, scale = TRUE), by= foto_mes]
    dataset[ , (campo) := NULL ]
  }
} 


#FUNCION4: FIX por inflación:
drift_deflacion  <- function( campos_monetarios )
{
  vfoto_mes <- c( 201901, 201902, 201903, 201904, 201905, 201906,
                  201907, 201908, 201909, 201910, 201911, 201912,
                  202001, 202002, 202003, 202004, 202005, 202006,
                  202007, 202008, 202009, 202010, 202011, 202012,
                  202101, 202102, 202103, 202104, 202105, 202106,
                  202107  )
  
  vIPC  <- c( 1.9903030878, 1.9174403544, 1.8296186587,
              1.7728862972, 1.7212488323, 1.6776304408,
              1.6431248196, 1.5814483345, 1.4947526791,
              1.4484037589, 1.3913580777, 1.3404220402,
              1.3154288912, 1.2921698342, 1.2472681797,
              1.2300475145, 1.2118694724, 1.1881073259,
              1.1693969743, 1.1375456949, 1.1065619600,
              1.0681100000, 1.0370000000, 1.0000000000,
              0.9680542110, 0.9344152616, 0.8882274350,
              0.8532444140, 0.8251880213, 0.8003763543,
              0.7763107219  )
  
  tb_IPC  <- data.table( "foto_mes"= vfoto_mes,
                         "IPC" = vIPC )
  
  dataset[ tb_IPC,
           on= c("foto_mes"),
           paste0("def_",(campos_monetarios)) :=  .SD * i.IPC ,
           .SDcols = campos_monetarios ]
  
}

#FUNCION5: renombra la columna original
renamecols_orig <- function(campos_drift) 
{
  for( campo in campos_drift )
  {
    cat( campo, " " )
    dataset[, paste0("orig_",campo) := get(campo)]
    dataset[ , (campo) := NULL ]
  }
} 

#FUNCION6: eliminar las columnas listadas
eliminacols  <- function( campos_drift )
{
  for( campo in campos_drift )
  {
    cat( campo, " " )
    dataset[ , (campo) := NULL ] 
  }
}   




#---INICIO: Carga dataset y verifica--------------------------------------------------------  
#Aqui se debe poner la carpeta de la materia de SU computadora local (el Working Directory)
setwd("~/buckets/b1")

#cargo el dataset
dataset <- fread("./datasets/competencia02_fix.csv.gz") 

# minima exploración por fecha
table(dataset$foto_mes)

# minima exploración por fecha y target
table(dataset$foto_mes, dataset$clase_ternaria )

#-----PROCESO: Applico Funciones para: +FE y fix_Drifting ------------------------------------------------------

#ordeno el dataset para aplicar luego los fixes
setorder( dataset, foto_mes, numero_de_cliente )

dim(dataset) #4562810     159

#Agregar el Feature Engineering (FE): Agrega 75 variables -> 159 + 75 = 234
Add_FE (dataset)

dim(dataset) #4562810     234


#Agrego RANK de las variables que quiero corregir: agrega 73 variables iniciadas rank_  -> 234 + 73 = 307
drift_rank_cero_fijo( cols_to_fix_tot )
dim(dataset)


#Agrego z-Scale de las variables que quiero corregir: agrega 73 variables iniciadas z_ -> 307 + 73 = 380
drift_zscale( cols_to_fix_tot )
dim(dataset)


#Agrego variables deflasionadas por inflación segun indice IPC: agrega 71 variables iniciadas def_ -> 380 + 71 = 451
drift_deflacion( campos_monetarios )
dim(dataset)


#Eliminamos las columnas de las variables originales, para evitar que el modelo prefiera esas en Train, al ser del propio mes, pero luego performe peor al predecir en datos futuros: elimina 73 variables -> 451 - 71 = 378
eliminacols( cols_to_fix_tot)

#En total agrega: 75 + 73 + 73 + 71 - 73 = 219 variables -> 75 del FE | 144 del Drifting -> pasaria de 159 a 378
dim(dataset)



#Si quisiera identificar las columnas que modificamos (en vez de eliminarlas) agregandoles un _orig adelante: renamecols_orig(cols_to_fix_tot )
# renamecols_orig(cols_to_fix_tot )

#Si quiero ESCALAR con z-score algunas variables, usar la función: escalar_variable( columnas ): agrega variables iniciadas con scale_
# escalar_variable( columnas )

#---EXPORTA DATABSE GENERADA A .CSV---------
setwd("~/buckets/b1/datasets/")

#EXPORTAMOS a un .CSV
fwrite( dataset,
        file= "competencia02_FE1_drift.csv.gz",
        sep= "," )

