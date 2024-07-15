library(dplyr)
library(openxlsx)
library(purrr)
library(lubridate)


prestador <- read.xlsx("/Users/paulmoreno/Downloads/base_caracterizaciones.xlsx",sheet="prestador")
pobserv <- read.xlsx("/Users/paulmoreno/Downloads/base_caracterizaciones.xlsx",sheet="poblacionservida")
fuente <- read.xlsx("/Users/paulmoreno/Downloads/base_caracterizaciones.xlsx",sheet="fuente")
captacion <- read.xlsx("/Users/paulmoreno/Downloads/base_caracterizaciones.xlsx",sheet="captacion")
sistemaagua <- read.xlsx("/Users/paulmoreno/Downloads/base_caracterizaciones.xlsx",sheet="sistemaagua")
reservorio <- read.xlsx("/Users/paulmoreno/Downloads/base_caracterizaciones.xlsx",sheet="reservorio")
sistemaalca <- read.xlsx("/Users/paulmoreno/Downloads/base_caracterizaciones.xlsx",sheet="sistemaalcantarillado")
ptar <- read.xlsx("/Users/paulmoreno/Downloads/base_caracterizaciones.xlsx",sheet="ptar")
disposicionfinal <- read.xlsx("/Users/paulmoreno/Downloads/base_caracterizaciones.xlsx",sheet="disposicionfinal")
ubs <- read.xlsx("/Users/paulmoreno/Downloads/base_caracterizaciones.xlsx",sheet="ubs")
usuario <- read.xlsx("/Users/paulmoreno/Downloads/base_caracterizaciones.xlsx",sheet="usuario")
ccpp_validado <- read.xlsx("/Users/paulmoreno/PedidoCCPP_validado.xlsx",sheet = "CCPP") %>% mutate(ubigeo = as.numeric(ubigeo_ccpp))
prestador_categoria <- read.xlsx("/Users/paulmoreno/Datass_Sunass_rev.xlsx",sheet = "Prestador") %>% filter(fuente=="SUNASS")

cols_igp <- c("p031A01b_cuentaconordenanzamunicipal","p031A01c_seencuentradentrodeestructuraorganicayrof","p031A01a_cuentaconcontabilidadindependiente",
              "p031C02_tienecontratosuscritoconlamunicipalidad",
         "p031B02f_resolucionmunicipaldereconocimientodelaoc","p031B02c_cuentaconlibropadrondeasociados","p053_cuentaconlibroderecaudouotrosimilar",
         "p035_cobracuota","p039_laocaplicalametodologiadecuotafamiliar","p059_lacuotacubrecostosdeoaym","p030a_numerototaldeasociados",
         "p060_cualeselporcentajedelatarifaquecubrelos",
         "p046_numerodeusuariosmorosos","elpagoestructuradodependedelamicromedicion","p018_agua","p018_alcantarillado","p018_tar",
         "p018_disposicionexcretas")

sum_pobserv <- pobserv %>% 
  group_by(codigodeprestador) %>% 
  summarise(conex_agua_tot = sum(p021_conexionesdeaguatotales), conex_tot_alca = sum(p023_conexionesdealcantarilladototales),conex_ubs = sum(p027_cantidaddeubsenelccpp),
            conex_micro = sum(p028_numerodeconexionesconmicromedicion), conti_horas_avenida = mean(p029a_continuidadpromedioenepocadelluviahorasdia),
            conti_horas_estiaje = mean(p029b_continuidadpromedioenepocadeestiajehorasdia), conti_dias_avenida = mean(p029c_diasalasemanaconservicioavenida),
            conti_dias_estiaje = mean(p029d_diasalasemanaconservicioestiaje))

prestador_aux <- prestador %>% select(codigodeprestador,centropoblado,cols_igp) %>% mutate(centropoblado = as.numeric(centropoblado)) %>% 
  left_join(sum_pobserv,by="codigodeprestador") %>% 
  left_join(ccpp_validado %>% select(ubigeo,densidad_pob,POBTOTAL),by=c("centropoblado"="ubigeo")) 

prestador_aux$cob_agua <- (prestador_aux$conex_agua_tot * prestador_aux$densidad_pob) / prestador_aux$POBTOTAL
prestador_aux$cob_alca <- (prestador_aux$conex_tot_alca * prestador_aux$densidad_pob) / prestador_aux$POBTOTAL
prestador_aux$cob_excretas <- (prestador_aux$conex_ubs * prestador_aux$densidad_pob) / prestador_aux$POBTOTAL
prestador_aux$cob_agua <- ifelse(prestador_aux$cob_agua > 1, 1, prestador_aux$cob_agua)
prestador_aux$cob_alca <- ifelse(prestador_aux$cob_alca > 1, 1, prestador_aux$cob_alca)
prestador_aux$cob_excretas <- ifelse(prestador_aux$cob_excretas > 1, 1, prestador_aux$cob_excretas)

igp <- prestador_aux %>% select(codigodeprestador,p031A01b_cuentaconordenanzamunicipal,p031A01c_seencuentradentrodeestructuraorganicayrof,
                                p031A01a_cuentaconcontabilidadindependiente,p031C02_tienecontratosuscritoconlamunicipalidad,
                                p031B02f_resolucionmunicipaldereconocimientodelaoc,p031B02c_cuentaconlibropadrondeasociados,
                                p053_cuentaconlibroderecaudouotrosimilar,p035_cobracuota,p039_laocaplicalametodologiadecuotafamiliar,
                                p059_lacuotacubrecostosdeoaym,p060_cualeselporcentajedelatarifaquecubrelos,conex_agua_tot,p046_numerodeusuariosmorosos)

igp <- left_join(prestador %>% select(codigodeprestador,p031_quetipodeprestadores) %>% mutate(
  p031_quetipodeprestadores = case_when(
    p031_quetipodeprestadores == "Prestación Indirecta del Servicio - Organización Comunal" ~ "OC",
    p031_quetipodeprestadores %in% c("Prestación Directa del Servicio - Prestador Municipal","Prestación Directa del Servicio - UGM") ~ "PM",
    is.na(p031_quetipodeprestadores) ~ "Sin Prestador",
    TRUE ~ as.character(NA)
  )
),igp,by="codigodeprestador")

igp <- left_join(igp,prestador_categoria %>% select(Prestador_Codigo,ambito_prest),by=c("codigodeprestador"="Prestador_Codigo")) %>% 
  mutate(
    ambito_prest = case_when(
      ambito_prest %in% c("PC","Urbano no EPS") ~ "Urbano",
      ambito_prest == "Rural" ~ "Rural",
      TRUE ~ as.character(NA)
    )
  )

igp_fin <- igp %>% mutate(
  p031A01b_cuentaconordenanzamunicipal = case_when(
    p031A01b_cuentaconordenanzamunicipal == "Si" ~ 1,
    p031A01b_cuentaconordenanzamunicipal == "No" ~ 0,
    TRUE ~ NA_real_
  ),
  p031A01c_seencuentradentrodeestructuraorganicayrof = case_when(
    p031A01c_seencuentradentrodeestructuraorganicayrof == "Si" ~ 1,
    p031A01c_seencuentradentrodeestructuraorganicayrof == "No" ~ 0,
    TRUE ~ NA_real_
  ),
  p031A01a_cuentaconcontabilidadindependiente = case_when(
    p031A01a_cuentaconcontabilidadindependiente == "Si" ~ 1,
    p031A01a_cuentaconcontabilidadindependiente == "No" ~ 0,
    TRUE ~ NA_real_
  ),
  p031C02_tienecontratosuscritoconlamunicipalidad = case_when(
    p031C02_tienecontratosuscritoconlamunicipalidad == "Si" ~ 1,
    p031C02_tienecontratosuscritoconlamunicipalidad == "No" ~ 0,
    TRUE ~ NA_real_
  ),
  p031B02f_resolucionmunicipaldereconocimientodelaoc = case_when(
    p031B02f_resolucionmunicipaldereconocimientodelaoc == "Si" ~ 1,
    p031B02f_resolucionmunicipaldereconocimientodelaoc == "No" ~ 0,
    TRUE ~ NA_real_
  ),
  p031B02c_cuentaconlibropadrondeasociados = case_when(
    p031B02c_cuentaconlibropadrondeasociados == "Si" ~ 1,
    p031B02c_cuentaconlibropadrondeasociados == "No" ~ 0,
    TRUE ~ NA_real_
  ),
  p053_cuentaconlibroderecaudouotrosimilar = case_when(
    p053_cuentaconlibroderecaudouotrosimilar == "Si" ~ 1,
    p053_cuentaconlibroderecaudouotrosimilar == "No" ~ 0,
    TRUE ~ NA_real_
  ),
  p035_cobracuota = case_when(
    p035_cobracuota == "Si" ~ 1,
    p035_cobracuota == "No" ~ 0,
    TRUE ~ NA_real_
  ),
  p039_laocaplicalametodologiadecuotafamiliar = case_when(
    p039_laocaplicalametodologiadecuotafamiliar == "Si" ~ 1,
    p039_laocaplicalametodologiadecuotafamiliar == "No" ~ 0,
    TRUE ~ NA_real_
  ),
  p059_lacuotacubrecostosdeoaym = case_when(
    p059_lacuotacubrecostosdeoaym == "Si" ~ 1,
    p059_lacuotacubrecostosdeoaym == "No" ~ 0,
    TRUE ~ NA_real_
  ),
  p060_cualeselporcentajedelatarifaquecubrelos = case_when(
    p060_cualeselporcentajedelatarifaquecubrelos >= 65 ~ 1,
    p060_cualeselporcentajedelatarifaquecubrelos < 65 & p060_cualeselporcentajedelatarifaquecubrelos >= 30 ~ 0.5,
    p060_cualeselporcentajedelatarifaquecubrelos < 30 ~ 0,
    TRUE ~ NA_real_
  )
)


igp_fin_aux <- igp_fin %>% mutate(
  fdp = case_when(
    ambito_prest=="Rural" & p031_quetipodeprestadores == "OC" ~ p031B02f_resolucionmunicipaldereconocimientodelaoc * 0.75 +
      (p031B02f_resolucionmunicipaldereconocimientodelaoc) * (p031B02c_cuentaconlibropadrondeasociados)^(p031B02f_resolucionmunicipaldereconocimientodelaoc) * 0.5 -
      (2 * p031B02f_resolucionmunicipaldereconocimientodelaoc - 1) * (p031B02c_cuentaconlibropadrondeasociados) * 0.25,
    ambito_prest=="Rural" & p031_quetipodeprestadores == "PM" ~ p031A01b_cuentaconordenanzamunicipal*0.3+
      p031A01c_seencuentradentrodeestructuraorganicayrof*0.3+p031A01a_cuentaconcontabilidadindependiente*0.4,
    ambito_prest=="Rural" & p031_quetipodeprestadores == "Sin Prestador" ~ 0,
    ambito_prest=="Urbano" & p031_quetipodeprestadores == "OC" ~ p031B02f_resolucionmunicipaldereconocimientodelaoc+p031B02c_cuentaconlibropadrondeasociados,
    ambito_prest=="Urbano" & p031_quetipodeprestadores == "PM" ~ p031A01b_cuentaconordenanzamunicipal*0.4+
      p031A01c_seencuentradentrodeestructuraorganicayrof*0.3+p031A01a_cuentaconcontabilidadindependiente*0.3,
    ambito_prest=="Urbano" & p031_quetipodeprestadores == "Sin Prestador" ~ 0
  ),
  gigf = p053_cuentaconlibroderecaudouotrosimilar*0.25,
  sfp = case_when(
    ambito_prest=="Rural" ~ p035_cobracuota*(p039_laocaplicalametodologiadecuotafamiliar*0.2+p059_lacuotacubrecostosdeoaym*0.3+
                                               p060_cualeselporcentajedelatarifaquecubrelos*0.3+(1-(p046_numerodeusuariosmorosos/conex_agua_tot))*0.2),
    ambito_prest=="Urbano" ~ p035_cobracuota*(p059_lacuotacubrecostosdeoaym*0.3+
                                                p060_cualeselporcentajedelatarifaquecubrelos*0.4+(1-(p046_numerodeusuariosmorosos/conex_agua_tot))*0.3)
  )
)

igp_fin_aux <- igp_fin_aux %>%
  mutate(
    fdp = coalesce(fdp, 0),
    gigf = coalesce(gigf, 0),
    sfp = coalesce(sfp, 0),
    igp = case_when(
      ambito_prest == "Rural" ~ fdp * 0.3 + gigf * 0.2 + sfp * 0.5,
      ambito_prest == "Urbano" ~ fdp * 0.6 + sfp * 0.4
    ),
    igp = ifelse(is.infinite(igp) & igp < 0, 0, igp),
    igp = ifelse(igp > 1,1,igp)
  )
###### RECURSOS HIDRICOS ###### 
licencia_fuente <- fuente %>% 
  group_by(codigodeprestador) %>%
  mutate(cuentaconlicenciauso = case_when(
    all(cuentaconlicenciauso == "Si" & !is.na(cuentaconlicenciauso)) ~ 1,
    any(cuentaconlicenciauso == "Si" & !is.na(cuentaconlicenciauso)) ~ 0.5,
    all(cuentaconlicenciauso == "No" | is.na(cuentaconlicenciauso)) ~ 0,
    TRUE ~ NA_real_
  ))

capta_protegida <- captacion %>% 
  group_by(codigodeprestador) %>%
  mutate(captacionestaprotegida = case_when(
    all(captacionestaprotegida == "Si" & !is.na(captacionestaprotegida)) ~ 1,
    any(captacionestaprotegida == "Si" & !is.na(captacionestaprotegida)) ~ 0.5,
    all(captacionestaprotegida == "No" | is.na(captacionestaprotegida)) ~ 0,
    TRUE ~ NA_real_
  ),
  laproteccioncumplesufuncion_1 = case_when(
    all(laproteccioncumplesufuncion == "Si" & !is.na(laproteccioncumplesufuncion)) ~ 1,
    any(laproteccioncumplesufuncion == "Si" & !is.na(laproteccioncumplesufuncion)) ~ 0.5,
    all(laproteccioncumplesufuncion == "No" | is.na(laproteccioncumplesufuncion)) ~ 0,
    TRUE ~ NA_real_
  ))

dfs <- list(prestador %>% select(codigodeprestador,p010_harealizadointervencionparasuconservacion), 
            licencia_fuente %>% select(codigodeprestador,cuentaconlicenciauso), 
            capta_protegida %>% select(codigodeprestador,captacionestaprotegida,laproteccioncumplesufuncion))
irh <- reduce(dfs, left_join, by = "codigodeprestador")


irh <- irh %>% mutate(
  p010_harealizadointervencionparasuconservacion = case_when(
    p010_harealizadointervencionparasuconservacion == "Si" ~ 1,
    p010_harealizadointervencionparasuconservacion == "No" ~ 0,
    TRUE ~ NA_real_
  ),
  laproteccioncumplesufuncion = case_when(
    laproteccioncumplesufuncion == "Si" ~ 1,
    laproteccioncumplesufuncion == "No" ~ 0,
    TRUE ~ NA_real_
  ),
)

irh$irh <- (irh$p010_harealizadointervencionparasuconservacion + irh$cuentaconlicenciauso +
  irh$captacionestaprotegida + irh$laproteccioncumplesufuncion)*0.25

###### SISTEMA DE AGUA ###### 
agua <- sistemaagua %>% distinct(codigodeprestador, .keep_all = TRUE) %>% 
  select(codigodeprestador,p010_realizaelmantenimientodelsistema,p030_realizacloracion,p029_sistemadecloracionoperativo,p031_sinocuentacomoclora,
         p038_realizamedicionclororesidual,p043_clororesidualpuntomaslejano)

res <- reservorio %>% distinct(codigodeprestador, .keep_all = TRUE) %>% 
  select(codigodeprestador,estadooperativodereservorio,clororesidualmgl) %>% 
  mutate(estadooperativodereservorio = case_when(
    estadooperativodereservorio %in% c("Opera normal","Opera limitado") ~ "Si",
    estadooperativodereservorio == "Inoperativo" ~ "No",
    TRUE ~ NA_character_
  ))

agua_res <- left_join(agua,res,by="codigodeprestador")



alca <- sistemaalca %>% distinct(codigodeprestador, .keep_all = TRUE) %>% select(
  codigodeprestador,p008_realizamantenimientoalareddealcantarillado,estadooperativodelsistemadealcantarillado
)
tar <- ptar %>% distinct(codigodeprestador, .keep_all = TRUE) %>% select(
  codigodeprestador,p020_serealizamantenimientoalaptar,eorejas
)
df <- disposicionfinal %>% distinct(codigodeprestador, .keep_all = TRUE) %>% select(codigodeprestador,codigodeprestador,p029_autorizaciondevertimiento)
ubs_aux <- ubs %>% distinct(codigodeprestador, .keep_all = TRUE) %>% select(codigodeprestador,codigodeprestador,realizavisitasdeinspeccion)


dfs <- list(prestador %>% select(codigodeprestador), agua_res,
            alca, tar, df, ubs_aux)
sistemas <- reduce(dfs, left_join, by = "codigodeprestador")


prest_sis <- left_join(prestador_aux,sistemas,by="codigodeprestador")
iss <- prest_sis %>% 
  mutate(porcent_micro = ifelse(conex_micro/conex_agua_tot > 1, 1, conex_micro/conex_agua_tot)) %>% 
  select(codigodeprestador,p018_agua,cob_agua,p010_realizaelmantenimientodelsistema,estadooperativodereservorio,
                     p018_alcantarillado,cob_alca,p008_realizamantenimientoalareddealcantarillado,estadooperativodelsistemadealcantarillado,
                     p018_tar,p020_serealizamantenimientoalaptar,eorejas,p029_autorizaciondevertimiento,p018_disposicionexcretas,
                     cob_excretas,realizavisitasdeinspeccion,p030_realizacloracion,p029_sistemadecloracionoperativo,
                     p031_sinocuentacomoclora,p038_realizamedicionclororesidual,clororesidualmgl,p043_clororesidualpuntomaslejano,
                     conti_horas_avenida,conti_horas_estiaje,conti_dias_avenida,conti_dias_estiaje,elpagoestructuradodependedelamicromedicion,
         porcent_micro)


iss_aux <- iss %>% mutate(
  p018_agua = case_when(
    p018_agua == "Si" ~ 1,
    p018_agua == "No" ~ 0,
    TRUE ~ NA_real_
  ),
  p018_alcantarillado = case_when(
    p018_alcantarillado == "Si" ~ 1,
    p018_alcantarillado == "No" ~ 0,
    TRUE ~ NA_real_
  ),
  p018_tar = case_when(
    p018_tar == "Si" ~ 1,
    p018_tar == "No" ~ 0,
    TRUE ~ NA_real_
  ),
  p018_disposicionexcretas = case_when(
    p018_disposicionexcretas == "Si" ~ 1,
    p018_disposicionexcretas == "No" ~ 0,
    TRUE ~ NA_real_
  ),
  p010_realizaelmantenimientodelsistema = case_when(
    p010_realizaelmantenimientodelsistema %in% c("Si","Sólo de algunos") ~ 1,
    p010_realizaelmantenimientodelsistema == "No" ~ 0,
    TRUE ~ NA_real_
  ),
  estadooperativodereservorio = case_when(
    estadooperativodereservorio == "Si" ~ 1,
    estadooperativodereservorio == "No" ~ 0,
    TRUE ~ NA_real_
  ),
  p008_realizamantenimientoalareddealcantarillado = case_when(
    p008_realizamantenimientoalareddealcantarillado == "Si" ~ 1,
    p008_realizamantenimientoalareddealcantarillado == "No" ~ 0,
    TRUE ~ NA_real_
  ),
  estadooperativodelsistemadealcantarillado = case_when(
    estadooperativodelsistemadealcantarillado == "Si" ~ 1,
    estadooperativodelsistemadealcantarillado == "No" ~ 0,
    TRUE ~ NA_real_
  ),
  p020_serealizamantenimientoalaptar = case_when(
    p020_serealizamantenimientoalaptar == "Si" ~ 1,
    p020_serealizamantenimientoalaptar == "No" ~ 0,
    TRUE ~ NA_real_
  ),
  eorejas = case_when(
    eorejas %in% c("Opera normal","Opera limitado") ~ 1,
    eorejas == "Inoperativo" ~ 0,
    TRUE ~ NA_real_
  ),
  p029_autorizaciondevertimiento = case_when(
    p029_autorizaciondevertimiento == "Si" ~ 1,
    p029_autorizaciondevertimiento == "No" ~ 0,
    TRUE ~ NA_real_
  ),
  realizavisitasdeinspeccion = case_when(
    realizavisitasdeinspeccion %in% c("Algunas UBS","Todas las UBS") ~ 1,
    realizavisitasdeinspeccion == "Ninguna" ~ 0,
    TRUE ~ NA_real_
  ),
  p030_realizacloracion = case_when(
    p030_realizacloracion == "Si" ~ 1,
    p030_realizacloracion == "No" ~ 0,
    TRUE ~ NA_real_
  ),
  p029_sistemadecloracionoperativo = case_when(
    p029_sistemadecloracionoperativo %in% c("Opera limitado","Opera normal") ~ 1,
    p029_sistemadecloracionoperativo == "Inoperativo" ~ 0,
    TRUE ~ NA_real_
  ),
  p031_sinocuentacomoclora = case_when(
    p031_sinocuentacomoclora == "Directamente al reservorio de forma manual" ~ 1,
    p031_sinocuentacomoclora == "Otro" ~ 0,
    TRUE ~ NA_real_
  ),
  p038_realizamedicionclororesidual = case_when(
    p038_realizamedicionclororesidual == "Si" ~ 1,
    p038_realizamedicionclororesidual == "No" ~ 0,
    TRUE ~ NA_real_
  ),
  elpagoestructuradodependedelamicromedicion = case_when(
    elpagoestructuradodependedelamicromedicion == "Si" ~ 1,
    elpagoestructuradodependedelamicromedicion == "No" ~ 0,
    TRUE ~ NA_real_
  )
)


iss_aux$ssn <- (iss_aux$p018_agua * 
          (iss_aux$cob_agua * 0.5 + 
             iss_aux$p010_realizaelmantenimientodelsistema * 0.2 + 
             iss_aux$estadooperativodereservorio * 0.3) * 0.5) +
  ((iss_aux$p018_alcantarillado * 
      (iss_aux$cob_alca * 0.5 + 
         iss_aux$p008_realizamantenimientoalareddealcantarillado * 0.2 + 
         iss_aux$estadooperativodelsistemadealcantarillado * 0.3) * 0.25) +
     (iss_aux$p018_alcantarillado * iss_aux$p018_tar * 
        (iss_aux$p020_serealizamantenimientoalaptar * 0.3 + 
           iss_aux$estadooperativodelsistemadealcantarillado * 0.5 + 
           iss_aux$p029_autorizaciondevertimiento * 0.2) * 0.25) +
     ((1 - iss_aux$p018_alcantarillado) * iss_aux$p018_disposicionexcretas * 
        (iss_aux$cob_excretas * 0.5 + 
           iss_aux$realizavisitasdeinspeccion * 0.5) * 0.5))

iss_aux$clr <- iss_aux$p030_realizacloracion*(iss_aux$p029_sistemadecloracionoperativo*0.5+(1-iss_aux$p029_sistemadecloracionoperativo)*iss_aux$p031_sinocuentacomoclora*0.3+iss_aux$p038_realizamedicionclororesidual*0.3+iss_aux$clororesidualmgl*0.05+iss_aux$p043_clororesidualpuntomaslejano*0.15)
iss_aux$cds <- (iss_aux$conti_horas_avenida*iss_aux$conti_horas_estiaje+iss_aux$conti_dias_avenida*iss_aux$conti_dias_estiaje)/(168*2)
iss_aux$mcd <- iss_aux$elpagoestructuradodependedelamicromedicion*0.5+iss_aux$porcent_micro*0.5


iss_aux <- iss_aux %>%
  mutate(
    ssn = coalesce(ssn, 0),
    clr = coalesce(clr, 0),
    cds = coalesce(cds, 0),
    mcd = coalesce(mcd, 0),
    iss = ssn * 0.3 + clr * 0.35 + cds * 0.35 + mcd * 0,
    iss = ifelse(iss>1,1,iss)
  )


######### USUARIOS ######### 

categorizar <- function(satisfaccion) {
  if(satisfaccion %in% c("Satisfecho", "Muy satisfecho")) {
    return("Satisfechos")
  } else if(satisfaccion %in% c("Indiferente")) {
    return("Indiferentes")
  } else {
    return("Insatisfechos")
  }
}


# Aplicar la función de categorización
data <- usuario %>% 
  mutate(
    p010_niveldesatisfaccionconelservicio = sapply(p010_niveldesatisfaccionconelservicio, categorizar),
    p012_pagariaunmontoadicionalporelservicio = case_when(
      p012_pagariaunmontoadicionalporelservicio == "Si" ~ 1,
      p012_pagariaunmontoadicionalporelservicio == "No" ~ 0,
      TRUE ~ NA_real_
    ),
    p011_ustedestariadeacuerdoconusodemedidores = case_when(
      p011_ustedestariadeacuerdoconusodemedidores == "Si" ~ 1,
      p011_ustedestariadeacuerdoconusodemedidores == "No" ~ 0,
      TRUE ~ NA_real_
    )
  )

# Calcular porcentaje de cada categoría por cada `cod`
porcentajes <- data %>%
  group_by(codigodeprestador, p010_niveldesatisfaccionconelservicio) %>%
  summarise(count = n()) %>%
  mutate(total = sum(count), porcentaje = (count / total) * 100) %>%
  ungroup()

max_porcentaje <- porcentajes %>%
  group_by(codigodeprestador) %>%
  slice_max(porcentaje) %>%
  ungroup() %>% distinct(codigodeprestador, .keep_all = TRUE)

# Asignar indicador según los umbrales
indicador <- max_porcentaje %>%
  group_by(codigodeprestador) %>%
  summarise(indicador = case_when(
    any(p010_niveldesatisfaccionconelservicio == "Satisfechos" ) ~ 1,
    any(p010_niveldesatisfaccionconelservicio == "Indiferentes" ) ~ 0.5,
    any(p010_niveldesatisfaccionconelservicio == "Insatisfechos") ~ 0
  ))
tarifa <- data %>% group_by(codigodeprestador) %>% summarise(
  p012_pagariaunmontoadicionalporelservicio = mean(p012_pagariaunmontoadicionalporelservicio),
  p011_ustedestariadeacuerdoconusodemedidores = mean(p011_ustedestariadeacuerdoconusodemedidores)
) %>% mutate(
  p012_pagariaunmontoadicionalporelservicio = case_when(
    p012_pagariaunmontoadicionalporelservicio < 0.3 ~ 0,
    p012_pagariaunmontoadicionalporelservicio >= 0.3 & p012_pagariaunmontoadicionalporelservicio < 0.65 ~ 0.5,
    p012_pagariaunmontoadicionalporelservicio >= 0.65 ~ 1,
    TRUE ~ NA_real_
  ),
  p011_ustedestariadeacuerdoconusodemedidores = case_when(
    p011_ustedestariadeacuerdoconusodemedidores < 0.3 ~ 0,
    p011_ustedestariadeacuerdoconusodemedidores >= 0.3 & p011_ustedestariadeacuerdoconusodemedidores < 0.65 ~ 0.5,
    p011_ustedestariadeacuerdoconusodemedidores >= 0.65 ~ 1,
    TRUE ~ NA_real_
  )
)
ivs <- full_join(tarifa,indicador,by="codigodeprestador") 


ivs$ivs <- ivs$indicador*0.6+ivs$p012_pagariaunmontoadicionalporelservicio*0.4+ivs$p011_ustedestariadeacuerdoconusodemedidores*0


### Calificacion final

dfs <- list(igp_fin_aux, iss_aux,
            ivs, irh)
calificacion_final <- reduce(dfs, left_join, by = "codigodeprestador")



calificacion_final$cpss=calificacion_final$igp*0.4+calificacion_final$iss*0.4+calificacion_final$irh*0.1+calificacion_final$ivs*0.1


calificacion_final <- calificacion_final %>% mutate(
  label_cpss = case_when(
    cpss>=0 & cpss<=0.21 ~ "Muy Malo",
    cpss>0.21 & cpss<=0.41 ~ "Malo",
    cpss>0.41 & cpss<=0.61 ~ "Regular",
    cpss>0.61 & cpss<=0.81 ~ "Bueno",
    cpss>0.81 & cpss<=1 ~ "Muy Bueno"
  )
)

table(calificacion_final$label_cpss)


calificacion_final_fin <- inner_join(calificacion_final,prestador %>% select(codigodeprestador,p002_fechadecaracterizacion,p001_oficinadesconcentrada),by="codigodeprestador") %>% 
  mutate(p002_fechadecaracterizacion = as.Date(p002_fechadecaracterizacion, origin = "1900-01-01")) %>% 
  filter(year(p002_fechadecaracterizacion)==2024)


inner_join(calificacion_final_fin,prestador_categoria %>% select(Prestador_Codigo,ambito_prest),by=c("codigodeprestador"="Prestador_Codigo"))

write.xlsx(calificacion_final_fin,"/Users/paulmoreno/Downloads/calificacion.xlsx")
