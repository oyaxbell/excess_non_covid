#Variables CONEVAL relacionadas a SALUD

CONEVAL<-fread("/Users/nefoantonio/Downloads/Calculo_IRS_2020_R-2/base/ITER_NALCSV20.csv", stringsAsFactors = F, encoding="UTF-8") %>%
  as.data.frame() %>% rename_all(tolower) 
CONEVAL <- dplyr::select(CONEVAL, entidad, nom_ent, mun, nom_mun, loc, nom_loc, pobtot, pobmas, 
                         p_0a2, p_15ymas, p_6a11, p_12a14, p6a11_noa, p12a14noa, p15ym_an, p15ym_se, p15pri_in, 
                         p15pri_co, p15sec_in, psinder, vivparh_cv, vph_pisoti, vph_pisodt, vivpar_hab, 
                         tvivparhab, vivparh_cv, pro_ocup_c, vph_s_elec, vph_aguafv, vph_excsa, vph_letr, 
                         vph_nodren, vph_refri, vph_lavad ,phog_ind,pcon_disc,pobfem,pobmas,vph_snbien,pafil_ipriv)

# Se generan las claves de entidades, municipios y localidades
CONEVAL$clave_ent <- paste0(formatC(CONEVAL$entidad,width=2, flag="0"), sep="")
CONEVAL$clave_mun <- paste0(formatC(CONEVAL$entidad,width=2, flag="0"), 
                            formatC(CONEVAL$mun,width=3, flag="0"), sep="")
CONEVAL$clave_loc <- paste0(formatC(CONEVAL$entidad,width=2, flag="0"), 
                            formatC(CONEVAL$mun,width=3, flag="0"),
                            formatC(CONEVAL$loc,width=4, flag="0"), sep="")

CONEVAL2 <- CONEVAL %>% mutate_at(vars(-nom_ent,-nom_mun,-nom_loc, -clave_ent, 
                                       -clave_mun, -clave_loc), as.numeric)

###########################################
# Parte 1: estimación de variables insumo #
###########################################

CONEVAL3 <- filter(CONEVAL2, !(is.na(pobmas) | is.na(p_0a2)))

# Se estiman las variables insumo

# Población de 15 años o más analfabeta
CONEVAL3$i_analf    <- CONEVAL3$p15ym_an / CONEVAL3$p_15ymas 

# Población de 6 a 14 años que no asiste a la escuela
CONEVAL3$i_asistesc <- (CONEVAL3$p6a11_noa + CONEVAL3$p12a14noa) / ( CONEVAL3$p_6a11+ CONEVAL3$p_12a14)

# Población de 15 años y más con educación básica incompleta
CONEVAL3$i_edbasinc <- (CONEVAL3$p15ym_se + CONEVAL3$p15pri_in + CONEVAL3$p15pri_co + CONEVAL3$p15sec_in) / CONEVAL3$p_15ymas

# Población sin derechohabiencia a servicios de salud
CONEVAL3$i_sdsalud  <- CONEVAL3$psinder / CONEVAL3$pobtot

#Institucion Privada
CONEVAL3$i_sdsalud_privada  <- CONEVAL3$pafil_ipriv / CONEVAL3$pobtot

# Viviendas con piso de tierra
CONEVAL3$i_ptierra  <- CONEVAL3$vph_pisoti / CONEVAL3$vivparh_cv

# Viviendas que no disponen de excusado o sanitario
CONEVAL3$i_nosan    <- 1-( (CONEVAL3$vph_excsa + CONEVAL3$vph_letr) / CONEVAL3$vivparh_cv)

# Viviendas que no disponen de agua entubada de la red pública
CONEVAL3$i_noagua   <- CONEVAL3$vph_aguafv / CONEVAL3$vivparh_cv

# Viviendas que no disponen de drenaje
CONEVAL3$i_nodren   <- CONEVAL3$vph_nodren / CONEVAL3$vivparh_cv

# Viviendas que no disponen de energía eléctrica
CONEVAL3$i_noelec   <- CONEVAL3$vph_s_elec / CONEVAL3$vivparh_cv

# Viviendas que no disponen de lavadora
CONEVAL3$i_nolav    <- 1-( CONEVAL3$vph_lavad / CONEVAL3$vivparh_cv)

# Viviendas que no disponen de refrigerador
CONEVAL3$i_noref    <- 1-( CONEVAL3$vph_refri / CONEVAL3$vivparh_cv)

# Personas Indigenas 
CONEVAL3$i_indigena    <- CONEVAL3$phog_ind / CONEVAL3$pobtot

# Personas Discapacidad 
CONEVAL3$i_discapacidad    <- CONEVAL3$pcon_disc / CONEVAL3$pobtot

# Viviendas sin Bien 
CONEVAL3$i_sin_bienes    <- CONEVAL3$vph_snbien/ CONEVAL3$vivparh_cv

CONEVAL4 <- dplyr::select(CONEVAL3, clave_ent, nom_ent, clave_mun, nom_mun, clave_loc, nom_loc, 
                          vivparh_cv, p_6a11, p_12a14, p_15ymas, pobtot, i_analf, i_asistesc, i_edbasinc, 
                          i_sdsalud, i_ptierra, i_nosan, i_noagua, i_nodren, i_noelec, i_nolav, i_noref,i_indigena,i_discapacidad,i_sin_bienes,i_sdsalud_privada)

# CONEVAL municipios

CONEVAL_mun <- dplyr::filter(CONEVAL4, nom_loc=="Total del Municipio")
CONEVAL_mun <- dplyr::select(CONEVAL_mun, -clave_loc, -nom_loc)


