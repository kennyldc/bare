#MODELOS DE TESIS 
# Carlos Eduardo Lopez de la Cerda Bazaldua 
library (tidyverse)
library (stargazer)
library (lfe)
options (digits=8
         , scipen=8
         , show.signif.stars = FALSE
         , tibble.print_max=50
         , tibble.print_min=1
         , dplyr.width=900)
rm (list=ls ())
setwd("~/Tablet/descargas/Tesis/datos")

# PANEL 
panel_log <- read.csv ("panel_horarios.csv", sep=",", header=T)
panel_log$clave_turno <- as.factor(panel_log$clave_turno)
panel_log$year <- as.factor(panel_log$year)


# AJUSTES PARA PANEL_LOG (SUMAR 1) ----------------------------------------
panel_log$inc_tipo1_d250_t90h <- (panel_log$inc_tipo1_d250_t90h+1)
panel_log$inc_tipo2_d250_t90h <- (panel_log$inc_tipo2_d250_t90h+1)
panel_log$inc_tipo3_d250_t90h <- (panel_log$inc_tipo3_d250_t90h+1)
panel_log$inc_tipo1_d500_t90h <- (panel_log$inc_tipo1_d500_t90h+1)
panel_log$inc_tipo2_d500_t90h <- (panel_log$inc_tipo2_d500_t90h+1)
panel_log$inc_tipo3_d500_t90h <- (panel_log$inc_tipo3_d500_t90h+1)
panel_log$inc_tipo1_d1000_t90h <- (panel_log$inc_tipo1_d1000_t90h+1)
panel_log$inc_tipo2_d1000_t90h <- (panel_log$inc_tipo2_d1000_t90h+1)
panel_log$inc_tipo3_d1000_t90h <- (panel_log$inc_tipo3_d1000_t90h+1)

panel_log$inc_tipo1_d250_t29h <- (panel_log$inc_tipo1_d250_t29h+1)
panel_log$inc_tipo2_d250_t29h <- (panel_log$inc_tipo2_d250_t29h+1)
panel_log$inc_tipo3_d250_t29h <- (panel_log$inc_tipo3_d250_t29h+1)
panel_log$inc_tipo1_d500_t29h <- (panel_log$inc_tipo1_d500_t29h+1)
panel_log$inc_tipo2_d500_t29h <- (panel_log$inc_tipo2_d500_t29h+1)
panel_log$inc_tipo3_d500_t29h <- (panel_log$inc_tipo3_d500_t29h+1)
panel_log$inc_tipo1_d1000_t29h <- (panel_log$inc_tipo1_d1000_t29h+1)
panel_log$inc_tipo2_d1000_t29h <- (panel_log$inc_tipo2_d1000_t29h+1)
panel_log$inc_tipo3_d1000_t29h <- (panel_log$inc_tipo3_d1000_t29h+1)

panel_log$inc_tipo1_d250_t10h <- (panel_log$inc_tipo1_d250_t10h+1)
panel_log$inc_tipo2_d250_t10h <- (panel_log$inc_tipo2_d250_t10h+1)
panel_log$inc_tipo3_d250_t10h <- (panel_log$inc_tipo3_d250_t10h+1)
panel_log$inc_tipo1_d500_t10h <- (panel_log$inc_tipo1_d500_t10h+1)
panel_log$inc_tipo2_d500_t10h <- (panel_log$inc_tipo2_d500_t10h+1)
panel_log$inc_tipo3_d500_t10h <- (panel_log$inc_tipo3_d500_t10h+1)
panel_log$inc_tipo1_d1000_t10h <- (panel_log$inc_tipo1_d1000_t10h+1)
panel_log$inc_tipo2_d1000_t10h <- (panel_log$inc_tipo2_d1000_t10h+1)
panel_log$inc_tipo3_d1000_t10h <- (panel_log$inc_tipo3_d1000_t10h+1)


# CORTES PRIMARIA - SECUNDARIA --------------------------------------------
panel_primarias_log <- panel_log %>%
  filter(nivel=="PRIMARIA")
panel_secundarias_log <- panel_log %>%
  filter(nivel=="SECUNDARIA")

# TABLA 1 LYC PRIMARIAS BASE LOG-LOG  ---------------------------------------------
# TIPO 1 -  D.V. AMBITO PUBLICO
logfelycp_tipo1_d250_t90h <- felm(log(lyc) ~ log(inc_tipo1_d250_t90h) | id_unico + year | 0 | id_unico, data=panel_primarias_log)
logfelycp_tipo1_d500_t90h <- felm(log(lyc) ~ log(inc_tipo1_d500_t90h) | id_unico + year | 0 | id_unico, data=panel_primarias_log)
logfelycp_tipo1_d1000_t90h<- felm(log(lyc) ~ log(inc_tipo1_d1000_t90h)| id_unico + year | 0 | id_unico, data=panel_primarias_log)
logfelycp_tipo1_d250_t29h <- felm(log(lyc) ~ log(inc_tipo1_d250_t29h) | id_unico + year | 0 | id_unico, data=panel_primarias_log)
logfelycp_tipo1_d500_t29h <- felm(log(lyc) ~ log(inc_tipo1_d500_t29h) | id_unico + year | 0 | id_unico, data=panel_primarias_log)
logfelycp_tipo1_d1000_t29h<- felm(log(lyc) ~ log(inc_tipo1_d1000_t29h)| id_unico + year | 0 | id_unico, data=panel_primarias_log)
logfelycp_tipo1_d250_t10h <- felm(log(lyc) ~ log(inc_tipo1_d250_t10h) | id_unico + year | 0 | id_unico, data=panel_primarias_log)
logfelycp_tipo1_d500_t10h <- felm(log(lyc) ~ log(inc_tipo1_d500_t10h) | id_unico + year | 0 | id_unico, data=panel_primarias_log)
logfelycp_tipo1_d1000_t10h<- felm(log(lyc) ~ log(inc_tipo1_d1000_t10h)| id_unico + year | 0 | id_unico, data=panel_primarias_log)
# PRINT
stargazer (logfelycp_tipo1_d250_t90h, logfelycp_tipo1_d500_t90h, logfelycp_tipo1_d1000_t90h, logfelycp_tipo1_d250_t29h, logfelycp_tipo1_d500_t29h, logfelycp_tipo1_d1000_t29h,logfelycp_tipo1_d250_t10h, logfelycp_tipo1_d500_t10h, logfelycp_tipo1_d1000_t10h
           , digits=3
           , omit.stat=c ("f", "ser", "aic", "bic", "ll")
           , omit.table.layout="n"
           , no.space=TRUE
           , type="text")

# PRIMARIAS LYC TIPO 2 BASE 
# TIPO 2 - D.V. AMBITO PRIVADO
logfelycp_tipo2_d250_t90h <- felm(log(lyc) ~ log(inc_tipo2_d250_t90h) | id_unico + year | 0 | id_unico, data=panel_primarias_log)
logfelycp_tipo2_d500_t90h <- felm(log(lyc) ~ log(inc_tipo2_d500_t90h) | id_unico + year | 0 | id_unico, data=panel_primarias_log)
logfelycp_tipo2_d1000_t90h<- felm(log(lyc) ~ log(inc_tipo2_d1000_t90h)| id_unico + year | 0 | id_unico, data=panel_primarias_log)
logfelycp_tipo2_d250_t29h <- felm(log(lyc) ~ log(inc_tipo2_d250_t29h) | id_unico + year | 0 | id_unico, data=panel_primarias_log)
logfelycp_tipo2_d500_t29h <- felm(log(lyc) ~ log(inc_tipo2_d500_t29h) | id_unico + year | 0 | id_unico, data=panel_primarias_log)
logfelycp_tipo2_d1000_t29h<- felm(log(lyc) ~ log(inc_tipo2_d1000_t29h)| id_unico + year | 0 | id_unico, data=panel_primarias_log)
logfelycp_tipo2_d250_t10h <- felm(log(lyc) ~ log(inc_tipo2_d250_t10h) | id_unico + year | 0 | id_unico, data=panel_primarias_log)
logfelycp_tipo2_d500_t10h <- felm(log(lyc) ~ log(inc_tipo2_d500_t10h) | id_unico + year | 0 | id_unico, data=panel_primarias_log)
logfelycp_tipo2_d1000_t10h<- felm(log(lyc) ~ log(inc_tipo2_d1000_t10h)| id_unico + year | 0 | id_unico, data=panel_primarias_log)
# PRINT 
stargazer (logfelycp_tipo2_d250_t90h, logfelycp_tipo2_d500_t90h, logfelycp_tipo2_d1000_t90h, logfelycp_tipo2_d250_t29h, logfelycp_tipo2_d500_t29h, logfelycp_tipo2_d1000_t29h,logfelycp_tipo2_d250_t10h, logfelycp_tipo2_d500_t10h, logfelycp_tipo2_d1000_t10h
           , digits=3
           , omit.stat=c ("f", "ser", "aic", "bic", "ll")
           , omit.table.layout="n"
           , no.space=TRUE
           , type="text")


# PRIMARIAS LYC TIPO 3 BASE 
# TIPO 3 - D.V. HOMICIDIOS
logfelycp_tipo3_d250_t90h <- felm(log(lyc) ~ log(inc_tipo3_d250_t90h) | id_unico + year | 0 | id_unico, data=panel_primarias_log)
logfelycp_tipo3_d500_t90h <- felm(log(lyc) ~ log(inc_tipo3_d500_t90h) | id_unico + year | 0 | id_unico, data=panel_primarias_log)
logfelycp_tipo3_d1000_t90h<- felm(log(lyc) ~ log(inc_tipo3_d1000_t90h)| id_unico + year | 0 | id_unico, data=panel_primarias_log)
logfelycp_tipo3_d250_t29h <- felm(log(lyc) ~ log(inc_tipo3_d250_t29h) | id_unico + year | 0 | id_unico, data=panel_primarias_log)
logfelycp_tipo3_d500_t29h <- felm(log(lyc) ~ log(inc_tipo3_d500_t29h) | id_unico + year | 0 | id_unico, data=panel_primarias_log)
logfelycp_tipo3_d1000_t29h<- felm(log(lyc) ~ log(inc_tipo3_d1000_t29h)| id_unico + year | 0 | id_unico, data=panel_primarias_log)
logfelycp_tipo3_d250_t10h <- felm(log(lyc) ~ log(inc_tipo3_d250_t10h) | id_unico + year | 0 | id_unico, data=panel_primarias_log)
logfelycp_tipo3_d500_t10h <- felm(log(lyc) ~ log(inc_tipo3_d500_t10h) | id_unico + year | 0 | id_unico, data=panel_primarias_log)
logfelycp_tipo3_d1000_t10h<- felm(log(lyc) ~ log(inc_tipo3_d1000_t10h)| id_unico + year | 0 | id_unico, data=panel_primarias_log)
# PRINT 
stargazer (logfelycp_tipo3_d250_t90h, logfelycp_tipo3_d500_t90h, logfelycp_tipo3_d1000_t90h, logfelycp_tipo3_d250_t29h, logfelycp_tipo3_d500_t29h, logfelycp_tipo3_d1000_t29h,logfelycp_tipo3_d250_t10h, logfelycp_tipo3_d500_t10h, logfelycp_tipo3_d1000_t10h
           , digits=3
           , omit.stat=c ("f", "ser", "aic", "bic", "ll")
           , omit.table.layout="n"
           , no.space=TRUE
           , type="text")

# TABLA 2. MAT PRIMARIA BASE MODELO DOBLE LOG TESIS ----------------------------------
### TIPO 1 D.V. PUBLICO
logfematp_tipo1_d250_t90h <- felm(log(mat) ~ log(inc_tipo1_d250_t90h) | id_unico + year | 0 | id_unico, data=panel_primarias_log)
logfematp_tipo1_d500_t90h <- felm(log(mat) ~ log(inc_tipo1_d500_t90h) | id_unico + year | 0 | id_unico, data=panel_primarias_log)
logfematp_tipo1_d1000_t90h<- felm(log(mat) ~ log(inc_tipo1_d1000_t90h)| id_unico + year | 0 | id_unico, data=panel_primarias_log)
logfematp_tipo1_d250_t29h <- felm(log(mat) ~ log(inc_tipo1_d250_t29h) | id_unico + year | 0 | id_unico, data=panel_primarias_log)
logfematp_tipo1_d500_t29h <- felm(log(mat) ~ log(inc_tipo1_d500_t29h) | id_unico + year | 0 | id_unico, data=panel_primarias_log)
logfematp_tipo1_d1000_t29h<- felm(log(mat) ~ log(inc_tipo1_d1000_t29h)| id_unico + year | 0 | id_unico, data=panel_primarias_log)
logfematp_tipo1_d250_t10h <- felm(log(mat) ~ log(inc_tipo1_d250_t10h) | id_unico + year | 0 | id_unico, data=panel_primarias_log)
logfematp_tipo1_d500_t10h <- felm(log(mat) ~ log(inc_tipo1_d500_t10h) | id_unico + year | 0 | id_unico, data=panel_primarias_log)
logfematp_tipo1_d1000_t10h<- felm(log(mat) ~ log(inc_tipo1_d1000_t10h)| id_unico + year | 0 | id_unico, data=panel_primarias_log)
# PRINT
stargazer (logfematp_tipo1_d250_t90h, logfematp_tipo1_d500_t90h, logfematp_tipo1_d1000_t90h, logfematp_tipo1_d250_t29h, logfematp_tipo1_d500_t29h, logfematp_tipo1_d1000_t29h,logfematp_tipo1_d250_t10h, logfematp_tipo1_d500_t10h, logfematp_tipo1_d1000_t10h
           , digits=3
           , omit.stat=c ("f", "ser", "aic", "bic", "ll")
           , omit.table.layout="n"
           , no.space=TRUE
           , type="text")

# PRIMARIAS mat TIPO 2 BASE 
# TIPO 2 - D.V. AMBITO PRIVADO
logfematp_tipo2_d250_t90h <- felm(log(mat) ~ log(inc_tipo2_d250_t90h) | id_unico + year | 0 | id_unico, data=panel_primarias_log)
logfematp_tipo2_d500_t90h <- felm(log(mat) ~ log(inc_tipo2_d500_t90h) | id_unico + year | 0 | id_unico, data=panel_primarias_log)
logfematp_tipo2_d1000_t90h<- felm(log(mat) ~ log(inc_tipo2_d1000_t90h)| id_unico + year | 0 | id_unico, data=panel_primarias_log)
logfematp_tipo2_d250_t29h <- felm(log(mat) ~ log(inc_tipo2_d250_t29h) | id_unico + year | 0 | id_unico, data=panel_primarias_log)
logfematp_tipo2_d500_t29h <- felm(log(mat) ~ log(inc_tipo2_d500_t29h) | id_unico + year | 0 | id_unico, data=panel_primarias_log)
logfematp_tipo2_d1000_t29h<- felm(log(mat) ~ log(inc_tipo2_d1000_t29h)| id_unico + year | 0 | id_unico, data=panel_primarias_log)
logfematp_tipo2_d250_t10h <- felm(log(mat) ~ log(inc_tipo2_d250_t10h) | id_unico + year | 0 | id_unico, data=panel_primarias_log)
logfematp_tipo2_d500_t10h <- felm(log(mat) ~ log(inc_tipo2_d500_t10h) | id_unico + year | 0 | id_unico, data=panel_primarias_log)
logfematp_tipo2_d1000_t10h<- felm(log(mat) ~ log(inc_tipo2_d1000_t10h)| id_unico + year | 0 | id_unico, data=panel_primarias_log)
# PRINT 
stargazer (logfematp_tipo2_d250_t90h, logfematp_tipo2_d500_t90h, logfematp_tipo2_d1000_t90h, logfematp_tipo2_d250_t29h, logfematp_tipo2_d500_t29h, logfematp_tipo2_d1000_t29h,logfematp_tipo2_d250_t10h, logfematp_tipo2_d500_t10h, logfematp_tipo2_d1000_t10h
           , digits=3
           , omit.stat=c ("f", "ser", "aic", "bic", "ll")
           , omit.table.layout="n"
           , no.space=TRUE
           , type="text")


# PRIMARIAS mat TIPO 3 BASE 
# TIPO 3 - D.V. HOMICIDIOS
logfematp_tipo3_d250_t90h <- felm(log(mat) ~ log(inc_tipo3_d250_t90h) | id_unico + year | 0 | id_unico, data=panel_primarias_log)
logfematp_tipo3_d500_t90h <- felm(log(mat) ~ log(inc_tipo3_d500_t90h) | id_unico + year | 0 | id_unico, data=panel_primarias_log)
logfematp_tipo3_d1000_t90h<- felm(log(mat) ~ log(inc_tipo3_d1000_t90h)| id_unico + year | 0 | id_unico, data=panel_primarias_log)
logfematp_tipo3_d250_t29h <- felm(log(mat) ~ log(inc_tipo3_d250_t29h) | id_unico + year | 0 | id_unico, data=panel_primarias_log)
logfematp_tipo3_d500_t29h <- felm(log(mat) ~ log(inc_tipo3_d500_t29h) | id_unico + year | 0 | id_unico, data=panel_primarias_log)
logfematp_tipo3_d1000_t29h<- felm(log(mat) ~ log(inc_tipo3_d1000_t29h)| id_unico + year | 0 | id_unico, data=panel_primarias_log)
logfematp_tipo3_d250_t10h <- felm(log(mat) ~ log(inc_tipo3_d250_t10h) | id_unico + year | 0 | id_unico, data=panel_primarias_log)
logfematp_tipo3_d500_t10h <- felm(log(mat) ~ log(inc_tipo3_d500_t10h) | id_unico + year | 0 | id_unico, data=panel_primarias_log)
logfematp_tipo3_d1000_t10h<- felm(log(mat) ~ log(inc_tipo3_d1000_t10h)| id_unico + year | 0 | id_unico, data=panel_primarias_log)
# PRINT 
stargazer (logfematp_tipo3_d250_t90h, logfematp_tipo3_d500_t90h, logfematp_tipo3_d1000_t90h, logfematp_tipo3_d250_t29h, logfematp_tipo3_d500_t29h, logfematp_tipo3_d1000_t29h,logfematp_tipo3_d250_t10h, logfematp_tipo3_d500_t10h, logfematp_tipo3_d1000_t10h
           , digits=3
           , omit.stat=c ("f", "ser", "aic", "bic", "ll")
           , omit.table.layout="n"
           , no.space=TRUE
           , type="text")

# TABLA 3. MODELO DOBLE LOG TESIS LYC SECUNDARIA MODELO BASICO----------------------------------
# TIPO 1 - DV. AMBITO PUBLICO 
logfelycs_tipo1_d250_t90h <- felm(log(lyc) ~ log(inc_tipo1_d250_t90h) | id_unico + year | 0 | id_unico, data=panel_secundarias_log)
logfelycs_tipo1_d500_t90h <- felm(log(lyc) ~ log(inc_tipo1_d500_t90h) | id_unico + year | 0 | id_unico, data=panel_secundarias_log)
logfelycs_tipo1_d1000_t90h<- felm(log(lyc) ~ log(inc_tipo1_d1000_t90h)| id_unico + year | 0 | id_unico, data=panel_secundarias_log)
logfelycs_tipo1_d250_t29h <- felm(log(lyc) ~ log(inc_tipo1_d250_t29h) | id_unico + year | 0 | id_unico, data=panel_secundarias_log)
logfelycs_tipo1_d500_t29h <- felm(log(lyc) ~ log(inc_tipo1_d500_t29h) | id_unico + year | 0 | id_unico, data=panel_secundarias_log)
logfelycs_tipo1_d1000_t29h<- felm(log(lyc) ~ log(inc_tipo1_d1000_t29h)| id_unico + year | 0 | id_unico, data=panel_secundarias_log)
logfelycs_tipo1_d250_t10h <- felm(log(lyc) ~ log(inc_tipo1_d250_t10h) | id_unico + year | 0 | id_unico, data=panel_secundarias_log)
logfelycs_tipo1_d500_t10h <- felm(log(lyc) ~ log(inc_tipo1_d500_t10h) | id_unico + year | 0 | id_unico, data=panel_secundarias_log)
logfelycs_tipo1_d1000_t10h<- felm(log(lyc) ~ log(inc_tipo1_d1000_t10h)| id_unico + year | 0 | id_unico, data=panel_secundarias_log)
# PRINT
stargazer (logfelycs_tipo1_d250_t90h, logfelycs_tipo1_d500_t90h, logfelycs_tipo1_d1000_t90h, logfelycs_tipo1_d250_t29h, logfelycs_tipo1_d500_t29h, logfelycs_tipo1_d1000_t29h,logfelycs_tipo1_d250_t10h, logfelycs_tipo1_d500_t10h, logfelycs_tipo1_d1000_t10h
           , digits=3
           , omit.stat=c ("f", "ser", "aic", "bic", "ll")
           , omit.table.layout="n"
           , no.space=TRUE
           , type="text")

# SECUNDARIAS LYC TIPO 2 BASE 
# TIPO 2 - D.V. AMBITO PRIVADO
logfelycs_tipo2_d250_t90h <- felm(log(lyc) ~ log(inc_tipo2_d250_t90h) | id_unico + year | 0 | id_unico, data=panel_secundarias_log)
logfelycs_tipo2_d500_t90h <- felm(log(lyc) ~ log(inc_tipo2_d500_t90h) | id_unico + year | 0 | id_unico, data=panel_secundarias_log)
logfelycs_tipo2_d1000_t90h<- felm(log(lyc) ~ log(inc_tipo2_d1000_t90h)| id_unico + year | 0 | id_unico, data=panel_secundarias_log)
logfelycs_tipo2_d250_t29h <- felm(log(lyc) ~ log(inc_tipo2_d250_t29h) | id_unico + year | 0 | id_unico, data=panel_secundarias_log)
logfelycs_tipo2_d500_t29h <- felm(log(lyc) ~ log(inc_tipo2_d500_t29h) | id_unico + year | 0 | id_unico, data=panel_secundarias_log)
logfelycs_tipo2_d1000_t29h<- felm(log(lyc) ~ log(inc_tipo2_d1000_t29h)| id_unico + year | 0 | id_unico, data=panel_secundarias_log)
logfelycs_tipo2_d250_t10h <- felm(log(lyc) ~ log(inc_tipo2_d250_t10h) | id_unico + year | 0 | id_unico, data=panel_secundarias_log)
logfelycs_tipo2_d500_t10h <- felm(log(lyc) ~ log(inc_tipo2_d500_t10h) | id_unico + year | 0 | id_unico, data=panel_secundarias_log)
logfelycs_tipo2_d1000_t10h<- felm(log(lyc) ~ log(inc_tipo2_d1000_t10h)| id_unico + year | 0 | id_unico, data=panel_secundarias_log)
# PRINT 
stargazer (logfelycs_tipo2_d250_t90h, logfelycs_tipo2_d500_t90h, logfelycs_tipo2_d1000_t90h, logfelycs_tipo2_d250_t29h, logfelycs_tipo2_d500_t29h, logfelycs_tipo2_d1000_t29h,logfelycs_tipo2_d250_t10h, logfelycs_tipo2_d500_t10h, logfelycs_tipo2_d1000_t10h
           , digits=3
           , omit.stat=c ("f", "ser", "aic", "bic", "ll")
           , omit.table.layout="n"
           , no.space=TRUE
           , type="text")


# SECUNDARIAS LYC TIPO 3 BASE 
# TIPO 3 - D.V. HOMICIDIOS
logfelycs_tipo3_d250_t90h <- felm(log(lyc) ~ log(inc_tipo3_d250_t90h) | id_unico + year | 0 | id_unico, data=panel_secundarias_log)
logfelycs_tipo3_d500_t90h <- felm(log(lyc) ~ log(inc_tipo3_d500_t90h) | id_unico + year | 0 | id_unico, data=panel_secundarias_log)
logfelycs_tipo3_d1000_t90h<- felm(log(lyc) ~ log(inc_tipo3_d1000_t90h)| id_unico + year | 0 | id_unico, data=panel_secundarias_log)
logfelycs_tipo3_d250_t29h <- felm(log(lyc) ~ log(inc_tipo3_d250_t29h) | id_unico + year | 0 | id_unico, data=panel_secundarias_log)
logfelycs_tipo3_d500_t29h <- felm(log(lyc) ~ log(inc_tipo3_d500_t29h) | id_unico + year | 0 | id_unico, data=panel_secundarias_log)
logfelycs_tipo3_d1000_t29h<- felm(log(lyc) ~ log(inc_tipo3_d1000_t29h)| id_unico + year | 0 | id_unico, data=panel_secundarias_log)
logfelycs_tipo3_d250_t10h <- felm(log(lyc) ~ log(inc_tipo3_d250_t10h) | id_unico + year | 0 | id_unico, data=panel_secundarias_log)
logfelycs_tipo3_d500_t10h <- felm(log(lyc) ~ log(inc_tipo3_d500_t10h) | id_unico + year | 0 | id_unico, data=panel_secundarias_log)
logfelycs_tipo3_d1000_t10h<- felm(log(lyc) ~ log(inc_tipo3_d1000_t10h)| id_unico + year | 0 | id_unico, data=panel_secundarias_log)
# PRINT 
stargazer (logfelycs_tipo3_d250_t90h, logfelycs_tipo3_d500_t90h, logfelycs_tipo3_d1000_t90h, logfelycs_tipo3_d250_t29h, logfelycs_tipo3_d500_t29h, logfelycs_tipo3_d1000_t29h,logfelycs_tipo3_d250_t10h, logfelycs_tipo3_d500_t10h, logfelycs_tipo3_d1000_t10h
           , digits=3
           , omit.stat=c ("f", "ser", "aic", "bic", "ll")
           , omit.table.layout="n"
           , no.space=TRUE
           , type="text")

# TABLA 4. MODELO DOBLE LOG TESIS MAT SECUNDARIA MODELO BASICO----------------------------------
# TIPO 1 - DV. AMBITO PUBLICO 
logfemats_tipo1_d250_t90h <- felm(log(mat) ~ log(inc_tipo1_d250_t90h) | id_unico + year | 0 | id_unico, data=panel_secundarias_log)
logfemats_tipo1_d500_t90h <- felm(log(mat) ~ log(inc_tipo1_d500_t90h) | id_unico + year | 0 | id_unico, data=panel_secundarias_log)
logfemats_tipo1_d1000_t90h<- felm(log(mat) ~ log(inc_tipo1_d1000_t90h)| id_unico + year | 0 | id_unico, data=panel_secundarias_log)
logfemats_tipo1_d250_t29h <- felm(log(mat) ~ log(inc_tipo1_d250_t29h) | id_unico + year | 0 | id_unico, data=panel_secundarias_log)
logfemats_tipo1_d500_t29h <- felm(log(mat) ~ log(inc_tipo1_d500_t29h) | id_unico + year | 0 | id_unico, data=panel_secundarias_log)
logfemats_tipo1_d1000_t29h<- felm(log(mat) ~ log(inc_tipo1_d1000_t29h)| id_unico + year | 0 | id_unico, data=panel_secundarias_log)
logfemats_tipo1_d250_t10h <- felm(log(mat) ~ log(inc_tipo1_d250_t10h) | id_unico + year | 0 | id_unico, data=panel_secundarias_log)
logfemats_tipo1_d500_t10h <- felm(log(mat) ~ log(inc_tipo1_d500_t10h) | id_unico + year | 0 | id_unico, data=panel_secundarias_log)
logfemats_tipo1_d1000_t10h<- felm(log(mat) ~ log(inc_tipo1_d1000_t10h)| id_unico + year | 0 | id_unico, data=panel_secundarias_log)
# PRINT
stargazer (logfemats_tipo1_d250_t90h, logfemats_tipo1_d500_t90h, logfemats_tipo1_d1000_t90h, logfemats_tipo1_d250_t29h, logfemats_tipo1_d500_t29h, logfemats_tipo1_d1000_t29h,logfemats_tipo1_d250_t10h, logfemats_tipo1_d500_t10h, logfemats_tipo1_d1000_t10h
           , digits=3
           , omit.stat=c ("f", "ser", "aic", "bic", "ll")
           , omit.table.layout="n"
           , no.space=TRUE
           , type="text")

# SECUNDARIAS mat TIPO 2 BASE 
# TIPO 2 - D.V. AMBITO PRIVADO
logfemats_tipo2_d250_t90h <- felm(log(mat) ~ log(inc_tipo2_d250_t90h) | id_unico + year | 0 | id_unico, data=panel_secundarias_log)
logfemats_tipo2_d500_t90h <- felm(log(mat) ~ log(inc_tipo2_d500_t90h) | id_unico + year | 0 | id_unico, data=panel_secundarias_log)
logfemats_tipo2_d1000_t90h<- felm(log(mat) ~ log(inc_tipo2_d1000_t90h)| id_unico + year | 0 | id_unico, data=panel_secundarias_log)
logfemats_tipo2_d250_t29h <- felm(log(mat) ~ log(inc_tipo2_d250_t29h) | id_unico + year | 0 | id_unico, data=panel_secundarias_log)
logfemats_tipo2_d500_t29h <- felm(log(mat) ~ log(inc_tipo2_d500_t29h) | id_unico + year | 0 | id_unico, data=panel_secundarias_log)
logfemats_tipo2_d1000_t29h<- felm(log(mat) ~ log(inc_tipo2_d1000_t29h)| id_unico + year | 0 | id_unico, data=panel_secundarias_log)
logfemats_tipo2_d250_t10h <- felm(log(mat) ~ log(inc_tipo2_d250_t10h) | id_unico + year | 0 | id_unico, data=panel_secundarias_log)
logfemats_tipo2_d500_t10h <- felm(log(mat) ~ log(inc_tipo2_d500_t10h) | id_unico + year | 0 | id_unico, data=panel_secundarias_log)
logfemats_tipo2_d1000_t10h<- felm(log(mat) ~ log(inc_tipo2_d1000_t10h)| id_unico + year | 0 | id_unico, data=panel_secundarias_log)
# PRINT 
stargazer (logfemats_tipo2_d250_t90h, logfemats_tipo2_d500_t90h, logfemats_tipo2_d1000_t90h, logfemats_tipo2_d250_t29h, logfemats_tipo2_d500_t29h, logfemats_tipo2_d1000_t29h,logfemats_tipo2_d250_t10h, logfemats_tipo2_d500_t10h, logfemats_tipo2_d1000_t10h
           , digits=3
           , omit.stat=c ("f", "ser", "aic", "bic", "ll")
           , omit.table.layout="n"
           , no.space=TRUE
           , type="text")


# SECUNDARIAS mat TIPO 3 BASE 
# TIPO 3 - D.V. HOMICIDIOS
logfemats_tipo3_d250_t90h <- felm(log(mat) ~ log(inc_tipo3_d250_t90h) | id_unico + year | 0 | id_unico, data=panel_secundarias_log)
logfemats_tipo3_d500_t90h <- felm(log(mat) ~ log(inc_tipo3_d500_t90h) | id_unico + year | 0 | id_unico, data=panel_secundarias_log)
logfemats_tipo3_d1000_t90h<- felm(log(mat) ~ log(inc_tipo3_d1000_t90h)| id_unico + year | 0 | id_unico, data=panel_secundarias_log)
logfemats_tipo3_d250_t29h <- felm(log(mat) ~ log(inc_tipo3_d250_t29h) | id_unico + year | 0 | id_unico, data=panel_secundarias_log)
logfemats_tipo3_d500_t29h <- felm(log(mat) ~ log(inc_tipo3_d500_t29h) | id_unico + year | 0 | id_unico, data=panel_secundarias_log)
logfemats_tipo3_d1000_t29h<- felm(log(mat) ~ log(inc_tipo3_d1000_t29h)| id_unico + year | 0 | id_unico, data=panel_secundarias_log)
logfemats_tipo3_d250_t10h <- felm(log(mat) ~ log(inc_tipo3_d250_t10h) | id_unico + year | 0 | id_unico, data=panel_secundarias_log)
logfemats_tipo3_d500_t10h <- felm(log(mat) ~ log(inc_tipo3_d500_t10h) | id_unico + year | 0 | id_unico, data=panel_secundarias_log)
logfemats_tipo3_d1000_t10h<- felm(log(mat) ~ log(inc_tipo3_d1000_t10h)| id_unico + year | 0 | id_unico, data=panel_secundarias_log)
# PRINT 
stargazer (logfemats_tipo3_d250_t90h, logfemats_tipo3_d500_t90h, logfemats_tipo3_d1000_t90h, logfemats_tipo3_d250_t29h, logfemats_tipo3_d500_t29h, logfemats_tipo3_d1000_t29h,logfemats_tipo3_d250_t10h, logfemats_tipo3_d500_t10h, logfemats_tipo3_d1000_t10h
           , digits=3
           , omit.stat=c ("f", "ser", "aic", "bic", "ll")
           , omit.table.layout="n"
           , no.space=TRUE
           , type="text")

# TABLA 5. INTERACCIONES_FINANCIAMIENTO PRIMARIAS LYC  -------------------------
# TIPO 1 D.A. PUBLICO 
inter_finlogfelycp_tipo1_d250_t90h <- felm(log(lyc) ~ log(inc_tipo1_d250_t90h) + log(inc_tipo1_d250_t90h)*financiamiento | id_unico + year | 0 | id_unico, data=panel_primarias_log)
inter_finlogfelycp_tipo1_d500_t90h <- felm(log(lyc) ~ log(inc_tipo1_d500_t90h) + log(inc_tipo1_d500_t90h)*financiamiento | id_unico + year | 0 | id_unico, data=panel_primarias_log)
inter_finlogfelycp_tipo1_d1000_t90h<- felm(log(lyc) ~ log(inc_tipo1_d1000_t90h) + log(inc_tipo1_d1000_t90h)*financiamiento | id_unico + year | 0 | id_unico, data=panel_primarias_log)
inter_finlogfelycp_tipo1_d250_t29h <- felm(log(lyc) ~ log(inc_tipo1_d250_t29h) + log(inc_tipo1_d250_t29h)*financiamiento | id_unico + year | 0 | id_unico, data=panel_primarias_log)
inter_finlogfelycp_tipo1_d500_t29h <- felm(log(lyc) ~ log(inc_tipo1_d500_t29h) + log(inc_tipo1_d500_t29h)*financiamiento | id_unico + year | 0 | id_unico, data=panel_primarias_log)
inter_finlogfelycp_tipo1_d1000_t29h<- felm(log(lyc) ~ log(inc_tipo1_d1000_t29h) + log(inc_tipo1_d1000_t29h)*financiamiento | id_unico + year | 0 | id_unico, data=panel_primarias_log)
inter_finlogfelycp_tipo1_d250_t10h <- felm(log(lyc) ~ log(inc_tipo1_d250_t10h) + log(inc_tipo1_d250_t10h)*financiamiento | id_unico + year | 0 | id_unico, data=panel_primarias_log)
inter_finlogfelycp_tipo1_d500_t10h <- felm(log(lyc) ~ log(inc_tipo1_d500_t10h) + log(inc_tipo1_d500_t10h)*financiamiento | id_unico + year | 0 | id_unico, data=panel_primarias_log)
inter_finlogfelycp_tipo1_d1000_t10h<- felm(log(lyc) ~ log(inc_tipo1_d1000_t10h) + log(inc_tipo1_d1000_t10h)*financiamiento | id_unico + year | 0 | id_unico, data=panel_primarias_log)
# PRINT 
stargazer (inter_finlogfelycp_tipo1_d250_t90h,
           inter_finlogfelycp_tipo1_d500_t90h,
           inter_finlogfelycp_tipo1_d1000_t90h,
           inter_finlogfelycp_tipo1_d250_t29h,
           inter_finlogfelycp_tipo1_d500_t29h,
           inter_finlogfelycp_tipo1_d1000_t29h,
           inter_finlogfelycp_tipo1_d250_t10h,
           inter_finlogfelycp_tipo1_d500_t10h,
           inter_finlogfelycp_tipo1_d1000_t10h
           , digits=3
           , omit.stat=c ("f", "ser", "aic", "bic", "ll")
           , omit.table.layout="n"
           , no.space=TRUE
           , order = c(1,11,2,12,3,13,4,14,5,15,6,16,7,17,8,18,9,19,10)
           , type="text")

## TIPO 2 D.A. PRIVADO
inter_finlogfelycp_tipo2_d250_t90h <- felm(log(lyc) ~ log(inc_tipo2_d250_t90h) + log(inc_tipo2_d250_t90h)*financiamiento | id_unico + year | 0 | id_unico, data=panel_primarias_log)
inter_finlogfelycp_tipo2_d500_t90h <- felm(log(lyc) ~ log(inc_tipo2_d500_t90h) + log(inc_tipo2_d500_t90h)*financiamiento | id_unico + year | 0 | id_unico, data=panel_primarias_log)
inter_finlogfelycp_tipo2_d1000_t90h<- felm(log(lyc) ~ log(inc_tipo2_d1000_t90h) + log(inc_tipo2_d1000_t90h)*financiamiento | id_unico + year | 0 | id_unico, data=panel_primarias_log)
inter_finlogfelycp_tipo2_d250_t29h <- felm(log(lyc) ~ log(inc_tipo2_d250_t29h) + log(inc_tipo2_d250_t29h)*financiamiento | id_unico + year | 0 | id_unico, data=panel_primarias_log)
inter_finlogfelycp_tipo2_d500_t29h <- felm(log(lyc) ~ log(inc_tipo2_d500_t29h) + log(inc_tipo2_d500_t29h)*financiamiento | id_unico + year | 0 | id_unico, data=panel_primarias_log)
inter_finlogfelycp_tipo2_d1000_t29h<- felm(log(lyc) ~ log(inc_tipo2_d1000_t29h) + log(inc_tipo2_d1000_t29h)*financiamiento | id_unico + year | 0 | id_unico, data=panel_primarias_log)
inter_finlogfelycp_tipo2_d250_t10h <- felm(log(lyc) ~ log(inc_tipo2_d250_t10h) + log(inc_tipo2_d250_t10h)*financiamiento | id_unico + year | 0 | id_unico, data=panel_primarias_log)
inter_finlogfelycp_tipo2_d500_t10h <- felm(log(lyc) ~ log(inc_tipo2_d500_t10h) + log(inc_tipo2_d500_t10h)*financiamiento | id_unico + year | 0 | id_unico, data=panel_primarias_log)
inter_finlogfelycp_tipo2_d1000_t10h<- felm(log(lyc) ~ log(inc_tipo2_d1000_t10h) + log(inc_tipo2_d1000_t10h)*financiamiento | id_unico + year | 0 | id_unico, data=panel_primarias_log)
# PRINT 
stargazer (inter_finlogfelycp_tipo2_d250_t90h,
           inter_finlogfelycp_tipo2_d500_t90h,
           inter_finlogfelycp_tipo2_d1000_t90h,
           inter_finlogfelycp_tipo2_d250_t29h,
           inter_finlogfelycp_tipo2_d500_t29h,
           inter_finlogfelycp_tipo2_d1000_t29h,
           inter_finlogfelycp_tipo2_d250_t10h,
           inter_finlogfelycp_tipo2_d500_t10h,
           inter_finlogfelycp_tipo2_d1000_t10h
           , digits=3
           , omit.stat=c ("f", "ser", "aic", "bic", "ll")
           , omit.table.layout="n"
           , no.space=TRUE
           , order = c(1,11,2,12,3,13,4,14,5,15,6,16,7,17,8,18,9,19,10)
           , type="text")

# TIPO 3 D.V. HOMICIDIOS
inter_finlogfelycp_tipo3_d250_t90h <- felm(log(lyc) ~ log(inc_tipo3_d250_t90h) + log(inc_tipo3_d250_t90h)*financiamiento | id_unico + year | 0 | id_unico, data=panel_primarias_log)
inter_finlogfelycp_tipo3_d500_t90h <- felm(log(lyc) ~ log(inc_tipo3_d500_t90h) + log(inc_tipo3_d500_t90h)*financiamiento | id_unico + year | 0 | id_unico, data=panel_primarias_log)
inter_finlogfelycp_tipo3_d1000_t90h<- felm(log(lyc) ~ log(inc_tipo3_d1000_t90h) + log(inc_tipo3_d1000_t90h)*financiamiento | id_unico + year | 0 | id_unico, data=panel_primarias_log)
inter_finlogfelycp_tipo3_d250_t29h <- felm(log(lyc) ~ log(inc_tipo3_d250_t29h) + log(inc_tipo3_d250_t29h)*financiamiento | id_unico + year | 0 | id_unico, data=panel_primarias_log)
inter_finlogfelycp_tipo3_d500_t29h <- felm(log(lyc) ~ log(inc_tipo3_d500_t29h) + log(inc_tipo3_d500_t29h)*financiamiento | id_unico + year | 0 | id_unico, data=panel_primarias_log)
inter_finlogfelycp_tipo3_d1000_t29h<- felm(log(lyc) ~ log(inc_tipo3_d1000_t29h) + log(inc_tipo3_d1000_t29h)*financiamiento | id_unico + year | 0 | id_unico, data=panel_primarias_log)
inter_finlogfelycp_tipo3_d250_t10h <- felm(log(lyc) ~ log(inc_tipo3_d250_t10h) + log(inc_tipo3_d250_t10h)*financiamiento | id_unico + year | 0 | id_unico, data=panel_primarias_log)
inter_finlogfelycp_tipo3_d500_t10h <- felm(log(lyc) ~ log(inc_tipo3_d500_t10h) + log(inc_tipo3_d500_t10h)*financiamiento | id_unico + year | 0 | id_unico, data=panel_primarias_log)
inter_finlogfelycp_tipo3_d1000_t10h<- felm(log(lyc) ~ log(inc_tipo3_d1000_t10h) + log(inc_tipo3_d1000_t10h)*financiamiento | id_unico + year | 0 | id_unico, data=panel_primarias_log)
# PRINT 
stargazer (inter_finlogfelycp_tipo3_d250_t90h,
           inter_finlogfelycp_tipo3_d500_t90h,
           inter_finlogfelycp_tipo3_d1000_t90h,
           inter_finlogfelycp_tipo3_d250_t29h,
           inter_finlogfelycp_tipo3_d500_t29h,
           inter_finlogfelycp_tipo3_d1000_t29h,
           inter_finlogfelycp_tipo3_d250_t10h,
           inter_finlogfelycp_tipo3_d500_t10h,
           inter_finlogfelycp_tipo3_d1000_t10h
           , digits=3
           , omit.stat=c ("f", "ser", "aic", "bic", "ll")
           , omit.table.layout="n"
           , no.space=TRUE
           , order = c(1,11,2,12,3,13,4,14,5,15,6,16,7,17,8,18,9,19,10)
           , type="text")

# TABLA 6. INTERACCIONES_FINANCIAMIENTO PRIMARIAS MAT  -------------------------
# TIPO 1 D.A. PUBLICO 
inter_finlogfematp_tipo1_d250_t90h <- felm(log(mat) ~ log(inc_tipo1_d250_t90h) + log(inc_tipo1_d250_t90h)*financiamiento | id_unico + year | 0 | id_unico, data=panel_primarias_log)
inter_finlogfematp_tipo1_d500_t90h <- felm(log(mat) ~ log(inc_tipo1_d500_t90h) + log(inc_tipo1_d500_t90h)*financiamiento | id_unico + year | 0 | id_unico, data=panel_primarias_log)
inter_finlogfematp_tipo1_d1000_t90h<- felm(log(mat) ~ log(inc_tipo1_d1000_t90h) + log(inc_tipo1_d1000_t90h)*financiamiento | id_unico + year | 0 | id_unico, data=panel_primarias_log)
inter_finlogfematp_tipo1_d250_t29h <- felm(log(mat) ~ log(inc_tipo1_d250_t29h) + log(inc_tipo1_d250_t29h)*financiamiento | id_unico + year | 0 | id_unico, data=panel_primarias_log)
inter_finlogfematp_tipo1_d500_t29h <- felm(log(mat) ~ log(inc_tipo1_d500_t29h) + log(inc_tipo1_d500_t29h)*financiamiento | id_unico + year | 0 | id_unico, data=panel_primarias_log)
inter_finlogfematp_tipo1_d1000_t29h<- felm(log(mat) ~ log(inc_tipo1_d1000_t29h) + log(inc_tipo1_d1000_t29h)*financiamiento | id_unico + year | 0 | id_unico, data=panel_primarias_log)
inter_finlogfematp_tipo1_d250_t10h <- felm(log(mat) ~ log(inc_tipo1_d250_t10h) + log(inc_tipo1_d250_t10h)*financiamiento | id_unico + year | 0 | id_unico, data=panel_primarias_log)
inter_finlogfematp_tipo1_d500_t10h <- felm(log(mat) ~ log(inc_tipo1_d500_t10h) + log(inc_tipo1_d500_t10h)*financiamiento | id_unico + year | 0 | id_unico, data=panel_primarias_log)
inter_finlogfematp_tipo1_d1000_t10h<- felm(log(mat) ~ log(inc_tipo1_d1000_t10h) + log(inc_tipo1_d1000_t10h)*financiamiento | id_unico + year | 0 | id_unico, data=panel_primarias_log)
# PRINT 
stargazer (inter_finlogfematp_tipo1_d250_t90h,
           inter_finlogfematp_tipo1_d500_t90h,
           inter_finlogfematp_tipo1_d1000_t90h,
           inter_finlogfematp_tipo1_d250_t29h,
           inter_finlogfematp_tipo1_d500_t29h,
           inter_finlogfematp_tipo1_d1000_t29h,
           inter_finlogfematp_tipo1_d250_t10h,
           inter_finlogfematp_tipo1_d500_t10h,
           inter_finlogfematp_tipo1_d1000_t10h
           , digits=3
           , omit.stat=c ("f", "ser", "aic", "bic", "ll")
           , omit.table.layout="n"
           , no.space=TRUE
           , order = c(1,11,2,12,3,13,4,14,5,15,6,16,7,17,8,18,9,19,10)
           , type="text")

## TIPO 2 D.A. PRIVADO
inter_finlogfematp_tipo2_d250_t90h <- felm(log(mat) ~ log(inc_tipo2_d250_t90h) + log(inc_tipo2_d250_t90h)*financiamiento | id_unico + year | 0 | id_unico, data=panel_primarias_log)
inter_finlogfematp_tipo2_d500_t90h <- felm(log(mat) ~ log(inc_tipo2_d500_t90h) + log(inc_tipo2_d500_t90h)*financiamiento | id_unico + year | 0 | id_unico, data=panel_primarias_log)
inter_finlogfematp_tipo2_d1000_t90h<- felm(log(mat) ~ log(inc_tipo2_d1000_t90h) + log(inc_tipo2_d1000_t90h)*financiamiento | id_unico + year | 0 | id_unico, data=panel_primarias_log)
inter_finlogfematp_tipo2_d250_t29h <- felm(log(mat) ~ log(inc_tipo2_d250_t29h) + log(inc_tipo2_d250_t29h)*financiamiento | id_unico + year | 0 | id_unico, data=panel_primarias_log)
inter_finlogfematp_tipo2_d500_t29h <- felm(log(mat) ~ log(inc_tipo2_d500_t29h) + log(inc_tipo2_d500_t29h)*financiamiento | id_unico + year | 0 | id_unico, data=panel_primarias_log)
inter_finlogfematp_tipo2_d1000_t29h<- felm(log(mat) ~ log(inc_tipo2_d1000_t29h) + log(inc_tipo2_d1000_t29h)*financiamiento | id_unico + year | 0 | id_unico, data=panel_primarias_log)
inter_finlogfematp_tipo2_d250_t10h <- felm(log(mat) ~ log(inc_tipo2_d250_t10h) + log(inc_tipo2_d250_t10h)*financiamiento | id_unico + year | 0 | id_unico, data=panel_primarias_log)
inter_finlogfematp_tipo2_d500_t10h <- felm(log(mat) ~ log(inc_tipo2_d500_t10h) + log(inc_tipo2_d500_t10h)*financiamiento | id_unico + year | 0 | id_unico, data=panel_primarias_log)
inter_finlogfematp_tipo2_d1000_t10h<- felm(log(mat) ~ log(inc_tipo2_d1000_t10h) + log(inc_tipo2_d1000_t10h)*financiamiento | id_unico + year | 0 | id_unico, data=panel_primarias_log)
# PRINT 
stargazer (inter_finlogfematp_tipo2_d250_t90h,
           inter_finlogfematp_tipo2_d500_t90h,
           inter_finlogfematp_tipo2_d1000_t90h,
           inter_finlogfematp_tipo2_d250_t29h,
           inter_finlogfematp_tipo2_d500_t29h,
           inter_finlogfematp_tipo2_d1000_t29h,
           inter_finlogfematp_tipo2_d250_t10h,
           inter_finlogfematp_tipo2_d500_t10h,
           inter_finlogfematp_tipo2_d1000_t10h
           , digits=3
           , omit.stat=c ("f", "ser", "aic", "bic", "ll")
           , omit.table.layout="n"
           , no.space=TRUE
           , order = c(1,11,2,12,3,13,4,14,5,15,6,16,7,17,8,18,9,19,10)
           , type="text")

# TIPO 3 D.V. HOMICIDIOS
inter_finlogfematp_tipo3_d250_t90h <- felm(log(mat) ~ log(inc_tipo3_d250_t90h) + log(inc_tipo3_d250_t90h)*financiamiento | id_unico + year | 0 | id_unico, data=panel_primarias_log)
inter_finlogfematp_tipo3_d500_t90h <- felm(log(mat) ~ log(inc_tipo3_d500_t90h) + log(inc_tipo3_d500_t90h)*financiamiento | id_unico + year | 0 | id_unico, data=panel_primarias_log)
inter_finlogfematp_tipo3_d1000_t90h<- felm(log(mat) ~ log(inc_tipo3_d1000_t90h) + log(inc_tipo3_d1000_t90h)*financiamiento | id_unico + year | 0 | id_unico, data=panel_primarias_log)
inter_finlogfematp_tipo3_d250_t29h <- felm(log(mat) ~ log(inc_tipo3_d250_t29h) + log(inc_tipo3_d250_t29h)*financiamiento | id_unico + year | 0 | id_unico, data=panel_primarias_log)
inter_finlogfematp_tipo3_d500_t29h <- felm(log(mat) ~ log(inc_tipo3_d500_t29h) + log(inc_tipo3_d500_t29h)*financiamiento | id_unico + year | 0 | id_unico, data=panel_primarias_log)
inter_finlogfematp_tipo3_d1000_t29h<- felm(log(mat) ~ log(inc_tipo3_d1000_t29h) + log(inc_tipo3_d1000_t29h)*financiamiento | id_unico + year | 0 | id_unico, data=panel_primarias_log)
inter_finlogfematp_tipo3_d250_t10h <- felm(log(mat) ~ log(inc_tipo3_d250_t10h) + log(inc_tipo3_d250_t10h)*financiamiento | id_unico + year | 0 | id_unico, data=panel_primarias_log)
inter_finlogfematp_tipo3_d500_t10h <- felm(log(mat) ~ log(inc_tipo3_d500_t10h) + log(inc_tipo3_d500_t10h)*financiamiento | id_unico + year | 0 | id_unico, data=panel_primarias_log)
inter_finlogfematp_tipo3_d1000_t10h<- felm(log(mat) ~ log(inc_tipo3_d1000_t10h) + log(inc_tipo3_d1000_t10h)*financiamiento | id_unico + year | 0 | id_unico, data=panel_primarias_log)
# PRINT 
stargazer (inter_finlogfematp_tipo3_d250_t90h,
           inter_finlogfematp_tipo3_d500_t90h,
           inter_finlogfematp_tipo3_d1000_t90h,
           inter_finlogfematp_tipo3_d250_t29h,
           inter_finlogfematp_tipo3_d500_t29h,
           inter_finlogfematp_tipo3_d1000_t29h,
           inter_finlogfematp_tipo3_d250_t10h,
           inter_finlogfematp_tipo3_d500_t10h,
           inter_finlogfematp_tipo3_d1000_t10h
           , digits=3
           , omit.stat=c ("f", "ser", "aic", "bic", "ll")
           , omit.table.layout="n"
           , no.space=TRUE
           , order = c(1,11,2,12,3,13,4,14,5,15,6,16,7,17,8,18,9,19,10)
           , type="text")

# TABLA 7. INTERACCIONES_FINANCIAMIENTO SECUNDARIAS LYC  -------------------------
# TIPO 1 D.A. PUBLICO 
inter_finlogfelycs_tipo1_d250_t90h <- felm(log(lyc) ~ log(inc_tipo1_d250_t90h) + log(inc_tipo1_d250_t90h)*financiamiento | id_unico + year | 0 | id_unico, data=panel_secundarias_log)
inter_finlogfelycs_tipo1_d500_t90h <- felm(log(lyc) ~ log(inc_tipo1_d500_t90h) + log(inc_tipo1_d500_t90h)*financiamiento | id_unico + year | 0 | id_unico, data=panel_secundarias_log)
inter_finlogfelycs_tipo1_d1000_t90h<- felm(log(lyc) ~ log(inc_tipo1_d1000_t90h) + log(inc_tipo1_d1000_t90h)*financiamiento | id_unico + year | 0 | id_unico, data=panel_secundarias_log)
inter_finlogfelycs_tipo1_d250_t29h <- felm(log(lyc) ~ log(inc_tipo1_d250_t29h) + log(inc_tipo1_d250_t29h)*financiamiento | id_unico + year | 0 | id_unico, data=panel_secundarias_log)
inter_finlogfelycs_tipo1_d500_t29h <- felm(log(lyc) ~ log(inc_tipo1_d500_t29h) + log(inc_tipo1_d500_t29h)*financiamiento | id_unico + year | 0 | id_unico, data=panel_secundarias_log)
inter_finlogfelycs_tipo1_d1000_t29h<- felm(log(lyc) ~ log(inc_tipo1_d1000_t29h) + log(inc_tipo1_d1000_t29h)*financiamiento | id_unico + year | 0 | id_unico, data=panel_secundarias_log)
inter_finlogfelycs_tipo1_d250_t10h <- felm(log(lyc) ~ log(inc_tipo1_d250_t10h) + log(inc_tipo1_d250_t10h)*financiamiento | id_unico + year | 0 | id_unico, data=panel_secundarias_log)
inter_finlogfelycs_tipo1_d500_t10h <- felm(log(lyc) ~ log(inc_tipo1_d500_t10h) + log(inc_tipo1_d500_t10h)*financiamiento | id_unico + year | 0 | id_unico, data=panel_secundarias_log)
inter_finlogfelycs_tipo1_d1000_t10h<- felm(log(lyc) ~ log(inc_tipo1_d1000_t10h) + log(inc_tipo1_d1000_t10h)*financiamiento | id_unico + year | 0 | id_unico, data=panel_secundarias_log)
# PRINT 
stargazer (inter_finlogfelycs_tipo1_d250_t90h,
           inter_finlogfelycs_tipo1_d500_t90h,
           inter_finlogfelycs_tipo1_d1000_t90h,
           inter_finlogfelycs_tipo1_d250_t29h,
           inter_finlogfelycs_tipo1_d500_t29h,
           inter_finlogfelycs_tipo1_d1000_t29h,
           inter_finlogfelycs_tipo1_d250_t10h,
           inter_finlogfelycs_tipo1_d500_t10h,
           inter_finlogfelycs_tipo1_d1000_t10h
           , digits=3
           , omit.stat=c ("f", "ser", "aic", "bic", "ll")
           , omit.table.layout="n"
           , no.space=TRUE
           , order = c(1,11,2,12,3,13,4,14,5,15,6,16,7,17,8,18,9,19,10)
           , type="text")

## TIPO 2 D.A. PRIVADO
inter_finlogfelycs_tipo2_d250_t90h <- felm(log(lyc) ~ log(inc_tipo2_d250_t90h) + log(inc_tipo2_d250_t90h)*financiamiento | id_unico + year | 0 | id_unico, data=panel_secundarias_log)
inter_finlogfelycs_tipo2_d500_t90h <- felm(log(lyc) ~ log(inc_tipo2_d500_t90h) + log(inc_tipo2_d500_t90h)*financiamiento | id_unico + year | 0 | id_unico, data=panel_secundarias_log)
inter_finlogfelycs_tipo2_d1000_t90h<- felm(log(lyc) ~ log(inc_tipo2_d1000_t90h) + log(inc_tipo2_d1000_t90h)*financiamiento | id_unico + year | 0 | id_unico, data=panel_secundarias_log)
inter_finlogfelycs_tipo2_d250_t29h <- felm(log(lyc) ~ log(inc_tipo2_d250_t29h) + log(inc_tipo2_d250_t29h)*financiamiento | id_unico + year | 0 | id_unico, data=panel_secundarias_log)
inter_finlogfelycs_tipo2_d500_t29h <- felm(log(lyc) ~ log(inc_tipo2_d500_t29h) + log(inc_tipo2_d500_t29h)*financiamiento | id_unico + year | 0 | id_unico, data=panel_secundarias_log)
inter_finlogfelycs_tipo2_d1000_t29h<- felm(log(lyc) ~ log(inc_tipo2_d1000_t29h) + log(inc_tipo2_d1000_t29h)*financiamiento | id_unico + year | 0 | id_unico, data=panel_secundarias_log)
inter_finlogfelycs_tipo2_d250_t10h <- felm(log(lyc) ~ log(inc_tipo2_d250_t10h) + log(inc_tipo2_d250_t10h)*financiamiento | id_unico + year | 0 | id_unico, data=panel_secundarias_log)
inter_finlogfelycs_tipo2_d500_t10h <- felm(log(lyc) ~ log(inc_tipo2_d500_t10h) + log(inc_tipo2_d500_t10h)*financiamiento | id_unico + year | 0 | id_unico, data=panel_secundarias_log)
inter_finlogfelycs_tipo2_d1000_t10h<- felm(log(lyc) ~ log(inc_tipo2_d1000_t10h) + log(inc_tipo2_d1000_t10h)*financiamiento | id_unico + year | 0 | id_unico, data=panel_secundarias_log)
# PRINT 
stargazer (inter_finlogfelycs_tipo2_d250_t90h,
           inter_finlogfelycs_tipo2_d500_t90h,
           inter_finlogfelycs_tipo2_d1000_t90h,
           inter_finlogfelycs_tipo2_d250_t29h,
           inter_finlogfelycs_tipo2_d500_t29h,
           inter_finlogfelycs_tipo2_d1000_t29h,
           inter_finlogfelycs_tipo2_d250_t10h,
           inter_finlogfelycs_tipo2_d500_t10h,
           inter_finlogfelycs_tipo2_d1000_t10h
           , digits=3
           , omit.stat=c ("f", "ser", "aic", "bic", "ll")
           , omit.table.layout="n"
           , no.space=TRUE
           , order = c(1,11,2,12,3,13,4,14,5,15,6,16,7,17,8,18,9,19,10)
           , type="text")

# TIPO 3 D.V. HOMICIDIOS
inter_finlogfelycs_tipo3_d250_t90h <- felm(log(lyc) ~ log(inc_tipo3_d250_t90h) + log(inc_tipo3_d250_t90h)*financiamiento | id_unico + year | 0 | id_unico, data=panel_secundarias_log)
inter_finlogfelycs_tipo3_d500_t90h <- felm(log(lyc) ~ log(inc_tipo3_d500_t90h) + log(inc_tipo3_d500_t90h)*financiamiento | id_unico + year | 0 | id_unico, data=panel_secundarias_log)
inter_finlogfelycs_tipo3_d1000_t90h<- felm(log(lyc) ~ log(inc_tipo3_d1000_t90h) + log(inc_tipo3_d1000_t90h)*financiamiento | id_unico + year | 0 | id_unico, data=panel_secundarias_log)
inter_finlogfelycs_tipo3_d250_t29h <- felm(log(lyc) ~ log(inc_tipo3_d250_t29h) + log(inc_tipo3_d250_t29h)*financiamiento | id_unico + year | 0 | id_unico, data=panel_secundarias_log)
inter_finlogfelycs_tipo3_d500_t29h <- felm(log(lyc) ~ log(inc_tipo3_d500_t29h) + log(inc_tipo3_d500_t29h)*financiamiento | id_unico + year | 0 | id_unico, data=panel_secundarias_log)
inter_finlogfelycs_tipo3_d1000_t29h<- felm(log(lyc) ~ log(inc_tipo3_d1000_t29h) + log(inc_tipo3_d1000_t29h)*financiamiento | id_unico + year | 0 | id_unico, data=panel_secundarias_log)
inter_finlogfelycs_tipo3_d250_t10h <- felm(log(lyc) ~ log(inc_tipo3_d250_t10h) + log(inc_tipo3_d250_t10h)*financiamiento | id_unico + year | 0 | id_unico, data=panel_secundarias_log)
inter_finlogfelycs_tipo3_d500_t10h <- felm(log(lyc) ~ log(inc_tipo3_d500_t10h) + log(inc_tipo3_d500_t10h)*financiamiento | id_unico + year | 0 | id_unico, data=panel_secundarias_log)
inter_finlogfelycs_tipo3_d1000_t10h<- felm(log(lyc) ~ log(inc_tipo3_d1000_t10h) + log(inc_tipo3_d1000_t10h)*financiamiento | id_unico + year | 0 | id_unico, data=panel_secundarias_log)
# PRINT 
stargazer (inter_finlogfelycs_tipo3_d250_t90h,
           inter_finlogfelycs_tipo3_d500_t90h,
           inter_finlogfelycs_tipo3_d1000_t90h,
           inter_finlogfelycs_tipo3_d250_t29h,
           inter_finlogfelycs_tipo3_d500_t29h,
           inter_finlogfelycs_tipo3_d1000_t29h,
           inter_finlogfelycs_tipo3_d250_t10h,
           inter_finlogfelycs_tipo3_d500_t10h,
           inter_finlogfelycs_tipo3_d1000_t10h
           , digits=3
           , omit.stat=c ("f", "ser", "aic", "bic", "ll")
           , omit.table.layout="n"
           , no.space=TRUE
           , order = c(1,11,2,12,3,13,4,14,5,15,6,16,7,17,8,18,9,19,10)
           , type="text")

# TABLA 8. INTERACCIONES_FINANCIAMIENTO SECUNDARIAS MAT  -------------------------
# TIPO 1 D.A. PUBLICO 
inter_finlogfemats_tipo1_d250_t90h <- felm(log(mat) ~ log(inc_tipo1_d250_t90h) + log(inc_tipo1_d250_t90h)*financiamiento | id_unico + year | 0 | id_unico, data=panel_secundarias_log)
inter_finlogfemats_tipo1_d500_t90h <- felm(log(mat) ~ log(inc_tipo1_d500_t90h) + log(inc_tipo1_d500_t90h)*financiamiento | id_unico + year | 0 | id_unico, data=panel_secundarias_log)
inter_finlogfemats_tipo1_d1000_t90h<- felm(log(mat) ~ log(inc_tipo1_d1000_t90h) + log(inc_tipo1_d1000_t90h)*financiamiento | id_unico + year | 0 | id_unico, data=panel_secundarias_log)
inter_finlogfemats_tipo1_d250_t29h <- felm(log(mat) ~ log(inc_tipo1_d250_t29h) + log(inc_tipo1_d250_t29h)*financiamiento | id_unico + year | 0 | id_unico, data=panel_secundarias_log)
inter_finlogfemats_tipo1_d500_t29h <- felm(log(mat) ~ log(inc_tipo1_d500_t29h) + log(inc_tipo1_d500_t29h)*financiamiento | id_unico + year | 0 | id_unico, data=panel_secundarias_log)
inter_finlogfemats_tipo1_d1000_t29h<- felm(log(mat) ~ log(inc_tipo1_d1000_t29h) + log(inc_tipo1_d1000_t29h)*financiamiento | id_unico + year | 0 | id_unico, data=panel_secundarias_log)
inter_finlogfemats_tipo1_d250_t10h <- felm(log(mat) ~ log(inc_tipo1_d250_t10h) + log(inc_tipo1_d250_t10h)*financiamiento | id_unico + year | 0 | id_unico, data=panel_secundarias_log)
inter_finlogfemats_tipo1_d500_t10h <- felm(log(mat) ~ log(inc_tipo1_d500_t10h) + log(inc_tipo1_d500_t10h)*financiamiento | id_unico + year | 0 | id_unico, data=panel_secundarias_log)
inter_finlogfemats_tipo1_d1000_t10h<- felm(log(mat) ~ log(inc_tipo1_d1000_t10h) + log(inc_tipo1_d1000_t10h)*financiamiento | id_unico + year | 0 | id_unico, data=panel_secundarias_log)
# PRINT 
stargazer (inter_finlogfemats_tipo1_d250_t90h,
           inter_finlogfemats_tipo1_d500_t90h,
           inter_finlogfemats_tipo1_d1000_t90h,
           inter_finlogfemats_tipo1_d250_t29h,
           inter_finlogfemats_tipo1_d500_t29h,
           inter_finlogfemats_tipo1_d1000_t29h,
           inter_finlogfemats_tipo1_d250_t10h,
           inter_finlogfemats_tipo1_d500_t10h,
           inter_finlogfemats_tipo1_d1000_t10h
           , digits=3
           , omit.stat=c ("f", "ser", "aic", "bic", "ll")
           , omit.table.layout="n"
           , no.space=TRUE
           , order = c(1,11,2,12,3,13,4,14,5,15,6,16,7,17,8,18,9,19,10)
           , type="text")

## TIPO 2 D.A. PRIVADO
inter_finlogfemats_tipo2_d250_t90h <- felm(log(mat) ~ log(inc_tipo2_d250_t90h) + log(inc_tipo2_d250_t90h)*financiamiento | id_unico + year | 0 | id_unico, data=panel_secundarias_log)
inter_finlogfemats_tipo2_d500_t90h <- felm(log(mat) ~ log(inc_tipo2_d500_t90h) + log(inc_tipo2_d500_t90h)*financiamiento | id_unico + year | 0 | id_unico, data=panel_secundarias_log)
inter_finlogfemats_tipo2_d1000_t90h<- felm(log(mat) ~ log(inc_tipo2_d1000_t90h) + log(inc_tipo2_d1000_t90h)*financiamiento | id_unico + year | 0 | id_unico, data=panel_secundarias_log)
inter_finlogfemats_tipo2_d250_t29h <- felm(log(mat) ~ log(inc_tipo2_d250_t29h) + log(inc_tipo2_d250_t29h)*financiamiento | id_unico + year | 0 | id_unico, data=panel_secundarias_log)
inter_finlogfemats_tipo2_d500_t29h <- felm(log(mat) ~ log(inc_tipo2_d500_t29h) + log(inc_tipo2_d500_t29h)*financiamiento | id_unico + year | 0 | id_unico, data=panel_secundarias_log)
inter_finlogfemats_tipo2_d1000_t29h<- felm(log(mat) ~ log(inc_tipo2_d1000_t29h) + log(inc_tipo2_d1000_t29h)*financiamiento | id_unico + year | 0 | id_unico, data=panel_secundarias_log)
inter_finlogfemats_tipo2_d250_t10h <- felm(log(mat) ~ log(inc_tipo2_d250_t10h) + log(inc_tipo2_d250_t10h)*financiamiento | id_unico + year | 0 | id_unico, data=panel_secundarias_log)
inter_finlogfemats_tipo2_d500_t10h <- felm(log(mat) ~ log(inc_tipo2_d500_t10h) + log(inc_tipo2_d500_t10h)*financiamiento | id_unico + year | 0 | id_unico, data=panel_secundarias_log)
inter_finlogfemats_tipo2_d1000_t10h<- felm(log(mat) ~ log(inc_tipo2_d1000_t10h) + log(inc_tipo2_d1000_t10h)*financiamiento | id_unico + year | 0 | id_unico, data=panel_secundarias_log)
# PRINT 
stargazer (inter_finlogfemats_tipo2_d250_t90h,
           inter_finlogfemats_tipo2_d500_t90h,
           inter_finlogfemats_tipo2_d1000_t90h,
           inter_finlogfemats_tipo2_d250_t29h,
           inter_finlogfemats_tipo2_d500_t29h,
           inter_finlogfemats_tipo2_d1000_t29h,
           inter_finlogfemats_tipo2_d250_t10h,
           inter_finlogfemats_tipo2_d500_t10h,
           inter_finlogfemats_tipo2_d1000_t10h
           , digits=3
           , omit.stat=c ("f", "ser", "aic", "bic", "ll")
           , omit.table.layout="n"
           , no.space=TRUE
           , order = c(1,11,2,12,3,13,4,14,5,15,6,16,7,17,8,18,9,19,10)
           , type="text")

# TIPO 3 D.V. HOMICIDIOS
inter_finlogfemats_tipo3_d250_t90h <- felm(log(mat) ~ log(inc_tipo3_d250_t90h) + log(inc_tipo3_d250_t90h)*financiamiento | id_unico + year | 0 | id_unico, data=panel_secundarias_log)
inter_finlogfemats_tipo3_d500_t90h <- felm(log(mat) ~ log(inc_tipo3_d500_t90h) + log(inc_tipo3_d500_t90h)*financiamiento | id_unico + year | 0 | id_unico, data=panel_secundarias_log)
inter_finlogfemats_tipo3_d1000_t90h<- felm(log(mat) ~ log(inc_tipo3_d1000_t90h) + log(inc_tipo3_d1000_t90h)*financiamiento | id_unico + year | 0 | id_unico, data=panel_secundarias_log)
inter_finlogfemats_tipo3_d250_t29h <- felm(log(mat) ~ log(inc_tipo3_d250_t29h) + log(inc_tipo3_d250_t29h)*financiamiento | id_unico + year | 0 | id_unico, data=panel_secundarias_log)
inter_finlogfemats_tipo3_d500_t29h <- felm(log(mat) ~ log(inc_tipo3_d500_t29h) + log(inc_tipo3_d500_t29h)*financiamiento | id_unico + year | 0 | id_unico, data=panel_secundarias_log)
inter_finlogfemats_tipo3_d1000_t29h<- felm(log(mat) ~ log(inc_tipo3_d1000_t29h) + log(inc_tipo3_d1000_t29h)*financiamiento | id_unico + year | 0 | id_unico, data=panel_secundarias_log)
inter_finlogfemats_tipo3_d250_t10h <- felm(log(mat) ~ log(inc_tipo3_d250_t10h) + log(inc_tipo3_d250_t10h)*financiamiento | id_unico + year | 0 | id_unico, data=panel_secundarias_log)
inter_finlogfemats_tipo3_d500_t10h <- felm(log(mat) ~ log(inc_tipo3_d500_t10h) + log(inc_tipo3_d500_t10h)*financiamiento | id_unico + year | 0 | id_unico, data=panel_secundarias_log)
inter_finlogfemats_tipo3_d1000_t10h<- felm(log(mat) ~ log(inc_tipo3_d1000_t10h) + log(inc_tipo3_d1000_t10h)*financiamiento | id_unico + year | 0 | id_unico, data=panel_secundarias_log)
# PRINT 
stargazer (inter_finlogfemats_tipo3_d250_t90h,
           inter_finlogfemats_tipo3_d500_t90h,
           inter_finlogfemats_tipo3_d1000_t90h,
           inter_finlogfemats_tipo3_d250_t29h,
           inter_finlogfemats_tipo3_d500_t29h,
           inter_finlogfemats_tipo3_d1000_t29h,
           inter_finlogfemats_tipo3_d250_t10h,
           inter_finlogfemats_tipo3_d500_t10h,
           inter_finlogfemats_tipo3_d1000_t10h
           , digits=3
           , omit.stat=c ("f", "ser", "aic", "bic", "ll")
           , omit.table.layout="n"
           , no.space=TRUE
           , order = c(1,11,2,12,3,13,4,14,5,15,6,16,7,17,8,18,9,19,10)
           , type="text")

# TABLA 9. INTERACCIONES_TURNO PRIMARIAS LYC  -------------------------
# TODAS EN SUBCONJUNTO DE ESCUELAS PUBLICAS
# TIPO 1 D.A. PUBLICO 
inter_turlogfelycp_tipo1_d250_t90h <- felm(log(lyc) ~ log(inc_tipo1_d250_t90h) + log(inc_tipo1_d250_t90h)*clave_turno | id_unico + year | 0 | id_unico, data=filter (panel_primarias_log, financiamiento=="PUBLICO"))
inter_turlogfelycp_tipo1_d500_t90h <- felm(log(lyc) ~ log(inc_tipo1_d500_t90h) + log(inc_tipo1_d500_t90h)*clave_turno | id_unico + year | 0 | id_unico, data=filter (panel_primarias_log, financiamiento=="PUBLICO"))
inter_turlogfelycp_tipo1_d1000_t90h<- felm(log(lyc) ~ log(inc_tipo1_d1000_t90h) + log(inc_tipo1_d1000_t90h)*clave_turno | id_unico + year | 0 | id_unico, data=filter (panel_primarias_log, financiamiento=="PUBLICO"))
inter_turlogfelycp_tipo1_d250_t29h <- felm(log(lyc) ~ log(inc_tipo1_d250_t29h) + log(inc_tipo1_d250_t29h)*clave_turno | id_unico + year | 0 | id_unico, data=filter (panel_primarias_log, financiamiento=="PUBLICO"))
inter_turlogfelycp_tipo1_d500_t29h <- felm(log(lyc) ~ log(inc_tipo1_d500_t29h) + log(inc_tipo1_d500_t29h)*clave_turno | id_unico + year | 0 | id_unico, data=filter (panel_primarias_log, financiamiento=="PUBLICO"))
inter_turlogfelycp_tipo1_d1000_t29h<- felm(log(lyc) ~ log(inc_tipo1_d1000_t29h) + log(inc_tipo1_d1000_t29h)*clave_turno | id_unico + year | 0 | id_unico, data=filter (panel_primarias_log, financiamiento=="PUBLICO"))
inter_turlogfelycp_tipo1_d250_t10h <- felm(log(lyc) ~ log(inc_tipo1_d250_t10h) + log(inc_tipo1_d250_t10h)*clave_turno | id_unico + year | 0 | id_unico, data=filter (panel_primarias_log, financiamiento=="PUBLICO"))
inter_turlogfelycp_tipo1_d500_t10h <- felm(log(lyc) ~ log(inc_tipo1_d500_t10h) + log(inc_tipo1_d500_t10h)*clave_turno | id_unico + year | 0 | id_unico, data=filter (panel_primarias_log, financiamiento=="PUBLICO"))
inter_turlogfelycp_tipo1_d1000_t10h<- felm(log(lyc) ~ log(inc_tipo1_d1000_t10h) + log(inc_tipo1_d1000_t10h)*clave_turno | id_unico + year | 0 | id_unico, data=filter (panel_primarias_log, financiamiento=="PUBLICO"))
# PRINT 
stargazer (inter_turlogfelycp_tipo1_d250_t90h,
           inter_turlogfelycp_tipo1_d500_t90h,
           inter_turlogfelycp_tipo1_d1000_t90h,
           inter_turlogfelycp_tipo1_d250_t29h,
           inter_turlogfelycp_tipo1_d500_t29h,
           inter_turlogfelycp_tipo1_d1000_t29h,
           inter_turlogfelycp_tipo1_d250_t10h,
           inter_turlogfelycp_tipo1_d500_t10h,
           inter_turlogfelycp_tipo1_d1000_t10h
           , digits=3
           , omit.stat=c ("f", "ser", "aic", "bic", "ll")
           , omit.table.layout="n"
           , no.space=TRUE
           , order = c(1,11,2,12,3,13,4,14,5,15,6,16,7,17,8,18,9,19,10)
           , type="text")

## TIPO 2 D.A. PRIVADO
inter_turlogfelycp_tipo2_d250_t90h <- felm(log(lyc) ~ log(inc_tipo2_d250_t90h) + log(inc_tipo2_d250_t90h)*clave_turno | id_unico + year | 0 | id_unico, data=filter (panel_primarias_log, financiamiento=="PUBLICO"))
inter_turlogfelycp_tipo2_d500_t90h <- felm(log(lyc) ~ log(inc_tipo2_d500_t90h) + log(inc_tipo2_d500_t90h)*clave_turno | id_unico + year | 0 | id_unico, data=filter (panel_primarias_log, financiamiento=="PUBLICO"))
inter_turlogfelycp_tipo2_d1000_t90h<- felm(log(lyc) ~ log(inc_tipo2_d1000_t90h) + log(inc_tipo2_d1000_t90h)*clave_turno | id_unico + year | 0 | id_unico, data=filter (panel_primarias_log, financiamiento=="PUBLICO"))
inter_turlogfelycp_tipo2_d250_t29h <- felm(log(lyc) ~ log(inc_tipo2_d250_t29h) + log(inc_tipo2_d250_t29h)*clave_turno | id_unico + year | 0 | id_unico, data=filter (panel_primarias_log, financiamiento=="PUBLICO"))
inter_turlogfelycp_tipo2_d500_t29h <- felm(log(lyc) ~ log(inc_tipo2_d500_t29h) + log(inc_tipo2_d500_t29h)*clave_turno | id_unico + year | 0 | id_unico, data=filter (panel_primarias_log, financiamiento=="PUBLICO"))
inter_turlogfelycp_tipo2_d1000_t29h<- felm(log(lyc) ~ log(inc_tipo2_d1000_t29h) + log(inc_tipo2_d1000_t29h)*clave_turno | id_unico + year | 0 | id_unico, data=filter (panel_primarias_log, financiamiento=="PUBLICO"))
inter_turlogfelycp_tipo2_d250_t10h <- felm(log(lyc) ~ log(inc_tipo2_d250_t10h) + log(inc_tipo2_d250_t10h)*clave_turno | id_unico + year | 0 | id_unico, data=filter (panel_primarias_log, financiamiento=="PUBLICO"))
inter_turlogfelycp_tipo2_d500_t10h <- felm(log(lyc) ~ log(inc_tipo2_d500_t10h) + log(inc_tipo2_d500_t10h)*clave_turno | id_unico + year | 0 | id_unico, data=filter (panel_primarias_log, financiamiento=="PUBLICO"))
inter_turlogfelycp_tipo2_d1000_t10h<- felm(log(lyc) ~ log(inc_tipo2_d1000_t10h) + log(inc_tipo2_d1000_t10h)*clave_turno | id_unico + year | 0 | id_unico, data=filter (panel_primarias_log, financiamiento=="PUBLICO"))
# PRINT 
stargazer (inter_turlogfelycp_tipo2_d250_t90h,
           inter_turlogfelycp_tipo2_d500_t90h,
           inter_turlogfelycp_tipo2_d1000_t90h,
           inter_turlogfelycp_tipo2_d250_t29h,
           inter_turlogfelycp_tipo2_d500_t29h,
           inter_turlogfelycp_tipo2_d1000_t29h,
           inter_turlogfelycp_tipo2_d250_t10h,
           inter_turlogfelycp_tipo2_d500_t10h,
           inter_turlogfelycp_tipo2_d1000_t10h
           , digits=3
           , omit.stat=c ("f", "ser", "aic", "bic", "ll")
           , omit.table.layout="n"
           , no.space=TRUE
           , order = c(1,11,2,12,3,13,4,14,5,15,6,16,7,17,8,18,9,19,10)
           , type="text")

# TIPO 3 D.V. HOMICIDIOS
inter_turlogfelycp_tipo3_d250_t90h <- felm(log(lyc) ~ log(inc_tipo3_d250_t90h) + log(inc_tipo3_d250_t90h)*clave_turno | id_unico + year | 0 | id_unico, data=filter (panel_primarias_log, financiamiento=="PUBLICO"))
inter_turlogfelycp_tipo3_d500_t90h <- felm(log(lyc) ~ log(inc_tipo3_d500_t90h) + log(inc_tipo3_d500_t90h)*clave_turno | id_unico + year | 0 | id_unico, data=filter (panel_primarias_log, financiamiento=="PUBLICO"))
inter_turlogfelycp_tipo3_d1000_t90h<- felm(log(lyc) ~ log(inc_tipo3_d1000_t90h) + log(inc_tipo3_d1000_t90h)*clave_turno | id_unico + year | 0 | id_unico, data=filter (panel_primarias_log, financiamiento=="PUBLICO"))
inter_turlogfelycp_tipo3_d250_t29h <- felm(log(lyc) ~ log(inc_tipo3_d250_t29h) + log(inc_tipo3_d250_t29h)*clave_turno | id_unico + year | 0 | id_unico, data=filter (panel_primarias_log, financiamiento=="PUBLICO"))
inter_turlogfelycp_tipo3_d500_t29h <- felm(log(lyc) ~ log(inc_tipo3_d500_t29h) + log(inc_tipo3_d500_t29h)*clave_turno | id_unico + year | 0 | id_unico, data=filter (panel_primarias_log, financiamiento=="PUBLICO"))
inter_turlogfelycp_tipo3_d1000_t29h<- felm(log(lyc) ~ log(inc_tipo3_d1000_t29h) + log(inc_tipo3_d1000_t29h)*clave_turno | id_unico + year | 0 | id_unico, data=filter (panel_primarias_log, financiamiento=="PUBLICO"))
inter_turlogfelycp_tipo3_d250_t10h <- felm(log(lyc) ~ log(inc_tipo3_d250_t10h) + log(inc_tipo3_d250_t10h)*clave_turno | id_unico + year | 0 | id_unico, data=filter (panel_primarias_log, financiamiento=="PUBLICO"))
inter_turlogfelycp_tipo3_d500_t10h <- felm(log(lyc) ~ log(inc_tipo3_d500_t10h) + log(inc_tipo3_d500_t10h)*clave_turno | id_unico + year | 0 | id_unico, data=filter (panel_primarias_log, financiamiento=="PUBLICO"))
inter_turlogfelycp_tipo3_d1000_t10h<- felm(log(lyc) ~ log(inc_tipo3_d1000_t10h) + log(inc_tipo3_d1000_t10h)*clave_turno | id_unico + year | 0 | id_unico, data=filter (panel_primarias_log, financiamiento=="PUBLICO"))
# PRINT 
stargazer (inter_turlogfelycp_tipo3_d250_t90h,
           inter_turlogfelycp_tipo3_d500_t90h,
           inter_turlogfelycp_tipo3_d1000_t90h,
           inter_turlogfelycp_tipo3_d250_t29h,
           inter_turlogfelycp_tipo3_d500_t29h,
           inter_turlogfelycp_tipo3_d1000_t29h,
           inter_turlogfelycp_tipo3_d250_t10h,
           inter_turlogfelycp_tipo3_d500_t10h,
           inter_turlogfelycp_tipo3_d1000_t10h
           , digits=3
           , omit.stat=c ("f", "ser", "aic", "bic", "ll")
           , omit.table.layout="n"
           , no.space=TRUE
           , order = c(1,11,2,12,3,13,4,14,5,15,6,16,7,17,8,18,9,19,10)
           , type="text")

# TABLA 10. INTERACCIONES_TURNO PRIMARIAS MAT  -------------------------
# TODAS EN SUBCONJUNTO DE ESCUELAS PUBLICAS
# TIPO 1 D.A. PUBLICO 
inter_turlogfematp_tipo1_d250_t90h <- felm(log(mat) ~ log(inc_tipo1_d250_t90h) + log(inc_tipo1_d250_t90h)*clave_turno | id_unico + year | 0 | id_unico, data=filter (panel_primarias_log, financiamiento=="PUBLICO"))
inter_turlogfematp_tipo1_d500_t90h <- felm(log(mat) ~ log(inc_tipo1_d500_t90h) + log(inc_tipo1_d500_t90h)*clave_turno | id_unico + year | 0 | id_unico, data=filter (panel_primarias_log, financiamiento=="PUBLICO"))
inter_turlogfematp_tipo1_d1000_t90h<- felm(log(mat) ~ log(inc_tipo1_d1000_t90h) + log(inc_tipo1_d1000_t90h)*clave_turno | id_unico + year | 0 | id_unico, data=filter (panel_primarias_log, financiamiento=="PUBLICO"))
inter_turlogfematp_tipo1_d250_t29h <- felm(log(mat) ~ log(inc_tipo1_d250_t29h) + log(inc_tipo1_d250_t29h)*clave_turno | id_unico + year | 0 | id_unico, data=filter (panel_primarias_log, financiamiento=="PUBLICO"))
inter_turlogfematp_tipo1_d500_t29h <- felm(log(mat) ~ log(inc_tipo1_d500_t29h) + log(inc_tipo1_d500_t29h)*clave_turno | id_unico + year | 0 | id_unico, data=filter (panel_primarias_log, financiamiento=="PUBLICO"))
inter_turlogfematp_tipo1_d1000_t29h<- felm(log(mat) ~ log(inc_tipo1_d1000_t29h) + log(inc_tipo1_d1000_t29h)*clave_turno | id_unico + year | 0 | id_unico, data=filter (panel_primarias_log, financiamiento=="PUBLICO"))
inter_turlogfematp_tipo1_d250_t10h <- felm(log(mat) ~ log(inc_tipo1_d250_t10h) + log(inc_tipo1_d250_t10h)*clave_turno | id_unico + year | 0 | id_unico, data=filter (panel_primarias_log, financiamiento=="PUBLICO"))
inter_turlogfematp_tipo1_d500_t10h <- felm(log(mat) ~ log(inc_tipo1_d500_t10h) + log(inc_tipo1_d500_t10h)*clave_turno | id_unico + year | 0 | id_unico, data=filter (panel_primarias_log, financiamiento=="PUBLICO"))
inter_turlogfematp_tipo1_d1000_t10h<- felm(log(mat) ~ log(inc_tipo1_d1000_t10h) + log(inc_tipo1_d1000_t10h)*clave_turno | id_unico + year | 0 | id_unico, data=filter (panel_primarias_log, financiamiento=="PUBLICO"))
# PRINT 
stargazer (inter_turlogfematp_tipo1_d250_t90h,
           inter_turlogfematp_tipo1_d500_t90h,
           inter_turlogfematp_tipo1_d1000_t90h,
           inter_turlogfematp_tipo1_d250_t29h,
           inter_turlogfematp_tipo1_d500_t29h,
           inter_turlogfematp_tipo1_d1000_t29h,
           inter_turlogfematp_tipo1_d250_t10h,
           inter_turlogfematp_tipo1_d500_t10h,
           inter_turlogfematp_tipo1_d1000_t10h
           , digits=3
           , omit.stat=c ("f", "ser", "aic", "bic", "ll")
           , omit.table.layout="n"
           , no.space=TRUE
           , order = c(1,11,2,12,3,13,4,14,5,15,6,16,7,17,8,18,9,19,10)
           , type="text")

## TIPO 2 D.A. PRIVADO
inter_turlogfematp_tipo2_d250_t90h <- felm(log(mat) ~ log(inc_tipo2_d250_t90h) + log(inc_tipo2_d250_t90h)*clave_turno | id_unico + year | 0 | id_unico, data=filter (panel_primarias_log, financiamiento=="PUBLICO"))
inter_turlogfematp_tipo2_d500_t90h <- felm(log(mat) ~ log(inc_tipo2_d500_t90h) + log(inc_tipo2_d500_t90h)*clave_turno | id_unico + year | 0 | id_unico, data=filter (panel_primarias_log, financiamiento=="PUBLICO"))
inter_turlogfematp_tipo2_d1000_t90h<- felm(log(mat) ~ log(inc_tipo2_d1000_t90h) + log(inc_tipo2_d1000_t90h)*clave_turno | id_unico + year | 0 | id_unico, data=filter (panel_primarias_log, financiamiento=="PUBLICO"))
inter_turlogfematp_tipo2_d250_t29h <- felm(log(mat) ~ log(inc_tipo2_d250_t29h) + log(inc_tipo2_d250_t29h)*clave_turno | id_unico + year | 0 | id_unico, data=filter (panel_primarias_log, financiamiento=="PUBLICO"))
inter_turlogfematp_tipo2_d500_t29h <- felm(log(mat) ~ log(inc_tipo2_d500_t29h) + log(inc_tipo2_d500_t29h)*clave_turno | id_unico + year | 0 | id_unico, data=filter (panel_primarias_log, financiamiento=="PUBLICO"))
inter_turlogfematp_tipo2_d1000_t29h<- felm(log(mat) ~ log(inc_tipo2_d1000_t29h) + log(inc_tipo2_d1000_t29h)*clave_turno | id_unico + year | 0 | id_unico, data=filter (panel_primarias_log, financiamiento=="PUBLICO"))
inter_turlogfematp_tipo2_d250_t10h <- felm(log(mat) ~ log(inc_tipo2_d250_t10h) + log(inc_tipo2_d250_t10h)*clave_turno | id_unico + year | 0 | id_unico, data=filter (panel_primarias_log, financiamiento=="PUBLICO"))
inter_turlogfematp_tipo2_d500_t10h <- felm(log(mat) ~ log(inc_tipo2_d500_t10h) + log(inc_tipo2_d500_t10h)*clave_turno | id_unico + year | 0 | id_unico, data=filter (panel_primarias_log, financiamiento=="PUBLICO"))
inter_turlogfematp_tipo2_d1000_t10h<- felm(log(mat) ~ log(inc_tipo2_d1000_t10h) + log(inc_tipo2_d1000_t10h)*clave_turno | id_unico + year | 0 | id_unico, data=filter (panel_primarias_log, financiamiento=="PUBLICO"))
# PRINT 
stargazer (inter_turlogfematp_tipo2_d250_t90h,
           inter_turlogfematp_tipo2_d500_t90h,
           inter_turlogfematp_tipo2_d1000_t90h,
           inter_turlogfematp_tipo2_d250_t29h,
           inter_turlogfematp_tipo2_d500_t29h,
           inter_turlogfematp_tipo2_d1000_t29h,
           inter_turlogfematp_tipo2_d250_t10h,
           inter_turlogfematp_tipo2_d500_t10h,
           inter_turlogfematp_tipo2_d1000_t10h
           , digits=3
           , omit.stat=c ("f", "ser", "aic", "bic", "ll")
           , omit.table.layout="n"
           , no.space=TRUE
           , order = c(1,11,2,12,3,13,4,14,5,15,6,16,7,17,8,18,9,19,10)
           , type="text")

# TIPO 3 D.V. HOMICIDIOS
inter_turlogfematp_tipo3_d250_t90h <- felm(log(mat) ~ log(inc_tipo3_d250_t90h) + log(inc_tipo3_d250_t90h)*clave_turno | id_unico + year | 0 | id_unico, data=filter (panel_primarias_log, financiamiento=="PUBLICO"))
inter_turlogfematp_tipo3_d500_t90h <- felm(log(mat) ~ log(inc_tipo3_d500_t90h) + log(inc_tipo3_d500_t90h)*clave_turno | id_unico + year | 0 | id_unico, data=filter (panel_primarias_log, financiamiento=="PUBLICO"))
inter_turlogfematp_tipo3_d1000_t90h<- felm(log(mat) ~ log(inc_tipo3_d1000_t90h) + log(inc_tipo3_d1000_t90h)*clave_turno | id_unico + year | 0 | id_unico, data=filter (panel_primarias_log, financiamiento=="PUBLICO"))
inter_turlogfematp_tipo3_d250_t29h <- felm(log(mat) ~ log(inc_tipo3_d250_t29h) + log(inc_tipo3_d250_t29h)*clave_turno | id_unico + year | 0 | id_unico, data=filter (panel_primarias_log, financiamiento=="PUBLICO"))
inter_turlogfematp_tipo3_d500_t29h <- felm(log(mat) ~ log(inc_tipo3_d500_t29h) + log(inc_tipo3_d500_t29h)*clave_turno | id_unico + year | 0 | id_unico, data=filter (panel_primarias_log, financiamiento=="PUBLICO"))
inter_turlogfematp_tipo3_d1000_t29h<- felm(log(mat) ~ log(inc_tipo3_d1000_t29h) + log(inc_tipo3_d1000_t29h)*clave_turno | id_unico + year | 0 | id_unico, data=filter (panel_primarias_log, financiamiento=="PUBLICO"))
inter_turlogfematp_tipo3_d250_t10h <- felm(log(mat) ~ log(inc_tipo3_d250_t10h) + log(inc_tipo3_d250_t10h)*clave_turno | id_unico + year | 0 | id_unico, data=filter (panel_primarias_log, financiamiento=="PUBLICO"))
inter_turlogfematp_tipo3_d500_t10h <- felm(log(mat) ~ log(inc_tipo3_d500_t10h) + log(inc_tipo3_d500_t10h)*clave_turno | id_unico + year | 0 | id_unico, data=filter (panel_primarias_log, financiamiento=="PUBLICO"))
inter_turlogfematp_tipo3_d1000_t10h<- felm(log(mat) ~ log(inc_tipo3_d1000_t10h) + log(inc_tipo3_d1000_t10h)*clave_turno | id_unico + year | 0 | id_unico, data=filter (panel_primarias_log, financiamiento=="PUBLICO"))
# PRINT 
stargazer (inter_turlogfematp_tipo3_d250_t90h,
           inter_turlogfematp_tipo3_d500_t90h,
           inter_turlogfematp_tipo3_d1000_t90h,
           inter_turlogfematp_tipo3_d250_t29h,
           inter_turlogfematp_tipo3_d500_t29h,
           inter_turlogfematp_tipo3_d1000_t29h,
           inter_turlogfematp_tipo3_d250_t10h,
           inter_turlogfematp_tipo3_d500_t10h,
           inter_turlogfematp_tipo3_d1000_t10h
           , digits=3
           , omit.stat=c ("f", "ser", "aic", "bic", "ll")
           , omit.table.layout="n"
           , no.space=TRUE
           , order = c(1,11,2,12,3,13,4,14,5,15,6,16,7,17,8,18,9,19,10)
           , type="text")

# TABLA 11. INTERACCIONES_TURNO SECUNDARIAS LYC  -------------------------
# TODAS EN SUBCONJUNTO DE ESCUELAS PUBLICAS
# TIPO 1 D.A. PUBLICO 
inter_turlogfelycs_tipo1_d250_t90h <- felm(log(lyc) ~ log(inc_tipo1_d250_t90h) + log(inc_tipo1_d250_t90h)*clave_turno | id_unico + year | 0 | id_unico, data=filter (panel_secundarias_log, financiamiento=="PUBLICO"))
inter_turlogfelycs_tipo1_d500_t90h <- felm(log(lyc) ~ log(inc_tipo1_d500_t90h) + log(inc_tipo1_d500_t90h)*clave_turno | id_unico + year | 0 | id_unico, data=filter (panel_secundarias_log, financiamiento=="PUBLICO"))
inter_turlogfelycs_tipo1_d1000_t90h<- felm(log(lyc) ~ log(inc_tipo1_d1000_t90h) + log(inc_tipo1_d1000_t90h)*clave_turno | id_unico + year | 0 | id_unico, data=filter (panel_secundarias_log, financiamiento=="PUBLICO"))
inter_turlogfelycs_tipo1_d250_t29h <- felm(log(lyc) ~ log(inc_tipo1_d250_t29h) + log(inc_tipo1_d250_t29h)*clave_turno | id_unico + year | 0 | id_unico, data=filter (panel_secundarias_log, financiamiento=="PUBLICO"))
inter_turlogfelycs_tipo1_d500_t29h <- felm(log(lyc) ~ log(inc_tipo1_d500_t29h) + log(inc_tipo1_d500_t29h)*clave_turno | id_unico + year | 0 | id_unico, data=filter (panel_secundarias_log, financiamiento=="PUBLICO"))
inter_turlogfelycs_tipo1_d1000_t29h<- felm(log(lyc) ~ log(inc_tipo1_d1000_t29h) + log(inc_tipo1_d1000_t29h)*clave_turno | id_unico + year | 0 | id_unico, data=filter (panel_secundarias_log, financiamiento=="PUBLICO"))
inter_turlogfelycs_tipo1_d250_t10h <- felm(log(lyc) ~ log(inc_tipo1_d250_t10h) + log(inc_tipo1_d250_t10h)*clave_turno | id_unico + year | 0 | id_unico, data=filter (panel_secundarias_log, financiamiento=="PUBLICO"))
inter_turlogfelycs_tipo1_d500_t10h <- felm(log(lyc) ~ log(inc_tipo1_d500_t10h) + log(inc_tipo1_d500_t10h)*clave_turno | id_unico + year | 0 | id_unico, data=filter (panel_secundarias_log, financiamiento=="PUBLICO"))
inter_turlogfelycs_tipo1_d1000_t10h<- felm(log(lyc) ~ log(inc_tipo1_d1000_t10h) + log(inc_tipo1_d1000_t10h)*clave_turno | id_unico + year | 0 | id_unico, data=filter (panel_secundarias_log, financiamiento=="PUBLICO"))
# PRINT 
stargazer (inter_turlogfelycs_tipo1_d250_t90h,
           inter_turlogfelycs_tipo1_d500_t90h,
           inter_turlogfelycs_tipo1_d1000_t90h,
           inter_turlogfelycs_tipo1_d250_t29h,
           inter_turlogfelycs_tipo1_d500_t29h,
           inter_turlogfelycs_tipo1_d1000_t29h,
           inter_turlogfelycs_tipo1_d250_t10h,
           inter_turlogfelycs_tipo1_d500_t10h,
           inter_turlogfelycs_tipo1_d1000_t10h
           , digits=3
           , omit.stat=c ("f", "ser", "aic", "bic", "ll")
           , omit.table.layout="n"
           , no.space=TRUE
           , order = c(1,11,2,12,3,13,4,14,5,15,6,16,7,17,8,18,9,19,10)
           , type="text")

## TIPO 2 D.A. PRIVADO
inter_turlogfelycs_tipo2_d250_t90h <- felm(log(lyc) ~ log(inc_tipo2_d250_t90h) + log(inc_tipo2_d250_t90h)*clave_turno | id_unico + year | 0 | id_unico, data=filter (panel_secundarias_log, financiamiento=="PUBLICO"))
inter_turlogfelycs_tipo2_d500_t90h <- felm(log(lyc) ~ log(inc_tipo2_d500_t90h) + log(inc_tipo2_d500_t90h)*clave_turno | id_unico + year | 0 | id_unico, data=filter (panel_secundarias_log, financiamiento=="PUBLICO"))
inter_turlogfelycs_tipo2_d1000_t90h<- felm(log(lyc) ~ log(inc_tipo2_d1000_t90h) + log(inc_tipo2_d1000_t90h)*clave_turno | id_unico + year | 0 | id_unico, data=filter (panel_secundarias_log, financiamiento=="PUBLICO"))
inter_turlogfelycs_tipo2_d250_t29h <- felm(log(lyc) ~ log(inc_tipo2_d250_t29h) + log(inc_tipo2_d250_t29h)*clave_turno | id_unico + year | 0 | id_unico, data=filter (panel_secundarias_log, financiamiento=="PUBLICO"))
inter_turlogfelycs_tipo2_d500_t29h <- felm(log(lyc) ~ log(inc_tipo2_d500_t29h) + log(inc_tipo2_d500_t29h)*clave_turno | id_unico + year | 0 | id_unico, data=filter (panel_secundarias_log, financiamiento=="PUBLICO"))
inter_turlogfelycs_tipo2_d1000_t29h<- felm(log(lyc) ~ log(inc_tipo2_d1000_t29h) + log(inc_tipo2_d1000_t29h)*clave_turno | id_unico + year | 0 | id_unico, data=filter (panel_secundarias_log, financiamiento=="PUBLICO"))
inter_turlogfelycs_tipo2_d250_t10h <- felm(log(lyc) ~ log(inc_tipo2_d250_t10h) + log(inc_tipo2_d250_t10h)*clave_turno | id_unico + year | 0 | id_unico, data=filter (panel_secundarias_log, financiamiento=="PUBLICO"))
inter_turlogfelycs_tipo2_d500_t10h <- felm(log(lyc) ~ log(inc_tipo2_d500_t10h) + log(inc_tipo2_d500_t10h)*clave_turno | id_unico + year | 0 | id_unico, data=filter (panel_secundarias_log, financiamiento=="PUBLICO"))
inter_turlogfelycs_tipo2_d1000_t10h<- felm(log(lyc) ~ log(inc_tipo2_d1000_t10h) + log(inc_tipo2_d1000_t10h)*clave_turno | id_unico + year | 0 | id_unico, data=filter (panel_secundarias_log, financiamiento=="PUBLICO"))
# PRINT 
stargazer (inter_turlogfelycs_tipo2_d250_t90h,
           inter_turlogfelycs_tipo2_d500_t90h,
           inter_turlogfelycs_tipo2_d1000_t90h,
           inter_turlogfelycs_tipo2_d250_t29h,
           inter_turlogfelycs_tipo2_d500_t29h,
           inter_turlogfelycs_tipo2_d1000_t29h,
           inter_turlogfelycs_tipo2_d250_t10h,
           inter_turlogfelycs_tipo2_d500_t10h,
           inter_turlogfelycs_tipo2_d1000_t10h
           , digits=3
           , omit.stat=c ("f", "ser", "aic", "bic", "ll")
           , omit.table.layout="n"
           , no.space=TRUE
           , order = c(1,11,2,12,3,13,4,14,5,15,6,16,7,17,8,18,9,19,10)
           , type="text")

# TIPO 3 D.V. HOMICIDIOS
inter_turlogfelycs_tipo3_d250_t90h <- felm(log(lyc) ~ log(inc_tipo3_d250_t90h) + log(inc_tipo3_d250_t90h)*clave_turno | id_unico + year | 0 | id_unico, data=filter (panel_secundarias_log, financiamiento=="PUBLICO"))
inter_turlogfelycs_tipo3_d500_t90h <- felm(log(lyc) ~ log(inc_tipo3_d500_t90h) + log(inc_tipo3_d500_t90h)*clave_turno | id_unico + year | 0 | id_unico, data=filter (panel_secundarias_log, financiamiento=="PUBLICO"))
inter_turlogfelycs_tipo3_d1000_t90h<- felm(log(lyc) ~ log(inc_tipo3_d1000_t90h) + log(inc_tipo3_d1000_t90h)*clave_turno | id_unico + year | 0 | id_unico, data=filter (panel_secundarias_log, financiamiento=="PUBLICO"))
inter_turlogfelycs_tipo3_d250_t29h <- felm(log(lyc) ~ log(inc_tipo3_d250_t29h) + log(inc_tipo3_d250_t29h)*clave_turno | id_unico + year | 0 | id_unico, data=filter (panel_secundarias_log, financiamiento=="PUBLICO"))
inter_turlogfelycs_tipo3_d500_t29h <- felm(log(lyc) ~ log(inc_tipo3_d500_t29h) + log(inc_tipo3_d500_t29h)*clave_turno | id_unico + year | 0 | id_unico, data=filter (panel_secundarias_log, financiamiento=="PUBLICO"))
inter_turlogfelycs_tipo3_d1000_t29h<- felm(log(lyc) ~ log(inc_tipo3_d1000_t29h) + log(inc_tipo3_d1000_t29h)*clave_turno | id_unico + year | 0 | id_unico, data=filter (panel_secundarias_log, financiamiento=="PUBLICO"))
inter_turlogfelycs_tipo3_d250_t10h <- felm(log(lyc) ~ log(inc_tipo3_d250_t10h) + log(inc_tipo3_d250_t10h)*clave_turno | id_unico + year | 0 | id_unico, data=filter (panel_secundarias_log, financiamiento=="PUBLICO"))
inter_turlogfelycs_tipo3_d500_t10h <- felm(log(lyc) ~ log(inc_tipo3_d500_t10h) + log(inc_tipo3_d500_t10h)*clave_turno | id_unico + year | 0 | id_unico, data=filter (panel_secundarias_log, financiamiento=="PUBLICO"))
inter_turlogfelycs_tipo3_d1000_t10h<- felm(log(lyc) ~ log(inc_tipo3_d1000_t10h) + log(inc_tipo3_d1000_t10h)*clave_turno | id_unico + year | 0 | id_unico, data=filter (panel_secundarias_log, financiamiento=="PUBLICO"))
# PRINT 
stargazer (inter_turlogfelycs_tipo3_d250_t90h,
           inter_turlogfelycs_tipo3_d500_t90h,
           inter_turlogfelycs_tipo3_d1000_t90h,
           inter_turlogfelycs_tipo3_d250_t29h,
           inter_turlogfelycs_tipo3_d500_t29h,
           inter_turlogfelycs_tipo3_d1000_t29h,
           inter_turlogfelycs_tipo3_d250_t10h,
           inter_turlogfelycs_tipo3_d500_t10h,
           inter_turlogfelycs_tipo3_d1000_t10h
           , digits=3
           , omit.stat=c ("f", "ser", "aic", "bic", "ll")
           , omit.table.layout="n"
           , no.space=TRUE
           , order = c(1,11,2,12,3,13,4,14,5,15,6,16,7,17,8,18,9,19,10)
           , type="text")

# TABLA 12. INTERACCIONES_TURNO SECUNDARIAS MAT  -------------------------
# TODAS EN SUBCONJUNTO DE ESCUELAS PUBLICAS
# TIPO 1 D.A. PUBLICO 
inter_turlogfelycs_tipo1_d250_t90h <- felm(log(lyc) ~ log(inc_tipo1_d250_t90h) + log(inc_tipo1_d250_t90h)*clave_turno | id_unico + year | 0 | id_unico, data=filter (panel_secundarias_log, financiamiento=="PUBLICO"))
inter_turlogfelycs_tipo1_d500_t90h <- felm(log(lyc) ~ log(inc_tipo1_d500_t90h) + log(inc_tipo1_d500_t90h)*clave_turno | id_unico + year | 0 | id_unico, data=filter (panel_secundarias_log, financiamiento=="PUBLICO"))
inter_turlogfelycs_tipo1_d1000_t90h<- felm(log(lyc) ~ log(inc_tipo1_d1000_t90h) + log(inc_tipo1_d1000_t90h)*clave_turno | id_unico + year | 0 | id_unico, data=filter (panel_secundarias_log, financiamiento=="PUBLICO"))
inter_turlogfelycs_tipo1_d250_t29h <- felm(log(lyc) ~ log(inc_tipo1_d250_t29h) + log(inc_tipo1_d250_t29h)*clave_turno | id_unico + year | 0 | id_unico, data=filter (panel_secundarias_log, financiamiento=="PUBLICO"))
inter_turlogfelycs_tipo1_d500_t29h <- felm(log(lyc) ~ log(inc_tipo1_d500_t29h) + log(inc_tipo1_d500_t29h)*clave_turno | id_unico + year | 0 | id_unico, data=filter (panel_secundarias_log, financiamiento=="PUBLICO"))
inter_turlogfelycs_tipo1_d1000_t29h<- felm(log(lyc) ~ log(inc_tipo1_d1000_t29h) + log(inc_tipo1_d1000_t29h)*clave_turno | id_unico + year | 0 | id_unico, data=filter (panel_secundarias_log, financiamiento=="PUBLICO"))
inter_turlogfelycs_tipo1_d250_t10h <- felm(log(lyc) ~ log(inc_tipo1_d250_t10h) + log(inc_tipo1_d250_t10h)*clave_turno | id_unico + year | 0 | id_unico, data=filter (panel_secundarias_log, financiamiento=="PUBLICO"))
inter_turlogfelycs_tipo1_d500_t10h <- felm(log(lyc) ~ log(inc_tipo1_d500_t10h) + log(inc_tipo1_d500_t10h)*clave_turno | id_unico + year | 0 | id_unico, data=filter (panel_secundarias_log, financiamiento=="PUBLICO"))
inter_turlogfelycs_tipo1_d1000_t10h<- felm(log(lyc) ~ log(inc_tipo1_d1000_t10h) + log(inc_tipo1_d1000_t10h)*clave_turno | id_unico + year | 0 | id_unico, data=filter (panel_secundarias_log, financiamiento=="PUBLICO"))
# PRINT 
stargazer (inter_turlogfelycs_tipo1_d250_t90h,
           inter_turlogfelycs_tipo1_d500_t90h,
           inter_turlogfelycs_tipo1_d1000_t90h,
           inter_turlogfelycs_tipo1_d250_t29h,
           inter_turlogfelycs_tipo1_d500_t29h,
           inter_turlogfelycs_tipo1_d1000_t29h,
           inter_turlogfelycs_tipo1_d250_t10h,
           inter_turlogfelycs_tipo1_d500_t10h,
           inter_turlogfelycs_tipo1_d1000_t10h
           , digits=3
           , omit.stat=c ("f", "ser", "aic", "bic", "ll")
           , omit.table.layout="n"
           , no.space=TRUE
           , order = c(1,11,2,12,3,13,4,14,5,15,6,16,7,17,8,18,9,19,10)
           , type="text")

## TIPO 2 D.A. PRIVADO
inter_turlogfelycs_tipo2_d250_t90h <- felm(log(lyc) ~ log(inc_tipo2_d250_t90h) + log(inc_tipo2_d250_t90h)*clave_turno | id_unico + year | 0 | id_unico, data=filter (panel_secundarias_log, financiamiento=="PUBLICO"))
inter_turlogfelycs_tipo2_d500_t90h <- felm(log(lyc) ~ log(inc_tipo2_d500_t90h) + log(inc_tipo2_d500_t90h)*clave_turno | id_unico + year | 0 | id_unico, data=filter (panel_secundarias_log, financiamiento=="PUBLICO"))
inter_turlogfelycs_tipo2_d1000_t90h<- felm(log(lyc) ~ log(inc_tipo2_d1000_t90h) + log(inc_tipo2_d1000_t90h)*clave_turno | id_unico + year | 0 | id_unico, data=filter (panel_secundarias_log, financiamiento=="PUBLICO"))
inter_turlogfelycs_tipo2_d250_t29h <- felm(log(lyc) ~ log(inc_tipo2_d250_t29h) + log(inc_tipo2_d250_t29h)*clave_turno | id_unico + year | 0 | id_unico, data=filter (panel_secundarias_log, financiamiento=="PUBLICO"))
inter_turlogfelycs_tipo2_d500_t29h <- felm(log(lyc) ~ log(inc_tipo2_d500_t29h) + log(inc_tipo2_d500_t29h)*clave_turno | id_unico + year | 0 | id_unico, data=filter (panel_secundarias_log, financiamiento=="PUBLICO"))
inter_turlogfelycs_tipo2_d1000_t29h<- felm(log(lyc) ~ log(inc_tipo2_d1000_t29h) + log(inc_tipo2_d1000_t29h)*clave_turno | id_unico + year | 0 | id_unico, data=filter (panel_secundarias_log, financiamiento=="PUBLICO"))
inter_turlogfelycs_tipo2_d250_t10h <- felm(log(lyc) ~ log(inc_tipo2_d250_t10h) + log(inc_tipo2_d250_t10h)*clave_turno | id_unico + year | 0 | id_unico, data=filter (panel_secundarias_log, financiamiento=="PUBLICO"))
inter_turlogfelycs_tipo2_d500_t10h <- felm(log(lyc) ~ log(inc_tipo2_d500_t10h) + log(inc_tipo2_d500_t10h)*clave_turno | id_unico + year | 0 | id_unico, data=filter (panel_secundarias_log, financiamiento=="PUBLICO"))
inter_turlogfelycs_tipo2_d1000_t10h<- felm(log(lyc) ~ log(inc_tipo2_d1000_t10h) + log(inc_tipo2_d1000_t10h)*clave_turno | id_unico + year | 0 | id_unico, data=filter (panel_secundarias_log, financiamiento=="PUBLICO"))
# PRINT 
stargazer (inter_turlogfelycs_tipo2_d250_t90h,
           inter_turlogfelycs_tipo2_d500_t90h,
           inter_turlogfelycs_tipo2_d1000_t90h,
           inter_turlogfelycs_tipo2_d250_t29h,
           inter_turlogfelycs_tipo2_d500_t29h,
           inter_turlogfelycs_tipo2_d1000_t29h,
           inter_turlogfelycs_tipo2_d250_t10h,
           inter_turlogfelycs_tipo2_d500_t10h,
           inter_turlogfelycs_tipo2_d1000_t10h
           , digits=3
           , omit.stat=c ("f", "ser", "aic", "bic", "ll")
           , omit.table.layout="n"
           , no.space=TRUE
           , order = c(1,11,2,12,3,13,4,14,5,15,6,16,7,17,8,18,9,19,10)
           , type="text")

# TIPO 3 D.V. HOMICIDIOS
inter_turlogfelycs_tipo3_d250_t90h <- felm(log(lyc) ~ log(inc_tipo3_d250_t90h) + log(inc_tipo3_d250_t90h)*clave_turno | id_unico + year | 0 | id_unico, data=filter (panel_secundarias_log, financiamiento=="PUBLICO"))
inter_turlogfelycs_tipo3_d500_t90h <- felm(log(lyc) ~ log(inc_tipo3_d500_t90h) + log(inc_tipo3_d500_t90h)*clave_turno | id_unico + year | 0 | id_unico, data=filter (panel_secundarias_log, financiamiento=="PUBLICO"))
inter_turlogfelycs_tipo3_d1000_t90h<- felm(log(lyc) ~ log(inc_tipo3_d1000_t90h) + log(inc_tipo3_d1000_t90h)*clave_turno | id_unico + year | 0 | id_unico, data=filter (panel_secundarias_log, financiamiento=="PUBLICO"))
inter_turlogfelycs_tipo3_d250_t29h <- felm(log(lyc) ~ log(inc_tipo3_d250_t29h) + log(inc_tipo3_d250_t29h)*clave_turno | id_unico + year | 0 | id_unico, data=filter (panel_secundarias_log, financiamiento=="PUBLICO"))
inter_turlogfelycs_tipo3_d500_t29h <- felm(log(lyc) ~ log(inc_tipo3_d500_t29h) + log(inc_tipo3_d500_t29h)*clave_turno | id_unico + year | 0 | id_unico, data=filter (panel_secundarias_log, financiamiento=="PUBLICO"))
inter_turlogfelycs_tipo3_d1000_t29h<- felm(log(lyc) ~ log(inc_tipo3_d1000_t29h) + log(inc_tipo3_d1000_t29h)*clave_turno | id_unico + year | 0 | id_unico, data=filter (panel_secundarias_log, financiamiento=="PUBLICO"))
inter_turlogfelycs_tipo3_d250_t10h <- felm(log(lyc) ~ log(inc_tipo3_d250_t10h) + log(inc_tipo3_d250_t10h)*clave_turno | id_unico + year | 0 | id_unico, data=filter (panel_secundarias_log, financiamiento=="PUBLICO"))
inter_turlogfelycs_tipo3_d500_t10h <- felm(log(lyc) ~ log(inc_tipo3_d500_t10h) + log(inc_tipo3_d500_t10h)*clave_turno | id_unico + year | 0 | id_unico, data=filter (panel_secundarias_log, financiamiento=="PUBLICO"))
inter_turlogfelycs_tipo3_d1000_t10h<- felm(log(lyc) ~ log(inc_tipo3_d1000_t10h) + log(inc_tipo3_d1000_t10h)*clave_turno | id_unico + year | 0 | id_unico, data=filter (panel_secundarias_log, financiamiento=="PUBLICO"))
# PRINT 
stargazer (inter_turlogfelycs_tipo3_d250_t90h,
           inter_turlogfelycs_tipo3_d500_t90h,
           inter_turlogfelycs_tipo3_d1000_t90h,
           inter_turlogfelycs_tipo3_d250_t29h,
           inter_turlogfelycs_tipo3_d500_t29h,
           inter_turlogfelycs_tipo3_d1000_t29h,
           inter_turlogfelycs_tipo3_d250_t10h,
           inter_turlogfelycs_tipo3_d500_t10h,
           inter_turlogfelycs_tipo3_d1000_t10h
           , digits=3
           , omit.stat=c ("f", "ser", "aic", "bic", "ll")
           , omit.table.layout="n"
           , no.space=TRUE
           , order = c(1,11,2,12,3,13,4,14,5,15,6,16,7,17,8,18,9,19,10)
           , type="text")

# TABLA A. INTERACCIONES_CARENCIAS PRIMARIAS LYC  -------------------------
# TIPO 1 D.A. PUBLICO 
inter_carlogfelycp_tipo1_d250_t90h <- felm(log(lyc) ~ log(inc_tipo1_d250_t90h) + log(inc_tipo1_d250_t90h)*log(CP1) | id_unico + year | 0 | id_unico, data=panel_primarias_log)
inter_carlogfelycp_tipo1_d500_t90h <- felm(log(lyc) ~ log(inc_tipo1_d500_t90h) + log(inc_tipo1_d500_t90h)*log(CP1) | id_unico + year | 0 | id_unico, data=panel_primarias_log)
inter_carlogfelycp_tipo1_d1000_t90h<- felm(log(lyc) ~ log(inc_tipo1_d1000_t90h) + log(inc_tipo1_d1000_t90h)*log(CP1) | id_unico + year | 0 | id_unico, data=panel_primarias_log)
inter_carlogfelycp_tipo1_d250_t29h <- felm(log(lyc) ~ log(inc_tipo1_d250_t29h) + log(inc_tipo1_d250_t29h)*log(CP1) | id_unico + year | 0 | id_unico, data=panel_primarias_log)
inter_carlogfelycp_tipo1_d500_t29h <- felm(log(lyc) ~ log(inc_tipo1_d500_t29h) + log(inc_tipo1_d500_t29h)*log(CP1) | id_unico + year | 0 | id_unico, data=panel_primarias_log)
inter_carlogfelycp_tipo1_d1000_t29h<- felm(log(lyc) ~ log(inc_tipo1_d1000_t29h) + log(inc_tipo1_d1000_t29h)*log(CP1) | id_unico + year | 0 | id_unico, data=panel_primarias_log)
inter_carlogfelycp_tipo1_d250_t10h <- felm(log(lyc) ~ log(inc_tipo1_d250_t10h) + log(inc_tipo1_d250_t10h)*log(CP1) | id_unico + year | 0 | id_unico, data=panel_primarias_log)
inter_carlogfelycp_tipo1_d500_t10h <- felm(log(lyc) ~ log(inc_tipo1_d500_t10h) + log(inc_tipo1_d500_t10h)*log(CP1) | id_unico + year | 0 | id_unico, data=panel_primarias_log)
inter_carlogfelycp_tipo1_d1000_t10h<- felm(log(lyc) ~ log(inc_tipo1_d1000_t10h) + log(inc_tipo1_d1000_t10h)*log(CP1) | id_unico + year | 0 | id_unico, data=panel_primarias_log)
# PRINT 
stargazer (inter_carlogfelycp_tipo1_d250_t90h,
           inter_carlogfelycp_tipo1_d500_t90h,
           inter_carlogfelycp_tipo1_d1000_t90h,
           inter_carlogfelycp_tipo1_d250_t29h,
           inter_carlogfelycp_tipo1_d500_t29h,
           inter_carlogfelycp_tipo1_d1000_t29h,
           inter_carlogfelycp_tipo1_d250_t10h,
           inter_carlogfelycp_tipo1_d500_t10h,
           inter_carlogfelycp_tipo1_d1000_t10h
           , digits=3
           , omit.stat=c ("f", "ser", "aic", "bic", "ll")
           , omit.table.layout="n"
           , no.space=TRUE
           , order = c(1,11,2,12,3,13,4,14,5,15,6,16,7,17,8,18,9,19,10)
           , type="text")

## TIPO 2 D.A. PRIVADO
inter_carlogfelycp_tipo2_d250_t90h <- felm(log(lyc) ~ log(inc_tipo2_d250_t90h) + log(inc_tipo2_d250_t90h)*log(CP1) | id_unico + year | 0 | id_unico, data=panel_primarias_log)
inter_carlogfelycp_tipo2_d500_t90h <- felm(log(lyc) ~ log(inc_tipo2_d500_t90h) + log(inc_tipo2_d500_t90h)*log(CP1) | id_unico + year | 0 | id_unico, data=panel_primarias_log)
inter_carlogfelycp_tipo2_d1000_t90h<- felm(log(lyc) ~ log(inc_tipo2_d1000_t90h) + log(inc_tipo2_d1000_t90h)*log(CP1) | id_unico + year | 0 | id_unico, data=panel_primarias_log)
inter_carlogfelycp_tipo2_d250_t29h <- felm(log(lyc) ~ log(inc_tipo2_d250_t29h) + log(inc_tipo2_d250_t29h)*log(CP1) | id_unico + year | 0 | id_unico, data=panel_primarias_log)
inter_carlogfelycp_tipo2_d500_t29h <- felm(log(lyc) ~ log(inc_tipo2_d500_t29h) + log(inc_tipo2_d500_t29h)*log(CP1) | id_unico + year | 0 | id_unico, data=panel_primarias_log)
inter_carlogfelycp_tipo2_d1000_t29h<- felm(log(lyc) ~ log(inc_tipo2_d1000_t29h) + log(inc_tipo2_d1000_t29h)*log(CP1) | id_unico + year | 0 | id_unico, data=panel_primarias_log)
inter_carlogfelycp_tipo2_d250_t10h <- felm(log(lyc) ~ log(inc_tipo2_d250_t10h) + log(inc_tipo2_d250_t10h)*log(CP1) | id_unico + year | 0 | id_unico, data=panel_primarias_log)
inter_carlogfelycp_tipo2_d500_t10h <- felm(log(lyc) ~ log(inc_tipo2_d500_t10h) + log(inc_tipo2_d500_t10h)*log(CP1) | id_unico + year | 0 | id_unico, data=panel_primarias_log)
inter_carlogfelycp_tipo2_d1000_t10h<- felm(log(lyc) ~ log(inc_tipo2_d1000_t10h) + log(inc_tipo2_d1000_t10h)*log(CP1) | id_unico + year | 0 | id_unico, data=panel_primarias_log)
# PRINT 
stargazer (inter_carlogfelycp_tipo2_d250_t90h,
           inter_carlogfelycp_tipo2_d500_t90h,
           inter_carlogfelycp_tipo2_d1000_t90h,
           inter_carlogfelycp_tipo2_d250_t29h,
           inter_carlogfelycp_tipo2_d500_t29h,
           inter_carlogfelycp_tipo2_d1000_t29h,
           inter_carlogfelycp_tipo2_d250_t10h,
           inter_carlogfelycp_tipo2_d500_t10h,
           inter_carlogfelycp_tipo2_d1000_t10h
           , digits=3
           , omit.stat=c ("f", "ser", "aic", "bic", "ll")
           , omit.table.layout="n"
           , no.space=TRUE
           , order = c(1,11,2,12,3,13,4,14,5,15,6,16,7,17,8,18,9,19,10)
           , type="text")

# TIPO 3 D.V. HOMICIDIOS
inter_carlogfelycp_tipo3_d250_t90h <- felm(log(lyc) ~ log(inc_tipo3_d250_t90h) + log(inc_tipo3_d250_t90h)*log(CP1) | id_unico + year | 0 | id_unico, data=panel_primarias_log)
inter_carlogfelycp_tipo3_d500_t90h <- felm(log(lyc) ~ log(inc_tipo3_d500_t90h) + log(inc_tipo3_d500_t90h)*log(CP1) | id_unico + year | 0 | id_unico, data=panel_primarias_log)
inter_carlogfelycp_tipo3_d1000_t90h<- felm(log(lyc) ~ log(inc_tipo3_d1000_t90h) + log(inc_tipo3_d1000_t90h)*log(CP1) | id_unico + year | 0 | id_unico, data=panel_primarias_log)
inter_carlogfelycp_tipo3_d250_t29h <- felm(log(lyc) ~ log(inc_tipo3_d250_t29h) + log(inc_tipo3_d250_t29h)*log(CP1) | id_unico + year | 0 | id_unico, data=panel_primarias_log)
inter_carlogfelycp_tipo3_d500_t29h <- felm(log(lyc) ~ log(inc_tipo3_d500_t29h) + log(inc_tipo3_d500_t29h)*log(CP1) | id_unico + year | 0 | id_unico, data=panel_primarias_log)
inter_carlogfelycp_tipo3_d1000_t29h<- felm(log(lyc) ~ log(inc_tipo3_d1000_t29h) + log(inc_tipo3_d1000_t29h)*log(CP1) | id_unico + year | 0 | id_unico, data=panel_primarias_log)
inter_carlogfelycp_tipo3_d250_t10h <- felm(log(lyc) ~ log(inc_tipo3_d250_t10h) + log(inc_tipo3_d250_t10h)*log(CP1) | id_unico + year | 0 | id_unico, data=panel_primarias_log)
inter_carlogfelycp_tipo3_d500_t10h <- felm(log(lyc) ~ log(inc_tipo3_d500_t10h) + log(inc_tipo3_d500_t10h)*log(CP1) | id_unico + year | 0 | id_unico, data=panel_primarias_log)
inter_carlogfelycp_tipo3_d1000_t10h<- felm(log(lyc) ~ log(inc_tipo3_d1000_t10h) + log(inc_tipo3_d1000_t10h)*log(CP1) | id_unico + year | 0 | id_unico, data=panel_primarias_log)
# PRINT 
stargazer (inter_carlogfelycp_tipo3_d250_t90h,
           inter_carlogfelycp_tipo3_d500_t90h,
           inter_carlogfelycp_tipo3_d1000_t90h,
           inter_carlogfelycp_tipo3_d250_t29h,
           inter_carlogfelycp_tipo3_d500_t29h,
           inter_carlogfelycp_tipo3_d1000_t29h,
           inter_carlogfelycp_tipo3_d250_t10h,
           inter_carlogfelycp_tipo3_d500_t10h,
           inter_carlogfelycp_tipo3_d1000_t10h
           , digits=3
           , omit.stat=c ("f", "ser", "aic", "bic", "ll")
           , omit.table.layout="n"
           , no.space=TRUE
           , order = c(1,11,2,12,3,13,4,14,5,15,6,16,7,17,8,18,9,19,10)
           , type="text")

# TABLA B. INTERACCIONES_CARENCIAS PRIMARIAS MAT  -------------------------
# TIPO 1 D.A. PUBLICO 
inter_carlogfematp_tipo1_d250_t90h <- felm(log(mat) ~ log(inc_tipo1_d250_t90h) + log(inc_tipo1_d250_t90h)*log(CP1) | id_unico + year | 0 | id_unico, data=panel_primarias_log)
inter_carlogfematp_tipo1_d500_t90h <- felm(log(mat) ~ log(inc_tipo1_d500_t90h) + log(inc_tipo1_d500_t90h)*log(CP1) | id_unico + year | 0 | id_unico, data=panel_primarias_log)
inter_carlogfematp_tipo1_d1000_t90h<- felm(log(mat) ~ log(inc_tipo1_d1000_t90h) + log(inc_tipo1_d1000_t90h)*log(CP1) | id_unico + year | 0 | id_unico, data=panel_primarias_log)
inter_carlogfematp_tipo1_d250_t29h <- felm(log(mat) ~ log(inc_tipo1_d250_t29h) + log(inc_tipo1_d250_t29h)*log(CP1) | id_unico + year | 0 | id_unico, data=panel_primarias_log)
inter_carlogfematp_tipo1_d500_t29h <- felm(log(mat) ~ log(inc_tipo1_d500_t29h) + log(inc_tipo1_d500_t29h)*log(CP1) | id_unico + year | 0 | id_unico, data=panel_primarias_log)
inter_carlogfematp_tipo1_d1000_t29h<- felm(log(mat) ~ log(inc_tipo1_d1000_t29h) + log(inc_tipo1_d1000_t29h)*log(CP1) | id_unico + year | 0 | id_unico, data=panel_primarias_log)
inter_carlogfematp_tipo1_d250_t10h <- felm(log(mat) ~ log(inc_tipo1_d250_t10h) + log(inc_tipo1_d250_t10h)*log(CP1) | id_unico + year | 0 | id_unico, data=panel_primarias_log)
inter_carlogfematp_tipo1_d500_t10h <- felm(log(mat) ~ log(inc_tipo1_d500_t10h) + log(inc_tipo1_d500_t10h)*log(CP1) | id_unico + year | 0 | id_unico, data=panel_primarias_log)
inter_carlogfematp_tipo1_d1000_t10h<- felm(log(mat) ~ log(inc_tipo1_d1000_t10h) + log(inc_tipo1_d1000_t10h)*log(CP1) | id_unico + year | 0 | id_unico, data=panel_primarias_log)
# PRINT 
stargazer (inter_carlogfematp_tipo1_d250_t90h,
           inter_carlogfematp_tipo1_d500_t90h,
           inter_carlogfematp_tipo1_d1000_t90h,
           inter_carlogfematp_tipo1_d250_t29h,
           inter_carlogfematp_tipo1_d500_t29h,
           inter_carlogfematp_tipo1_d1000_t29h,
           inter_carlogfematp_tipo1_d250_t10h,
           inter_carlogfematp_tipo1_d500_t10h,
           inter_carlogfematp_tipo1_d1000_t10h
           , digits=3
           , omit.stat=c ("f", "ser", "aic", "bic", "ll")
           , omit.table.layout="n"
           , no.space=TRUE
           , order = c(1,11,2,12,3,13,4,14,5,15,6,16,7,17,8,18,9,19,10)
           , type="text")

## TIPO 2 D.A. PRIVADO
inter_carlogfematp_tipo2_d250_t90h <- felm(log(mat) ~ log(inc_tipo2_d250_t90h) + log(inc_tipo2_d250_t90h)*log(CP1) | id_unico + year | 0 | id_unico, data=panel_primarias_log)
inter_carlogfematp_tipo2_d500_t90h <- felm(log(mat) ~ log(inc_tipo2_d500_t90h) + log(inc_tipo2_d500_t90h)*log(CP1) | id_unico + year | 0 | id_unico, data=panel_primarias_log)
inter_carlogfematp_tipo2_d1000_t90h<- felm(log(mat) ~ log(inc_tipo2_d1000_t90h) + log(inc_tipo2_d1000_t90h)*log(CP1) | id_unico + year | 0 | id_unico, data=panel_primarias_log)
inter_carlogfematp_tipo2_d250_t29h <- felm(log(mat) ~ log(inc_tipo2_d250_t29h) + log(inc_tipo2_d250_t29h)*log(CP1) | id_unico + year | 0 | id_unico, data=panel_primarias_log)
inter_carlogfematp_tipo2_d500_t29h <- felm(log(mat) ~ log(inc_tipo2_d500_t29h) + log(inc_tipo2_d500_t29h)*log(CP1) | id_unico + year | 0 | id_unico, data=panel_primarias_log)
inter_carlogfematp_tipo2_d1000_t29h<- felm(log(mat) ~ log(inc_tipo2_d1000_t29h) + log(inc_tipo2_d1000_t29h)*log(CP1) | id_unico + year | 0 | id_unico, data=panel_primarias_log)
inter_carlogfematp_tipo2_d250_t10h <- felm(log(mat) ~ log(inc_tipo2_d250_t10h) + log(inc_tipo2_d250_t10h)*log(CP1) | id_unico + year | 0 | id_unico, data=panel_primarias_log)
inter_carlogfematp_tipo2_d500_t10h <- felm(log(mat) ~ log(inc_tipo2_d500_t10h) + log(inc_tipo2_d500_t10h)*log(CP1) | id_unico + year | 0 | id_unico, data=panel_primarias_log)
inter_carlogfematp_tipo2_d1000_t10h<- felm(log(mat) ~ log(inc_tipo2_d1000_t10h) + log(inc_tipo2_d1000_t10h)*log(CP1) | id_unico + year | 0 | id_unico, data=panel_primarias_log)
# PRINT 
stargazer (inter_carlogfematp_tipo2_d250_t90h,
           inter_carlogfematp_tipo2_d500_t90h,
           inter_carlogfematp_tipo2_d1000_t90h,
           inter_carlogfematp_tipo2_d250_t29h,
           inter_carlogfematp_tipo2_d500_t29h,
           inter_carlogfematp_tipo2_d1000_t29h,
           inter_carlogfematp_tipo2_d250_t10h,
           inter_carlogfematp_tipo2_d500_t10h,
           inter_carlogfematp_tipo2_d1000_t10h
           , digits=3
           , omit.stat=c ("f", "ser", "aic", "bic", "ll")
           , omit.table.layout="n"
           , no.space=TRUE
           , order = c(1,11,2,12,3,13,4,14,5,15,6,16,7,17,8,18,9,19,10)
           , type="text")

# TIPO 3 D.V. HOMICIDIOS
inter_carlogfematp_tipo3_d250_t90h <- felm(log(mat) ~ log(inc_tipo3_d250_t90h) + log(inc_tipo3_d250_t90h)*log(CP1) | id_unico + year | 0 | id_unico, data=panel_primarias_log)
inter_carlogfematp_tipo3_d500_t90h <- felm(log(mat) ~ log(inc_tipo3_d500_t90h) + log(inc_tipo3_d500_t90h)*log(CP1) | id_unico + year | 0 | id_unico, data=panel_primarias_log)
inter_carlogfematp_tipo3_d1000_t90h<- felm(log(mat) ~ log(inc_tipo3_d1000_t90h) + log(inc_tipo3_d1000_t90h)*log(CP1) | id_unico + year | 0 | id_unico, data=panel_primarias_log)
inter_carlogfematp_tipo3_d250_t29h <- felm(log(mat) ~ log(inc_tipo3_d250_t29h) + log(inc_tipo3_d250_t29h)*log(CP1) | id_unico + year | 0 | id_unico, data=panel_primarias_log)
inter_carlogfematp_tipo3_d500_t29h <- felm(log(mat) ~ log(inc_tipo3_d500_t29h) + log(inc_tipo3_d500_t29h)*log(CP1) | id_unico + year | 0 | id_unico, data=panel_primarias_log)
inter_carlogfematp_tipo3_d1000_t29h<- felm(log(mat) ~ log(inc_tipo3_d1000_t29h) + log(inc_tipo3_d1000_t29h)*log(CP1) | id_unico + year | 0 | id_unico, data=panel_primarias_log)
inter_carlogfematp_tipo3_d250_t10h <- felm(log(mat) ~ log(inc_tipo3_d250_t10h) + log(inc_tipo3_d250_t10h)*log(CP1) | id_unico + year | 0 | id_unico, data=panel_primarias_log)
inter_carlogfematp_tipo3_d500_t10h <- felm(log(mat) ~ log(inc_tipo3_d500_t10h) + log(inc_tipo3_d500_t10h)*log(CP1) | id_unico + year | 0 | id_unico, data=panel_primarias_log)
inter_carlogfematp_tipo3_d1000_t10h<- felm(log(mat) ~ log(inc_tipo3_d1000_t10h) + log(inc_tipo3_d1000_t10h)*log(CP1) | id_unico + year | 0 | id_unico, data=panel_primarias_log)
# PRINT 
stargazer (inter_carlogfematp_tipo3_d250_t90h,
           inter_carlogfematp_tipo3_d500_t90h,
           inter_carlogfematp_tipo3_d1000_t90h,
           inter_carlogfematp_tipo3_d250_t29h,
           inter_carlogfematp_tipo3_d500_t29h,
           inter_carlogfematp_tipo3_d1000_t29h,
           inter_carlogfematp_tipo3_d250_t10h,
           inter_carlogfematp_tipo3_d500_t10h,
           inter_carlogfematp_tipo3_d1000_t10h
           , digits=3
           , omit.stat=c ("f", "ser", "aic", "bic", "ll")
           , omit.table.layout="n"
           , no.space=TRUE
           , order = c(1,11,2,12,3,13,4,14,5,15,6,16,7,17,8,18,9,19,10)
           , type="text")

# TABLA C. INTERACCIONES_CARENCIAS SECUNDARIAS LYC  -------------------------
# TIPO 1 D.A. PUBLICO 
inter_carlogfelycs_tipo1_d250_t90h <- felm(log(lyc) ~ log(inc_tipo1_d250_t90h) + log(inc_tipo1_d250_t90h)*log(CP1) | id_unico + year | 0 | id_unico, data=panel_secundarias_log)
inter_carlogfelycs_tipo1_d500_t90h <- felm(log(lyc) ~ log(inc_tipo1_d500_t90h) + log(inc_tipo1_d500_t90h)*log(CP1) | id_unico + year | 0 | id_unico, data=panel_secundarias_log)
inter_carlogfelycs_tipo1_d1000_t90h<- felm(log(lyc) ~ log(inc_tipo1_d1000_t90h) + log(inc_tipo1_d1000_t90h)*log(CP1) | id_unico + year | 0 | id_unico, data=panel_secundarias_log)
inter_carlogfelycs_tipo1_d250_t29h <- felm(log(lyc) ~ log(inc_tipo1_d250_t29h) + log(inc_tipo1_d250_t29h)*log(CP1) | id_unico + year | 0 | id_unico, data=panel_secundarias_log)
inter_carlogfelycs_tipo1_d500_t29h <- felm(log(lyc) ~ log(inc_tipo1_d500_t29h) + log(inc_tipo1_d500_t29h)*log(CP1) | id_unico + year | 0 | id_unico, data=panel_secundarias_log)
inter_carlogfelycs_tipo1_d1000_t29h<- felm(log(lyc) ~ log(inc_tipo1_d1000_t29h) + log(inc_tipo1_d1000_t29h)*log(CP1) | id_unico + year | 0 | id_unico, data=panel_secundarias_log)
inter_carlogfelycs_tipo1_d250_t10h <- felm(log(lyc) ~ log(inc_tipo1_d250_t10h) + log(inc_tipo1_d250_t10h)*log(CP1) | id_unico + year | 0 | id_unico, data=panel_secundarias_log)
inter_carlogfelycs_tipo1_d500_t10h <- felm(log(lyc) ~ log(inc_tipo1_d500_t10h) + log(inc_tipo1_d500_t10h)*log(CP1) | id_unico + year | 0 | id_unico, data=panel_secundarias_log)
inter_carlogfelycs_tipo1_d1000_t10h<- felm(log(lyc) ~ log(inc_tipo1_d1000_t10h) + log(inc_tipo1_d1000_t10h)*log(CP1) | id_unico + year | 0 | id_unico, data=panel_secundarias_log)
# PRINT 
stargazer (inter_carlogfelycs_tipo1_d250_t90h,
           inter_carlogfelycs_tipo1_d500_t90h,
           inter_carlogfelycs_tipo1_d1000_t90h,
           inter_carlogfelycs_tipo1_d250_t29h,
           inter_carlogfelycs_tipo1_d500_t29h,
           inter_carlogfelycs_tipo1_d1000_t29h,
           inter_carlogfelycs_tipo1_d250_t10h,
           inter_carlogfelycs_tipo1_d500_t10h,
           inter_carlogfelycs_tipo1_d1000_t10h
           , digits=3
           , omit.stat=c ("f", "ser", "aic", "bic", "ll")
           , omit.table.layout="n"
           , no.space=TRUE
           , order = c(1,11,2,12,3,13,4,14,5,15,6,16,7,17,8,18,9,19,10)
           , type="text")

## TIPO 2 D.A. PRIVADO
inter_carlogfelycs_tipo2_d250_t90h <- felm(log(lyc) ~ log(inc_tipo2_d250_t90h) + log(inc_tipo2_d250_t90h)*log(CP1) | id_unico + year | 0 | id_unico, data=panel_secundarias_log)
inter_carlogfelycs_tipo2_d500_t90h <- felm(log(lyc) ~ log(inc_tipo2_d500_t90h) + log(inc_tipo2_d500_t90h)*log(CP1) | id_unico + year | 0 | id_unico, data=panel_secundarias_log)
inter_carlogfelycs_tipo2_d1000_t90h<- felm(log(lyc) ~ log(inc_tipo2_d1000_t90h) + log(inc_tipo2_d1000_t90h)*log(CP1) | id_unico + year | 0 | id_unico, data=panel_secundarias_log)
inter_carlogfelycs_tipo2_d250_t29h <- felm(log(lyc) ~ log(inc_tipo2_d250_t29h) + log(inc_tipo2_d250_t29h)*log(CP1) | id_unico + year | 0 | id_unico, data=panel_secundarias_log)
inter_carlogfelycs_tipo2_d500_t29h <- felm(log(lyc) ~ log(inc_tipo2_d500_t29h) + log(inc_tipo2_d500_t29h)*log(CP1) | id_unico + year | 0 | id_unico, data=panel_secundarias_log)
inter_carlogfelycs_tipo2_d1000_t29h<- felm(log(lyc) ~ log(inc_tipo2_d1000_t29h) + log(inc_tipo2_d1000_t29h)*log(CP1) | id_unico + year | 0 | id_unico, data=panel_secundarias_log)
inter_carlogfelycs_tipo2_d250_t10h <- felm(log(lyc) ~ log(inc_tipo2_d250_t10h) + log(inc_tipo2_d250_t10h)*log(CP1) | id_unico + year | 0 | id_unico, data=panel_secundarias_log)
inter_carlogfelycs_tipo2_d500_t10h <- felm(log(lyc) ~ log(inc_tipo2_d500_t10h) + log(inc_tipo2_d500_t10h)*log(CP1) | id_unico + year | 0 | id_unico, data=panel_secundarias_log)
inter_carlogfelycs_tipo2_d1000_t10h<- felm(log(lyc) ~ log(inc_tipo2_d1000_t10h) + log(inc_tipo2_d1000_t10h)*log(CP1) | id_unico + year | 0 | id_unico, data=panel_secundarias_log)
# PRINT 
stargazer (inter_carlogfelycs_tipo2_d250_t90h,
           inter_carlogfelycs_tipo2_d500_t90h,
           inter_carlogfelycs_tipo2_d1000_t90h,
           inter_carlogfelycs_tipo2_d250_t29h,
           inter_carlogfelycs_tipo2_d500_t29h,
           inter_carlogfelycs_tipo2_d1000_t29h,
           inter_carlogfelycs_tipo2_d250_t10h,
           inter_carlogfelycs_tipo2_d500_t10h,
           inter_carlogfelycs_tipo2_d1000_t10h
           , digits=3
           , omit.stat=c ("f", "ser", "aic", "bic", "ll")
           , omit.table.layout="n"
           , no.space=TRUE
           , order = c(1,11,2,12,3,13,4,14,5,15,6,16,7,17,8,18,9,19,10)
           , type="text")

# TIPO 3 D.V. HOMICIDIOS
inter_carlogfelycs_tipo3_d250_t90h <- felm(log(lyc) ~ log(inc_tipo3_d250_t90h) + log(inc_tipo3_d250_t90h)*log(CP1) | id_unico + year | 0 | id_unico, data=panel_secundarias_log)
inter_carlogfelycs_tipo3_d500_t90h <- felm(log(lyc) ~ log(inc_tipo3_d500_t90h) + log(inc_tipo3_d500_t90h)*log(CP1) | id_unico + year | 0 | id_unico, data=panel_secundarias_log)
inter_carlogfelycs_tipo3_d1000_t90h<- felm(log(lyc) ~ log(inc_tipo3_d1000_t90h) + log(inc_tipo3_d1000_t90h)*log(CP1) | id_unico + year | 0 | id_unico, data=panel_secundarias_log)
inter_carlogfelycs_tipo3_d250_t29h <- felm(log(lyc) ~ log(inc_tipo3_d250_t29h) + log(inc_tipo3_d250_t29h)*log(CP1) | id_unico + year | 0 | id_unico, data=panel_secundarias_log)
inter_carlogfelycs_tipo3_d500_t29h <- felm(log(lyc) ~ log(inc_tipo3_d500_t29h) + log(inc_tipo3_d500_t29h)*log(CP1) | id_unico + year | 0 | id_unico, data=panel_secundarias_log)
inter_carlogfelycs_tipo3_d1000_t29h<- felm(log(lyc) ~ log(inc_tipo3_d1000_t29h) + log(inc_tipo3_d1000_t29h)*log(CP1) | id_unico + year | 0 | id_unico, data=panel_secundarias_log)
inter_carlogfelycs_tipo3_d250_t10h <- felm(log(lyc) ~ log(inc_tipo3_d250_t10h) + log(inc_tipo3_d250_t10h)*log(CP1) | id_unico + year | 0 | id_unico, data=panel_secundarias_log)
inter_carlogfelycs_tipo3_d500_t10h <- felm(log(lyc) ~ log(inc_tipo3_d500_t10h) + log(inc_tipo3_d500_t10h)*log(CP1) | id_unico + year | 0 | id_unico, data=panel_secundarias_log)
inter_carlogfelycs_tipo3_d1000_t10h<- felm(log(lyc) ~ log(inc_tipo3_d1000_t10h) + log(inc_tipo3_d1000_t10h)*log(CP1) | id_unico + year | 0 | id_unico, data=panel_secundarias_log)
# PRINT 
stargazer (inter_carlogfelycs_tipo3_d250_t90h,
           inter_carlogfelycs_tipo3_d500_t90h,
           inter_carlogfelycs_tipo3_d1000_t90h,
           inter_carlogfelycs_tipo3_d250_t29h,
           inter_carlogfelycs_tipo3_d500_t29h,
           inter_carlogfelycs_tipo3_d1000_t29h,
           inter_carlogfelycs_tipo3_d250_t10h,
           inter_carlogfelycs_tipo3_d500_t10h,
           inter_carlogfelycs_tipo3_d1000_t10h
           , digits=3
           , omit.stat=c ("f", "ser", "aic", "bic", "ll")
           , omit.table.layout="n"
           , no.space=TRUE
           , order = c(1,11,2,12,3,13,4,14,5,15,6,16,7,17,8,18,9,19,10)
           , type="text")

# TABLA D. INTERACCIONES_CARENCIAS SECUNDARIAS MAT  -------------------------
# TIPO 1 D.A. PUBLICO 
inter_carlogfemats_tipo1_d250_t90h <- felm(log(mat) ~ log(inc_tipo1_d250_t90h) + log(inc_tipo1_d250_t90h)*log(CP1) | id_unico + year | 0 | id_unico, data=panel_secundarias_log)
inter_carlogfemats_tipo1_d500_t90h <- felm(log(mat) ~ log(inc_tipo1_d500_t90h) + log(inc_tipo1_d500_t90h)*log(CP1) | id_unico + year | 0 | id_unico, data=panel_secundarias_log)
inter_carlogfemats_tipo1_d1000_t90h<- felm(log(mat) ~ log(inc_tipo1_d1000_t90h) + log(inc_tipo1_d1000_t90h)*log(CP1) | id_unico + year | 0 | id_unico, data=panel_secundarias_log)
inter_carlogfemats_tipo1_d250_t29h <- felm(log(mat) ~ log(inc_tipo1_d250_t29h) + log(inc_tipo1_d250_t29h)*log(CP1) | id_unico + year | 0 | id_unico, data=panel_secundarias_log)
inter_carlogfemats_tipo1_d500_t29h <- felm(log(mat) ~ log(inc_tipo1_d500_t29h) + log(inc_tipo1_d500_t29h)*log(CP1) | id_unico + year | 0 | id_unico, data=panel_secundarias_log)
inter_carlogfemats_tipo1_d1000_t29h<- felm(log(mat) ~ log(inc_tipo1_d1000_t29h) + log(inc_tipo1_d1000_t29h)*log(CP1) | id_unico + year | 0 | id_unico, data=panel_secundarias_log)
inter_carlogfemats_tipo1_d250_t10h <- felm(log(mat) ~ log(inc_tipo1_d250_t10h) + log(inc_tipo1_d250_t10h)*log(CP1) | id_unico + year | 0 | id_unico, data=panel_secundarias_log)
inter_carlogfemats_tipo1_d500_t10h <- felm(log(mat) ~ log(inc_tipo1_d500_t10h) + log(inc_tipo1_d500_t10h)*log(CP1) | id_unico + year | 0 | id_unico, data=panel_secundarias_log)
inter_carlogfemats_tipo1_d1000_t10h<- felm(log(mat) ~ log(inc_tipo1_d1000_t10h) + log(inc_tipo1_d1000_t10h)*log(CP1) | id_unico + year | 0 | id_unico, data=panel_secundarias_log)
# PRINT 
stargazer (inter_carlogfemats_tipo1_d250_t90h,
           inter_carlogfemats_tipo1_d500_t90h,
           inter_carlogfemats_tipo1_d1000_t90h,
           inter_carlogfemats_tipo1_d250_t29h,
           inter_carlogfemats_tipo1_d500_t29h,
           inter_carlogfemats_tipo1_d1000_t29h,
           inter_carlogfemats_tipo1_d250_t10h,
           inter_carlogfemats_tipo1_d500_t10h,
           inter_carlogfemats_tipo1_d1000_t10h
           , digits=3
           , omit.stat=c ("f", "ser", "aic", "bic", "ll")
           , omit.table.layout="n"
           , no.space=TRUE
           , order = c(1,11,2,12,3,13,4,14,5,15,6,16,7,17,8,18,9,19,10)
           , type="text")

## TIPO 2 D.A. PRIVADO
inter_carlogfemats_tipo2_d250_t90h <- felm(log(mat) ~ log(inc_tipo2_d250_t90h) + log(inc_tipo2_d250_t90h)*log(CP1) | id_unico + year | 0 | id_unico, data=panel_secundarias_log)
inter_carlogfemats_tipo2_d500_t90h <- felm(log(mat) ~ log(inc_tipo2_d500_t90h) + log(inc_tipo2_d500_t90h)*log(CP1) | id_unico + year | 0 | id_unico, data=panel_secundarias_log)
inter_carlogfemats_tipo2_d1000_t90h<- felm(log(mat) ~ log(inc_tipo2_d1000_t90h) + log(inc_tipo2_d1000_t90h)*log(CP1) | id_unico + year | 0 | id_unico, data=panel_secundarias_log)
inter_carlogfemats_tipo2_d250_t29h <- felm(log(mat) ~ log(inc_tipo2_d250_t29h) + log(inc_tipo2_d250_t29h)*log(CP1) | id_unico + year | 0 | id_unico, data=panel_secundarias_log)
inter_carlogfemats_tipo2_d500_t29h <- felm(log(mat) ~ log(inc_tipo2_d500_t29h) + log(inc_tipo2_d500_t29h)*log(CP1) | id_unico + year | 0 | id_unico, data=panel_secundarias_log)
inter_carlogfemats_tipo2_d1000_t29h<- felm(log(mat) ~ log(inc_tipo2_d1000_t29h) + log(inc_tipo2_d1000_t29h)*log(CP1) | id_unico + year | 0 | id_unico, data=panel_secundarias_log)
inter_carlogfemats_tipo2_d250_t10h <- felm(log(mat) ~ log(inc_tipo2_d250_t10h) + log(inc_tipo2_d250_t10h)*log(CP1) | id_unico + year | 0 | id_unico, data=panel_secundarias_log)
inter_carlogfemats_tipo2_d500_t10h <- felm(log(mat) ~ log(inc_tipo2_d500_t10h) + log(inc_tipo2_d500_t10h)*log(CP1) | id_unico + year | 0 | id_unico, data=panel_secundarias_log)
inter_carlogfemats_tipo2_d1000_t10h<- felm(log(mat) ~ log(inc_tipo2_d1000_t10h) + log(inc_tipo2_d1000_t10h)*log(CP1) | id_unico + year | 0 | id_unico, data=panel_secundarias_log)
# PRINT 
stargazer (inter_carlogfemats_tipo2_d250_t90h,
           inter_carlogfemats_tipo2_d500_t90h,
           inter_carlogfemats_tipo2_d1000_t90h,
           inter_carlogfemats_tipo2_d250_t29h,
           inter_carlogfemats_tipo2_d500_t29h,
           inter_carlogfemats_tipo2_d1000_t29h,
           inter_carlogfemats_tipo2_d250_t10h,
           inter_carlogfemats_tipo2_d500_t10h,
           inter_carlogfemats_tipo2_d1000_t10h
           , digits=3
           , omit.stat=c ("f", "ser", "aic", "bic", "ll")
           , omit.table.layout="n"
           , no.space=TRUE
           , order = c(1,11,2,12,3,13,4,14,5,15,6,16,7,17,8,18,9,19,10)
           , type="text")

# TIPO 3 D.V. HOMICIDIOS
inter_carlogfemats_tipo3_d250_t90h <- felm(log(mat) ~ log(inc_tipo3_d250_t90h) + log(inc_tipo3_d250_t90h)*log(CP1) | id_unico + year | 0 | id_unico, data=panel_secundarias_log)
inter_carlogfemats_tipo3_d500_t90h <- felm(log(mat) ~ log(inc_tipo3_d500_t90h) + log(inc_tipo3_d500_t90h)*log(CP1) | id_unico + year | 0 | id_unico, data=panel_secundarias_log)
inter_carlogfemats_tipo3_d1000_t90h<- felm(log(mat) ~ log(inc_tipo3_d1000_t90h) + log(inc_tipo3_d1000_t90h)*log(CP1) | id_unico + year | 0 | id_unico, data=panel_secundarias_log)
inter_carlogfemats_tipo3_d250_t29h <- felm(log(mat) ~ log(inc_tipo3_d250_t29h) + log(inc_tipo3_d250_t29h)*log(CP1) | id_unico + year | 0 | id_unico, data=panel_secundarias_log)
inter_carlogfemats_tipo3_d500_t29h <- felm(log(mat) ~ log(inc_tipo3_d500_t29h) + log(inc_tipo3_d500_t29h)*log(CP1) | id_unico + year | 0 | id_unico, data=panel_secundarias_log)
inter_carlogfemats_tipo3_d1000_t29h<- felm(log(mat) ~ log(inc_tipo3_d1000_t29h) + log(inc_tipo3_d1000_t29h)*log(CP1) | id_unico + year | 0 | id_unico, data=panel_secundarias_log)
inter_carlogfemats_tipo3_d250_t10h <- felm(log(mat) ~ log(inc_tipo3_d250_t10h) + log(inc_tipo3_d250_t10h)*log(CP1) | id_unico + year | 0 | id_unico, data=panel_secundarias_log)
inter_carlogfemats_tipo3_d500_t10h <- felm(log(mat) ~ log(inc_tipo3_d500_t10h) + log(inc_tipo3_d500_t10h)*log(CP1) | id_unico + year | 0 | id_unico, data=panel_secundarias_log)
inter_carlogfemats_tipo3_d1000_t10h<- felm(log(mat) ~ log(inc_tipo3_d1000_t10h) + log(inc_tipo3_d1000_t10h)*log(CP1) | id_unico + year | 0 | id_unico, data=panel_secundarias_log)
# PRINT 
stargazer (inter_carlogfemats_tipo3_d250_t90h,
           inter_carlogfemats_tipo3_d500_t90h,
           inter_carlogfemats_tipo3_d1000_t90h,
           inter_carlogfemats_tipo3_d250_t29h,
           inter_carlogfemats_tipo3_d500_t29h,
           inter_carlogfemats_tipo3_d1000_t29h,
           inter_carlogfemats_tipo3_d250_t10h,
           inter_carlogfemats_tipo3_d500_t10h,
           inter_carlogfemats_tipo3_d1000_t10h
           , digits=3
           , omit.stat=c ("f", "ser", "aic", "bic", "ll")
           , omit.table.layout="n"
           , no.space=TRUE
           , order = c(1,11,2,12,3,13,4,14,5,15,6,16,7,17,8,18,9,19,10)
           , type="text")
