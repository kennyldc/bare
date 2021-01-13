library (tidyverse)
library(scales)
library(gridExtra)
get_legend<-function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}
options (digits=9
         , scipen=8
         , show.signif.stars = FALSE
         , tibble.print_max=50
         , tibble.print_min=1
         , dplyr.width=900) # ajustes para notacion, etc.

anio <- c(2015, 2016, 2017, 2018, 2019)
incidencia <- c(169701, 179720, 204078, 241030, 242849)

x <-c(1,2,3)
y <-c(100,200,300)
x_name <- "cond"
y_name <- "rating"

df <- data.frame(x,y)
names(df) <- c(x_name,y_name)
print(df)

x_name <- "anio"
y_name <- "denuncias"
df <- data.frame(anio,incidencia)
names(df) <- c(x_name,y_name)

(p_denuncias <- ggplot(df, aes(x=anio, y=denuncias))
  + geom_col(fill="#089C74")
  + geom_text(aes(label = comma(denuncias)), vjust = 1.5, color="white")
  + ggtitle("Denuncias por Delitos del fuero com脙潞n en la Ciudad de M脙漏xico 2015-2019" )
  + xlab("")
  + ylab("Denuncias\n")
  + scale_y_continuous(label=comma, limits=c(0,300000), breaks=seq(0,300000, by = 50000))
  + theme_minimal())

# Graficas de las escuelas -------------------------------------------------
summary(escuelas)
pri <- escuelas %>%
  filter(nivel=="PRIMARIA")
summary(pri)

sec <- escuelas %>%
  filter(nivel=="SECUNDARIA")
summary(sec)

alcaldia <- escuelas %>%
  group_by(alcaldia) %>%
  count()
alcaldia$sup <- c(96.17,33.66,26.63,54.4,74.28, 32.4, 94.07, 23.3, 117, 74.58, 46.99, 228.41, 85.34,312, 33.4, 122)
alcaldia <- alcaldia %>%
  mutate(porkm = n/sup)
View(alcaldia)

# # 3. ANALISIS EXPLORATORIO Y MAPAS --------------------------------------

register_google(key="AIzaSyCdS_a0BC_hBXRPjgy_d2SWlY7N8D4l-2s")
cdmx<- c(lon=-99.133209, lat=19.40)
cdmx_map<- get_map(location = cdmx, zoom=11)

#cdmx_map2<- get_map(location = cdmx, zoom=10)
## PUNTO POR ESCUELA 
# NORMALITO
(plot_escuela<- ggmap(cdmx_map)
  + geom_point(aes(Longitud, Latitud), color="blue", shape=".", size=0.8, data = filter(escuelas, nivel=="PRIMARIA")) 
  + geom_point(aes(Longitud, Latitud), color="tomato1", shape=".", size=0.8, data = filter(escuelas, nivel=="SECUNDARIA")) 
  + xlab("")
  + ylab(""))
ggsave("escuelas_cdmx.png", width = 5.66,
       height = 3.65,
       units = c("in"))

# INTERACTIVO
#(plot_escuela2<- ggmap(cdmx_map2)
#  + geom_point(aes(Longitud, Latitud), color="blue", shape=".", size=0.8, data = filter(escuelas, nivel=="PRIMARIA")) 
#  + geom_point(aes(Longitud, Latitud), color="tomato1", shape=".", size=0.8, data = filter(escuelas, nivel=="SECUNDARIA")) 
#  + xlab("")
#  + ylab(""))
#escuelas_plotly <- ggplotly(plot_escuela2)
#htmlwidgets::saveWidget(as_widget(escuelas_plotly), "escuelas_plotly.html")

prim <- escuelas %>%
  filter(nivel=="PRIMARIA")
summary(prim)

sd(prim$lyc16)

secu <- escuelas %>%
  filter(nivel=="SECUNDARIA")
summary(secu)

secu2 <- escuelas %>%
  filter(nivel=="SECUNDARIA") %>%
  filter(caso=="SECUNDARIA CASO 1")
summary(secu2)

privadas <- escuelas %>%
  filter(financiamiento=="PRIVADO")
View(privadas)

scores_prim16<- prim %>%
  select(lyc16, mat16)
scores_prim16 <- scores_prim16 %>%
  tidyr::pivot_longer(names_to = "seccion",
                      values_to = "valores",
                      cols = c(lyc16, mat16)) # este es un paso para cambiar la forma de la base de datos y poder graficar 

(gprim_16 <- ggplot (scores_prim16, aes (x=valores, fill=seccion))
  + geom_density(alpha=0.5)
  + ylab ("Densidad \n")
  + scale_fill_manual("Secci贸n", values=c("#00A1BA","#ff0000"), labels=c("Lenguaje y Comunicaci贸n", "Matem谩ticas"))
  + ggtitle("A帽o 2016")
  + xlab ("\nPuntaje")
  + scale_y_continuous(limits=c(0,0.032), breaks=seq(0,0.032, by = 0.01))
  + theme(legend.position="none", panel.background = element_rect(fill = "white"), panel.grid = element_line(colour = "gray92"), plot.title = element_text(size = rel(1.8))))

scores_prim18<- prim %>%
  select(lyc18, mat18)
scores_prim18 <- scores_prim18 %>%
  tidyr::pivot_longer(names_to = "seccion",
                      values_to = "valores",
                      cols = c(lyc18, mat18)) # este es un paso para cambiar la forma de la base de datos y poder graficar 


(gprim_18 <- ggplot (scores_prim18, aes (x=valores, fill=seccion))
  + geom_density(alpha=0.5)
  + ylab ("Densidad \n")
  + scale_fill_manual("Secci贸n", values=c("#00A1BA","#ff0000"), labels=c("Lenguaje y Comunicaci贸n", "Matem谩ticas"))
  + ggtitle("A帽o 2018")
  + xlab ("\nPuntaje")
  + scale_y_continuous(limits=c(0,0.032), breaks=seq(0,0.032, by = 0.01))
  + theme(legend.position="none", panel.background = element_rect(fill = "white"), panel.grid = element_line(colour = "gray92"), plot.title = element_text(size = rel(1.8))))

#legend <- get_legend(gprim_18) por si hay que repetirlo, para sacar legend hay que construir el plot solamente con theme_minimal y sacar el legend, luego ya se pueden hacer los cambios en theme para que ese sea el que entre al grid extra

grid.arrange(gprim_16, gprim_18, legend, ncol = 3, nrow = 1, widths=c(2.3, 2.3, 0.99))
ggsave("primarias.png", g, width = 7.66,
       height = 5.65,
       units = c("in"))

### SECUNDARIAS 
scores_sec16<- secu %>%
  filter(lyc16!="NA" & mat16!="NA") %>%
  select(lyc16, mat16)
scores_sec16 <- scores_sec16 %>%
  tidyr::pivot_longer(names_to = "seccion",
                      values_to = "valores",
                      cols = c(lyc16, mat16)) # este es un paso para cambiar la forma de la base de datos y poder graficar 

(gsec_16 <- ggplot (scores_sec16, aes (x=valores, fill=seccion))
  + geom_density(alpha=0.5)
  + ylab ("Densidad \n")
  + scale_fill_manual("Secci贸n", values=c("#00A1BA","#ff0000"), labels=c("Lenguaje y Comunicaci贸n", "Matem谩ticas"))
  + ggtitle("A帽o 2016")
  + xlab ("\nPuntaje")
  + scale_y_continuous(limits=c(0,0.033), breaks=seq(0,0.033, by = 0.01))
  + theme(legend.position="none", panel.background = element_rect(fill = "white"), panel.grid = element_line(colour = "gray92"), plot.title = element_text(size = rel(1.8))))

scores_sec17<- secu %>%
  filter(lyc17!="NA" & mat17!="NA") %>%
  select(lyc17, mat17)
scores_sec17 <- scores_sec17 %>%
  tidyr::pivot_longer(names_to = "seccion",
                      values_to = "valores",
                      cols = c(lyc17, mat17)) # este es un paso para cambiar la forma de la base de datos y poder graficar 


(gsec_17 <- ggplot (scores_sec17, aes (x=valores, fill=seccion))
  + geom_density(alpha=0.5)
  + ylab ("Densidad \n")
  + scale_fill_manual("Secci贸n", values=c("#00A1BA","#ff0000"), labels=c("Lenguaje y Comunicaci贸n", "Matem谩ticas"))
  + ggtitle("A帽o 2017")
  + xlab ("\nPuntaje")
  + scale_y_continuous(limits=c(0,0.033), breaks=seq(0,0.033, by = 0.01))
  + theme(legend.position="none", panel.background = element_rect(fill = "white"), panel.grid = element_line(colour = "gray92"), plot.title = element_text(size = rel(1.8))))

scores_sec19<- secu %>%
  filter(lyc19!="NA" & mat19!="NA") %>%
  select(lyc19, mat19)
scores_sec19 <- scores_sec19 %>%
  tidyr::pivot_longer(names_to = "seccion",
                      values_to = "valores",
                      cols = c(lyc19, mat19)) # este es un paso para cambiar la forma de la base de datos y poder graficar 

(gsec_19 <- ggplot (scores_sec19, aes (x=valores, fill=seccion))
  + geom_density(alpha=0.5)
  + ylab ("Densidad \n")
  + scale_fill_manual("Secci贸n", values=c("#00A1BA","#ff0000"), labels=c("Lenguaje y Comunicaci贸n", "Matem谩ticas"))
  + ggtitle("A帽o 2019")
  + xlab ("\nPuntaje")
  + scale_y_continuous(limits=c(0,0.033), breaks=seq(0,0.033, by = 0.01))
  + theme(legend.position="none", panel.background = element_rect(fill = "white"), panel.grid = element_line(colour = "gray92"), plot.title = element_text(size = rel(1.8))))

grid.arrange(gsec_16, gsec_17, gsec_19, legend, ncol = 4, nrow = 1, widths=c(2.1, 2.1, 2.1, 1.2))


# Densidad diferenciado por financiamiento --------------------------------
pub_prim16 <- publicas %>%
  filter(nivel == "PRIMARIA") %>%
  select(lyc16, mat16) %>%
  pivot_longer(names_to = "seccion",
               values_to = "valores",
               cols = c(lyc16, mat16))

(gpub_prim16 <- ggplot (pub_prim16, aes (x=valores, fill=seccion))
  + geom_density(alpha=0.5)
  + ylab ("Densidad \n")
  + scale_fill_manual("Secci髇", values=c("#00A1BA","#ff0000"), labels=c("Lenguaje y Comunicaci髇", "Matem醫icas"))
  + ggtitle("A駉 2016")
  + xlab ("\nPuntaje")
  + scale_y_continuous(limits=c(0,0.05), breaks=seq(0,0.05, by = 0.01))
  + scale_x_continuous(limits=c(14.2857,100), breaks=seq(25,100, by = 25))
  + theme(legend.position="none", panel.background = element_rect(fill = "white"), panel.grid = element_line(colour = "gray92"), plot.title = element_text(size = rel(1.8))))

pub_prim18 <- publicas %>%
  filter(nivel == "PRIMARIA") %>%
  select(lyc18, mat18) %>%
  pivot_longer(names_to = "seccion",
               values_to = "valores",
               cols = c(lyc18, mat18))

(gpub_prim18 <- ggplot (pub_prim18, aes (x=valores, fill=seccion))
  + geom_density(alpha=0.5)
  + ylab ("Densidad \n")
  + scale_fill_manual("Secci髇", values=c("#00A1BA","#ff0000"), labels=c("Lenguaje y Comunicaci髇", "Matem醫icas"))
  + ggtitle("A駉 2018")
  + xlab ("\nPuntaje")
  + scale_y_continuous(limits=c(0,0.05), breaks=seq(0,0.05, by = 0.01))
  + scale_x_continuous(limits=c(14.2857,100), breaks=seq(25,100, by = 25))
  + theme(legend.position="none", panel.background = element_rect(fill = "white"), panel.grid = element_line(colour = "gray92"), plot.title = element_text(size = rel(1.8))))

legend <- get_legend(gprim_18)

publicas_sexto<- grid.arrange(gpub_prim16, gpub_prim18, legend, ncol = 3, nrow = 1, widths=c(2.3, 2.3, 0.99))
ggsave("publicas_primaria.png", publicas_sexto, width = 14.66,
       height = 5.746,
       units = c("in"))

priv_prim16 <- privadas %>%
  filter(nivel == "PRIMARIA") %>%
  select(lyc16, mat16) %>%
  pivot_longer(names_to = "seccion",
               values_to = "valores",
               cols = c(lyc16, mat16))

(gpriv_prim16 <- ggplot (priv_prim16, aes (x=valores, fill=seccion))
  + geom_density(alpha=0.5)
  + ylab ("Densidad \n")
  + scale_fill_manual("Secci髇", values=c("#00A1BA","#ff0000"), labels=c("Lenguaje y Comunicaci髇", "Matem醫icas"))
  + ggtitle("A駉 2016")
  + xlab ("\nPuntaje")
  + scale_y_continuous(limits=c(0,0.05), breaks=seq(0,0.05, by = 0.01))
  + scale_x_continuous(limits=c(14.2857,100), breaks=seq(25,100, by = 25))
  + theme(legend.position="none", panel.background = element_rect(fill = "white"), panel.grid = element_line(colour = "gray92"), plot.title = element_text(size = rel(1.8))))

priv_prim18 <- privadas %>%
  filter(nivel == "PRIMARIA") %>%
  select(lyc18, mat18) %>%
  pivot_longer(names_to = "seccion",
               values_to = "valores",
               cols = c(lyc18, mat18))

(gpriv_prim18 <- ggplot (priv_prim18, aes (x=valores, fill=seccion))
  + geom_density(alpha=0.5)
  + ylab ("Densidad \n")
  + scale_fill_manual("Secci髇", values=c("#00A1BA","#ff0000"), labels=c("Lenguaje y Comunicaci髇", "Matem醫icas"))
  + ggtitle("A駉 2018")
  + xlab ("\nPuntaje")
  + scale_y_continuous(limits=c(0,0.05), breaks=seq(0,0.05, by = 0.01))
  + scale_x_continuous(limits=c(14.2857,100), breaks=seq(25,100, by = 25))
  + theme(legend.position="none", panel.background = element_rect(fill = "white"), panel.grid = element_line(colour = "gray92"), plot.title = element_text(size = rel(1.8))))

privadas_sexto<- grid.arrange(gpriv_prim16, gpriv_prim18, legend, ncol = 3, nrow = 1, widths=c(2.3, 2.3, 0.99))
ggsave("privadas_primaria.png", privadas_sexto, width = 14.66,
       height = 5.746,
       units = c("in"))

#### SECUNDARIAS 
pub_secu16 <- publicas %>%
  filter(nivel == "SECUNDARIA") %>%
  filter(lyc16 != "NA" & mat16 != "NA") %>%
  select(lyc16, mat16) %>%
  pivot_longer(names_to = "seccion",
               values_to = "valores",
               cols = c(lyc16, mat16))

(gpub_secu16 <- ggplot (pub_secu16, aes (x=valores, fill=seccion))
  + geom_density(alpha=0.5)
  + ylab ("Densidad \n")
  + scale_fill_manual("Secci髇", values=c("#00A1BA","#ff0000"), labels=c("Lenguaje y Comunicaci髇", "Matem醫icas"))
  + ggtitle("A駉 2016")
  + xlab ("\nPuntaje")
  + scale_y_continuous(limits=c(0,0.05), breaks=seq(0,0.05, by = 0.01))
  + scale_x_continuous(limits=c(14.2857,100), breaks=seq(25,100, by = 25))
  + theme(legend.position="none", panel.background = element_rect(fill = "white"), panel.grid = element_line(colour = "gray92"), plot.title = element_text(size = rel(1.8))))

pub_secu17 <- publicas %>%
  filter(nivel == "SECUNDARIA") %>%
  filter(lyc17 != "NA" & mat17 != "NA") %>%
  select(lyc17, mat17) %>%
  pivot_longer(names_to = "seccion",
               values_to = "valores",
               cols = c(lyc17, mat17))

(gpub_secu17 <- ggplot (pub_secu17, aes (x=valores, fill=seccion))
  + geom_density(alpha=0.5)
  + ylab ("Densidad \n")
  + scale_fill_manual("Secci髇", values=c("#00A1BA","#ff0000"), labels=c("Lenguaje y Comunicaci髇", "Matem醫icas"))
  + ggtitle("A駉 2017")
  + xlab ("\nPuntaje")
  + scale_y_continuous(limits=c(0,0.05), breaks=seq(0,0.05, by = 0.01))
  + scale_x_continuous(limits=c(14.2857,100), breaks=seq(25,100, by = 25))
  + theme(legend.position="none", panel.background = element_rect(fill = "white"), panel.grid = element_line(colour = "gray92"), plot.title = element_text(size = rel(1.8))))

pub_secu19 <- publicas %>%
  filter(nivel == "SECUNDARIA") %>%
  filter(lyc19 != "NA" & mat19 != "NA") %>%
  select(lyc19, mat19) %>%
  pivot_longer(names_to = "seccion",
               values_to = "valores",
               cols = c(lyc19, mat19))

(gpub_secu19 <- ggplot (pub_secu19, aes (x=valores, fill=seccion))
  + geom_density(alpha=0.5)
  + ylab ("Densidad \n")
  + scale_fill_manual("Secci髇", values=c("#00A1BA","#ff0000"), labels=c("Lenguaje y Comunicaci髇", "Matem醫icas"))
  + ggtitle("A駉 2019")
  + xlab ("\nPuntaje")
  + scale_y_continuous(limits=c(0,0.05), breaks=seq(0,0.05, by = 0.01))
  + scale_x_continuous(limits=c(14.2857,100), breaks=seq(25,100, by = 25))
  + theme(legend.position="none", panel.background = element_rect(fill = "white"), panel.grid = element_line(colour = "gray92"), plot.title = element_text(size = rel(1.8))))

publicas_secundaria<- grid.arrange(gpub_secu16, gpub_secu17, gpub_secu19, legend, ncol = 4, nrow = 1, widths=c(2.1, 2.1, 2.1, 1.2))
ggsave("publicas_secundaria.png", publicas_secundaria, width = 14.66,
       height = 5.746,
       units = c("in"))

priv_secu16 <- privadas %>%
  filter(nivel == "SECUNDARIA") %>%
  filter(lyc16 != "NA" & mat16 != "NA") %>%
  select(lyc16, mat16) %>%
  pivot_longer(names_to = "seccion",
               values_to = "valores",
               cols = c(lyc16, mat16))

(gpriv_secu16 <- ggplot (priv_secu16, aes (x=valores, fill=seccion))
  + geom_density(alpha=0.5)
  + ylab ("Densidad \n")
  + scale_fill_manual("Secci髇", values=c("#00A1BA","#ff0000"), labels=c("Lenguaje y Comunicaci髇", "Matem醫icas"))
  + ggtitle("A駉 2016")
  + xlab ("\nPuntaje")
  + scale_y_continuous(limits=c(0,0.05), breaks=seq(0,0.05, by = 0.01))
  + scale_x_continuous(limits=c(14.2857,100), breaks=seq(25,100, by = 25))
  + theme(legend.position="none", panel.background = element_rect(fill = "white"), panel.grid = element_line(colour = "gray92"), plot.title = element_text(size = rel(1.8))))

priv_secu17 <- privadas %>%
  filter(nivel == "SECUNDARIA") %>%
  filter(lyc17 != "NA" & mat17 != "NA") %>%
  select(lyc17, mat17) %>%
  pivot_longer(names_to = "seccion",
               values_to = "valores",
               cols = c(lyc17, mat17))

(gpriv_secu17 <- ggplot (priv_secu17, aes (x=valores, fill=seccion))
  + geom_density(alpha=0.5)
  + ylab ("Densidad \n")
  + scale_fill_manual("Secci髇", values=c("#00A1BA","#ff0000"), labels=c("Lenguaje y Comunicaci髇", "Matem醫icas"))
  + ggtitle("A駉 2017")
  + xlab ("\nPuntaje")
  + scale_y_continuous(limits=c(0,0.05), breaks=seq(0,0.05, by = 0.01))
  + scale_x_continuous(limits=c(14.2857,100), breaks=seq(25,100, by = 25))
  + theme(legend.position="none", panel.background = element_rect(fill = "white"), panel.grid = element_line(colour = "gray92"), plot.title = element_text(size = rel(1.8))))

priv_secu19 <- privadas %>%
  filter(nivel == "SECUNDARIA") %>%
  filter(lyc19 != "NA" & mat19 != "NA") %>%
  select(lyc19, mat19) %>%
  pivot_longer(names_to = "seccion",
               values_to = "valores",
               cols = c(lyc19, mat19))

(gpriv_secu19 <- ggplot (priv_secu19, aes (x=valores, fill=seccion))
  + geom_density(alpha=0.5)
  + ylab ("Densidad \n")
  + scale_fill_manual("Secci髇", values=c("#00A1BA","#ff0000"), labels=c("Lenguaje y Comunicaci髇", "Matem醫icas"))
  + ggtitle("A駉 2019")
  + xlab ("\nPuntaje")
  + scale_y_continuous(limits=c(0,0.05), breaks=seq(0,0.05, by = 0.01))
  + scale_x_continuous(limits=c(14.2857,100), breaks=seq(25,100, by = 25))
  + theme(legend.position="none", panel.background = element_rect(fill = "white"), panel.grid = element_line(colour = "gray92"), plot.title = element_text(size = rel(1.8))))

privadas_secundaria<- grid.arrange(gpriv_secu16, gpriv_secu17, gpriv_secu19, legend, ncol = 4, nrow = 1, widths=c(2.1, 2.1, 2.1, 1.2))
ggsave("privadas_secundaria.png", privadas_secundaria, width = 14.66,
       height = 5.746,
       units = c("in"))

# densidad diferenciado por turno -----------------------------------------
matutino <- escuelas %>%
  filter(clave_turno==1)

matu_prim16 <- matutino %>%
  filter(nivel == "PRIMARIA") %>%
  select(lyc16, mat16) %>%
  pivot_longer(names_to = "seccion",
               values_to = "valores",
               cols = c(lyc16, mat16))

(gmatu_prim16 <- ggplot (matu_prim16, aes (x=valores, fill=seccion))
  + geom_density(alpha=0.5)
  + ylab ("Densidad \n")
  + scale_fill_manual("Secci髇", values=c("#00A1BA","#ff0000"), labels=c("Lenguaje y Comunicaci髇", "Matem醫icas"))
  + ggtitle("A駉 2016")
  + xlab ("\nPuntaje")
  + scale_y_continuous(limits=c(0,0.05), breaks=seq(0,0.05, by = 0.01))
  + scale_x_continuous(limits=c(14.2857,100), breaks=seq(25,100, by = 25))
  + theme(legend.position="none", panel.background = element_rect(fill = "white"), panel.grid = element_line(colour = "gray92"), plot.title = element_text(size = rel(1.8))))

matu_prim18 <- matutino %>%
  filter(nivel == "PRIMARIA") %>%
  select(lyc18, mat18) %>%
  pivot_longer(names_to = "seccion",
               values_to = "valores",
               cols = c(lyc18, mat18))

(gmatu_prim18 <- ggplot (matu_prim18, aes (x=valores, fill=seccion))
  + geom_density(alpha=0.5)
  + ylab ("Densidad \n")
  + scale_fill_manual("Secci髇", values=c("#00A1BA","#ff0000"), labels=c("Lenguaje y Comunicaci髇", "Matem醫icas"))
  + ggtitle("A駉 2018")
  + xlab ("\nPuntaje")
  + scale_y_continuous(limits=c(0,0.05), breaks=seq(0,0.05, by = 0.01))
  + scale_x_continuous(limits=c(14.2857,100), breaks=seq(25,100, by = 25))
  + theme(legend.position="none", panel.background = element_rect(fill = "white"), panel.grid = element_line(colour = "gray92"), plot.title = element_text(size = rel(1.8))))

matutino_sexto<- grid.arrange(gmatu_prim16, gmatu_prim18, legend, ncol = 3, nrow = 1, widths=c(2.3, 2.3, 0.99))
ggsave("matutino_primaria.png", matutino_sexto, width = 14.66,
       height = 5.746,
       units = c("in"))

vespertino <- escuelas %>%
  filter(clave_turno==2)

vesp_prim16 <- vespertino %>%
  filter(nivel == "PRIMARIA") %>%
  select(lyc16, mat16) %>%
  pivot_longer(names_to = "seccion",
               values_to = "valores",
               cols = c(lyc16, mat16))

(gvesp_prim16 <- ggplot (vesp_prim16, aes (x=valores, fill=seccion))
  + geom_density(alpha=0.5)
  + ylab ("Densidad \n")
  + scale_fill_manual("Secci髇", values=c("#00A1BA","#ff0000"), labels=c("Lenguaje y Comunicaci髇", "Matem醫icas"))
  + ggtitle("A駉 2016")
  + xlab ("\nPuntaje")
  + scale_y_continuous(limits=c(0,0.05), breaks=seq(0,0.05, by = 0.01))
  + scale_x_continuous(limits=c(14.2857,100), breaks=seq(25,100, by = 25))
  + theme(legend.position="none", panel.background = element_rect(fill = "white"), panel.grid = element_line(colour = "gray92"), plot.title = element_text(size = rel(1.8))))

vesp_prim18 <- vespertino %>%
  filter(nivel == "PRIMARIA") %>%
  select(lyc18, mat18) %>%
  pivot_longer(names_to = "seccion",
               values_to = "valores",
               cols = c(lyc18, mat18))

(gvesp_prim18 <- ggplot (vesp_prim18, aes (x=valores, fill=seccion))
  + geom_density(alpha=0.5)
  + ylab ("Densidad \n")
  + scale_fill_manual("Secci髇", values=c("#00A1BA","#ff0000"), labels=c("Lenguaje y Comunicaci髇", "Matem醫icas"))
  + ggtitle("A駉 2018")
  + xlab ("\nPuntaje")
  + scale_y_continuous(limits=c(0,0.06), breaks=seq(0,0.06, by = 0.01))
  + scale_x_continuous(limits=c(14.2857,100), breaks=seq(25,100, by = 25))
  + theme(legend.position="none", panel.background = element_rect(fill = "white"), panel.grid = element_line(colour = "gray92"), plot.title = element_text(size = rel(1.8))))

vespertino_sexto<- grid.arrange(gvesp_prim16, gvesp_prim18, legend, ncol = 3, nrow = 1, widths=c(2.3, 2.3, 0.99))
ggsave("vespertino_primaria.png", vespertino_sexto, width = 14.66,
       height = 5.746,
       units = c("in"))

matu_secu16 <- matutino %>%
  filter(nivel == "SECUNDARIA") %>%
  filter(lyc16 != "NA" & mat16 != "NA") %>%
  select(lyc16, mat16) %>%
  pivot_longer(names_to = "seccion",
               values_to = "valores",
               cols = c(lyc16, mat16))

(gmatu_secu16 <- ggplot (matu_secu16, aes (x=valores, fill=seccion))
  + geom_density(alpha=0.5)
  + ylab ("Densidad \n")
  + scale_fill_manual("Secci髇", values=c("#00A1BA","#ff0000"), labels=c("Lenguaje y Comunicaci髇", "Matem醫icas"))
  + ggtitle("A駉 2016")
  + xlab ("\nPuntaje")
  + scale_y_continuous(limits=c(0,0.06), breaks=seq(0,0.06, by = 0.01))
  + scale_x_continuous(limits=c(14.2857,100), breaks=seq(25,100, by = 25))
  + theme(legend.position="none", panel.background = element_rect(fill = "white"), panel.grid = element_line(colour = "gray92"), plot.title = element_text(size = rel(1.8))))

matu_secu17 <- matutino %>%
  filter(nivel == "SECUNDARIA") %>%
  filter(lyc17 != "NA" & mat17 != "NA") %>%
  select(lyc17, mat17) %>%
  pivot_longer(names_to = "seccion",
               values_to = "valores",
               cols = c(lyc17, mat17))

(gmatu_secu17<- ggplot (matu_secu17, aes (x=valores, fill=seccion))
  + geom_density(alpha=0.5)
  + ylab ("Densidad \n")
  + scale_fill_manual("Secci髇", values=c("#00A1BA","#ff0000"), labels=c("Lenguaje y Comunicaci髇", "Matem醫icas"))
  + ggtitle("A駉 2017")
  + xlab ("\nPuntaje")
  + scale_y_continuous(limits=c(0,0.06), breaks=seq(0,0.06, by = 0.01))
  + scale_x_continuous(limits=c(14.2857,100), breaks=seq(25,100, by = 25))
  + theme(legend.position="none", panel.background = element_rect(fill = "white"), panel.grid = element_line(colour = "gray92"), plot.title = element_text(size = rel(1.8))))

matu_secu19 <- matutino %>%
  filter(nivel == "SECUNDARIA") %>%
  filter(lyc19 != "NA" & mat19 != "NA") %>%
  select(lyc19, mat19) %>%
  pivot_longer(names_to = "seccion",
               values_to = "valores",
               cols = c(lyc19, mat19))

(gmatu_secu19<- ggplot (matu_secu19, aes (x=valores, fill=seccion))
  + geom_density(alpha=0.5)
  + ylab ("Densidad \n")
  + scale_fill_manual("Secci髇", values=c("#00A1BA","#ff0000"), labels=c("Lenguaje y Comunicaci髇", "Matem醫icas"))
  + ggtitle("A駉 2019")
  + xlab ("\nPuntaje")
  + scale_y_continuous(limits=c(0,0.05), breaks=seq(0,0.05, by = 0.01))
  + scale_x_continuous(limits=c(14.2857,100), breaks=seq(25,100, by = 25))
  + theme(legend.position="none", panel.background = element_rect(fill = "white"), panel.grid = element_line(colour = "gray92"), plot.title = element_text(size = rel(1.8))))

matutino_secundaria<- grid.arrange(gmatu_secu16, gmatu_secu17, gmatu_secu19, legend, ncol = 4, nrow = 1, widths=c(2.1, 2.1, 2.1, 1.2))
ggsave("matutino_secundaria.png", matutino_secundaria, width = 14.66,
       height = 5.746,
       units = c("in"))

vesp_secu16 <- vespertino %>%
  filter(nivel == "SECUNDARIA") %>%
  filter(lyc16 != "NA" & mat16 != "NA") %>%
  select(lyc16, mat16) %>%
  pivot_longer(names_to = "seccion",
               values_to = "valores",
               cols = c(lyc16, mat16))

(gvesp_secu16 <- ggplot (vesp_secu16, aes (x=valores, fill=seccion))
  + geom_density(alpha=0.5)
  + ylab ("Densidad \n")
  + scale_fill_manual("Secci髇", values=c("#00A1BA","#ff0000"), labels=c("Lenguaje y Comunicaci髇", "Matem醫icas"))
  + ggtitle("A駉 2016")
  + xlab ("\nPuntaje")
  + scale_y_continuous(limits=c(0,0.06), breaks=seq(0,0.06, by = 0.01))
  + scale_x_continuous(limits=c(14.2857,100), breaks=seq(25,100, by = 25))
  + theme(legend.position="none", panel.background = element_rect(fill = "white"), panel.grid = element_line(colour = "gray92"), plot.title = element_text(size = rel(1.8))))

vesp_secu17 <- vespertino %>%
  filter(nivel == "SECUNDARIA") %>%
  filter(lyc17 != "NA" & mat17 != "NA") %>%
  select(lyc17, mat17) %>%
  pivot_longer(names_to = "seccion",
               values_to = "valores",
               cols = c(lyc17, mat17))

(gvesp_secu17<- ggplot (vesp_secu17, aes (x=valores, fill=seccion))
  + geom_density(alpha=0.5)
  + ylab ("Densidad \n")
  + scale_fill_manual("Secci髇", values=c("#00A1BA","#ff0000"), labels=c("Lenguaje y Comunicaci髇", "Matem醫icas"))
  + ggtitle("A駉 2017")
  + xlab ("\nPuntaje")
  + scale_y_continuous(limits=c(0,0.06), breaks=seq(0,0.06, by = 0.01))
  + scale_x_continuous(limits=c(14.2857,100), breaks=seq(25,100, by = 25))
  + theme(legend.position="none", panel.background = element_rect(fill = "white"), panel.grid = element_line(colour = "gray92"), plot.title = element_text(size = rel(1.8))))

vesp_secu19 <- vespertino %>%
  filter(nivel == "SECUNDARIA") %>%
  filter(lyc19 != "NA" & mat19 != "NA") %>%
  select(lyc19, mat19) %>%
  pivot_longer(names_to = "seccion",
               values_to = "valores",
               cols = c(lyc19, mat19))

(gvesp_secu19<- ggplot (vesp_secu19, aes (x=valores, fill=seccion))
  + geom_density(alpha=0.5)
  + ylab ("Densidad \n")
  + scale_fill_manual("Secci髇", values=c("#00A1BA","#ff0000"), labels=c("Lenguaje y Comunicaci髇", "Matem醫icas"))
  + ggtitle("A駉 2019")
  + xlab ("\nPuntaje")
  + scale_y_continuous(limits=c(0,0.06), breaks=seq(0,0.06, by = 0.01))
  + scale_x_continuous(limits=c(14.2857,100), breaks=seq(25,100, by = 25))
  + theme(legend.position="none", panel.background = element_rect(fill = "white"), panel.grid = element_line(colour = "gray92"), plot.title = element_text(size = rel(1.8))))

vespertino_secundaria<- grid.arrange(gvesp_secu16, gvesp_secu17, gvesp_secu19, legend, ncol = 4, nrow = 1, widths=c(2.1, 2.1, 2.1, 1.2))
ggsave("vespertino_secundaria.png", vespertino_secundaria, width = 14.66,
       height = 5.746,
       units = c("in"))

# Heatmaps delitos 90 -------------------------------------------------------
# Recortes de los incidentes 
incidentes<- delitos %>%
  filter(tipo!="RELACIONADO") # DE AQUI EN ADELANTE DEJARE LOS DELITOS QUE SON VIOLENTOS (TIPO 1 Y 2) Y AQUELLOS QUE NO TIENEN NADA QUE VER CON LA VIOLENCIA Y SERVIRAN PARA LA PRUEBA DE ROBUSTEZ/PLACEBO ("SIN RELACION")
incidentes$fecha_hechos <- as.POSIXct(incidentes$fecha_hechos,format='%d/%m/%Y %H:%M')
incidentes <- incidentes %>% # primer filtro 90 dias
  mutate(meses= if_else(fecha_hechos>="2016-01-19 00:00:00" & fecha_hechos<="2016-06-15 23:59:59", "SI",
                if_else(fecha_hechos>="2017-01-13 00:00:00" & fecha_hechos<="2017-06-14 23:59:59", "SI",
                if_else(fecha_hechos>="2018-01-12 00:00:00" & fecha_hechos<="2018-06-12 23:59:59", "SI",
                if_else(fecha_hechos>="2019-01-11 00:00:00" & fecha_hechos<="2019-06-11 23:59:59", "SI", "NO")))))
incidentes <- incidentes %>%
  filter(meses=="SI")
incidentes <- incidentes %>% # segundo filtro inhabiles
  mutate(inhabiles16 = if_else(fecha_hechos>="2016-01-29 00:00:00" & fecha_hechos<="2016-01-29 23:59:59", "SI",
                       if_else(fecha_hechos>="2016-02-01 00:00:00" & fecha_hechos<="2016-02-01 23:59:59", "SI",
                       if_else(fecha_hechos>="2016-02-26 00:00:00" & fecha_hechos<="2016-02-26 23:59:59", "SI",
                       if_else(fecha_hechos>="2016-03-21 00:00:00" & fecha_hechos<="2016-04-04 23:59:59", "SI", #SEMANA SANTA
                       if_else(fecha_hechos>="2016-04-29 00:00:00" & fecha_hechos<="2016-04-29 23:59:59", "SI",
                       if_else(fecha_hechos>="2016-05-01 00:00:00" & fecha_hechos<="2016-05-01 23:59:59", "SI",
                       if_else(fecha_hechos>="2016-05-05 00:00:00" & fecha_hechos<="2016-05-05 23:59:59", "SI",
                       if_else(fecha_hechos>="2016-05-27 00:00:00" & fecha_hechos<="2016-05-27 23:59:59", "SI", "NO")))))))),
         inhabiles17 = if_else(fecha_hechos>="2017-01-06 00:00:00" & fecha_hechos<="2017-01-06 23:59:59", "SI",
                       if_else(fecha_hechos>="2017-01-27 00:00:00" & fecha_hechos<="2017-01-27 23:59:59", "SI",
                       if_else(fecha_hechos>="2017-02-06 00:00:00" & fecha_hechos<="2017-02-06 23:59:59", "SI",
                       if_else(fecha_hechos>="2017-02-24 00:00:00" & fecha_hechos<="2017-02-24 23:59:59", "SI",
                       if_else(fecha_hechos>="2017-03-20 00:00:00" & fecha_hechos<="2017-03-20 23:59:59", "SI",
                       if_else(fecha_hechos>="2017-03-31 00:00:00" & fecha_hechos<="2017-03-31 23:59:59", "SI",
                       if_else(fecha_hechos>="2017-04-10 00:00:00" & fecha_hechos<="2017-04-21 23:59:59", "SI", #SEMANA SANTA
                       if_else(fecha_hechos>="2017-04-27 00:00:00" & fecha_hechos<="2017-04-27 23:59:59", "SI",
                       if_else(fecha_hechos>="2017-05-01 00:00:00" & fecha_hechos<="2017-05-01 23:59:59", "SI",
                       if_else(fecha_hechos>="2017-05-05 00:00:00" & fecha_hechos<="2017-05-05 23:59:59", "SI",
                       if_else(fecha_hechos>="2017-05-15 00:00:00" & fecha_hechos<="2017-05-15 23:59:59", "SI",
                       if_else(fecha_hechos>="2017-05-26 00:00:00" & fecha_hechos<="2017-05-26 23:59:59", "SI", "NO")))))))))))),
         inhabiles18 = if_else(fecha_hechos>="2018-01-26 00:00:00" & fecha_hechos<="2018-01-26 23:59:59", "SI",
                       if_else(fecha_hechos>="2018-02-05 00:00:00" & fecha_hechos<="2018-02-05 23:59:59", "SI",
                       if_else(fecha_hechos>="2018-02-23 00:00:00" & fecha_hechos<="2018-02-23 23:59:59", "SI",
                       if_else(fecha_hechos>="2018-03-19 00:00:00" & fecha_hechos<="2018-03-19 23:59:59", "SI",
                       if_else(fecha_hechos>="2018-03-26 00:00:00" & fecha_hechos<="2018-04-06 23:59:59", "SI", #SEMANA SANTA
                       if_else(fecha_hechos>="2018-05-01 00:00:00" & fecha_hechos<="2018-05-01 23:59:59", "SI",
                       if_else(fecha_hechos>="2018-05-15 00:00:00" & fecha_hechos<="2018-05-15 23:59:59", "SI",
                       if_else(fecha_hechos>="2018-05-25 00:00:00" & fecha_hechos<="2018-05-25 23:59:59", "SI","NO")))))))),
         inhabiles19 = if_else(fecha_hechos>="2019-01-25 00:00:00" & fecha_hechos<="2019-01-25 23:59:59", "SI",
                       if_else(fecha_hechos>="2019-02-04 00:00:00" & fecha_hechos<="2019-02-04 23:59:59", "SI",
                       if_else(fecha_hechos>="2019-03-01 00:00:00" & fecha_hechos<="2019-03-01 23:59:59", "SI",
                       if_else(fecha_hechos>="2019-03-18 00:00:00" & fecha_hechos<="2019-03-18 23:59:59", "SI",
                       if_else(fecha_hechos>="2019-04-05 00:00:00" & fecha_hechos<="2019-04-05 23:59:59", "SI",
                       if_else(fecha_hechos>="2019-04-15 00:00:00" & fecha_hechos<="2019-04-26 23:59:59", "SI", #SEMANA SANTA
                       if_else(fecha_hechos>="2019-05-01 00:00:00" & fecha_hechos<="2019-05-01 23:59:59", "SI",
                       if_else(fecha_hechos>="2019-05-15 00:00:00" & fecha_hechos<="2019-05-15 23:59:59", "SI",
                       if_else(fecha_hechos>="2019-05-24 00:00:00" & fecha_hechos<="2019-05-24 23:59:59", "SI", "NO"))))))))))
incidentes <- incidentes %>%
  filter(inhabiles16=="NO" & inhabiles17=="NO" & inhabiles18=="NO" & inhabiles19=="NO")

incidentes <- incidentes %>% # tercer filtro dias de la semana
  mutate (dia_semana = as.numeric(format(incidentes$fecha_hechos, format = "%u")), #REGRESA EL NUMERO DE DIA DE LA SEMANA. EMPEZANDO POR LUNES=1
          hora_dia = format(incidentes$fecha_hechos, format="%H:%M:%S")) # REGRESA UNICAMENTE LAS HORAS DE FECHA_HECHOS
incidentes <- incidentes %>%
  filter(dia_semana!=6 & dia_semana!=7) %>% #QUITANDO LOS INCIDENTES EN FIN DE SEMANA.
  filter(hora_dia >= "07:30:00" & hora_dia <= "21:30:00") #TAMBIEN LOS DELITOS DE LAS MADRUGADAS. 

# FIGURE 1 
setwd("~/Tablet/descargas/Tesis/datos/Imagenes para pdf")
# HEATMAP DE INCIDENTES VIOLENTOS (TIPO 1) delitos violentos en el ambito publico 2016
dos16 <- filter(incidentes, ao_hechos=="2016")
# tipo 1 
(tipo1_2016<- ggmap(cdmx_map) +
    stat_density_2d (aes(longitud, latitud, fill = stat(level)), alpha=.1, bins = 25, geom = "polygon", data = filter(dos16, tipo=="TIPO 1"))
  + scale_fill_gradientn(colors=brewer.pal(7, "YlOrRd"))
  + xlab("")
  + ylab("")
  + labs(subtitle = "Delitos violentos 醡bito p鷅lico")
  + theme(legend.position="none"))
ggsave("hmap_publicos16.png", width = 5.66,
       height = 3.65,
       units = c("in"))

# tipo 2 
(tipo2_2016<- ggmap(cdmx_map) +
    stat_density_2d (aes(longitud, latitud, fill = stat(level)), alpha=.1, bins = 25, geom = "polygon", data = filter(dos16, tipo=="TIPO 2"))
  + scale_fill_gradientn(colors=brewer.pal(7, "YlOrRd"))
  + xlab("")
  + ylab("")
  + labs(subtitle = "Delitos violentos 醡bito privado")
  + theme(legend.position="none"))
ggsave("hmap_privados16.png", width = 5.66,
       height = 3.65,
       units = c("in"))

# homicidios
(hom_2016<- ggmap(cdmx_map) +
    stat_density_2d (aes(longitud, latitud, fill = stat(level)), alpha=.1, bins = 25, geom = "polygon", data = filter(dos16, categoria_delito=="HOMICIDIO DOLOSO"))
  + scale_fill_gradientn(colors=brewer.pal(7, "YlOrRd"))
  + xlab("")
  + ylab("")
  + labs(subtitle = "Homicidios")
  + theme(legend.position="none"))
ggsave("hmap_homicidios16.png", width = 5.66,
       height = 3.65,
       units = c("in"))

#2017
dos17 <- filter(incidentes, ao_hechos=="2017")
# tipo 1 
(tipo1_2017<- ggmap(cdmx_map) +
    stat_density_2d (aes(longitud, latitud, fill = stat(level)), alpha=.1, bins = 25, geom = "polygon", data = filter(dos17, tipo=="TIPO 1"))
  + scale_fill_gradientn(colors=brewer.pal(7, "YlOrRd"))
  + xlab("")
  + ylab("")
  + labs(subtitle = "Delitos violentos 醡bito p鷅lico")
  + theme(legend.position="none"))
ggsave("hmap_publicos17.png", width = 5.66,
       height = 3.65,
       units = c("in"))

# tipo 2 
(tipo2_2017<- ggmap(cdmx_map) +
    stat_density_2d (aes(longitud, latitud, fill = stat(level)), alpha=.1, bins = 25, geom = "polygon", data = filter(dos17, tipo=="TIPO 2"))
  + scale_fill_gradientn(colors=brewer.pal(7, "YlOrRd"))
  + xlab("")
  + ylab("")
  + labs(subtitle = "Delitos violentos 醡bito privado")
  + theme(legend.position="none"))
ggsave("hmap_privados17.png", width = 5.66,
       height = 3.65,
       units = c("in"))

# homicidios
(hom_2017<- ggmap(cdmx_map) +
    stat_density_2d (aes(longitud, latitud, fill = stat(level)), alpha=.1, bins = 25, geom = "polygon", data = filter(dos17, categoria_delito=="HOMICIDIO DOLOSO"))
  + scale_fill_gradientn(colors=brewer.pal(7, "YlOrRd"))
  + xlab("")
  + ylab("")
  + labs(subtitle = "Homicidios")
  + theme(legend.position="none"))
ggsave("hmap_homicidios17.png", width = 5.66,
       height = 3.65,
       units = c("in"))

# 2018
dos18 <- filter(incidentes, ao_hechos=="2018")
# tipo 1 
(tipo1_2018<- ggmap(cdmx_map) +
    stat_density_2d (aes(longitud, latitud, fill = stat(level)), alpha=.1, bins = 25, geom = "polygon", data = filter(dos18, tipo=="TIPO 1"))
  + scale_fill_gradientn(colors=brewer.pal(7, "YlOrRd"))
  + xlab("")
  + ylab("")
  + labs(subtitle = "Delitos violentos 醡bito p鷅lico")
  + theme(legend.position="none"))
ggsave("hmap_publicos18.png", width = 5.66,
       height = 3.65,
       units = c("in"))

# tipo 2 
(tipo2_2018<- ggmap(cdmx_map) +
    stat_density_2d (aes(longitud, latitud, fill = stat(level)), alpha=.1, bins = 25, geom = "polygon", data = filter(dos18, tipo=="TIPO 2"))
  + scale_fill_gradientn(colors=brewer.pal(7, "YlOrRd"))
  + xlab("")
  + ylab("")
  + labs(subtitle = "Delitos violentos 醡bito privado")
  + theme(legend.position="none"))
ggsave("hmap_privados18.png", width = 5.66,
       height = 3.65,
       units = c("in"))

# homicidios
(hom_2018<- ggmap(cdmx_map) +
    stat_density_2d (aes(longitud, latitud, fill = stat(level)), alpha=.1, bins = 25, geom = "polygon", data = filter(dos18, categoria_delito=="HOMICIDIO DOLOSO"))
  + scale_fill_gradientn(colors=brewer.pal(7, "YlOrRd"))
  + xlab("")
  + ylab("")
  + labs(subtitle = "Homicidios")
  + theme(legend.position="none"))
ggsave("hmap_homicidios18.png", width = 5.66,
       height = 3.65,
       units = c("in"))

# 2019
dos19 <- filter(incidentes, ao_hechos=="2019")
# tipo 1 
(tipo1_2019<- ggmap(cdmx_map) +
    stat_density_2d (aes(longitud, latitud, fill = stat(level)), alpha=.1, bins = 25, geom = "polygon", data = filter(dos19, tipo=="TIPO 1"))
  + scale_fill_gradientn(colors=brewer.pal(7, "YlOrRd"))
  + xlab("")
  + ylab("")
  + labs(subtitle = "Delitos violentos 醡bito p鷅lico")
  + theme(legend.position="none"))
ggsave("hmap_publicos19.png", width = 5.66,
       height = 3.65,
       units = c("in"))

# tipo 2 
(tipo2_2019<- ggmap(cdmx_map) +
    stat_density_2d (aes(longitud, latitud, fill = stat(level)), alpha=.1, bins = 25, geom = "polygon", data = filter(dos19, tipo=="TIPO 2"))
  + scale_fill_gradientn(colors=brewer.pal(7, "YlOrRd"))
  + xlab("")
  + ylab("")
  + labs(subtitle = "Delitos violentos 醡bito privado")
  + theme(legend.position="none"))
ggsave("hmap_privados19.png", width = 5.66,
       height = 3.65,
       units = c("in"))

# homicidios
(hom_2019<- ggmap(cdmx_map) +
    stat_density_2d (aes(longitud, latitud, fill = stat(level)), alpha=.1, bins = 25, geom = "polygon", data = filter(dos19, categoria_delito=="HOMICIDIO DOLOSO"))
  + scale_fill_gradientn(colors=brewer.pal(7, "YlOrRd"))
  + xlab("")
  + ylab("")
  + labs(subtitle = "Homicidios")
  + theme(legend.position="none"))
ggsave("hmap_homicidios19.png", width = 5.66,
       height = 3.65,
       units = c("in"))


# heatmap delitos 29 dias -------------------------------------------------
incidentes<- delitos %>%
  filter(tipo!="RELACIONADO") # DE AQUI EN ADELANTE DEJARE LOS DELITOS QUE SON VIOLENTOS (TIPO 1 Y 2) Y AQUELLOS QUE NO TIENEN NADA QUE VER CON LA VIOLENCIA Y SERVIRAN PARA LA PRUEBA DE ROBUSTEZ/PLACEBO ("SIN RELACION")
incidentes$fecha_hechos <- as.POSIXct(incidentes$fecha_hechos,format='%d/%m/%Y %H:%M')
incidentes <- incidentes %>% # segundo filtro inhabiles
  mutate(inhabiles16 = if_else(fecha_hechos>="2016-01-29 00:00:00" & fecha_hechos<="2016-01-29 23:59:59", "SI",
                       if_else(fecha_hechos>="2016-02-01 00:00:00" & fecha_hechos<="2016-02-01 23:59:59", "SI",
                       if_else(fecha_hechos>="2016-02-26 00:00:00" & fecha_hechos<="2016-02-26 23:59:59", "SI",
                       if_else(fecha_hechos>="2016-03-21 00:00:00" & fecha_hechos<="2016-04-04 23:59:59", "SI", #SEMANA SANTA
                       if_else(fecha_hechos>="2016-04-29 00:00:00" & fecha_hechos<="2016-04-29 23:59:59", "SI",
                       if_else(fecha_hechos>="2016-05-01 00:00:00" & fecha_hechos<="2016-05-01 23:59:59", "SI",
                       if_else(fecha_hechos>="2016-05-05 00:00:00" & fecha_hechos<="2016-05-05 23:59:59", "SI",
                       if_else(fecha_hechos>="2016-05-27 00:00:00" & fecha_hechos<="2016-05-27 23:59:59", "SI", "NO")))))))),
         inhabiles17 = if_else(fecha_hechos>="2017-01-06 00:00:00" & fecha_hechos<="2017-01-06 23:59:59", "SI",
                       if_else(fecha_hechos>="2017-01-27 00:00:00" & fecha_hechos<="2017-01-27 23:59:59", "SI",
                       if_else(fecha_hechos>="2017-02-06 00:00:00" & fecha_hechos<="2017-02-06 23:59:59", "SI",
                       if_else(fecha_hechos>="2017-02-24 00:00:00" & fecha_hechos<="2017-02-24 23:59:59", "SI",
                       if_else(fecha_hechos>="2017-03-20 00:00:00" & fecha_hechos<="2017-03-20 23:59:59", "SI",
                       if_else(fecha_hechos>="2017-03-31 00:00:00" & fecha_hechos<="2017-03-31 23:59:59", "SI",
                       if_else(fecha_hechos>="2017-04-10 00:00:00" & fecha_hechos<="2017-04-21 23:59:59", "SI", #SEMANA SANTA
                       if_else(fecha_hechos>="2017-04-27 00:00:00" & fecha_hechos<="2017-04-27 23:59:59", "SI",
                       if_else(fecha_hechos>="2017-05-01 00:00:00" & fecha_hechos<="2017-05-01 23:59:59", "SI",
                       if_else(fecha_hechos>="2017-05-05 00:00:00" & fecha_hechos<="2017-05-05 23:59:59", "SI",
                       if_else(fecha_hechos>="2017-05-15 00:00:00" & fecha_hechos<="2017-05-15 23:59:59", "SI",
                       if_else(fecha_hechos>="2017-05-26 00:00:00" & fecha_hechos<="2017-05-26 23:59:59", "SI", "NO")))))))))))),
         inhabiles18 = if_else(fecha_hechos>="2018-01-26 00:00:00" & fecha_hechos<="2018-01-26 23:59:59", "SI",
                       if_else(fecha_hechos>="2018-02-05 00:00:00" & fecha_hechos<="2018-02-05 23:59:59", "SI",
                       if_else(fecha_hechos>="2018-02-23 00:00:00" & fecha_hechos<="2018-02-23 23:59:59", "SI",
                       if_else(fecha_hechos>="2018-03-19 00:00:00" & fecha_hechos<="2018-03-19 23:59:59", "SI",
                       if_else(fecha_hechos>="2018-03-26 00:00:00" & fecha_hechos<="2018-04-06 23:59:59", "SI", #SEMANA SANTA
                       if_else(fecha_hechos>="2018-05-01 00:00:00" & fecha_hechos<="2018-05-01 23:59:59", "SI",
                       if_else(fecha_hechos>="2018-05-15 00:00:00" & fecha_hechos<="2018-05-15 23:59:59", "SI",
                       if_else(fecha_hechos>="2018-05-25 00:00:00" & fecha_hechos<="2018-05-25 23:59:59", "SI","NO")))))))),
         inhabiles19 = if_else(fecha_hechos>="2019-01-25 00:00:00" & fecha_hechos<="2019-01-25 23:59:59", "SI",
                       if_else(fecha_hechos>="2019-02-04 00:00:00" & fecha_hechos<="2019-02-04 23:59:59", "SI",
                       if_else(fecha_hechos>="2019-03-01 00:00:00" & fecha_hechos<="2019-03-01 23:59:59", "SI",
                       if_else(fecha_hechos>="2019-03-18 00:00:00" & fecha_hechos<="2019-03-18 23:59:59", "SI",
                       if_else(fecha_hechos>="2019-04-05 00:00:00" & fecha_hechos<="2019-04-05 23:59:59", "SI",
                       if_else(fecha_hechos>="2019-04-15 00:00:00" & fecha_hechos<="2019-04-26 23:59:59", "SI", #SEMANA SANTA
                       if_else(fecha_hechos>="2019-05-01 00:00:00" & fecha_hechos<="2019-05-01 23:59:59", "SI",
                       if_else(fecha_hechos>="2019-05-15 00:00:00" & fecha_hechos<="2019-05-15 23:59:59", "SI",
                       if_else(fecha_hechos>="2019-05-24 00:00:00" & fecha_hechos<="2019-05-24 23:59:59", "SI", "NO"))))))))))
incidentes <- incidentes %>%
  filter(inhabiles16=="NO" & inhabiles17=="NO" & inhabiles18=="NO" & inhabiles19=="NO")

incidentes <- incidentes %>% # filtro 29 dias
  mutate(meses= if_else(fecha_hechos>="2016-05-04 00:00:00" & fecha_hechos<="2016-06-15 23:59:59", "SI",
                if_else(fecha_hechos>="2017-05-02 00:00:00" & fecha_hechos<="2017-06-14 23:59:59", "SI",
                if_else(fecha_hechos>="2018-04-30 00:00:00" & fecha_hechos<="2018-06-12 23:59:59", "SI",
                if_else(fecha_hechos>="2019-04-29 00:00:00" & fecha_hechos<="2019-06-11 23:59:59", "SI", "NO")))))
incidentes <- incidentes %>%
  filter(meses=="SI")

incidentes <- incidentes %>% # tercer filtro dias de la semana
  mutate (dia_semana = as.numeric(format(incidentes$fecha_hechos, format = "%u")), #REGRESA EL NUMERO DE DIA DE LA SEMANA. EMPEZANDO POR LUNES=1
          hora_dia = format(incidentes$fecha_hechos, format="%H:%M:%S")) # REGRESA UNICAMENTE LAS HORAS DE FECHA_HECHOS
incidentes <- incidentes %>%
  filter(dia_semana!=6 & dia_semana!=7) %>% #QUITANDO LOS INCIDENTES EN FIN DE SEMANA.
  filter(hora_dia >= "07:30:00" & hora_dia <= "21:30:00") #TAMBIEN LOS DELITOS DE LAS MADRUGADAS. 

# FIGURE 1 
setwd("~/Tablet/descargas/Tesis/datos/Imagenes para pdf/29")
# HEATMAP DE INCIDENTES VIOLENTOS (TIPO 1) delitos violentos en el ambito publico 2016
dos16 <- filter(incidentes, ao_hechos=="2016")
# tipo 1 
(tipo1_2016<- ggmap(cdmx_map) +
    stat_density_2d (aes(longitud, latitud, fill = stat(level)), alpha=.1, bins = 25, geom = "polygon", data = filter(dos16, tipo=="TIPO 1"))
  + scale_fill_gradientn(colors=brewer.pal(7, "YlOrRd"))
  + xlab("")
  + ylab("")
  + labs(subtitle = "Delitos violentos 醡bito p鷅lico")
  + theme(legend.position="none"))
ggsave("hmap_publicos16.png", width = 5.66,
       height = 3.65,
       units = c("in"))

# tipo 2 
(tipo2_2016<- ggmap(cdmx_map) +
    stat_density_2d (aes(longitud, latitud, fill = stat(level)), alpha=.1, bins = 25, geom = "polygon", data = filter(dos16, tipo=="TIPO 2"))
  + scale_fill_gradientn(colors=brewer.pal(7, "YlOrRd"))
  + xlab("")
  + ylab("")
  + labs(subtitle = "Delitos violentos 醡bito privado")
  + theme(legend.position="none"))
ggsave("hmap_privados16.png", width = 5.66,
       height = 3.65,
       units = c("in"))

# homicidios
(hom_2016<- ggmap(cdmx_map) +
    stat_density_2d (aes(longitud, latitud, fill = stat(level)), alpha=.1, bins = 25, geom = "polygon", data = filter(dos16, categoria_delito=="HOMICIDIO DOLOSO"))
  + scale_fill_gradientn(colors=brewer.pal(7, "YlOrRd"))
  + xlab("")
  + ylab("")
  + labs(subtitle = "Homicidios")
  + theme(legend.position="none"))
ggsave("hmap_homicidios16.png", width = 5.66,
       height = 3.65,
       units = c("in"))

#2017
dos17 <- filter(incidentes, ao_hechos=="2017")
# tipo 1 
(tipo1_2017<- ggmap(cdmx_map) +
    stat_density_2d (aes(longitud, latitud, fill = stat(level)), alpha=.1, bins = 25, geom = "polygon", data = filter(dos17, tipo=="TIPO 1"))
  + scale_fill_gradientn(colors=brewer.pal(7, "YlOrRd"))
  + xlab("")
  + ylab("")
  + labs(subtitle = "Delitos violentos 醡bito p鷅lico")
  + theme(legend.position="none"))
ggsave("hmap_publicos17.png", width = 5.66,
       height = 3.65,
       units = c("in"))

# tipo 2 
(tipo2_2017<- ggmap(cdmx_map) +
    stat_density_2d (aes(longitud, latitud, fill = stat(level)), alpha=.1, bins = 25, geom = "polygon", data = filter(dos17, tipo=="TIPO 2"))
  + scale_fill_gradientn(colors=brewer.pal(7, "YlOrRd"))
  + xlab("")
  + ylab("")
  + labs(subtitle = "Delitos violentos 醡bito privado")
  + theme(legend.position="none"))
ggsave("hmap_privados17.png", width = 5.66,
       height = 3.65,
       units = c("in"))

# homicidios
(hom_2017<- ggmap(cdmx_map) +
    stat_density_2d (aes(longitud, latitud, fill = stat(level)), alpha=.1, bins = 25, geom = "polygon", data = filter(dos17, categoria_delito=="HOMICIDIO DOLOSO"))
  + scale_fill_gradientn(colors=brewer.pal(7, "YlOrRd"))
  + xlab("")
  + ylab("")
  + labs(subtitle = "Homicidios")
  + theme(legend.position="none"))
ggsave("hmap_homicidios17.png", width = 5.66,
       height = 3.65,
       units = c("in"))

# 2018
dos18 <- filter(incidentes, ao_hechos=="2018")
# tipo 1 
(tipo1_2018<- ggmap(cdmx_map) +
    stat_density_2d (aes(longitud, latitud, fill = stat(level)), alpha=.1, bins = 25, geom = "polygon", data = filter(dos18, tipo=="TIPO 1"))
  + scale_fill_gradientn(colors=brewer.pal(7, "YlOrRd"))
  + xlab("")
  + ylab("")
  + labs(subtitle = "Delitos violentos 醡bito p鷅lico")
  + theme(legend.position="none"))
ggsave("hmap_publicos18.png", width = 5.66,
       height = 3.65,
       units = c("in"))

# tipo 2 
(tipo2_2018<- ggmap(cdmx_map) +
    stat_density_2d (aes(longitud, latitud, fill = stat(level)), alpha=.1, bins = 25, geom = "polygon", data = filter(dos18, tipo=="TIPO 2"))
  + scale_fill_gradientn(colors=brewer.pal(7, "YlOrRd"))
  + xlab("")
  + ylab("")
  + labs(subtitle = "Delitos violentos 醡bito privado")
  + theme(legend.position="none"))
ggsave("hmap_privados18.png", width = 5.66,
       height = 3.65,
       units = c("in"))

# homicidios
(hom_2018<- ggmap(cdmx_map) +
    stat_density_2d (aes(longitud, latitud, fill = stat(level)), alpha=.1, bins = 25, geom = "polygon", data = filter(dos18, categoria_delito=="HOMICIDIO DOLOSO"))
  + scale_fill_gradientn(colors=brewer.pal(7, "YlOrRd"))
  + xlab("")
  + ylab("")
  + labs(subtitle = "Homicidios")
  + theme(legend.position="none"))
ggsave("hmap_homicidios18.png", width = 5.66,
       height = 3.65,
       units = c("in"))

# 2019
dos19 <- filter(incidentes, ao_hechos=="2019")
# tipo 1 
(tipo1_2019<- ggmap(cdmx_map) +
    stat_density_2d (aes(longitud, latitud, fill = stat(level)), alpha=.1, bins = 25, geom = "polygon", data = filter(dos19, tipo=="TIPO 1"))
  + scale_fill_gradientn(colors=brewer.pal(7, "YlOrRd"))
  + xlab("")
  + ylab("")
  + labs(subtitle = "Delitos violentos 醡bito p鷅lico")
  + theme(legend.position="none"))
ggsave("hmap_publicos19.png", width = 5.66,
       height = 3.65,
       units = c("in"))

# tipo 2 
(tipo2_2019<- ggmap(cdmx_map) +
    stat_density_2d (aes(longitud, latitud, fill = stat(level)), alpha=.1, bins = 25, geom = "polygon", data = filter(dos19, tipo=="TIPO 2"))
  + scale_fill_gradientn(colors=brewer.pal(7, "YlOrRd"))
  + xlab("")
  + ylab("")
  + labs(subtitle = "Delitos violentos 醡bito privado")
  + theme(legend.position="none"))
ggsave("hmap_privados19.png", width = 5.66,
       height = 3.65,
       units = c("in"))

# homicidios
(hom_2019<- ggmap(cdmx_map) +
    stat_density_2d (aes(longitud, latitud, fill = stat(level)), alpha=.1, bins = 25, geom = "polygon", data = filter(dos19, categoria_delito=="HOMICIDIO DOLOSO"))
  + scale_fill_gradientn(colors=brewer.pal(7, "YlOrRd"))
  + xlab("")
  + ylab("")
  + labs(subtitle = "Homicidios")
  + theme(legend.position="none"))
ggsave("hmap_homicidios19.png", width = 5.66,
       height = 3.65,
       units = c("in"))
# heatmap delitos 10 dias -------------------------------------------------
incidentes<- delitos %>%
  filter(tipo!="RELACIONADO") # DE AQUI EN ADELANTE DEJARE LOS DELITOS QUE SON VIOLENTOS (TIPO 1 Y 2) Y AQUELLOS QUE NO TIENEN NADA QUE VER CON LA VIOLENCIA Y SERVIRAN PARA LA PRUEBA DE ROBUSTEZ/PLACEBO ("SIN RELACION")
incidentes$fecha_hechos <- as.POSIXct(incidentes$fecha_hechos,format='%d/%m/%Y %H:%M')
incidentes <- incidentes %>% # segundo filtro inhabiles
  mutate(inhabiles16 = if_else(fecha_hechos>="2016-01-29 00:00:00" & fecha_hechos<="2016-01-29 23:59:59", "SI",
                       if_else(fecha_hechos>="2016-02-01 00:00:00" & fecha_hechos<="2016-02-01 23:59:59", "SI",
                       if_else(fecha_hechos>="2016-02-26 00:00:00" & fecha_hechos<="2016-02-26 23:59:59", "SI",
                       if_else(fecha_hechos>="2016-03-21 00:00:00" & fecha_hechos<="2016-04-04 23:59:59", "SI", #SEMANA SANTA
                       if_else(fecha_hechos>="2016-04-29 00:00:00" & fecha_hechos<="2016-04-29 23:59:59", "SI",
                       if_else(fecha_hechos>="2016-05-01 00:00:00" & fecha_hechos<="2016-05-01 23:59:59", "SI",
                       if_else(fecha_hechos>="2016-05-05 00:00:00" & fecha_hechos<="2016-05-05 23:59:59", "SI",
                                                                               if_else(fecha_hechos>="2016-05-27 00:00:00" & fecha_hechos<="2016-05-27 23:59:59", "SI", "NO")))))))),
         inhabiles17 = if_else(fecha_hechos>="2017-01-06 00:00:00" & fecha_hechos<="2017-01-06 23:59:59", "SI",
                       if_else(fecha_hechos>="2017-01-27 00:00:00" & fecha_hechos<="2017-01-27 23:59:59", "SI",
                       if_else(fecha_hechos>="2017-02-06 00:00:00" & fecha_hechos<="2017-02-06 23:59:59", "SI",
                       if_else(fecha_hechos>="2017-02-24 00:00:00" & fecha_hechos<="2017-02-24 23:59:59", "SI",
                       if_else(fecha_hechos>="2017-03-20 00:00:00" & fecha_hechos<="2017-03-20 23:59:59", "SI",
                       if_else(fecha_hechos>="2017-03-31 00:00:00" & fecha_hechos<="2017-03-31 23:59:59", "SI",
                       if_else(fecha_hechos>="2017-04-10 00:00:00" & fecha_hechos<="2017-04-21 23:59:59", "SI", #SEMANA SANTA
                       if_else(fecha_hechos>="2017-04-27 00:00:00" & fecha_hechos<="2017-04-27 23:59:59", "SI",
                       if_else(fecha_hechos>="2017-05-01 00:00:00" & fecha_hechos<="2017-05-01 23:59:59", "SI",
                       if_else(fecha_hechos>="2017-05-05 00:00:00" & fecha_hechos<="2017-05-05 23:59:59", "SI",
                       if_else(fecha_hechos>="2017-05-15 00:00:00" & fecha_hechos<="2017-05-15 23:59:59", "SI",
                       if_else(fecha_hechos>="2017-05-26 00:00:00" & fecha_hechos<="2017-05-26 23:59:59", "SI", "NO")))))))))))),
         inhabiles18 = if_else(fecha_hechos>="2018-01-26 00:00:00" & fecha_hechos<="2018-01-26 23:59:59", "SI",
                       if_else(fecha_hechos>="2018-02-05 00:00:00" & fecha_hechos<="2018-02-05 23:59:59", "SI",
                       if_else(fecha_hechos>="2018-02-23 00:00:00" & fecha_hechos<="2018-02-23 23:59:59", "SI",
                       if_else(fecha_hechos>="2018-03-19 00:00:00" & fecha_hechos<="2018-03-19 23:59:59", "SI",
                       if_else(fecha_hechos>="2018-03-26 00:00:00" & fecha_hechos<="2018-04-06 23:59:59", "SI", #SEMANA SANTA
                       if_else(fecha_hechos>="2018-05-01 00:00:00" & fecha_hechos<="2018-05-01 23:59:59", "SI",
                       if_else(fecha_hechos>="2018-05-15 00:00:00" & fecha_hechos<="2018-05-15 23:59:59", "SI",
                       if_else(fecha_hechos>="2018-05-25 00:00:00" & fecha_hechos<="2018-05-25 23:59:59", "SI","NO")))))))),
         inhabiles19 = if_else(fecha_hechos>="2019-01-25 00:00:00" & fecha_hechos<="2019-01-25 23:59:59", "SI",
                       if_else(fecha_hechos>="2019-02-04 00:00:00" & fecha_hechos<="2019-02-04 23:59:59", "SI",
                       if_else(fecha_hechos>="2019-03-01 00:00:00" & fecha_hechos<="2019-03-01 23:59:59", "SI",
                       if_else(fecha_hechos>="2019-03-18 00:00:00" & fecha_hechos<="2019-03-18 23:59:59", "SI",
                       if_else(fecha_hechos>="2019-04-05 00:00:00" & fecha_hechos<="2019-04-05 23:59:59", "SI",
                       if_else(fecha_hechos>="2019-04-15 00:00:00" & fecha_hechos<="2019-04-26 23:59:59", "SI", #SEMANA SANTA
                       if_else(fecha_hechos>="2019-05-01 00:00:00" & fecha_hechos<="2019-05-01 23:59:59", "SI",
                       if_else(fecha_hechos>="2019-05-15 00:00:00" & fecha_hechos<="2019-05-15 23:59:59", "SI",
                       if_else(fecha_hechos>="2019-05-24 00:00:00" & fecha_hechos<="2019-05-24 23:59:59", "SI", "NO"))))))))))
incidentes <- incidentes %>%
  filter(inhabiles16=="NO" & inhabiles17=="NO" & inhabiles18=="NO" & inhabiles19=="NO")

incidentes <- incidentes %>% # filtro 10 dias
  mutate(meses= if_else(fecha_hechos>="2016-06-02 00:00:00" & fecha_hechos<="2016-06-15 23:59:59", "SI",
                        if_else(fecha_hechos>="2017-06-01 00:00:00" & fecha_hechos<="2017-06-14 23:59:59", "SI",
                        if_else(fecha_hechos>="2018-05-30 00:00:00" & fecha_hechos<="2018-06-12 23:59:59", "SI",
                        if_else(fecha_hechos>="2019-05-29 00:00:00" & fecha_hechos<="2019-06-11 23:59:59", "SI", "NO")))))
incidentes <- incidentes %>%
  filter(meses=="SI")

incidentes <- incidentes %>% # tercer filtro dias de la semana
  mutate (dia_semana = as.numeric(format(incidentes$fecha_hechos, format = "%u")), #REGRESA EL NUMERO DE DIA DE LA SEMANA. EMPEZANDO POR LUNES=1
          hora_dia = format(incidentes$fecha_hechos, format="%H:%M:%S")) # REGRESA UNICAMENTE LAS HORAS DE FECHA_HECHOS
incidentes <- incidentes %>%
  filter(dia_semana!=6 & dia_semana!=7) %>% #QUITANDO LOS INCIDENTES EN FIN DE SEMANA.
  filter(hora_dia >= "07:30:00" & hora_dia <= "21:30:00") #TAMBIEN LOS DELITOS DE LAS MADRUGADAS. 

# FIGURE 1 
setwd("~/Tablet/descargas/Tesis/datos/Imagenes para pdf/10")
# HEATMAP DE INCIDENTES VIOLENTOS (TIPO 1) delitos violentos en el ambito publico 2016
dos16 <- filter(incidentes, ao_hechos=="2016")
# tipo 1 
(tipo1_2016<- ggmap(cdmx_map) +
    stat_density_2d (aes(longitud, latitud, fill = stat(level)), alpha=.1, bins = 25, geom = "polygon", data = filter(dos16, tipo=="TIPO 1"))
  + scale_fill_gradientn(colors=brewer.pal(7, "YlOrRd"))
  + xlab("")
  + ylab("")
  + labs(subtitle = "Delitos violentos 醡bito p鷅lico")
  + theme(legend.position="none"))
ggsave("hmap_publicos16.png", width = 5.66,
       height = 3.65,
       units = c("in"))

# tipo 2 
(tipo2_2016<- ggmap(cdmx_map) +
    stat_density_2d (aes(longitud, latitud, fill = stat(level)), alpha=.1, bins = 25, geom = "polygon", data = filter(dos16, tipo=="TIPO 2"))
  + scale_fill_gradientn(colors=brewer.pal(7, "YlOrRd"))
  + xlab("")
  + ylab("")
  + labs(subtitle = "Delitos violentos 醡bito privado")
  + theme(legend.position="none"))
ggsave("hmap_privados16.png", width = 5.66,
       height = 3.65,
       units = c("in"))

# homicidios
(hom_2016<- ggmap(cdmx_map) +
    stat_density_2d (aes(longitud, latitud, fill = stat(level)), alpha=.1, bins = 25, geom = "polygon", data = filter(dos16, categoria_delito=="HOMICIDIO DOLOSO"))
  + scale_fill_gradientn(colors=brewer.pal(7, "YlOrRd"))
  + xlab("")
  + ylab("")
  + labs(subtitle = "Homicidios")
  + theme(legend.position="none"))
ggsave("hmap_homicidios16.png", width = 5.66,
       height = 3.65,
       units = c("in"))

#2017
dos17 <- filter(incidentes, ao_hechos=="2017")
# tipo 1 
(tipo1_2017<- ggmap(cdmx_map) +
    stat_density_2d (aes(longitud, latitud, fill = stat(level)), alpha=.1, bins = 25, geom = "polygon", data = filter(dos17, tipo=="TIPO 1"))
  + scale_fill_gradientn(colors=brewer.pal(7, "YlOrRd"))
  + xlab("")
  + ylab("")
  + labs(subtitle = "Delitos violentos 醡bito p鷅lico")
  + theme(legend.position="none"))
ggsave("hmap_publicos17.png", width = 5.66,
       height = 3.65,
       units = c("in"))

# tipo 2 
(tipo2_2017<- ggmap(cdmx_map) +
    stat_density_2d (aes(longitud, latitud, fill = stat(level)), alpha=.1, bins = 25, geom = "polygon", data = filter(dos17, tipo=="TIPO 2"))
  + scale_fill_gradientn(colors=brewer.pal(7, "YlOrRd"))
  + xlab("")
  + ylab("")
  + labs(subtitle = "Delitos violentos 醡bito privado")
  + theme(legend.position="none"))
ggsave("hmap_privados17.png", width = 5.66,
       height = 3.65,
       units = c("in"))

# homicidios
(hom_2017<- ggmap(cdmx_map) +
    stat_density_2d (aes(longitud, latitud, fill = stat(level)), alpha=.1, bins = 25, geom = "polygon", data = filter(dos17, categoria_delito=="HOMICIDIO DOLOSO"))
  + scale_fill_gradientn(colors=brewer.pal(7, "YlOrRd"))
  + xlab("")
  + ylab("")
  + labs(subtitle = "Homicidios")
  + theme(legend.position="none"))
ggsave("hmap_homicidios17.png", width = 5.66,
       height = 3.65,
       units = c("in"))

# 2018
dos18 <- filter(incidentes, ao_hechos=="2018")
# tipo 1 
(tipo1_2018<- ggmap(cdmx_map) +
    stat_density_2d (aes(longitud, latitud, fill = stat(level)), alpha=.1, bins = 25, geom = "polygon", data = filter(dos18, tipo=="TIPO 1"))
  + scale_fill_gradientn(colors=brewer.pal(7, "YlOrRd"))
  + xlab("")
  + ylab("")
  + labs(subtitle = "Delitos violentos 醡bito p鷅lico")
  + theme(legend.position="none"))
ggsave("hmap_publicos18.png", width = 5.66,
       height = 3.65,
       units = c("in"))

# tipo 2 
(tipo2_2018<- ggmap(cdmx_map) +
    stat_density_2d (aes(longitud, latitud, fill = stat(level)), alpha=.1, bins = 25, geom = "polygon", data = filter(dos18, tipo=="TIPO 2"))
  + scale_fill_gradientn(colors=brewer.pal(7, "YlOrRd"))
  + xlab("")
  + ylab("")
  + labs(subtitle = "Delitos violentos 醡bito privado")
  + theme(legend.position="none"))
ggsave("hmap_privados18.png", width = 5.66,
       height = 3.65,
       units = c("in"))

# homicidios
(hom_2018<- ggmap(cdmx_map) +
    stat_density_2d (aes(longitud, latitud, fill = stat(level)), alpha=.1, bins = 25, geom = "polygon", data = filter(dos18, categoria_delito=="HOMICIDIO DOLOSO"))
  + scale_fill_gradientn(colors=brewer.pal(7, "YlOrRd"))
  + xlab("")
  + ylab("")
  + labs(subtitle = "Homicidios")
  + theme(legend.position="none"))
ggsave("hmap_homicidios18.png", width = 5.66,
       height = 3.65,
       units = c("in"))

# 2019
dos19 <- filter(incidentes, ao_hechos=="2019")
# tipo 1 
(tipo1_2019<- ggmap(cdmx_map) +
    stat_density_2d (aes(longitud, latitud, fill = stat(level)), alpha=.1, bins = 25, geom = "polygon", data = filter(dos19, tipo=="TIPO 1"))
  + scale_fill_gradientn(colors=brewer.pal(7, "YlOrRd"))
  + xlab("")
  + ylab("")
  + labs(subtitle = "Delitos violentos 醡bito p鷅lico")
  + theme(legend.position="none"))
ggsave("hmap_publicos19.png", width = 5.66,
       height = 3.65,
       units = c("in"))

# tipo 2 
(tipo2_2019<- ggmap(cdmx_map) +
    stat_density_2d (aes(longitud, latitud, fill = stat(level)), alpha=.1, bins = 25, geom = "polygon", data = filter(dos19, tipo=="TIPO 2"))
  + scale_fill_gradientn(colors=brewer.pal(7, "YlOrRd"))
  + xlab("")
  + ylab("")
  + labs(subtitle = "Delitos violentos 醡bito privado")
  + theme(legend.position="none"))
ggsave("hmap_privados19.png", width = 5.66,
       height = 3.65,
       units = c("in"))

# homicidios
(hom_2019<- ggmap(cdmx_map) +
    stat_density_2d (aes(longitud, latitud, fill = stat(level)), alpha=.1, bins = 25, geom = "polygon", data = filter(dos19, categoria_delito=="HOMICIDIO DOLOSO"))
  + scale_fill_gradientn(colors=brewer.pal(7, "YlOrRd"))
  + xlab("")
  + ylab("")
  + labs(subtitle = "Homicidios")
  + theme(legend.position="none"))
ggsave("hmap_homicidios19.png", width = 5.66,
       height = 3.65,
       units = c("in"))

# MAPAS DE LOS INDICES ----------------------------------------------------
ageb_cdmx2 <- readOGR ("CDMX AGEB" 
                      , layer="df_ageb_urbana") 
ageb_cdmx2<- st_as_sf(ageb_cdmx2)
#ageb_cdmx2 <- ageb_cdmx2 %>% dplyr::select(CVEGEO, geometry)
ageb_cdmx2 <- st_transform (ageb_cdmx2, CRS (crs_utm))
summary(listado_servicios) # HAY QUE CORRER PRIMERO LAS LINEAS DEL PANEL
listado_servicios <- listado_servicios %>%
  dplyr::rename(CVEGEO = folio_ageb)
listado_servicios$CVEGEO <- as.factor(listado_servicios$CVEGEO)
ageb_cdmx2 <- left_join(ageb_cdmx2, listado_servicios, by="CVEGEO") 

ageb_cdmx2$nombre_municipio <- as.factor(ageb_cdmx2$nombre_municipio)
sum_age <- ageb_cdmx2 %>%
  group_by(nombre_municipio) %>%
  summarize(mean1CP1 = mean(CP1, na.rm = T),
            mean2CP2 = mean(CP2, na.rm = T))
View(sum_age)            
mean(ageb_cdmx2$CP1, na.rm=T)

gcp1 <- ageb_cdmx2 %>% dplyr::select(CP1, geometry)
(pgcp1<- plot(gcp1, main="Indice de Carencias Educativas y de Ingresos"))

gcp2 <- ageb_cdmx2 %>% dplyr::select(CP2, geometry)
(pgcp2<- plot(gcp2, main="Indice de Carencias f韘icas en vivienda"))

contribuciones <- as.data.frame(res.var$contrib)
contribuciones <- rownames_to_column(contribuciones)
colnames(contribuciones)[1] <- "Variable CONEVAL"
contribuciones <- contribuciones %>% 
  dplyr::select(1,2,3)
View(contribuciones)
#write.csv (contribuciones,"C:\\Users\\Lenovo\\Documents\\Tablet\\descargas\\Tesis\\contribuciones.csv", row.names = FALSE)
contribuciones <- read.csv ("contribuciones.csv", sep=",", header=T)


# PARA AGREGAR HOMICIDIOS -------------------------------------------------
panel <- read.csv ("panel.csv", sep=",", header=T)
panel_horarios <- read.csv ("panel_horarios.csv", sep=",", header=T)
panel_homicidios <- read.csv ("panel_homicidios.csv", sep=",", header=T)
panel_horarios_homicidios <- read.csv ("panel_horarios_homicidios.csv", sep=",", header=T)

panel_homicidios <- panel_homicidios %>%
  dplyr::select(id_unico, year, inc_tipo1_d250_t90, 
                inc_tipo1_d500_t90,
                inc_tipo1_d1000_t90,
                inc_tipo1_d250_t29,
                inc_tipo1_d500_t29,
                inc_tipo1_d1000_t29,
                inc_tipo1_d250_t10,
                inc_tipo1_d500_t10,
                inc_tipo1_d1000_t10) %>%
  dplyr::rename(inc_tipo3_d250_t90 = inc_tipo1_d250_t90,
                inc_tipo3_d500_t90 = inc_tipo1_d500_t90,
                inc_tipo3_d1000_t90 = inc_tipo1_d1000_t90,
                inc_tipo3_d250_t29 = inc_tipo1_d250_t29,
                inc_tipo3_d500_t29 = inc_tipo1_d500_t29,
                inc_tipo3_d1000_t29 = inc_tipo1_d1000_t29,
                inc_tipo3_d250_t10 = inc_tipo1_d250_t10,
                inc_tipo3_d500_t10 = inc_tipo1_d500_t10,
                inc_tipo3_d1000_t10 = inc_tipo1_d1000_t10)

panel_horarios_homicidios <- panel_horarios_homicidios %>%
  dplyr::select(id_unico, year, inc_tipo1_d250_t90h, 
                inc_tipo1_d500_t90h,
                inc_tipo1_d1000_t90h,
                inc_tipo1_d250_t29h,
                inc_tipo1_d500_t29h,
                inc_tipo1_d1000_t29h,
                inc_tipo1_d250_t10h,
                inc_tipo1_d500_t10h,
                inc_tipo1_d1000_t10h) %>%
  dplyr::rename(inc_tipo3_d250_t90h = inc_tipo1_d250_t90h,
                inc_tipo3_d500_t90h = inc_tipo1_d500_t90h,
                inc_tipo3_d1000_t90h = inc_tipo1_d1000_t90h,
                inc_tipo3_d250_t29h = inc_tipo1_d250_t29h,
                inc_tipo3_d500_t29h = inc_tipo1_d500_t29h,
                inc_tipo3_d1000_t29h = inc_tipo1_d1000_t29h,
                inc_tipo3_d250_t10h = inc_tipo1_d250_t10h,
                inc_tipo3_d500_t10h = inc_tipo1_d500_t10h,
                inc_tipo3_d1000_t10h = inc_tipo1_d1000_t10h)

#panel <- left_join(panel, panel_homicidios, by=c("id_unico", "year"))
#panel_horarios <- left_join(panel_horarios, panel_horarios_homicidios, by=c("id_unico","year"))
#write.csv (panel,"C:\\Users\\Lenovo\\Documents\\Tablet\\descargas\\Tesis\\panel.csv", row.names = FALSE)
#write.csv (panel_horarios,"C:\\Users\\Lenovo\\Documents\\Tablet\\descargas\\Tesis\\panel_horarios.csv", row.names = FALSE)

# PANEL HORARIO STATS -----------------------------------------------------
panel <- read.csv ("panel.csv", sep=",", header=T)
panel_horarios <- read.csv ("panel_horarios.csv", sep=",", header=T)

prim16 <- panel_horarios %>%
  filter(nivel=="PRIMARIA") %>%
  filter(year=="2016")
View(secu19)
mean(prim16$inc_tipo1_d1000_t10h)
mean(prim16$inc_tipo2_d1000_t10h)
mean(prim16$inc_tipo3_d1000_t90h)

secu19 <- panel_horarios %>%
  filter(nivel=="SECUNDARIA") %>%
  filter(year=="2019")
mean(secu19$inc_tipo1_d500_t90h)
mean(secu19$inc_tipo2_d500_t90h)
mean(secu19$inc_tipo3_d500_t90h)

sm_secu16 <- secu16 %>%
  group_by(alcaldia) %>%
  summarize(tipo2_d250t29 = mean(inc_tipo2_d250_t29h, na.rm=T),
            tipo2_d500t29 = mean(inc_tipo2_d500_t29h, na.rm=T),
            tipo2_d1000t29 = mean(inc_tipo2_d1000_t29h, na.rm=T))

sm_secu17 <- secu17 %>%
  group_by(alcaldia) %>%
  summarize(tipo2_d250t29 = mean(inc_tipo2_d250_t29h, na.rm=T),
            tipo2_d500t29 = mean(inc_tipo2_d500_t29h, na.rm=T),
            tipo2_d1000t29 = mean(inc_tipo2_d1000_t29h, na.rm=T))

sm_secu19 <- secu19 %>%
  group_by(alcaldia) %>%
  summarize(tipo2_d250t29 = mean(inc_tipo2_d250_t29h, na.rm=T),
            tipo2_d500t29 = mean(inc_tipo2_d500_t29h, na.rm=T),
            tipo2_d1000t29 = mean(inc_tipo2_d1000_t29h, na.rm=T))

# STATS 2 FINANCIAMIENTO
prim18_publico <- panel_horarios %>%
  filter(nivel=="PRIMARIA") %>%
  filter(year=="2018") %>%
  filter(financiamiento=="PUBLICO")
mean(prim18_publico$inc_tipo1_d1000_t10h)
mean(prim18_publico$inc_tipo2_d1000_t10h)
mean(prim18_publico$inc_tipo3_d1000_t10h)

prim18_privado <- panel_horarios %>%
  filter(nivel=="PRIMARIA") %>%
  filter(year=="2018") %>%
  filter(financiamiento=="PRIVADO")
mean(prim18_privado$inc_tipo1_d1000_t10h)
mean(prim18_privado$inc_tipo2_d1000_t10h)
mean(prim18_privado$inc_tipo3_d1000_t10h)

### secundarias

secu19_publico <- panel_horarios %>%
  filter(nivel=="SECUNDARIA") %>%
  filter(year=="2019") %>%
  filter(financiamiento=="PUBLICO")
mean(secu19_publico$inc_tipo1_d1000_t10h)
mean(secu19_publico$inc_tipo2_d1000_t10h)
mean(secu19_publico$inc_tipo3_d1000_t10h)

secu19_privado <- panel_horarios %>%
  filter(nivel=="SECUNDARIA") %>%
  filter(year=="2019") %>%
  filter(financiamiento=="PRIVADO")
mean(secu19_privado$inc_tipo1_d1000_t10h)
mean(secu19_privado$inc_tipo2_d1000_t10h)
mean(secu19_privado$inc_tipo3_d1000_t10h)


# STATS 3 TURNO
prim18_matutino <- panel_horarios %>%
  filter(nivel=="PRIMARIA") %>%
  filter(year=="2018") %>%
  filter(clave_turno==1)
mean(prim18_matutino$inc_tipo1_d1000_t10h)
mean(prim18_matutino$inc_tipo2_d1000_t10h)
mean(prim18_matutino$inc_tipo3_d1000_t10h)

prim18_vespertino <- panel_horarios %>%
  filter(nivel=="PRIMARIA") %>%
  filter(year=="2018") %>%
  filter(clave_turno==2)
mean(prim18_vespertino$inc_tipo1_d1000_t10h)
mean(prim18_vespertino$inc_tipo2_d1000_t10h)
mean(prim18_vespertino$inc_tipo3_d1000_t10h)

#### secundarias 
secu19_matutino <- panel_horarios %>%
  filter(nivel=="SECUNDARIA") %>%
  filter(year=="2019") %>%
  filter(clave_turno==1)
mean(secu19_matutino$inc_tipo1_d1000_t10h)
mean(secu19_matutino$inc_tipo2_d1000_t10h)
mean(secu19_matutino$inc_tipo3_d1000_t10h)

secu19_vespertino <- panel_horarios %>%
  filter(nivel=="SECUNDARIA") %>%
  filter(year=="2019") %>%
  filter(clave_turno==2)
mean(secu19_vespertino$inc_tipo1_d1000_t10h)
mean(secu19_vespertino$inc_tipo2_d1000_t10h)
mean(secu19_vespertino$inc_tipo3_d1000_t10h)

# que delitos ocurren alrededor -------------------------------------------
escuela_incidente250 <- escuela_incidente250 %>%
  filter(tipo=="TIPO 1" | tipo=="TIPO 2")
incidentes <- escuela_incidente250
# 250, 90 dias
incidentes <- incidentes %>% # primer filtro 90 dias
  mutate(meses= if_else(fecha_hechos>="2016-01-19 00:00:00" & fecha_hechos<="2016-06-15 23:59:59", "SI",
                if_else(fecha_hechos>="2017-01-13 00:00:00" & fecha_hechos<="2017-06-14 23:59:59", "SI",
                if_else(fecha_hechos>="2018-01-12 00:00:00" & fecha_hechos<="2018-06-12 23:59:59", "SI",
                if_else(fecha_hechos>="2019-01-11 00:00:00" & fecha_hechos<="2019-06-11 23:59:59", "SI", "NO")))))
incidentes <- incidentes %>%
  filter(meses=="SI")
incidentes <- incidentes %>% # segundo filtro inhabiles
  mutate(inhabiles16 = if_else(fecha_hechos>="2016-01-29 00:00:00" & fecha_hechos<="2016-01-29 23:59:59", "SI",
                       if_else(fecha_hechos>="2016-02-01 00:00:00" & fecha_hechos<="2016-02-01 23:59:59", "SI",
                       if_else(fecha_hechos>="2016-02-26 00:00:00" & fecha_hechos<="2016-02-26 23:59:59", "SI",
                       if_else(fecha_hechos>="2016-03-21 00:00:00" & fecha_hechos<="2016-04-04 23:59:59", "SI", #SEMANA SANTA
                       if_else(fecha_hechos>="2016-04-29 00:00:00" & fecha_hechos<="2016-04-29 23:59:59", "SI",
                       if_else(fecha_hechos>="2016-05-01 00:00:00" & fecha_hechos<="2016-05-01 23:59:59", "SI",
                       if_else(fecha_hechos>="2016-05-05 00:00:00" & fecha_hechos<="2016-05-05 23:59:59", "SI",
                       if_else(fecha_hechos>="2016-05-27 00:00:00" & fecha_hechos<="2016-05-27 23:59:59", "SI", "NO")))))))),
         inhabiles17 = if_else(fecha_hechos>="2017-01-06 00:00:00" & fecha_hechos<="2017-01-06 23:59:59", "SI",
                       if_else(fecha_hechos>="2017-01-27 00:00:00" & fecha_hechos<="2017-01-27 23:59:59", "SI",
                       if_else(fecha_hechos>="2017-02-06 00:00:00" & fecha_hechos<="2017-02-06 23:59:59", "SI",
                       if_else(fecha_hechos>="2017-02-24 00:00:00" & fecha_hechos<="2017-02-24 23:59:59", "SI",
                       if_else(fecha_hechos>="2017-03-20 00:00:00" & fecha_hechos<="2017-03-20 23:59:59", "SI",
                       if_else(fecha_hechos>="2017-03-31 00:00:00" & fecha_hechos<="2017-03-31 23:59:59", "SI",
                       if_else(fecha_hechos>="2017-04-10 00:00:00" & fecha_hechos<="2017-04-21 23:59:59", "SI", #SEMANA SANTA
                       if_else(fecha_hechos>="2017-04-27 00:00:00" & fecha_hechos<="2017-04-27 23:59:59", "SI",
                       if_else(fecha_hechos>="2017-05-01 00:00:00" & fecha_hechos<="2017-05-01 23:59:59", "SI",
                       if_else(fecha_hechos>="2017-05-05 00:00:00" & fecha_hechos<="2017-05-05 23:59:59", "SI",
                       if_else(fecha_hechos>="2017-05-15 00:00:00" & fecha_hechos<="2017-05-15 23:59:59", "SI",
                       if_else(fecha_hechos>="2017-05-26 00:00:00" & fecha_hechos<="2017-05-26 23:59:59", "SI", "NO")))))))))))),
         inhabiles18 = if_else(fecha_hechos>="2018-01-26 00:00:00" & fecha_hechos<="2018-01-26 23:59:59", "SI",
                       if_else(fecha_hechos>="2018-02-05 00:00:00" & fecha_hechos<="2018-02-05 23:59:59", "SI",
                       if_else(fecha_hechos>="2018-02-23 00:00:00" & fecha_hechos<="2018-02-23 23:59:59", "SI",
                       if_else(fecha_hechos>="2018-03-19 00:00:00" & fecha_hechos<="2018-03-19 23:59:59", "SI",
                       if_else(fecha_hechos>="2018-03-26 00:00:00" & fecha_hechos<="2018-04-06 23:59:59", "SI", #SEMANA SANTA
                       if_else(fecha_hechos>="2018-05-01 00:00:00" & fecha_hechos<="2018-05-01 23:59:59", "SI",
                       if_else(fecha_hechos>="2018-05-15 00:00:00" & fecha_hechos<="2018-05-15 23:59:59", "SI",
                       if_else(fecha_hechos>="2018-05-25 00:00:00" & fecha_hechos<="2018-05-25 23:59:59", "SI","NO")))))))),
         inhabiles19 = if_else(fecha_hechos>="2019-01-25 00:00:00" & fecha_hechos<="2019-01-25 23:59:59", "SI",
                       if_else(fecha_hechos>="2019-02-04 00:00:00" & fecha_hechos<="2019-02-04 23:59:59", "SI",
                       if_else(fecha_hechos>="2019-03-01 00:00:00" & fecha_hechos<="2019-03-01 23:59:59", "SI",
                       if_else(fecha_hechos>="2019-03-18 00:00:00" & fecha_hechos<="2019-03-18 23:59:59", "SI",
                       if_else(fecha_hechos>="2019-04-05 00:00:00" & fecha_hechos<="2019-04-05 23:59:59", "SI",
                       if_else(fecha_hechos>="2019-04-15 00:00:00" & fecha_hechos<="2019-04-26 23:59:59", "SI", #SEMANA SANTA
                       if_else(fecha_hechos>="2019-05-01 00:00:00" & fecha_hechos<="2019-05-01 23:59:59", "SI",
                       if_else(fecha_hechos>="2019-05-15 00:00:00" & fecha_hechos<="2019-05-15 23:59:59", "SI",
                       if_else(fecha_hechos>="2019-05-24 00:00:00" & fecha_hechos<="2019-05-24 23:59:59", "SI", "NO"))))))))))
incidentes <- incidentes %>%
  filter(inhabiles16=="NO" & inhabiles17=="NO" & inhabiles18=="NO" & inhabiles19=="NO")

incidentes <- incidentes %>% # tercer filtro dias de la semana
  mutate (dia_semana = as.numeric(format(incidentes$fecha_hechos, format = "%u")), #REGRESA EL NUMERO DE DIA DE LA SEMANA. EMPEZANDO POR LUNES=1
          hora_dia = format(incidentes$fecha_hechos, format="%H:%M:%S")) # REGRESA UNICAMENTE LAS HORAS DE FECHA_HECHOS
incidentes <- incidentes %>%
  filter(dia_semana!=6 & dia_semana!=7) %>% #QUITANDO LOS INCIDENTES EN FIN DE SEMANA.
  filter(hora_dia >= "07:30:00" & hora_dia <= "21:30:00") #TAMBIEN LOS DELITOS DE LAS MADRUGADAS. 

# keeep de los horarios
incidentes_250t90h <- incidentes %>% 
  mutate(keep250_t90h =if_else(nivel=="PRIMARIA" & clave_turno == 1 & (hora_dia >= "07:30:00" & hora_dia <= "16:30:00"),"SI",  # URNO 1 ES MATUTINO
                       if_else(nivel=="PRIMARIA" & clave_turno == 2 & (hora_dia >= "13:30:00" & hora_dia <= "19:00:00"),"SI", #TURNO 2 VESPERTINO
                       if_else(nivel=="SECUNDARIA" & clave_turno == 1 & (hora_dia >= "06:30:00" & hora_dia <= "16:50:00"),"SI", # turno 1   
                       if_else(nivel=="SECUNDARIA" & clave_turno == 2 & (hora_dia >= "13:30:00" & hora_dia <= "21:30:00"),"SI","NO"))))) # TURNO 2
                        
incidentes_250t90h$keep250_t90h <- as.factor(incidentes_250t90h$keep250_t90h)
incidentes_250t90h <- incidentes_250t90h %>%
  filter(keep250_t90h=="SI") #CORTE DE ESCUELA INCIDENTES A T=90

# tipo 1 prim 18 
incidentes_250t90h_tipo1prim18 <- incidentes_250t90h %>%
  filter(tipo=="TIPO 1" & nivel=="PRIMARIA" & ao_hechos==2018)
summary(incidentes_250t90h_tipo1prim18$delito)

otros <- incidentes_250t90h_tipo1prim18 %>%
  group_by(delito) %>% count() %>% filter(n<111)

delitos_250t90h_tipo1prim18 <- incidentes_250t90h_tipo1prim18 %>%
  group_by(delito) %>%
  count() %>%
  filter(n>111) %>%
  arrange(desc(n)) %>%
  ungroup() %>%
  add_row(delito = "OTROS", n=sum(otros$n)) %>%
  mutate(delito=factor(delito, levels = delito))

  View(delitos_250t90h_tipo1prim18)

(p130t90prim18 <- ggplot(delitos_250t90h_tipo1prim18, aes(x=reorder (delito, desc(delito)), y=n))
  + geom_col(fill="#089C74")
  + geom_label(mapping = aes(label = n, hjust = -0.1),colour='darkgreen')
  + xlab("")
  + ylab("\nOcurrencia") 
  + scale_x_discrete(labels=function(x) stringr::str_wrap(x, width = 35))
  + scale_y_continuous(limits=c(0,2700), breaks=seq(0,2700, by = 500))
  + coord_flip()
  + theme_minimal())
ggsave("hola.png", width = 12.8,
       height = 5.25,
       units = c("in"))


# tipo 2 prim 18 
incidentes_250t90h_tipo2prim18 <- incidentes_250t90h %>%
  filter(tipo=="TIPO 2" & nivel=="PRIMARIA" & ao_hechos==2018)
summary(incidentes_250t90h_tipo2prim18$delito)

otros <- incidentes_250t90h_tipo2prim18 %>%
  group_by(delito) %>% count() %>% filter(n<=3)

delitos_250t90h_tipo2prim18 <- incidentes_250t90h_tipo2prim18 %>%
  group_by(delito) %>%
  count() %>%
  filter(n>=4) %>%
  arrange(desc(n)) %>%
  ungroup() %>%
  add_row(delito = "OTROS", n=sum(otros$n)) %>%
  mutate(delito=factor(delito, levels = delito))

(ptipo2_250_90_prim18 <- ggplot(delitos_250t90h_tipo2prim18, aes(x=reorder (delito, desc(delito)), y=n))
  + geom_col(fill="#8B008B")
  + geom_label(mapping = aes(label = n, hjust = -0.1),color='#8B008B')
  + xlab("")
  + ylab("\nOcurrencia") 
  + scale_x_discrete(labels=function(x) stringr::str_wrap(x, width = 35))
  + scale_y_continuous(limits=c(0,2700), breaks=seq(0,2700, by = 500))
  + coord_flip()
  + theme_minimal())
ggsave("delitos_tipo2_d250_t90prim18.png", width = 12.8,
       height = 5.25,
       units = c("in"))

# homicidios 
# tipo 3 prim 18 
incidentes_250t90h_tipo3prim18 <- incidentes_250t90h %>%
  filter(categoria_delito=="HOMICIDIO DOLOSO" & nivel=="PRIMARIA" & ao_hechos==2018)
summary(incidentes_250t90h_tipo3prim18$delito)

delitos_250t90h_tipo3prim18 <- incidentes_250t90h_tipo3prim18 %>%
  group_by(delito) %>%
  count() %>%
  arrange(desc(n)) %>%
  ungroup() %>%
  mutate(delito=factor(delito, levels = delito))

(ptipo3_250_90_prim18 <- ggplot(delitos_250t90h_tipo3prim18, aes(x=reorder (delito, desc(delito)), y=n))
  + geom_col(fill="#800A51")
  + geom_label(mapping = aes(label = n, hjust = -0.1),color='#800A51')
  + xlab("")
  + ylab("\nOcurrencia") 
  + scale_x_discrete(labels=c("FEMINICIDIO", "HOMICIDIOS\n(OTROS)", "HOMICIDIO POR\nARMA BLANCA", "HOMICIDIO\nPOR GOLPES", "HOMICIDIO POR\nARMA DE FUEGO"))
  + scale_y_continuous(limits=c(0,75), breaks=seq(0,75, by = 25))
  + coord_flip()
  + theme_minimal())
ggsave("delitos_tipo3_d250_t90prim18.png", width = 7.66,
       height = 3.65,
       units = c("in"))
