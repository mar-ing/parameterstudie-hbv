library(ggplot2)
library(dplyr)
library(hrbrthemes)
library(data.table)
library(readxl)
library(extrafont)
library(tidyr)
library(gridExtra)
font_import()
import_roboto_condensed()
#loadfonts(device="win")
setwd(choose.dir())
#work_dir <- getwd()

# Ergebnisse einlesen
sim_data <- read.table("results.csv",sep=";",header=TRUE,row.names=NULL)

###########
# Test Code
#data <- NULL
#data <- sim_data[which(sim_data$con_parameter %like% "ASSY plus VG"),]

#nummer <- row_number(data[,which(data[,272]==min(data[which(data$parameter %like% "Balkenbreite"),272],na.rm = FALSE))])
# <- data$label[which(data[(data$parameter %like% "Balkenbreite") & (data[272]==min(data[which(data$parameter %like% "Balkenbreite"),272],na.rm = FALSE)),"Balkenbreite"])]
#label_i <- data$label[which(data$parameter %like% "Balkenbreite" & data[270] == min(data[which(data$parameter %like% "Balkenbreite"),270],na.rm = FALSE))]
#nummer <- match(,min(data[which(data$parameter %like% "Balkenbreite"),272],na.rm = FALSE))
#min(data[which(data$parameter %like% name),num_eta[j+1]],na.rm = FALSE)
#min(data[which(data$parameter %like% name),num_eta[j+1]],na.rm = FALSE)
#ref <- data[which(data$parameter %like% "Referenz"),]


###########


# Untersuchte Parameter
names_param <- c("Balkenbreite","Balkenhöhe","Balkenlänge","Betonhöhe","Holzgüte","Betongüte") #,"h_t","b_t","h_c","n_v_x","t_v")
# Parameter die ausgewertet werden (Zeilen im Plot), muss Spaltenbezeichnung der Ergebnisse entsprechen
sim_param <- c("b_t","h_t","l_x","h_c","mat_t","mat_c")
# Nachweisnamen f?r Plot
names_eta <- c("ETA,sig,t,u,0","ETA,sig,t,u,3-7","ETA,sig,t,u,inf","ETA,sig,c,o,0","ETA,sig,c,o,3-7","ETA,sig,c,o,inf","ETA,tau,t,0","ETA,tau,t,3-7","ETA,tau,t,inf","ETA,V,v,0","ETA,V,v,3-7","ETA,V,v,inf","ETA,w,0","ETA,w,inf") # enth?lt NW-Bezeichnungen
# Liste der VBM, die unterschieden werden
names_con <- c("ASSY plus VG L=430 mit FT-Verb.","ASSY plus VG L=430 ohne FT-Verb.","VG-48-7,5x165","Kerve")
# Liste der Nachweise die geplottet werden k?nnen
nachweis <- c("sig_t","sig_c","tau_t","V_v","w")

data <- sim_data[which(sim_data$con_parameter %like% "ASSY plus VG L=430 ohne FT-Verb."),] # Subset für ein VBM 
ref <- data[which(data$parameter %like% "Referenz"),]

plot_eta_lp <- function(sim_data,names_par,sim_param,nachweis,names_con){
  
  
  data <- sim_data[which(sim_data$con_parameter %like% names_con),] # Subset für ein VBM 
  eta <- data.frame()     # Dataframe fuer Ergebnisdaten
  ref <- data[which(data$parameter %like% "Referenz"),]
  i   <- 1                # Laufvariable
  x_k <- 0                # x-Koordinate im Plot (vertikale Position)
  rep <- 2                # Zaehler fuer untersuchte Zeitpunkte 0..2
  eta_time <- c("t=0","t=3-7","t=inf")  # Label fuer Zeitpunkte
  #eta_col  <- c("darkmagenta","mediumorchid2","hotpink1")  # F?rbung des Plots f?r Zeitpunkte
  #eta_col  <- c("aquamarine","aquamarine2","aquamarine4")
  eta_col  <- c("darkred","darkorange3","darkorange1")
  
  # Ueberschrift fuer Plot
  plot_title <- c("Nachweis der Zugspannung - UK Holz","Nachweis der Druckspannung - OK Beton",
                    "Nachweis der Schubspannung im Holz",
                    "Nachweis des Verbindungsmittels","Nachweis der Verformung")
  # Spaltenbezeichnungen der Nachweise in Ergebnisdatei - genaue Uebereinstimmung notwendig
  colname_eta <- c("t.eta_inst_gzt_hgzt_2_sep_sig_x_u","t.eta_t37_gzt_hgzt_2_sep_sig_x_u","t.eta_fin_gzt_hgzt_2_sep_sig_x_u",
                   "c.eta_inst_gzt_hgzt_2_sig_x_o","c.eta_t37_gzt_hgzt_2_sig_x_o","c.eta_fin_gzt_hgzt_2_sig_x_o",
                   "t.eta_inst_gzt_hgzt_2_tau","t.eta_t37_gzt_hgzt_2_tau","t.eta_fin_gzt_hgzt_2_tau",
                   "v.eta_inst_gzt_hgzt_2_Vy","v.eta_t37_gzt_hgzt_2_Vy","v.eta_fin_gzt_hgzt_2_Vy",
                   "g.eta_inst_gzg_hgzg_w","g.eta_fin_gzg_hgzg_w")
  # Erstelle Listen mit Spaltennummern die fuer die Konfiguration betrachtet werden
  n_eta <- c()
  for (n in colname_eta){
    n_eta <- c(n_eta,match(n,colnames(sim_data))) # Spaltennummern der Nachweise bestimmen
  }
  
  if(nachweis=="sig_t"){
    num_eta <- c(n_eta[1],n_eta[2],n_eta[3],1)
  }  else if(nachweis=="sig_c"){
    num_eta <- c(n_eta[4],n_eta[5],n_eta[6],2)
  } else if(nachweis=="tau_t"){
    num_eta <- c(n_eta[7],n_eta[8],n_eta[9],3)
  } else if(nachweis=="V_v"){
    num_eta <- c(n_eta[10],n_eta[11],n_eta[12],4)
  } else if(nachweis=="w"){
    num_eta <- c(n_eta[13],n_eta[14],0,5)
    rep <- 1
  } else return("Nachweisbezeichnung pruefen")
    
  # Filtern der Maximal und Minimawerte fuer Lollipop-Plot
  for(name in names_par){
    for(j in 0:rep){
      # Zeilenbeschriftung 
      eta[i+j,"name_par"]  <- name
      # Bemessungszwitpunkt
      eta[i+j,"eta_time"]  <- eta_time[j+1]
      # Ermittlung der mininalen und maximalen Ausnutzungsgrade
      eta[i+j,"eta_min"]   <- min(data[which(data$parameter %like% name),num_eta[j+1]],na.rm = FALSE)
      eta[i+j,"eta_max"]   <- max(data[which(data$parameter %like% name),num_eta[j+1]],na.rm = FALSE)
      # Ermittlung des zugeh?rgen Werts des Parameters
      eta[i+j,"param_min"] <- data$label[which(data$parameter %like% name & data[num_eta[j+1]] == min(data[which(data$parameter %like% name),num_eta[j+1]],na.rm = FALSE))]
      eta[i+j,"param_max"] <- data$label[which(data$parameter %like% name & data[num_eta[j+1]] == max(data[which(data$parameter %like% name),num_eta[j+1]],na.rm = FALSE))]
      #eta[i+j,"param_min"] <- min(data[(data$parameter %like% name) & (data[num_eta[j+1]]==min(data[which(data$parameter %like% name),num_eta[j+1]],na.rm = FALSE)),name])
      #eta[i+j,"param_max"] <- max(data[(data$parameter %like% name) & (data[num_eta[j+1]]==max(data[which(data$parameter %like% name),num_eta[j+1]],na.rm = FALSE)),name])
      # Hilfskoordinate fuer Plot: Vertikaler Abstand zwischen Linien
      eta[i+j,"x"]         <- x_k-j*0.25
      eta[i+j,"eta_col"]   <- eta_col[j+1]
      # Refernzwerte fuer Plot
      #eta[i+j,"eta_ref"] <- ref[num_eta[j+1]]
      # Test: Welche Daten werden ausgelesen
      print(eta[i+j,"param_min"])
      print(eta[i+j,"eta_min"])
      }
    i <-i+rep+1   # Laufvariable f?r Nachweiszeitpunkte (bei GZG ist rep=2, bei GZT-Nachweisen rep=3)
    x_k <- x_k-1  # Laufvariable f?r Hilfskoordinate x
  }

  # Geometrische Hilfvariablen f?r Positionierung der grafischen Elemente
  num   <- length(names_par)
  c_num <- seq(0:num*3)
  
  #Schriftart
  schrift <- "Times New Roman"
  
  # Plot erstellung 
  plot <- ggplot(eta) + 
    geom_hline(aes(yintercept=1),color="red",size=0.5,linetype="dashed") +
    ggtitle(paste(plot_title[num_eta[4]],names_con,sep=" ")) + ylim(min(eta$eta_min,na.rm = FALSE)-0.5,max(eta$eta_max,na.rm = FALSE)+0.5) + xlim(-num,0) +
    geom_segment(aes(x=x, xend=x, y=eta_min, yend=eta_max), color=eta$eta_col,size=1) +
    geom_point(  aes(x=x, y=eta_min),    color=eta$eta_col, size=2 ) +
    geom_text(   aes(x=x, y=eta_min-0.02,label=param_min,family=schrift,adj=1),size=3) +
    geom_point(  aes(x=x, y=eta_max),    color=eta$eta_col, size=2 ) +
    geom_text(   aes(x=x, y=eta_max+0.03,label=param_max,family=schrift,adj=0),size=3) +
    geom_point(aes(x=x,y=eta_ref),color="grey",size=1)+
    #geom_segment(aes(x=-c_num+0.25, xend=-c_num+0.25,y=0,yend=max(eta_max,na.rm = FALSE)+0.2),color="black",size=0.5) +
    geom_vline(aes(xintercept=0.25))+
    geom_vline(aes(xintercept=-1+0.25))+
    geom_vline(aes(xintercept=-2+0.25))+
    geom_vline(aes(xintercept=-3+0.25))+
    geom_vline(aes(xintercept=-4+0.25))+
    geom_vline(aes(xintercept=-5+0.25))+
    geom_vline(aes(xintercept=-6+0.25))+
    coord_flip() +
    theme_ipsum() +
    theme(legend.position = "bottom",
          panel.grid.minor.y = element_blank(), 
          panel.grid.major.y = element_blank(),
          plot.title=element_text(face="plain",family=schrift,size=12),
          panel.border = element_rect(colour = "black", fill=NA, size=1)) +
    scale_x_continuous(name= "Parameter",breaks=seq(-num+0.75,0,1),labels = rev(names_param)) +
    scale_y_continuous(name ="Ausnutzung [-]" )
  #for(po in 1:3){
  #  plot + geom_vline(aes(xintercept=-po))
   #plot <- plot + geom_segment(aes(x= 0.25-po,xend= 0.25-po,y=0,yend=max(eta_max,0.8,na.rm = FALSE)+0.2),color="black",size=0.5)
 # }
    
  ##    geom_segment(aes(x= 0.25,xend= 0.25,y=0,yend=max(eta_max,na.rm = FALSE)+0.2),color="black",size=0.5) +
return(plot)
  }

# Ergebnisausgabe sortiert nach Nachweisart (vgl. nachweis[x])
# Alle Plots in erzeugen und speichern
#plots<-data.frame()#"Nachweis","Verbindungsmittel","Plot")
#i<-1
#
#for(con in names_con){
#  for(nw in nachweis) {
#    #plots[i,"Nachweis"] <- nw
#    #plots[i,"Verbindungsmittel"] <- con
#    ##plots[i,"Plot"]<-plot_eta_lp(sim_data,names_param,sim_param,nw,con)
#    pp <- plot_eta_lp(sim_data,names_param,sim_param,nw,con)
#    pp
#    i <- i+1
#  }
#}
#
v_type <- 1
dataA <- plot_eta_lp(sim_data,names_param,sim_param,nachweis[1],names_con[v_type])
dataA
dataB <- plot_eta_lp(sim_data,names_param,sim_param,nachweis[2],names_con[v_type])
dataB 
dataC <- plot_eta_lp(sim_data,names_param,sim_param,nachweis[3],names_con[v_type])
dataC
dataD <- plot_eta_lp(sim_data,names_param,sim_param,nachweis[4],names_con[v_type])
dataD 
dataE <- plot_eta_lp(sim_data,names_param,sim_param,nachweis[5],names_con[v_type])
dataE

## Ausgabe auf einer definierten Seite
## Multiplot

grid.arrange(dataA,dataB,dataC,dataD,dataE,ncol=2)
ggsave(paste("LolliPop",names_con[v_type],sep="",".png"),
       plot = grid.arrange(dataA,dataB,dataC,dataD,dataE,ncol=2),
       device="png",
       path=getwd(),
       width=320,
       height=464,
       units="mm",
       dpi=300,
       limitsize=FALSE)
p

## Infobox f?r Plotseite
infobox <- function(){
  
}
param_text<-""
for(i in names_param){
param_text <- paste(param_text,i,":","\t","\t","\t","\t",sim_data$label[which(sim_data$parameter %like% i)],"\t","\t"," - ","\n")
#param_text <- cat("\t",param_text,i,":","\t","\t","von","\t","bis","\n")
}
infobox <- ggplot()+
  ggtitle("?bersicht")+
  geom_text(aes(x=0.5,y=0,label=paste("Referenz: ",sim_data$label[which(sim_data$parameter == "Referenz" && sim_data$con_parameter==names_con[v_type])],sep="")),hjust=0)+
  geom_text(aes(x=0.5,y=1,label=paste("Verbindung: ",names_con[v_type],sep="")),hjust=0)+
  geom_text(aes(x=0.5,y=0.5,label=param_text),hjust=0)+
  xlim(0.5,0.55)

infobox
# Effekte bestimmen

#fs <-data.frame()
#sim_data[,"h_real"] <- sim_data$h_c_eff_iter / sim_data$h_c


## Funktion Rissh?heniteration Beton
#h_c_i <- function(tol,zustand,h_c,h_c_eff){
#  #Beton gerissen?
#  if(zustand==T & h_c_eff==h_c){
#    h_c_eff_iter <- h_c_eff*0.5
#  else if(zustand==T){
#    h_c_eff_iter <-h_c_eff
#    h_c_eff_iter   
#    }
#    
#  }
#  
#}


# Plot mit Long Data

plot_eta_lp2 <- function(sim_data,names_par,sim_param,nachweis,names_con){
  
  
  data <- sim_data[which(sim_data$con_parameter %like% names_con),] # Teilliste mit genauem VBM erstellen
  eta <- data.frame()     # Dataframe f?r Ergebnisdaten
  ref <- data[which(data$parameter %like% "Referenz"),]
  i   <- 1                # Laufvariable
  x_k <- 0                # x-Koordinate im Plot (vertikale Position)
  rep <- 2                # Z?hler f?r untersuchte Zeitpunkte 0..2
  eta_time <- c("t=0","t=3-7","t=inf")  # Label f?r Zeitpunkte
  #eta_col  <- c("darkmagenta","mediumorchid2","hotpink1")  # F?rbung des Plots f?r Zeitpunkte
  #eta_col  <- c("aquamarine","aquamarine2","aquamarine4")
  eta_col  <- c("darkred","darkorange3","darkorange1")
  
  # ?berschrift f?r Plot
  plot_title <- c("Nachweis der Zugspannung - UK Holz","Nachweis der Druckspannung - OK Beton",
                  "Nachweis der Schubspannung im Holz",
                  "Nachweis des Verbindungsmittels","Nachweis der Verformung")
  # Spaltenbezeichnungen der Nachweise in Ergebnisdatei - genaue ?bereinstimmung notwendig
  colname_eta <- c("t.eta_inst_gzt_hgzt_2_sep_sig_x_u","t.eta_t37_gzt_hgzt_2_sep_sig_x_u","t.eta_fin_gzt_hgzt_2_sep_sig_x_u",
                   "c.eta_inst_gzt_hgzt_2_sig_x_o","c.eta_t37_gzt_hgzt_2_sig_x_o","c.eta_fin_gzt_hgzt_2_sig_x_o",
                   "t.eta_inst_gzt_hgzt_2_tau","t.eta_t37_gzt_hgzt_2_tau","t.eta_fin_gzt_hgzt_2_tau",
                   "v.eta_inst_gzt_hgzt_2_Vy","v.eta_t37_gzt_hgzt_2_Vy","v.eta_fin_gzt_hgzt_2_Vy",
                   "g.eta_inst_gzg_hgzg_w","g.eta_fin_gzg_hgzg_w")
  # Erstelle Listen mit Spaltennummern die f?r die Konfiguration betrachten werden
  n_eta <- c()
  for (n in colname_eta){
    n_eta <- c(n_eta,match(n,colnames(sim_data))) # Spaltennummern der Nachweise bestimmen
  }
  
  if(nachweis=="sig_t"){
    num_eta <- c(n_eta[1],n_eta[2],n_eta[3],1)
  }  else if(nachweis=="sig_c"){
    num_eta <- c(n_eta[4],n_eta[5],n_eta[6],2)
  } else if(nachweis=="tau_t"){
    num_eta <- c(n_eta[7],n_eta[8],n_eta[9],3)
  } else if(nachweis=="V_v"){
    num_eta <- c(n_eta[10],n_eta[11],n_eta[12],4)
  } else if(nachweis=="w"){
    num_eta <- c(n_eta[13],n_eta[14],0,5)
    rep <- 1
  } else return("Nachweisbezeichnung pr?fen")
  k <-1
  # Filtern der Maximal und Minimawerte f?r Lollipop-Plot
  for(name in names_par){
    for(j in 0:rep){
      # Spalte f?r Longdata
      eta[i+j,"subject"] <- k
      # Zeilenbeschriftung 
      eta[i+j,"name_par"]  <- name
      # Bemessungszwitpunkt
      eta[i+j,"eta_time"]  <- eta_time[j+1]
      # Ermittlung der minina?len und maximalen Ausnutzungsgrade
      eta[i+j,"eta_min"]   <- min(data[which(data$parameter %like% name),num_eta[j+1]],na.rm = FALSE)
      eta[i+j,"eta_max"]   <- max(data[which(data$parameter %like% name),num_eta[j+1]],na.rm = FALSE)
      # Ermittlung des zugeh?rgen Werts des Parameters
      eta[i+j,"param_min"] <- data$label[which(data$parameter %like% name & data[num_eta[j+1]] == min(data[which(data$parameter %like% name),num_eta[j+1]],na.rm = FALSE))]
      eta[i+j,"param_max"] <- data$label[which(data$parameter %like% name & data[num_eta[j+1]] == max(data[which(data$parameter %like% name),num_eta[j+1]],na.rm = FALSE))]
      #eta[i+j,"param_min"] <- min(data[(data$parameter %like% name) & (data[num_eta[j+1]]==min(data[which(data$parameter %like% name),num_eta[j+1]],na.rm = FALSE)),name])
      #eta[i+j,"param_max"] <- max(data[(data$parameter %like% name) & (data[num_eta[j+1]]==max(data[which(data$parameter %like% name),num_eta[j+1]],na.rm = FALSE)),name])
      # Hilfskoordinate f?r Plot: Vertikaler Abstand zwischen Linien
      eta[i+j,"x_ko"]         <- x_k-j*0.25
      eta[i+j,"eta_col"]   <- eta_col[j+1]
      # Refernzwerte f?r Plot
      eta[i+j,"eta_ref"] <- ref[num_eta[j+1]]
      # Test: Welche Daten werden ausgelesen
      print(eta[i+j,"param_min"])
      print(eta[i+j,"eta_min"])
      
      k<-k+1
    }
    i <-i+rep+1   # Laufvariable f?r Nachweiszeitpunkte (bei GZG ist rep=2, bei GZT-Nachweisen rep=3)
    x_k <- x_k-1  # Laufvariable f?r Hilfskoordinate x
  }
  
  # Geometrische Hilfvariablen f?r Positionierung der grafischen Elemente
  num   <- length(names_par)
  c_num <- seq(0:num)
  
  #Long-Data erstellen
  eta_long <- gather(eta,condition,measurement,name_par:eta_ref,factor_key=TRUE)
  
  # Plot erstellung 
  plot <- ggplot(eta_long,mapping = aes(x = x_ko, y = eta_min)) + 
#    geom_segment(aes(x=0.25,xend=-num+0.25,y=1,yend=1),color="red",size=0.5) +
#    ggtitle(plot_title[num_eta[4]]) + ylim(min(eta_min,na.rm = FALSE)-0.3,max(eta_max,na.rm = FALSE)+0.5) + xlim(-num-0.2,0.25) +
#    geom_segment(aes(x=x, xend=x, y=eta_min, yend=eta_max), color=eta_col,size=1) +
#    geom_point(  aes(x=x, y=eta_min),    color=eta$eta_col, size=2 ) +
#    geom_text(   aes(x=x, y=eta_min-0.02,label=param_min,family="Times New Roman",adj=1),size=3) +
#    geom_point(  aes(x=x, y=eta_max),    color=eta$eta_col, size=2 ) +
#    geom_text(   aes(x=x, y=eta_max+0.03,label=param_max,family="Times New Roman",adj=0),size=3) +
#    geom_point(aes(x=x,y=eta_ref),color="grey",size=1)+
#    #geom_segment(aes(x=-c_num+0.25, xend=-c_num+0.25,y=0,yend=max(eta_max,na.rm = FALSE)+0.2),color="black",size=0.5) +
    coord_flip() +
    theme_ipsum() +
    theme(
      legend.position = "bottom",
      panel.grid.minor.y = element_blank(), panel.grid.major.y = element_blank(),
      plot.title=element_text(face="plain",size=12,family="Times New Roman")
    ) +
    scale_x_continuous(name= "Parameter",breaks=seq(-num+0.75,0,1),labels = rev(names_param)) +
    scale_y_continuous(name ="Ausnutzung [-]" )
  #    geom_segment(aes(x= 0.25,xend= 0.25,y=0,yend=max(eta_max,na.rm = FALSE)+0.2),color="black",size=0.5) +
  #return(eta_long)
  return(plot)
}


df3 <- plot_eta_lp2(sim_data,names_param,sim_param,nachweis[1],names_con[v_type])
df3
