library(ggplot2)
library(dplyr)
library(hrbrthemes)
library(data.table)
library(readxl)
library(extrafont)
library(tidyr)
library(gridExtra)
library(lubridate)
font_import()
import_roboto_condensed()
#loadfonts(device="win")
setwd(choose.dir())
#work_dir <- getwd()

# Ergebnisse einlesen
sim_data <- read.table("results.csv",sep=";",header=TRUE,row.names=NULL)

# Untersuchte Parameter
names_param <- c("Balkenbreite","Balkenhöhe","Balkenlänge","Balkenabstand","Betonhöhe","Holzgüte","Betongüte") #,"h_t","b_t","h_c","n_v_x","t_v")
# Parameter die ausgewertet werden (Zeilen im Plot), muss Spaltenbezeichnung der Ergebnisse entsprechen
sim_param <- c("b_t","h_t","l_x","l_y","h_c","mat_t","mat_c")
# Nachweisnamen f?r Plot
names_eta <- c("ETA,sig,t,u,0","ETA,sig,t,u,3-7","ETA,sig,t,u,inf","ETA,sig,c,o,0","ETA,sig,c,o,3-7","ETA,sig,c,o,inf","ETA,tau,t,0","ETA,tau,t,3-7","ETA,tau,t,inf","ETA,V,v,0","ETA,V,v,3-7","ETA,V,v,inf","ETA,w,0","ETA,w,inf") # enth?lt NW-Bezeichnungen
# Liste der VBM, die unterschieden werden
names_con <- c("ASSY plus VG L=430 mit FT-Verb.","ASSY plus VG L=430 ohne FT-Verb.","VG-48-7,5x165","Kerve")
# Liste der Nachweise die geplottet werden k?nnen
nachweis <- c("sig_t","sig_c","tau_t","V_v","w")

######################################################################################
######################################################################################

# Funktion: Lollipop Ausnutzungsgrad der Nachweise
# Eingabe: 
#  - sim_data: Ergebnisdatei der Bemessung 
#  - names_par: Parameterbezeichnungen für Beschriftung
#  - sim_par: Spaltenüberschriften der Parameter für Auswertung
#  - nachweis: Art des untersuchten Nachweises (1 Eintrag) aus Array oben
#  - names_con: Genaue Bezeichnung des untersuchten VBM (1 Eintrag) aus Array oben

plot_eta_lp <- function(sim_data,names_par,sim_param,nachweis,names_con){
  
  
  data <-sim_data[which(sim_data$con_parameter %like% names_con),] # Subset für ein VBM 
  eta <- data.frame()     # Dataframe fuer Ergebnisdaten
  ref <- data[which(data$parameter %like% "Referenz"),]
  i   <- 1                # Laufvariable
  x_k <- 0                # x-Koordinate im Plot (vertikale Position)
  rep <- 2                # Zaehler fuer untersuchte Zeitpunkte 0..2
  eta_time <- c("t=0","t=3-7","t=inf")  # Label fuer Zeitpunkte
  #eta_col  <- c("darkmagenta","mediumorchid2","hotpink1")  # F?rbung des Plots f?r Zeitpunkte
  #eta_col  <- c("aquamarine","aquamarine2","aquamarine4")
  eta_col  <- c("darkgreen","chartreuse4","chartreuse")
  #eta_col  <- c("darkred","darkorange3","darkorange1")
  
  # Ueberschrift fuer Plot
  plot_title <- c("Nachweis der Zugspannung - UK Holz","Nachweis der Druckspannung - OK Beton",
                    "Nachweis der Schubspannung im Holz",
                    "Nachweis des Verbindungsmittels","Nachweis der Verformung")
  # Spaltenbezeichnungen der Nachweise in Ergebnisdatei - genaue Uebereinstimmung notwendig
  colname_eta <- c("t.eta_inst_gzt_ku_2_sep_sig_x_u","t.eta_t37_gzt_ku_2_sep_sig_x_u","t.eta_fin_gzt_ku_2_sep_sig_x_u",
                   "c.eta_inst_gzt_ku_2_sig_x_o","c.eta_t37_gzt_ku_2_sig_x_o","c.eta_fin_gzt_ku_2_sig_x_o",
                   "t.eta_inst_gzt_ku_2_tau","t.eta_t37_gzt_ku_2_tau","t.eta_fin_gzt_ku_2_tau",
                   "v.eta_inst_gzt_ku_2_Vy","v.eta_t37_gzt_ku_2_Vy","v.eta_fin_gzt_ku_2_Vy",
                   "g.eta_inst_gzg_ks_w","g.eta_fin_gzg_ks_w")
        #c("t.eta_inst_gzt_hgzt_2_sep_sig_x_u","t.eta_t37_gzt_hgzt_2_sep_sig_x_u","t.eta_fin_gzt_hgzt_2_sep_sig_x_u",
    #               "c.eta_inst_gzt_hgzt_2_sig_x_o","c.eta_t37_gzt_hgzt_2_sig_x_o","c.eta_fin_gzt_hgzt_2_sig_x_o",
    #               "t.eta_inst_gzt_hgzt_2_tau","t.eta_t37_gzt_hgzt_2_tau","t.eta_fin_gzt_hgzt_2_tau",
    #               "v.eta_inst_gzt_hgzt_2_Vy","v.eta_t37_gzt_hgzt_2_Vy","v.eta_fin_gzt_hgzt_2_Vy",
    #               "g.eta_inst_gzg_hgzg_w","g.eta_fin_gzg_hgzg_w")
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
      # Bemessungszeitpunkt
      eta[i+j,"eta_time"]  <- eta_time[j+1]
      # Ermittlung der mininalen und maximalen Ausnutzungsgrade
      eta[i+j,"eta_min"]   <- min(data[which(data$parameter %like% name),num_eta[j+1]],na.rm = FALSE)
      eta[i+j,"eta_max"]   <- max(data[which(data$parameter %like% name),num_eta[j+1]],na.rm = FALSE)
      # Ermittlung des zugeh?rgen Werts des Parameters
      eta[i+j,"param_min"] <- data$label[which(data$parameter %like% name & data[num_eta[j+1]] == min(data[which(data$parameter %like% name),num_eta[j+1]],na.rm = FALSE))]
      eta[i+j,"param_max"] <- data$label[which(data$parameter %like% name & data[num_eta[j+1]] == max(data[which(data$parameter %like% name),num_eta[j+1]],na.rm = FALSE))]
      # Hilfskoordinate fuer Plot: Vertikaler Abstand zwischen Linien
      eta[i+j,"x"]         <- x_k-j*0.25
      eta[i+j,"eta_col"]   <- eta_col[j+1]
      # Refernzwerte fuer Plot
      eta[i+j,"eta_ref"] <- ref[num_eta[j+1]]
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
  schrift <- "Arial"
  
  # Plot erstellung 
  if(names_con %like% "Kerve" & nachweis == "V_v"){
  plot <- ggplot(eta) + 
    geom_hline(aes(yintercept=min(eta_min,na.rm = FALSE)-0.2),color="white",alpha=(0),size=0.5) +
    geom_hline(aes(yintercept=max(eta_max,na.rm = FALSE)+0.2),color="white",alpha=(0),size=0.5) +
    geom_hline(aes(yintercept=1),color="red",size=0.5,linetype="dashed") +
    ggtitle(paste(plot_title[num_eta[4]])) + ylim(min(eta$eta_min,na.rm = FALSE)-2,max(eta$eta_max,na.rm = FALSE)+1) + xlim(-num,0) +
    geom_segment(aes(x=x, xend=x, y=eta_min, yend=eta_max), color=eta$eta_col,size=1) +
    geom_point(  aes(x=x, y=eta_min),    color=eta$eta_col, size=2 ) +
    geom_text(   aes(x=x, y=eta_min-0.025,label=param_min,family=schrift,adj=1),size=3) +
    geom_point(  aes(x=x, y=eta_max),    color=eta$eta_col, size=2 ) +
    geom_text(   aes(x=x, y=eta_max+0.03,label=param_max,family=schrift,adj=0),size=3) +
    geom_point(aes(x=x,y=eta_ref),color="darkgrey",size=1.5)+
    geom_vline(aes(xintercept=-1+0.25))+
    geom_vline(aes(xintercept=-2+0.25))+
    geom_vline(aes(xintercept=-3+0.25))+
    geom_vline(aes(xintercept=-4+0.25))+
    geom_vline(aes(xintercept=-5+0.25))+
    geom_vline(aes(xintercept=-6+0.25))+
    geom_vline(aes(xintercept=-7+0.25))+   # falls mehr Variablen
    geom_vline(aes(xintercept=-8+0.25))+
    coord_flip() +
    theme_ipsum() +
    theme(legend.position = "bottom",
          panel.grid.minor.y = element_blank(), 
          panel.grid.major.y = element_blank(),
          plot.title=element_text(face="plain",family=schrift,size=12),
          panel.border = element_rect(colour = "black", fill=NA, size=1)) +
    scale_x_continuous(name= "Parameter",breaks=seq(-num+0.75,0.25,1),labels = rev(names_par)) +
    scale_y_continuous(name ="Ausnutzung [-]" )
  } else if (!(names_con %like% "Kerve" & nachweis == "V_v")){
    plot <- ggplot(eta) + 
      geom_hline(aes(yintercept=min(eta_min,na.rm = FALSE)-0.2),color="white",alpha=(0), size=0.5) +
      geom_hline(aes(yintercept=max(eta_max,na.rm = FALSE)+0.2),color="white",alpha=(0),size=0.5) +
      geom_hline(aes(yintercept=1),color="red",size=0.5,linetype="dashed") +
      ggtitle(paste(plot_title[num_eta[4]])) + ylim(min(eta$eta_min,na.rm = FALSE)-2,max(eta$eta_max,na.rm = FALSE)+1) + xlim(-num,0) +
      geom_segment(aes(x=x, xend=x, y=eta_min, yend=eta_max), color=eta$eta_col,size=1) +
      geom_point(  aes(x=x, y=eta_min),    color=eta$eta_col, size=2 ) +
      geom_text(   aes(x=x, y=eta_min-0.02,label=param_min,family=schrift,adj=1),size=3) +
      geom_point(  aes(x=x, y=eta_max),    color=eta$eta_col, size=2 ) +
      geom_text(   aes(x=x, y=eta_max+0.03,label=param_max,family=schrift,adj=0),size=3) +
      geom_point(aes(x=x,y=eta_ref),color="darkgrey",size=1.5)+
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
      scale_x_continuous(name= "Parameter",breaks=seq(-num+0.75,0.25,1),labels = rev(names_par)) +
      scale_y_continuous(name ="Ausnutzung [-]" )
  }
  return(plot)
  }

######################################################################################
######################################################################################
# Ausgabe auf einer definierten Seite
# Multiplot
# Schrauben

print_date <- as.Date(Sys.Date(),format="%y%m%d")
names_param4 <- c("Balkenbreite","Balkenhöhe","Balkenlänge","Balkenabstand","Betonhöhe","Holzgüte","Betongüte", "Anzahl Kerven","Kerventiefe")
sim_param4 <- c("b_t","h_t","l_x","l_y","h_c","mat_t","mat_c","n_v_x","t_v")
for (i in 1:4) {
    plot1 <- plot_eta_lp(sim_data,names_param,sim_param,nachweis[1],names_con[i])
    plot2 <- plot_eta_lp(sim_data,names_param,sim_param,nachweis[2],names_con[i])
    plot3 <- plot_eta_lp(sim_data,names_param,sim_param,nachweis[3],names_con[i])
    if (i == 4){ 
      plot4 <- plot_eta_lp(sim_data,names_param4,sim_param4,nachweis[4],names_con[i])
    } else {
      plot4 <- plot_eta_lp(sim_data,names_param,sim_param,nachweis[4],names_con[i])
    }
    plot5 <- plot_eta_lp(sim_data,names_param,sim_param,nachweis[5],names_con[i])
  
  grid.arrange(plot1,plot2,plot3,plot4,plot5,ncol=2)
  ggsave(paste(print_date,"LolliPop_eta",names_con[i],sep="_",".png"),
         plot = grid.arrange(plot1,plot2,plot3,plot4,plot5,ncol=2),
         device="png",
         path=getwd(),
         width=320,
         height=464,
         units="mm",
         dpi=300,
         limitsize=FALSE)
}

######################################################################################
######################################################################################

# Funktion: Lollipop ungerissener Beton
# Eingabe: 
#  - sim_data: Ergebnisdatei der Bemessung 
#  - names_par: Parameterbezeichnungen für Beschriftung
#  - sim_par: Spaltenüberschriften der Parameter für Auswertung
#  - names_con: Genaue Bezeichnung des untersuchten VBM (1 Eintrag)

plot_riss_lp <- function(sim_data,names_par,sim_param,names_con){
  #sim_data[,"h_rel_inst"] <- sim_data$h_c_eff_inst_gzt / sim_data$h_c * 100
  #sim_data[,"h_rel_t37"] <-  sim_data$h_c_eff_t37_gzt  / sim_data$h_c * 100
  #sim_data[,"h_rel_fin"] <-  sim_data$h_c_eff_fin_gzt  / sim_data$h_c * 100
  
  sim_data[,"h_rel_gzt"] <- sim_data$h_c_eff_gzt / sim_data$h_c * 100
  sim_data[,"h_rel_gzg"] <- sim_data$h_c_eff_gzg / sim_data$h_c * 100
  
  data <-sim_data[which(sim_data$con_parameter %like% names_con),] # Subset für ein VBM 
  riss <- data.frame()     # Dataframe fuer Ergebnisdaten
  ref <- data[which(data$parameter %like% "Referenz"),]
  i   <- 1                # Laufvariable
  x_k <- 0                # x-Koordinate im Plot (vertikale Position)
  rep <- 1#2                # Zaehler fuer untersuchte Zeitpunkte 0..2
  riss_time <- c("t=0","t=3-7","t=inf")  # Label fuer Zeitpunkte
  #eta_col  <- c("darkmagenta","mediumorchid2","hotpink1")  # F?rbung des Plots f?r Zeitpunkte
  #eta_col  <- c("aquamarine","aquamarine2","aquamarine4")
  #eta_col  <- c("darkgreen","chartreuse4","chartreuse")
  riss_col  <- c("darkred","darkorange3","darkorange1")
  
  # Ueberschrift fuer Plot
  plot_title <- paste("Betonhöhe - ",names_con,sep="")
    
  # Spaltenbezeichnungen der Nachweise in Ergebnisdatei - genaue Uebereinstimmung notwendig
  #colname_riss <- c("h_rel_inst","h_rel_t37","h_rel_fin")
  colname_riss <- c("h_rel_gzt","h_rel_gzg")
  # Erstelle Listen mit Spaltennummern die fuer die Konfiguration betrachtet werden
  n_riss <- c()
  for (n in colname_riss){
    n_riss <- c(n_riss,match(n,colnames(sim_data))) # Spaltennummern der Nachweise bestimmen
  }
  
  # Filtern der Maximal und Minimawerte fuer Lollipop-Plot
  for(name in names_par){
    for(j in 0:rep){
      # Zeilenbeschriftung 
      riss[i+j,"name_par"]  <- name
      # Bemessungszeitpunkt
      riss[i+j,"riss_time"]  <- riss_time[j+1]
      # Ermittlung der mininalen und maximalen Ausnutzungsgrade
      riss[i+j,"riss_min"]   <- min(data[which(data$parameter %like% name),n_riss[j+1]],10000,na.rm = FALSE)
      riss[i+j,"riss_max"]   <- max(data[which(data$parameter %like% name),n_riss[j+1]],-10000,na.rm = FALSE)
      if(riss[i+j,"riss_min"] == riss[i+j,"riss_max"]){
        label_temp <- data[which(data$parameter %like% name),]
        riss[i+j,"param_min"] <- paste(label_temp$label[1],label_temp$label[2],sep=" - ")
        riss[i+j,"param_max"] <- "" 
      } else {
      # Ermittlung des zugeh?rgen Werts des Parameters
      riss[i+j,"param_min"] <- data$label[which(data$parameter %like% name & data[n_riss[j+1]] == min(data[which(data$parameter %like% name),n_riss[j+1]],10000,na.rm = FALSE))]
      riss[i+j,"param_max"] <- data$label[which(data$parameter %like% name & data[n_riss[j+1]] == max(data[which(data$parameter %like% name),n_riss[j+1]],-10000,na.rm = FALSE))]
      }
      # Hilfskoordinate fuer Plot: Vertikaler Abstand zwischen Linien
      riss[i+j,"x"]         <- x_k-j*0.33
      riss[i+j,"riss_col"]   <- riss_col[j+1]
      # Refernzwerte fuer Plot
      riss[i+j,"riss_ref"] <- ref[n_riss[j+1]]
      # Test: Welche Daten werden ausgelesen
      #print(riss[i+j,"param_min"])
      #print(riss[i+j,"riss_min"])
    }
    i <-i+rep+1   # Laufvariable f?r Nachweiszeitpunkte (bei GZG ist rep=2, bei GZT-Nachweisen rep=3)
    x_k <- x_k-1  # Laufvariable f?r Hilfskoordinate x
  }
  
  # Geometrische Hilfvariablen f?r Positionierung der grafischen Elemente
  num   <- length(names_par)
  c_num <- seq(0:num*3)
  
  #Schriftart
  schrift <- "Arial"
  
  # Plot erstellung
  if(names_con %like% "Kerve"){
  plot <- ggplot(riss) + 
    geom_hline(aes(yintercept=50),color="white",alpha=(0), size=0.5) +
    geom_hline(aes(yintercept=105),color="white",alpha=(0),size=0.5) +
    ggtitle(plot_title) + ylim(min(50,na.rm = FALSE)-2,max(120,na.rm = FALSE)+1) + xlim(-num,0) +
    geom_segment(aes(x=x, xend=x, y=riss_min, yend=riss_max), color=riss$riss_col,size=1) +
    geom_point(  aes(x=x, y=riss_min),    color=riss$riss_col, size=2 ) +
    geom_text(   aes(x=x, y=riss_min-1,label=param_min,family=schrift,adj=1),size=3) +
    geom_point(  aes(x=x, y=riss_max),    color=riss$riss_col, size=2 ) +
    geom_text(   aes(x=x, y=riss_max+1,label=param_max,family=schrift,adj=0),size=3) +
    geom_vline(aes(xintercept=-1+0.33))+
    geom_vline(aes(xintercept=-2+0.33))+
    geom_vline(aes(xintercept=-3+0.33))+
    geom_vline(aes(xintercept=-4+0.33))+
    geom_vline(aes(xintercept=-5+0.33))+
    geom_vline(aes(xintercept=-6+0.33))+
    geom_vline(aes(xintercept=-7+0.33))+   # falls mehr Variablen
    geom_vline(aes(xintercept=-8+0.33))+
    coord_flip() +
    theme_ipsum() +
    theme(legend.position = "bottom",
          panel.grid.minor.y = element_blank(), 
          panel.grid.major.y = element_blank(),
          plot.title=element_text(face="plain",family=schrift,size=12),
          panel.border = element_rect(colour = "black", fill=NA, size=1)) +
    scale_x_continuous(name= "Parameter",breaks=seq(-num+0.75,0.25,1),labels = rev(names_par)) +
    scale_y_continuous(name ="relative, ungerissene Betonhöhe [%]" )
  }
  else if (!(names_con %like% "Kerve")){
    plot <- ggplot(riss) + 
      geom_hline(aes(yintercept=50),color="white",alpha=(0), size=0.5) +
      geom_hline(aes(yintercept=105),color="white",alpha=(0),size=0.5) +
      ggtitle(plot_title) + ylim(min(50,na.rm = FALSE)-2,max(120,na.rm = FALSE)+1) + xlim(-num,0) +
      geom_segment(aes(x=x, xend=x, y=riss_min, yend=riss_max), color=riss$riss_col,size=1) +
      geom_point(  aes(x=x, y=riss_min),    color=riss$riss_col, size=2 ) +
      geom_text(   aes(x=x, y=riss_min-1,label=param_min,family=schrift,adj=1),size=3) +
      geom_point(  aes(x=x, y=riss_max),    color=riss$riss_col, size=2 ) +
      geom_text(   aes(x=x, y=riss_max+1,label=param_max,family=schrift,adj=0),size=3) +
      geom_vline(aes(xintercept=-1+0.33))+
      geom_vline(aes(xintercept=-2+0.33))+
      geom_vline(aes(xintercept=-3+0.33))+
      geom_vline(aes(xintercept=-4+0.33))+
      geom_vline(aes(xintercept=-5+0.33))+
      geom_vline(aes(xintercept=-6+0.33))+
      coord_flip() +
      theme_ipsum() +
      theme(legend.position = "bottom",
            panel.grid.minor.y = element_blank(), 
            panel.grid.major.y = element_blank(),
            plot.title=element_text(face="plain",family=schrift,size=12),
            panel.border = element_rect(colour = "black", fill=NA, size=1)) +
      scale_x_continuous(name= "Parameter",breaks=seq(-num+0.75,0.25,1),labels = rev(names_par)) +
      scale_y_continuous(name ="relative, ungerissene Betonhöhe [%]" )
  }
    return(plot)
}

######################################################################################
######################################################################################
# Ausgabe auf einer definierten Seite
# Multiplot
# Alle VBM
print_date <- as.Date(Sys.Date(),format="%y%m%d")
names_param4 <- c("Balkenbreite","Balkenhöhe","Balkenlänge","Balkenabstand","Betonhöhe","Holzgüte","Betongüte", "Anzahl Kerven","Kerventiefe")
sim_param4 <- c("b_t","h_t","l_x","l_y","h_c","mat_t","mat_c","n_v_x","t_v")
{
  plot1 <- plot_riss_lp(sim_data,names_param,sim_param,names_con[1])
  plot2 <- plot_riss_lp(sim_data,names_param,sim_param,names_con[2])
  plot3 <- plot_riss_lp(sim_data,names_param,sim_param,names_con[3])
  plot4 <- plot_riss_lp(sim_data,names_param4,sim_param4,names_con[4])
  
  grid.arrange(plot1,plot2,plot3,plot4,ncol=2)
  ggsave(paste(print_date,"LolliPop_Riss.png",sep="_"),
         plot = grid.arrange(plot1,plot2,plot3,plot4,ncol=2),
         device="png",
         path=getwd(),
         width=320,
         height=261,
         units="mm",
         dpi=300,
         limitsize=FALSE)
}
