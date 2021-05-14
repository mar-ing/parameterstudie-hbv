#### LIBRARIES #####
require("readxl")
require("plotly")
require("zoo")
require("dplyr")
require("RColorBrewer")

library(readxl)
library(plotly)
library(zoo)
library(dplyr)
library(RColorBrewer)


library(readxl)
dtr <- function(x) {
  x <- x/180*pi
  return(x)
}
# Beton Festigkeit
get_concrete <- function(class_c,gamma_c,alpha_cc) {
  # Zylinderdruckfestigkeit/W?rfeldruckfestigkeit
  # fcck = Zylinderdruckfestigkeit
  c_class <- c('C12/15',	'C16/20',	'C20/25',	'C25/30',	'C30/37',	'C35/45',	'C40/50',	'C45/55',	'C50/60',	'C55/67',	'C60/75',	'C70/85',	'C80/95',	'C90/105')
  e_c_m <- round(c(27.0851770935882,	28.6079048949614,	29.9619510546403,	31.4758062100193,	32.8365680313308,	34.0771461991893,	35.2204622889344,	36.2831882189141,	37.2778690916147,	38.214206461639,	39.0998737080491,	40.742817784549,	42.2442381689358,	43.6305315006581),2)
  e_c_m <- e_c_m*1000
  f_c_c_m <- c(20,	24,	28,	33,	38,	43,	48,	53,	58,	63,	68,	78,	88,	98)
  f_c_t_k005 <- round(c(1.10071138556774,	1.33341688365329,	1.54729322942896,	1.79547474401053,	2.02752770767182,	2.24697370918667,	2.45617490009881,	2.65681289570051,	2.85013849742465,	2.95000553266105,	3.04831962080419,	3.22733155468249,	3.38705546034397,	3.53124646304918),2)
  f_c_t_m <- round(c(1.57244483652534,	1.90488126236184,	2.21041889918423,	2.56496392001504,	2.89646815381689,	3.20996244169524,	3.50882128585544,	3.79544699385787,	4.07162642489236,	4.21429361808721,	4.35474231543456,	4.61047364954642,	4.83865065763424,	5.04463780435597),2)
  f_c_c_k <- c(12,	16,	20,	25,	30,	35,	40,	45,	50,	55,	60,	70,	80,	90)

  c_mat <- data.frame(c_class,e_c_m,f_c_c_m,f_c_c_k,f_c_t_m,f_c_t_k005)

  i <- sapply(class_c,function(x) which(c_class == x))
  
  if(length(i)==0) {
    print("Festigkeitsklasse nicht gefunden!")
    return(data.frame(c_class=NA,e_c_m=NA,f_c_c_m=NA,f_c_c_k=NA,f_c_t_m=NA,f_c_t_k005=NA))
  } else {
    output <- c_mat[i,]
    output$f_c_c_d <- alpha_cc*output$f_c_c_k/gamma_c
    output$f_c_t_d <- alpha_cc*output$f_c_t_k005/gamma_c
    return(output)
  }
}

# Holz Festigkeit 
get_timber <- function(class_t,gamma_t,h_t) {
t_class <- c(paste("GL",seq(20,32,2),"h",sep=""),paste("GL",seq(20,32,2),"c",sep=""))
e_t_p <- c(8400,10500,11500,12100,12600,13600,14200,10400,10400,11000,12000,12500,13000,13500)
f_t_m_k <- rep(seq(20,32,2),2)
f_t_t0_k <- c(16,17.6,19.2,20.8,22.3,24,25.6,15,16,17,19,19.5,19.5,19.5)
f_t_c0_k <- c(seq(20,32,2),18.5,20,21.5,23.5,24,24.5,24.5)
rho_t_k <- c(340,370,385,405,425,430,440,355,355,365,385,390,390,400)
rho_t_mean <- c(370,410,420,445,460,480,490,390,390,400,420,420,430,440)
f_t_v_k <- rep(3.5,14)
k_cr <- round(2.5/f_t_v_k,2) # k_cr wird bei Bemessung angesetzt und nicht bei Berechnung der Festigkeit
if(h_t <= 0.6) { 
  kh <- min((0.6/h_t)^0.1,1.1) }
else if (h_t > 0.6) {kh <- 1}
#0.6 iweder in 600 ?nder!

t_mat <- data.frame(t_class,e_t_p,f_t_m_k,f_t_t0_k,f_t_c0_k,rho_t_k,rho_t_mean,f_t_v_k,k_cr)
i <- sapply(class_t,function(x) which(t_class == x))

if(length(i)==0) {
  print("Festigkeitsklasse nicht gefunden!")
  return(data.frame(t_class=NA,e_t_p=NA,f_t_m_k=NA,f_t_t0_k=NA,f_t_c0_k=NA,rho_t_k=NA,rho_t_mean=NA,f_t_v_k=NA,f_t_t0_d=NA,
                    f_t_c0_d=NA,f_t_m_d=NA,k_cr=NA))
} else {
output  <- t_mat[i,]
output$f_t_t0_d <- round(output$f_t_t0_k*kh/gamma_t,2)
output$f_t_c0_d <- round(output$f_t_c0_k/gamma_t,2)
output$f_t_v_d <- round(output$f_t_v_k/gamma_t,2)
output$f_t_m_d <- round(output$f_t_m_k*kh/gamma_t,2)

  return(output)
}

}

# Beton Kriechwerte
get_phi <- function(f_c_c_m,t0,t,rh,h_null) {
  alpha1 <- (35/f_c_c_m)^0.7
  alpha2 <- (35/f_c_c_m)^0.2
  alpha3 <- (35/f_c_c_m)^0.5
  beta_h <- ifelse(f_c_c_m<=35,min(1.5*(1+(0.012*rh)^18)*h_null+250,1500),min(1.5*(1+(0.012*rh)^18)*h_null+250*alpha3,1500*alpha3))
  beta_fcm <- 16.8/(sqrt(f_c_c_m))
  beta_t0 <- 1/(0.1+t0^(0.2))
  phi_rh <- ifelse(f_c_c_m<=35,1+(1-rh/100)/(0.1*h_null^(1/3)),alpha2*(1+(1-rh/100)/(0.1*h_null^(1/3))*alpha1))
  phi_0 <- phi_rh*beta_fcm*beta_t0
  beta_ctt0 <- ((t-t0)/(beta_h+t-t0))^0.3
  phi <-phi_0*beta_ctt0
  return(phi)
}

# Verbundkriechwerte
get_psi_comp<- function(gam1,phi_c,k_def) {
  psi <- data.frame()
  for(x in 1:length(gam1)) {
    
    if (k_def[x]==0.6 && phi_c[x]==3.5) {
      psi[x,"c_t37"] <- 2.5-gam1[x]^1.1
      psi[x,"c_tinf"] <- 2.6-0.8*gam1[x]^2
    }
    
    if (k_def[x]==0.8 && phi_c==3.5) {
      psi[x,"c_t37"] <- 2.2-0.8*gam1[x]^1.2
      psi[x,"c_tinf"] <- 2.3-0.5*gam1[x]^2.6
    }
    
    if (k_def[x]==0.6 && phi_c[x] ==2.5) {
      psi[x,"c_t37"] <- 1.9-0.6*gam1[x]^1.1
      psi[x,"c_tinf"] <- 2.0-0.5*gam1[x]^1.9
    }
    
    if (k_def[x]==0.8 && phi_c[x]==2.5) {
      psi[x,"c_t37"] <- 1.7-0.5*gam1[x]^1.1
      psi[x,"c_tinf"] <- 1.8-0.3*gam1[x]^2.5
    }
    
    psi[x,"t_t37"] <- 0.5
    psi[x,"t_tinf"] <- 1.0
    psi[x,"k_t37"] <- 0.65
    psi[x,"k_tinf"] <- 1.0
    
  }
  return(psi)
} # arbeitet mit einem dataframe

# Kombbinationsbeiwerte
get_psi <- function(q_cat) {

psi_0 <- c(0.7,0.7,0.7,0.7,1.0)
psi_1 <- c(0.5,0.5,0.6,0.6,0.8)
psi_2 <- c(0.3,0.3,0.6,0.6,0.9)
cat <- c("Q_A","Q_B","Q_C","Q_D","Q_E")

psi <- data.frame(cat,psi_0,psi_1,psi_2)
i <- sapply(q_cat,function(x) which(psi[,"cat"]==x))

if(length(i)==0) {
  print("Kategorie nicht gefunden!")
  return(data.frame(psi_0=NA,psi_1=NA,psi_2=NA))
} else {
  output <- psi[i,]
  output <- output[,c("psi_0","psi_1","psi_2")]
  return(output)
}
}

#### VERBINDUNGSMITTEL TRAGFAEHIGKEIT ####
sofistify_vm <- function(vm_vals,connection) {
  names_screws <- c("schrauben","Schrauben","Screws","screws","screw","screwed","Screw","schraube","schrauben")
  names_notches <- c("Nocken","Kerven","notches","Notches","nocken","kerven","Kerve","kerve","nocke","nocken","notch","notched","Notch","Notched")
  names_mech_con <- c(names_notches,names_screws)
  names_glue <- c("klebung","glue","kleb","glued","Klebung","geklebt","rigid","starr")
  if(connection %in% names_glue) {out <- data.frame(k_s_inst=NA,k_u_inst=NA,a_v=NA)} else {
    k_s_pm <- c(vm_vals$k_s_pm,vm_vals$k_s_pm) # Wiederholen der Werte  fuer die zweite Traegerhaelfte, Sofistik sortiert die Werte um, sodass sie gespiegelt vorliegen
    x_v_eff <- c(vm_vals$x_v_eff,vm_vals$x_v_eff)# Wiederholen der Werte  fuer die zweite Traegeraelfte, Sofistik sortiert die Werte um, sodass sie gespiegelt vorliegen
    n_v <- c(vm_vals$n_v,vm_vals$n_v)# Wiederholen der Werte  fuer die zweite Traegerhaelfte, Sofistik sortiert die Werte um, sodass sie gespiegelt vorliegen
    l_v <- c(vm_vals$l_v,vm_vals$l_v) # Wiederholen der Werte  fuer die zweite Traegerhaelfte, Sofistik sortiert die Werte um, sodass sie gespiegelt vorliegen
    # ks muss fuer Kerven pro m Kervenbreite angegeben werden, fuer Schrauben ist ks abhaengig von Schraubenlaenge anzugeben
    # Einheit ks/m ist kN/mm/m -> Wuerth FT => k_s_pm = 45 mit x_eff in m ergibt kN/mm/Schraube 
    ks <- round(k_s_pm*x_v_eff*n_v,1) # x_eff = Breite der Kerve oder Einbindelaenge der Schraube, n_v = Anzahl Schrauben/Kerven
    if(connection %in% names_screws) {
      ku <- round(2/3*ks,1)
    } else {
      ku <- ks
    }
    
    # Zur Verarbeitung in Sofistik muessen die Werte mit einem Leerzeichen getrennt in der Tabelle stehen "als Text"
    k_s_inst <- paste(ks,collapse=",")
    k_u_inst <- paste(ku,collapse=",")
    #a_v <- c(vm_vals$a_v,vm_vals$a_v)
    a_v <- paste(na.omit(vm_vals$a_v),collapse=",")
    # Wiederholen der Werte  fuer die zweite Traegerhaelfte, Sofistik sortiert die Werte um, sodass sie gespiegelt vorliegen
    
    out <- data.frame(k_s_inst,k_u_inst,a_v)
  }
  return(out)
}
get_nettodist <- function(vm_vals,connection) {
  if (connection %in% names_glue) {a_v_net <- NA} else {
    if(connection %in% names_notches) {
      l_v_substract <- c(0,vm_vals$l_v[1:(length(vm_vals$a_v)-1)])
      a_v_substract <- c(0,vm_vals$a_v[1:(length(vm_vals$a_v)-1)])
      a_v_net <- vm_vals$a_v[1:(length(vm_vals$a_v))]-l_v_substract-a_v_substract
      #a_v_net[1] <- a_v[[x]][1]
      #a_v_net[length(a_v_net)] <- a_v[[x]][length(a_v_net)]
    } else {
      a_v_substract <- c(0,vm_vals$a_v[1:(length(vm_vals$a_v)-1)])
      a_v_net <- vm_vals$a_v[1:(length(vm_vals$a_v))]-a_v_substract
    }
    #a_v_net <- lapply(seq_along(a_v_net), function(x) c(a_v_net[[x]],a_v_net[[x]]))
  }
  return(a_v_net)
} 
get_f_v_r_k_notch <- function(b_n,t_n,l_v,a_v,f_t_c0_d,f_t_v_d,f_c_c_k,f_c_c_d,k_cr,h_c) { 
  # L?ngen in mm und Festigkeitswerte in MPA bzw N/mm?
  
  # nach Technical Spec
  nu <- (0.6*(1-f_c_c_k*1/250))
  min_teta <- max(atan(0.5*(h_c+t_n)/(l_v+a_v)),atan(t_n/l_v))
  # Grenzwerte f?r Teta nach EN 1992-1-1 6.2.3 (2) = 1 < cot teta < 2.5 [rad]
  min_teta_1992 <- atan(1/2.5)
  max_teta_1992 <- atan(1/1)
  #teta <- ifelse(1/tan(teta)<=1,atan(1),ifelse(1/tan(teta)>=2.5,atan(1/2.5),teta))
  teta <- min_teta
  #Widerstand in [kN]
  t_c <- round(t_n*f_t_c0_d*b_n*1/1000,2)
  t_v <- round(k_cr*min(t_n*8,a_v)*b_n*f_t_v_d*1/1000,2)
  c_c <- round(t_n*b_n*f_c_c_d*1/1000,2)
  f_c_v_d <- nu*f_c_c_d/(1/tan(teta)+tan(teta))
  c_v <- round(f_c_v_d*b_n*l_v*1/1000,2)
  min <- mapply(function(x) min(t_c[x],t_v[x],c_c[x],c_v[x]),x=seq_along(b_n)) 
  
  f_v_r_k <- data.frame(t_c=t_c,c_c=c_c,c_v=c_v,t_v=t_v,min_rd=min,teta=teta,f_c_v_d=f_c_v_d)
  return(f_v_r_k)
} 
get_f_v_r_k_screw <- function(l_ef,d,f_t_c0_k,f_t_v_k,f_c_c_k,f_v_tens_k,f_v_ax_k,alpha_v,rho_t,mu_v,gam_v) {
  # L?ngen in mm und Festigkeiten in N/mm? bzw MPa, rho in kg/m?, alpha in Grad
  if(missing(gam_v)) {gam_v <- 1.25}
  if(missing(rho_t)) {rho_t <- 350}
  if(missing(f_v_ax_k)) {f_v_ax_k <- 0.52*d^(-0.5)*l_ef^(-0.1)*rho_t^0.8}
  if(missing(mu_v)) {mu_v <- 0}
  if(missing(f_t_c0_k)) {f_t_c0_k <- 0}
  if(missing(f_t_v_k)) {f_t_v_k <- 0}
  if(missing(f_c_c_k)) {f_c_c_k <- 0}
  
  ax <- l_ef*d*f_v_ax_k/(1.2*cos(dtr(alpha_v))^2+sin(dtr(alpha_v))^2)*(rho_t/350)^0.8*1/1000 
  tens <- rep(f_v_tens_k,length(ax))
  
  
  min_net <- mapply(function(x) min(ax[x],tens[x]),x=seq_along(l_ef)) 
  min <- (cos(dtr(alpha_v))+mu_v*sin(dtr(alpha_v)))*min_net
  min_rd <- min/gam_v
  f_v_r_k <- data.frame(ax=ax,tens=tens,min_net=min_net,min=min,min_rd=min_rd)
  return(f_v_r_k)
}

#### Schwinden ##### 
get_shrink <- function(rh,CEM,f_c_c_m,f_c_c_k,h_null,tt,ts) {
  eps <- data.frame()
  for(x in 1:length(h_null)) {
    
    eps[x,"beta_rh"] <- 1.55*(1-(rh[x]/100)^3)
    eps[x,"alpha_ds1"] <- ifelse(CEM[x]=="S",3,ifelse(CEM[x]=="N",4,6))
    eps[x,"alpha_ds2"] <- ifelse(CEM[x]=="S",0.13,ifelse(CEM[x]=="N",0.12,0.11))
    eps[x,"eps_cd_0"] <- 0.85*((220+110*eps[x,"alpha_ds1"])*exp(-eps[x,"alpha_ds2"]*f_c_c_m[x]/10))*10^(-6)*eps[x,"beta_rh"]
    kh_h_null <- cbind(c(100,200,300,500),c(1,0.85,0.75,0.7))
    if (h_null[x]<100)  {eps[,"kh"] <- 1}
    if (h_null[x]>=500) {eps[,"kh"] <- 0.7}
    if (h_null[x]>100 && h_null[x]<500) {
      i <- which(kh_h_null[,1]<=h_null[x])[1]
      eps[x,"kh"] <- kh_h_null[i,2]+(kh_h_null[i+1,2]-kh_h_null[i,2])/(kh_h_null[i+1,1]-kh_h_null[i,1])*(h_null[x]-kh_h_null[i,1])
    }
    
    eps[x,"eps_cd_inf"]<- signif(eps[x,"eps_cd_0"]*eps[x,"kh"],2)
    eps[x,"eps_ca_inf"] <- signif(2.5*(f_c_c_k[x]-10)*10^-6,2)
    if(missing(tt)==FALSE) {
      ## tt betrachteter Zeitpunkt
      ## ts Beginn Trocknungsschwinden (Alter Ende Nachbehandlung)
      eps[,"beta_dst"] <- (tt[x]-ts[x])/((tt[x]-ts[x])+0.04*sqrt(h_null[x]^3))
      eps[x,"eps_cd_t"] <- signif(eps[x,"eps_cd_0"]*eps[,"kh"]*eps[x,"beta_dst"],2)
      
      epa[,"beta_ast"] <- 1-exp(-0.2*sqrt(tt[x])) # nach etwa 10 Tagen zur H?lfte abgeschlossen, nach etwa 1-2 Jahren abgeschlossen
      eps[x,"eps_ca_t"] <- signif(eps[x,"eps_ca_inf"]*eps[x,"beta_ast"],2)
      eps[x,"eps_cs_t"] <- signif(eps[x,"eps_ca_t"]+eps[x,"eps_cd_t"],2)
    } 
    eps[x,"eps_cs_inf"] <- signif(eps[x,"eps_cd_inf"]+eps[x,"eps_ca_inf"],2)
    eps[x,"eps_cd_0"] <- signif(eps[x,"eps_cd_0"])
  }
  return(eps)
} 

# Mitwirkende Plattenbreite
get_beff <- function(b_0,b_1,b_2,l_e) {
  if(missing(b_2)) {b_2 <- b_1}
b_e_1 <- min(b_1,l_e/8)
b_e_2 <- min(b_2,l_e/8)
b_e <- b_0+b_e_1+b_e_2
return(b_e)
}
# Querschnittswerte Rechteck
get_qs <- function(h,b,suffix) {
  if(missing(suffix)) {suffix <- NA}
  a <- h*b
  wy <- h^2*b/6
  iy <- h^3*b/12
  
  qs <- data.frame(a,wy,iy)
  if(is.na(suffix)) {} else {
  colnames(qs) <- paste(colnames(qs),suffix,sep="_")
  }
  return(qs)
}

# Einlesen und Auslesen
# zum erzegeugen der Eingabedatei hilfreich , ersetzt Variablen in einem Data-Frame
df_replace <- function(df,rows,cols,values) {
  for(i in seq_along(cols)) {
    df[rows[i],cols[i]] <- values[[i]] # nur Liste unterstützt verschiedene Datentypen
  }
  return(df)
}


# Template laden und in Einzelteile splitten
load_template <- function(template_names) {
  template_path <- paste("./templates/",template_names,".txt",sep="")
  template_list <- lapply(template_path,readLines)
  
  names(template_list) <- template_names
  template <- template_list
  
  # Aufteilen der Template beim Kennwort $CUT
  # Erzeugt Liste mit Sub-Liste fÃ¼r jede Sub-Template
  template <- lapply(template,function(x) split(x[x!="$CUT"],cumsum(x=="$CUT")[x!="$CUT"]))
  
  # Liest die Namen der Sub-Templates aus
  # Dazu muss hinter dem Kennwort $Cut in einer neuen Zeile der Kapitelname in Sofistik definiert sein
  # Die Sub-Template heisst dann wie das Kapitel in Sofistik
  template_subnames <- lapply(template,function(x) lapply(x,function(y) y[1]))
  template_subnames1 <- lapply(template_subnames,function(x) lapply(x,function(y) gsub("!#!","",y)))
  template_subnames2 <- lapply(template_subnames1,unlist)
  
  # Umbenennen der Sub-Templates, muss in schleife geschehen, da sonst der Ã¼bergeordnete Name verloren geht
  for(x in seq_along(template)) {
    names(template[[template_names[x]]]) <- template_subnames2[[x]]
  }
  
  return(template)
}

# Teddyfile und Pfade erzeugen; Bei write kann entschieden werden, ob bestehende Datei überschrieben werden sollird
make_teddyfile <- function(input,templates,zust,n_dirs,write,subfolder) {
  if(missing(subfolder)) {subfolder <- ""}
  if(missing(n_dirs)) {n_dirs <- seq_along(input[,1])}
  if(missing(write)) {write <- FALSE}
  if(isTRUE(write)) {
    if(missing(templates)) {
      print("Template fehlt!")
      }
    else{

  # Erstellt den Block mit Parameterdefinitionen
  inputlist <- split(input, seq(nrow(input)))
  define <- "#define"
  define <- paste("#define",colnames(input), sep=" ")
  teddyfile <- lapply(inputlist,function(x) paste(define,x,sep="="))
  
  # Hängt die zugehörige Sub-Template an
  teddyfile <- lapply(seq_along(teddyfile),function(x) lapply(zust[[x]],function(y) append(teddyfile[[x]],templates[[input[x,"template"]]][[y]]))) #SPEICHTER NUR DIE ITERATIONSDATEI
  #Single-template
  #teddyfile <- lapply(seq_along(teddyfile),function(x) append(teddyfile[[x]],get(parameters[x,"template"],envir = template)))
  #teddytitl <- mapply(function(x) paste(clippi,"/",titl_i[x],"/",titl_i[x],".dat",sep=""),x=seq_along(teddyfile))
  
  #Suffix
  #indexes <- seq(1,nrow(input),1)
  #temp_titl <-if(exists("titl_suf")) {if(nchar(titl_suf)>=1) {paste(titl,indexes,titl_suf,".dat",sep="_")} else
  #{paste(titl,indexes,sep="_")}} else {paste(titl,indexes,sep="_")}
  
  #zustand <- c("iter")
    teddyfile <- unlist(teddyfile,recursive=FALSE)
    }
  }
  teddytitl <- unlist(lapply(seq_along(zust),function(x) mapply(function(y) paste(subfolder,"/",titl,"_",n_dirs[x],"/",titl,"_",n_dirs[x],"_",zust[[x]][y],".dat",sep=""),y=seq_along(zust[[x]])))) 

  
  for(i in seq_along(teddytitl)) {
    if(isTRUE(write)) {
      if(file.exists(teddytitl[i])) {
        write.table(teddyfile[[i]],teddytitl[i],quote=FALSE,row.names=FALSE,col.names=FALSE) 
        print("Datei erfolgreich überschrieben.")
      } else {
        write.table(teddyfile[[i]],teddytitl[i],quote=FALSE,row.names=FALSE,col.names=FALSE)   
        print("Datei erfolgreich gespeichert.")
      }} else {
        if(file.exists(teddytitl[i])) {
          print("Datei existiert bereits! Keine neue Datei wurde gespeichert.")
        } else {
          print("Datei existiert noch nicht. Nur Pfade werden ausgegeben.")
        }
      }
  }
  return(teddytitl)
  #mapply(function(x) dir.create(file.path(paste(getwd(),paste("/",titl_i[x],sep=""),sep="/"))),x=seq_along(teddyfile)) 
  #mapply(write.table,teddyfile,teddytitl,quote=FALSE,row.names=FALSE,col.names=FALSE)
}

einlesen_sofi_mult_folders_to_list <- function(path,proj_name,filenames,n_dirs) {
  # Erzeugt folgenden Pfad: proj-Ordnern/"projekt_name_i"/"projektname_i_dateiname_j
  # Filenames ist eine Liste, die die entsprechenden Dateinamen für jeden Ordner enthält
  # Proj_name ist ein character mit dem Namen des übergeordneten Projekt-Ordners
  # n_dirs ist ein Vektor, der den Zähler jeder einzulesenden Datei enthält
  # Das ist so gedacht, weil ggf. gewisse Dateien verworfen werden und nicht eingelesen sollen werden
  full_path <- lapply(seq_along(n_dirs),function(x) mapply(function(y) paste(path,"/",proj_name,"_",n_dirs[x],"/",proj_name,"_",n_dirs[x],"_",y,".xlsx",sep=""),y=filenames[[x]])) 
  
  # Funktion über alle Elemente der Liste Fullpath (Länge Fullpath=Anzahl einzulesender Ordner) und innerhalb dessen über alle Einzelnen Dateien des Ordnerns
  sofi <- lapply(full_path,function(x) lapply(x,function(y) {
    # Abfrage ob Datei existiert und Information, wenn sie es tut und wenn nicht
    # Einlesen der Datei, wenn sie existiert, keine Aktion, wenn nicht (erzeugt NA)
    if(file.exists(y)) {temp <- as.data.frame(read_xlsx(y,sheet="sofisheet"))
    print(paste("Datei",y,"erfolgreich eingelesen!",sep=" "))
    return(temp)
    } else {print(paste("Datei",y,"existiert nicht!",sep=" "))
      return(NA)}
    }))
  # Wie zuvor genestete Funktion über alle Ordner und deren Dateien
  sofi <- lapply(seq_along(sofi),function(x) lapply(sofi[[x]],function(y) {
    #Wenn der erzeugte Data Frame nicht NA ist, werden die Spaltennamen vereinfacht
    if(is.na(y)[1]) {} 
    else {
      columns <- colnames(y)
      org <- c("X [m]","LF-Name","N [kN]","VZ [kN]","MY [kNm]","MZ [kNm]","SIGO [MPa]","SIGU [MPa]","TAU [MPa]","VY [kN]","sxu [MPa]","sxo [MPa]","syu [MPa]","syo [MPa]","Y [m]","u-Z [mm]","sx [MPa]","sxy [MPa]","sy [MPa]","NG","u-Y [mm]","u-X [mm]","Freq. [1/sec]")
      neu <-c("x","Name","N","Vz","My","Mz","sig_x_o","sig_x_u","tau","Vy","sig_x_o","sig_x_u","sig_y_o","sig_y_u","y","w","sig_x","tau_xy","sig_y","Gruppe","w","v","freq")
      #SIGO, SIGU absichtlich vertauscht, da Sofistik die Platte anders definiert
      namen <- cbind(org,neu)
      columns[columns %in% org] <- neu[match(columns,org,nomatch=0)]
      colnames(y) <- columns}
    return(y) }))
  return(sofi)
}
sofistik_sortieren <- function(l_1,sort_cols,relevant_suffixes,beiwerte) {
  #if(exists("x")) {remove(x)}
  #if(exists("y")) {remove(y)}
  l_1 <- lapply(seq_along(l_1),function(xi) {
    #cyclet durch die Liste aller Dateien
    if(is.na(l_1[[xi]][1])) {
      neu <- NA
    } else {
      neu <- list()
      
      for(i in 1:length(sort_cols[[xi]])) { # cyclet durch die DataFrames, die am Ende herauskommen sollen (My,N,Q,sig_x_o, usw.)
        yi <- sort_cols[[xi]][i] # name des zu erzeugenden Dataframe
        # Zuweisung von x und y Koordinatenspalten, je nach dem was vorhanden ist
        if(exists("x",where=l_1[[xi]][[1]])) {
          assign("x",l_1[[xi]][[1]][,"x"])  
        }
        if(exists("y",where=l_1[[xi]][[1]])) {
          assign("y",l_1[[xi]][[1]][,"y"])  
        }
        if(exists("x")) {
          if(exists("y")) {
            assign(yi,data.frame(x=x,y=y))
          } else {
            assign(yi,data.frame(x=x))  
          }} else {
            if(exists("y")) {
              assign(yi,data.frame(y=y))
            }
          }
        temp <- as.data.frame(mapply(function(zi) {  #cyclet durch die verschiedenen Zust?nde (spalten des DataFrames)
          #erzeugen des Spalten-Namens: inst_g, inst_q, inst_s, etc.
          # names_screws <- c("schrauben","Schrauben","Screws","screws","screw","screwed","Screw","schraube","schrauben")
          #names_notches <- c("Nocken","Kerven","notches","Notches","nocken","kerven","Kerve","kerve","nocke","nocken")
          dat_name <- names(l_1[[xi]])[[zi]]
          dat_name_neu <- unlist(strsplit(dat_name,"_"))
          #if(beiwerte[xi,"connection"] %in% names_screws || names_notches) {
          suffix_index <- seq(length(dat_name_neu)-relevant_suffixes+1,length(dat_name_neu))
          dat_name_neu <- paste(dat_name_neu[suffix_index],sep="_",collapse="_")
          #} else {
          # dat_name_neu <- paste(dat_name_neu[3],dat_name_neu[4],sep="_")}
          #vector_dat_name[i] <- dat_name_neu
          
          if(exists(yi,where=l_1[[xi]] %>% "[["(dat_name))) {
            assign(dat_name_neu,l_1[[xi]] %>% "[["(dat_name) %>% "[["(yi))
            #setNames(x %>% "[["(dat_name) %>% "[["(y),dat_name_neu)
            #sig %>% "[["(1) %>% "$"("sig_c_fin_s")
            #sig %>% "[["(1) %>% "$"("sig_c_fin_s") %>% "$"(x_u) %>% "["(1)
          }
        },zi=seq_along(l_1[[xi]])))
        vector_dat_name <- mapply(function(zi) {  #cyclet durch die verschiedenen Zust?nde (spalten des DataFrames)
          #erzeugen des Spalten-Namens: inst_g, inst_q, inst_s, etc.
          dat_name <- names(l_1[[xi]])[[zi]]
          dat_name_neu <- unlist(strsplit(dat_name,"_"))
          #if(beiwerte[xi,"connection"] %in% names_screws || names_notches) {
          suffix_index <- seq(length(dat_name_neu)-relevant_suffixes+1,length(dat_name_neu))
          vector_dat_name <- paste(dat_name_neu[suffix_index],sep="_",collapse="_")
          #  } else {
          #   vector_dat_name <- (paste(dat_name_neu[3],dat_name_neu[4],sep="_"))
          #}
        },zi=seq_along(l_1[[xi]]))
        if(exists(yi,where=l_1[[xi]][[1]]))  {
          colnames(temp) <- vector_dat_name
          assign(yi,cbind(get(yi),temp)) 
          neu[[yi]] <- as.data.frame(get(yi))
          #neu[[y]] <- mapply(function(x) neu[[y]][,x]<-names(neu[[y]])[x],x<-seq_along(x))
          #setNames(neu[[y]],colnames(names(x)))
        }
      }    
    }
    return(neu)
  })
  return(l_1)
}

sofistik_subset <- function(liste,subset_vars) {
  if(is.na(liste[1])) {NA} else {
  liste <- lapply(liste,function(x) x[,subset_vars])
  }
}

sofistik_suffix <- function(liste,n_suffix) {
  if(is.na(liste[1])) {NA} else {
  long_names <- names(liste)
  short_names <- strsplit(long_names,"_") %>% lapply(function(x) x[n_suffix]) %>% lapply(paste,collapse="_") %>% unlist()
  
  colnames_2_rename <- lapply(liste,function(x) grep("[x]",colnames(x),invert = TRUE,value=TRUE))
  colnames_renamed <- lapply(seq_along(colnames_2_rename),function(x) paste(colnames_2_rename[[x]],short_names[x],sep="_"))
  
  index_rename <- lapply(liste,function(x) grep("[x]",colnames(x),invert = TRUE,value=FALSE))
  
  new_names <- lapply(seq_along(liste),function(x) replace(colnames(liste[[x]]),index_rename[[x]],colnames_renamed[[x]]))

  
  liste <- lapply(seq_along(liste),function(x) {colnames(liste[[x]]) <- new_names[[x]];return(liste[[x]])}) 
return(liste)
  }
}

sofistik_join <- function(liste,join_vars) {
  if(is.na(liste[1])) {neu <- NA} else {
  neu <- do.call("cbind",liste)
  }
  return(neu)
}

lastfallkombi_iteration <- function(res_liste,beiwerte,names_glue,names_mech_con) {
  
  l_1 <- lapply(seq_along(res_liste),function(xi) {
    if(beiwerte[xi,"connection"] %in% c(names_glue,names_mech_con)) {
      
      if(beiwerte[xi,"connection"] %in% names_glue) {
        k_j <- "x"
      }
      if(beiwerte[xi,"connection"] %in% names_mech_con) {
        k_j <- c("ku","ks")
      }    
      l_1_xi <-lapply(res_liste[[xi]],function(yi) {
        
        l_2 <- yi
        for(j in k_j) { 
          gam_g <- beiwerte[xi,"gam_g"]
          gam_q <- beiwerte[xi,"gam_q"]
          gam_sh_fin <- beiwerte[xi,"gam_sh_fin"]
          gam_sh_t37 <- beiwerte[xi,"gam_sh_t37"]
          gam_37 <- beiwerte[xi,"fakt_g_t37"]
          psi_2 <- beiwerte[xi,"psi_2"]
          
          gzt_iter_k<- paste("iter","gzt",j,sep="_")
          
          gzg_iter_k<- paste("iter","gzg",j,sep="_")
          
          gk_iter <- l_2[,paste("iter",j,"g",sep="_")]
          qk_iter <- l_2[,paste("iter",j,"q",sep="_")]
          qk_iter_perm <- qk_iter*psi_2
          qk_iter_short <- qk_iter*(1-psi_2)
          
          
          l_2[,gzt_iter_k] <- gam_g*gam_37*gk_iter+gam_q*gam_37*qk_iter_perm+gam_q*qk_iter_short
          l_2[,gzg_iter_k] <- gk_iter+qk_iter
          
        }
        return(l_2)
        
      })
    } else {
      
      l_1_xi <- lapply(seq_along(res_liste[[xi]]),function(x) NA)
    }
    
    return(l_1_xi) 
  })
  return(l_1)
}

lastfallkombi <- function(res_liste,beiwerte,names_glue,names_mech_con) {
  
  l_1 <- lapply(seq_along(res_liste),function(xi) {
    if(beiwerte[xi,"connection"] %in% c(names_glue,names_mech_con)) {
      
      if(beiwerte[xi,"connection"] %in% names_glue) {
        k_j_gzt <- "hgzt" # alternativ x
        k_j_gzg <- "hgzg" # alternativ x
      }
      if(beiwerte[xi,"connection"] %in% names_mech_con) {
        k_j_gzt <- c("ku")
        k_j_gzg <- c("ks")
      }    
      l_1_xi <-lapply(res_liste[[xi]],function(yi) {
        
        l_2 <- yi
        
        gam_g <- beiwerte[xi,"gam_g"]
        gam_q <- beiwerte[xi,"gam_q"]
        gam_sh <- beiwerte[xi,"gam_sh"]
        gam_sh_fin <- beiwerte[xi,"gam_sh_fin"]
        gam_sh_t37 <- beiwerte[xi,"gam_sh_t37"]
        gam_37 <- beiwerte[xi,"fakt_g_t37"]
        psi_2 <- beiwerte[xi,"psi_2"]
        
        for(j in k_j_gzt) { 
          
          gzt_inst_k<- paste("inst","gzt",j,c(1,2),sep="_")
          gzt_fin_k <- paste("fin","gzt",j,c(1,2,3,4),sep="_")
          gzt_t37_k <- paste("t37","gzt",j,c(1,2,3,4),sep="_")
          
          gk_inst <- l_2[,paste("inst",j,"g",sep="_")]
          qk_inst <- l_2[,paste("inst",j,"q",sep="_")]
          qk_inst_perm <- qk_inst*psi_2
          qk_inst_short <- qk_inst*(1-psi_2)
          
          gk_fin <- l_2[,paste("fin",j,"g",sep="_")]
          qk_fin <- l_2[,paste("fin",j,"q",sep="_")]
          qk_fin_perm <- qk_fin*psi_2
          qk_fin_short <- qk_fin*(1-psi_2)
          
          sk_fin <- l_2[,paste("fin",j,"s",sep="_")]
          
          gk_t37 <- l_2[,paste("t37",j,"g",sep="_")]
          qk_t37 <- l_2[,paste("t37",j,"q",sep="_")]
          qk_t37_perm <- qk_t37*psi_2
          qk_t37_short <- qk_t37*(1-psi_2)
          
          sk_t37 <- l_2[,paste("t37",j,"s",sep="_")]
          
          
          l_2[,gzt_inst_k[1]] <- gam_g*gam_37*gk_inst # KLED st?ndig
          l_2[,gzt_inst_k[2]] <- gam_g*gam_37*gk_inst+gam_q*gam_37*qk_inst_perm+gam_q*qk_inst_short # KLED mittel
          
          
          l_2[,gzt_fin_k[1]] <- gam_g*gam_37*gk_fin+gam_sh_fin*gam_sh*gam_37*sk_fin # st?ndig 
          l_2[,gzt_fin_k[2]] <- gam_g*gam_37*gk_fin+gam_q*gam_37*qk_fin_perm+gam_sh_fin*gam_sh*gam_37*sk_fin+gam_q*qk_inst_short # mittel
          l_2[,gzt_fin_k[3]] <- gam_g*gam_37*gk_fin # KLED st?ndig ohne schwinden
          l_2[,gzt_fin_k[4]] <- gam_g*gam_37*gk_fin+gam_q*gam_37*qk_fin_perm+gam_q*qk_inst_short # mittel ohne Schwinden
          
          
          l_2[,gzt_t37_k[1]] <- gam_g*gk_t37+gam_sh_t37*sk_t37*gam_sh # st?ndig
          l_2[,gzt_t37_k[2]] <- gam_g*gk_t37+gam_q*qk_t37_perm+gam_sh_t37*sk_t37*gam_sh+gam_q*qk_inst_short #mittel
          l_2[,gzt_t37_k[3]] <- gam_g*gk_t37 #st?ndig ohne schwinden
          l_2[,gzt_t37_k[4]] <- gam_g*gk_t37+gam_q*qk_t37_perm+gam_q*qk_inst_short #mittel ohne schwinden
        } 
        
        for(j in k_j_gzg) { 
          
          gzg_inst_k<- paste("inst","gzg",j,sep="_")
          gzg_fin_k <- paste("fin","gzg",j,sep="_")
          
          gk_inst <- l_2[,paste("inst",j,"g",sep="_")]
          qk_inst <- l_2[,paste("inst",j,"q",sep="_")]
          qk_inst_perm <- qk_inst*psi_2
          qk_inst_short <- qk_inst*(1-psi_2)
          
          gk_fin <- l_2[,paste("fin",j,"g",sep="_")]
          qk_fin <- l_2[,paste("fin",j,"q",sep="_")]
          qk_fin_perm <- qk_fin*psi_2
          qk_fin_short <- qk_fin*(1-psi_2)
          
          sk_fin <- l_2[,paste("fin",j,"s",sep="_")]
          
          l_2[,gzg_inst_k] <- gk_inst+qk_inst
          l_2[,gzg_fin_k] <- gk_fin+qk_fin_perm+sk_fin*gam_sh_fin+qk_inst_short
        }
        return(l_2)
        
      })
    } else {
      
      l_1_xi <- lapply(seq_along(res_liste[[xi]]),function(x) NA)
    }
    
    return(l_1_xi) 
  })
  return(l_1)
}

mitt_eta_pro_nachweis <- function(liste,nachweisorte,was) {
  temp_list <- list()
  name_list <- list()
  for(i in nachweisorte) {
    row_no <- which(signif(liste[[i]][,"x"],3)>=signif(rev(liste[[i]][,"x"]),3)[1]/2)[1]
    col_koord <- grep("^[x]{1}",colnames(liste[[i]]),perl=TRUE)
    col_keep <- grep(was,colnames(liste[[i]]))
    names_col_keep <- colnames(liste[[i]])[col_keep]
    if(length(col_keep)==0) {
      temp_list[[i]] <- data.frame(eta=NA,x=liste[[i]][row_no,col_koord])
      name_list[[i]] <- i
    } else {
      temp_list[[i]] <- data.frame(eta=unlist(liste[[i]][row_no,col_keep]),x=liste[[i]][row_no,col_koord])
      name_list[[i]] <- paste(names_col_keep,i,sep="_")
    }
  }
  name_list <- unlist(name_list)
  df <- do.call(rbind,temp_list)
  row.names(df) <- name_list
  
  return(df)
} 

max_eta_pro_nachweis <- function(liste,nachweisorte,was) {
  if(is.na(liste[1])) {NA} else {
  temp_list <- list()
  names_list <- list()
  for(i in nachweisorte) {
    col_koord <- grep("^[x]{1}",colnames(liste[[i]]),perl=TRUE)
    col_keep <- grep(was,colnames(liste[[i]]))
    row_no <- mapply(function(x) which.max(signif(liste[[i]][,x],3)),x=col_keep)
    col_keep_names <- colnames(liste[[i]])[col_keep]
    if(length(col_keep)==0) {
      
      temp_list[[i]] <- data.frame(eta=NA,x=NA)
      names_list[[i]] <- paste(col_keep_names,i,sep="_")
    } else {
      row_no <- mapply(function(x) which.max(signif(liste[[i]][,x],3)),x=col_keep) 
      temp_list[[i]] <- data.frame(eta=mapply(function(x) liste[[i]][row_no[x],col_keep[x]],x=seq_along(col_keep)),x=liste[[i]][row_no,col_koord])
      names_list[[i]] <- paste(col_keep_names,i,sep="_")
    }
  }
  names_list <- unlist(names_list)
  df <- do.call(rbind,temp_list)
  row.names(df) <- names_list
  
  return(df)
  }
} 

# Zuweisungen
names_screws <- c("schrauben","Schrauben","Screws","screws","screw","screwed","Screw","schraube","schrauben")
names_notches <- c("Nocken","Kerven","notches","Notches","nocken","kerven","Kerve","kerve","nocke","nocken","notch","notched","Notch","Notched")
names_mech_con <- c(names_notches,names_screws)
names_glue <- c("klebung","glue","kleb","glued","Klebung","geklebt","rigid","starr")

# create long data
nestedlist_to_df <- function(list) {
  
  for(i in 1:length(list)) {
    for(j in 1:length(list[[i]]))
      #colnames(res_t[[i]][[j]]) <- paste(names(res_t[[i]])[j],colnames(res_t[[i]][[j]]),sep="_")
      list[[i]][[j]] <- cbind(list[[i]][[j]],results=rep(names(list[[i]])[j],nrow(list[[i]][[j]])),spec=rep(paste("spec_",i,sep=""),nrow(list[[i]][[j]])))
  }
  simplelist <- lapply(list,function(x) data.table::rbindlist(x,fill=TRUE))
  df <- data.table::rbindlist(simplelist,fill=TRUE)
  
  return(df)
}

# add jitter 
add_jitter <- function(data,jitter_variable,base_variable,amount) {
  jitter_values <- sort(unique(data[,jitter_variable]))
  base_values <- sort(unique(data[,base_variable]))
  max_jitter <- max(diff(base_values))*0.5*amount
  jitter <- seq(-max_jitter,max_jitter,length=length(jitter_values))
  new_values <- mapply(function(x) jitter[which(jitter_values==x)],x=data[,jitter_variable])+base_values
  return(new_values)
}

reverse_gamma <- function(res_t,input) {
  
  iter <- lapply(seq_along(res_t),function(x) if (input[x,"connection"]%in% names_glue)
  {c("iter_x_g","iter_x_g")} else {c("iter_ks_g","iter_ku_g")})

input[,"N_t_ks"] <- mapply(function(xi) max(res_t[[xi]][["N"]][,iter[[xi]][1]]),xi=seq_along(res_t))
input[,"N_t_ku"] <- mapply(function(xi) max(res_t[[xi]][["N"]][,iter[[xi]][2]]),xi=seq_along(res_t))
input[,"My_t_ks"] <- mapply(function(xi) max(res_t[[xi]][["My"]][,iter[[xi]][1]]),xi=seq_along(res_t))
input[,"My_t_ku"] <- mapply(function(xi) max(res_t[[xi]][["My"]][,iter[[xi]][2]]),xi=seq_along(res_t))
# Berechnung der Verbundsteifigkeit fuer die beiden Verschiebungsmoduln
input[,"gam_ks"] <- input[,"a_t"]*(input[,"e_t_p"]*1000)^2*input[,"iy_t"]*input[,"N_t_ks"]/(input[,"e_c_m"]*1000*input[,"a_c_eff"]*(input[,"e_t_p"]*1000*input[,"a_t"]*input[,"My_t_ks"]*(input[,"h_t"]/2+input[,"h_c"]-input[,"h_c_eff"]/2)-input[,"e_t_p"]*1000*input[,"iy_t"]*input[,"N_t_ks"]))
input[,"gam_ku"] <- input[,"a_t"]*(input[,"e_t_p"]*1000)^2*input[,"iy_t"]*input[,"N_t_ku"]/(input[,"e_c_m"]*1000*input[,"a_c_eff"]*(input[,"e_t_p"]*1000*input[,"a_t"]*input[,"My_t_ku"]*(input[,"h_t"]/2+input[,"h_c"]-input[,"h_c_eff"]/2)-input[,"e_t_p"]*1000*input[,"iy_t"]*input[,"N_t_ku"]))
# Ableitung des ideellen Schwerpunkts
input[,"z_s_ks"] <- round((input[,"e_t_p"]*input[,"a_t"]*(input[,"h_c"]+input[,"h_t"]/2)+input[,"e_c_m"]*input[,"a_c_eff"]*input[,"h_c_eff"]/2*input[,"gam_ks"])/(input[,"a_t"]*input[,"e_t_p"]*1+input[,"a_c_eff"]*input[,"e_c_m"]*input[,"gam_ks"]),3)
input[,"z_s_ku"] <- round((input[,"e_t_p"]*input[,"a_t"]*(input[,"h_c"]+input[,"h_t"]/2)+input[,"e_c_m"]*input[,"a_c_eff"]*input[,"h_c_eff"]/2*input[,"gam_ku"])/(input[,"a_t"]*input[,"e_t_p"]*1+input[,"a_c_eff"]*input[,"e_c_m"]*input[,"gam_ku"]),3)
# Errechnen der effektiven Gesamtsteifigkeit des Systems (bspw. fuer Schwingungsberechnung notwendig)
input[,"ei_eff_ks"] <- round((input[,"e_t_p"]*input[,"iy_t"]+input[,"e_c_m"]*input[,"iy_c"]+input[,"a_t"]*input[,"e_t_p"]*(input[,"h_c"]+input[,"h_t"]/2-input[,"z_s_ks"])^2+input[,"gam_ks"]*input[,"a_c_eff"]*input[,"e_c_m"]*(input[,"h_c_eff"]/2-input[,"z_s_ks"])^2)*1000000,2)
input[,"ei_eff_ku"] <- round((input[,"e_t_p"]*input[,"iy_t"]+input[,"e_c_m"]*input[,"iy_c"]+input[,"a_t"]*input[,"e_t_p"]*(input[,"h_c"]+input[,"h_t"]/2-input[,"z_s_ku"])^2+input[,"gam_ku"]*input[,"a_c_eff"]*input[,"e_c_m"]*(input[,"h_c_eff"]/2-input[,"z_s_ku"])^2)*1000000,2)

return(input)
}

# Make Nachweis Label
nachweislabel <- function(max_nachweis) {
  max_nachweis <- sub("[t]{1}[.]{1}","Holz",max_nachweis)
  max_nachweis <- sub("[c]{1}[.]{1}","Beton",max_nachweis)
  max_nachweis <- sub("[v]{1}[.]{1}","Fuge",max_nachweis)
  max_nachweis <- sub("[g]{1}[.]{1}","",max_nachweis)
  
  max_nachweis <- sub("eta","",max_nachweis)
  max_nachweis <- sub("hgzg","",max_nachweis)
  max_nachweis <- sub("hgzt","",max_nachweis)
  max_nachweis <- sub("gzg","GZG",max_nachweis)
  max_nachweis <- sub("gzt","GZT",max_nachweis)
  max_nachweis <- sub("x","",max_nachweis)
  max_nachweis <- sub("ku","",max_nachweis)
  max_nachweis <- sub("ks","",max_nachweis)
  max_nachweis <- sub("sig_x_u","UK",max_nachweis)
  max_nachweis <- sub("sig_x_o","OK",max_nachweis)
  max_nachweis <- sub("fin","t=\\infty",max_nachweis)
  max_nachweis <- sub("inst","t=0",max_nachweis)
  max_nachweis <- sub("t37","t=3-7a",max_nachweis)
  max_nachweis <- sub("Vz","QK",max_nachweis)
  max_nachweis <- sub("Vy","",max_nachweis)
  max_nachweis <- sub("tau","QK",max_nachweis)
  max_nachweis <- sub("_([123456789])_"," LF\\1",max_nachweis)
  max_nachweis <- sub("sep","*",max_nachweis)
  max_nnachweis <- sub("__","_",max_nachweis)
  max_nachweis <- sub("_w","w",max_nachweis)
  max_nachweis <- sub("eige","\\omega",max_nachweis)
  
  
  max_nachweis <- strsplit(max_nachweis,"_")
  v_order <- c("Holz","Beton","Fuge","Verformung","UK","OK","QK","GZT","GZG","K_u","K_s","t=0","t=infty","t=3-7a","LF1","LF2","LF3","LF4","LF5","LF6","LF7","LF8","LF9","LF1*","LF2*","LF3*","LF4*","LF5*","LF6*","LF7*","LF8*","LF9*")
  d_order <- lapply(max_nachweis,match,v_order)
  nachweis_label <- lapply(seq_along(max_nachweis),function(x) max_nachweis[[x]][order(d_order[[x]])])
  nachweis_label <- lapply(nachweis_label,paste,collapse=" ")
  nachweis_label <- unlist(nachweis_label)
}

