# Eingabe Bemessung Klebung
# Eingabe für Lolipop Chart

titl <- "VA51"
template <- "hbv_bemess_mech"
connection <- "notch"

l_x <- 8.1
l_y <- 1.35

b_t <- 0.24
h_t <- 0.32
mat_t <- "GL24h"

h_c <- 0.1
mat_c <- "C30/37"

#n_v_x <- 3
#t_v <- 0.035

parameter <- "Referenz"
label <- "GL24h, C30/37, 24x32 Holz, 10 cm Beton"
referenz <- data.frame(titl,template,parameter,connection,label,l_x,l_y,h_c,h_t,b_t,mat_t,mat_c)

change_var <- c("l_x","l_x",
                "l_y","l_y",
                "h_c","h_c",
                "h_t","h_t",
                "b_t","b_t",
                "mat_t","mat_t",
                "mat_c","mat_c")

change_row <- seq_along(change_var)+1
new_var <- list(6.75,9.75,0.675,2.025,0.07,0.12,0.24,0.4,0.2,0.3,"GL20h","GL32h","C25/30","C45/55") # Nur mit der Liste werden verschiedeneh Datentypen unterstÃ¼tzt
new_label <- c("6,75m","9,75m","0,675m","2,025m","7cm","12cm","24cm","40cm","20cm","30cm","GL20h","GL32h","C25/30","C45/55")
new_parameter <- c("Balkenlänge","Balkenlänge","Balkenabstand","Balkenabstand",
                   "Betonhöhe","Betonhöhe","Balkenhöhe","Balkenhöhe",
                   "Balkenbreite","Balkenbreite","Holzgüte","Holzgüte",
                   "Betongüte","Betongüte")

input <- referenz[rep(1,length(new_var)+1),]
input[seq_along(new_label)+1,"label"] <- new_label
input[seq_along(new_parameter)+1,"parameter"] <- new_parameter
input <- df_replace(input,change_row,change_var,new_var)

input[,"n_v_x"] <- NA
input[,"f_v_tens_k"] <- NA
input[,"f_v_ax_k"] <- NA
input[,"t_v"] <- NA
input[,"alpha_v"] <- NA
input[,"mu_v"] <- NA

input[,"con_parameter"] <- "Schraube"

n_v <- lapply(input[,"n_v_x"], function(x) NA)
k_s_pm <- lapply(input[,"n_v_x"], function(x) NA) # k_s_pm = Verschiebungsmodul pro Meter Kervenbreite oder pro einzelner Schraube (als Vektor mit einem Wert pro VM-Reihe) in kN/mm
x_v_eff<- lapply(input[,"n_v_x"], function(x) NA) # x_v_eff = Bei Kerven die Kervenbreite [m] oder effektive Einbindetiefe der Schraube ins Holz (immer als Vektor mit einem Wert pro VM-Reihe) [m]
l_v <- lapply(input[,"n_v_x"], function(x) NA) # l_v = Kervenlaenge [m], fuer Schrauben fuer jede Reihe zu 1.0 setzen Angabe als Vector: Fuer jede Verbindungsmittelreihe einen wert angeben
a_v <- lapply(input[,"n_v_x"], function(x) NA) # a_v = Abstaende der Verbindungsmittel immer bezogen auf das Traegerende [m]. cumsum() Schreibweise empfohlen


##### Kerven ####

##### Kerven ####

#input <- input
#input[,"template"] <- "hbv_bemess_mech"
#input[,"con_parameter"] <- "Kerve"
#input[,"connection"] <- "notch"
##input_notch[,"n_v_x"] <- 3
##input_notch[,"t_v"] <- 0.035
#input[,"min_a_screw"] <- NA # minimaler Abstand zwischen zwei Schrauben
#input[,"a1_screw"] <- NA
##c(3,3,1)[match(input$l_x,c(8.1,6,3))]
#
## Verbindungsmittel werden in Listen gespeichert, pro Fall eine Liste
#n_v <- lapply(input[,"n_v_x"], function(x) rep(1,x))
#k_s_pm <- lapply(input[,"n_v_x"], function(x) rep(1500,x)) # k_s_pm = Verschiebungsmodul pro Meter Kervenbreite oder pro einzelner Schraube (als Vektor mit einem Wert pro VM-Reihe) in kN/mm/mm/SChraube
#x_v_eff <- lapply(input[,"n_v_x"], function(x) rep(0.20 ,x)) # x_v_eff = Bei Kerven die Kervenbreite [m] oder effektive Einbindetiefe der Schraube ins Holz (immer als Vektor mit einem Wert pro VM-Reihe) [m]
#l_v <- lapply(input[,"n_v_x"], function(x) rep(0.15,x)) # l_v = Kervenlaenge [m], fuer Schrauben fuer jede Reihe zu 1.0 setzen Angabe als Vector: Fuer jede Verbindungsmittelreihe einen wert angeben
#a_v <- lapply(input[,"n_v_x"], function(x) cumsum(c(0.45,rep(0.6,x-1)))) # a_v = Abstaende der Verbindungsmittel immer bezogen auf das Traegerende [m]. cumsum() Schreibweise empfohlen
#
#
#input[,"f_v_ax_k"]  <-  NA  # f_v_ax_k = Ausziehwiderstand der Schrauben in N/mm?
#input[,"f_v_tens_k"]  <- NA # f_v_tens_k = Zugwiderstand der Schrauben in kN
#input[,"alpha_v"] <- NA   # alpha_v = Einschraubwinkel in deg
#input[,"mu_v"]   <- 0.25     # mu_v = Reibbeiwert (nur fuer Schraubenverbindungen)

#### Schrauben 1 #### 
# ASSY plus VG (Würth)

input_screw1 <- input
input_screw1[,"template"] <- "hbv_bemess_mech"
input_screw1[,"con_parameter"] <- "ASSY plus VG L=430 mit FT-Verb."
input_screw1[,"connection"] <- "screws"
input_screw1[,"n_v_x"] <- 24
input_screw1[,"t_v"] <- 0.01

# Verbindungsmittel werden in Listen gespeichert, pro Fall eine Liste
n_v_screw1 <- lapply(input_screw1[,"n_v_x"], function(x) rep(2,x))
k_s_pm_screw1 <- lapply(input_screw1[,"n_v_x"], function(x) rep(35.4,x)) # k_s_pm = Verschiebungsmodul pro Meter Kervenbreite oder pro einzelner Schraube (als Vektor mit einem Wert pro VM-Reihe) in kN/mm
x_v_eff_screw1 <- lapply(input_screw1[,"n_v_x"], function(x) rep(0.28,x)) # x_v_eff = Bei Kerven die Kervenbreite [m] oder effektive Einbindetiefe der Schraube ins Holz (immer als Vektor mit einem Wert pro VM-Reihe) [m]
l_v_screw1 <- lapply(input_screw1[,"n_v_x"], function(x) rep(1,x)) # l_v = Kervenlaenge [m], fuer Schrauben fuer jede Reihe zu 1.0 setzen Angabe als Vector: Fuer jede Verbindungsmittelreihe einen wert angeben
a_v_screw1 <- lapply(input_screw1[,"n_v_x"], function(x) cumsum(rep(0.12,x))) # a_v = Abstaende der Verbindungsmittel immer bezogen auf das Traegerende [m]. cumsum() Schreibweise empfohlen


input_screw1[,"f_v_ax_k"]  <-  10  # f_v_ax_k = Ausziehwiderstand der Schrauben in N/mm?
input_screw1[,"f_v_tens_k"]  <- 32 # f_v_tens_k = Zugwiderstand der Schrauben in kN
input_screw1[,"alpha_v"] <- 30   # alpha_v = Einschraubwinkel in deg
input_screw1[,"mu_v"]   <- 0.25     # mu_v = Reibbeiwert (nur fuer Schraubenverbindungen)


#### Schrauben 2 ####
# ASSY plus VG (Würth)
input_screw2 <- input
input_screw2[,"template"] <- "hbv_bemess_mech"
input_screw2[,"con_parameter"] <- "ASSY plus VG L=430"
input_screw2[,"connection"] <- "screws"
input_screw2[,"n_v_x"] <- 24
input_screw2[,"t_v"] <- 0.008

# Verbindungsmittel werden in Listen gespeichert, pro Fall eine Liste
n_v_screw2 <- lapply(input_screw2[,"n_v_x"], function(x) rep(2,x))
k_s_pm_screw2 <- lapply(input_screw2[,"n_v_x"], function(x) rep(50,x)) # k_s_pm = Verschiebungsmodul pro Meter Kervenbreite oder pro einzelner Schraube (als Vektor mit einem Wert pro VM-Reihe) in kN/mm
x_v_eff_screw2 <- lapply(input_screw2[,"n_v_x"], function(x) rep(0.34,x)) # x_v_eff = Bei Kerven die Kervenbreite [m] oder effektive Einbindetiefe der Schraube ins Holz (immer als Vektor mit einem Wert pro VM-Reihe) [m]
l_v_screw2 <- lapply(input_screw2[,"n_v_x"], function(x) rep(1,x)) # l_v = Kervenlaenge [m], fuer Schrauben fuer jede Reihe zu 1.0 setzen Angabe als Vector: Fuer jede Verbindungsmittelreihe einen wert angeben
a_v_screw2 <- lapply(input_screw2[,"n_v_x"], function(x) cumsum(rep(0.12,x))) # a_v = Abstaende der Verbindungsmittel immer bezogen auf das Traegerende [m]. cumsum() Schreibweise empfohlen


input_screw2[,"f_v_ax_k"]  <-  11  # f_v_ax_k = Ausziehwiderstand der Schrauben in N/mm?
input_screw2[,"f_v_tens_k"]  <- 17 # f_v_tens_k = Zugwiderstand der Schrauben in kN
input_screw2[,"alpha_v"] <- 45   # alpha_v = Einschraubwinkel in deg
input_screw2[,"mu_v"]   <- 0.25     # mu_v = Reibbeiwert (nur fuer Schraubenverbindungen)


#### Schrauben 3 ####
# VB-48-7,5 x 165
input_screw3 <- input
input_screw3[,"template"] <- "hbv_bemess_mech"
input_screw3[,"con_parameter"] <- "VG-48-7,5x165"
input_screw3[,"connection"] <- "screws"
input_screw3[,"n_v_x"] <- 24
input_screw3[,"t_v"] <- 0.0075

# Verbindungsmittel werden in Listen gespeichert, pro Fall eine Liste
n_v_screw3 <- lapply(input_screw3[,"n_v_x"], function(x) rep(2,x))
k_s_pm_screw3 <- lapply(input_screw3[,"n_v_x"], function(x) rep(240,x)) # k_s_pm = Verschiebungsmodul pro Meter Kervenbreite oder pro einzelner Schraube (als Vektor mit einem Wert pro VM-Reihe) in kN/mm
x_v_eff_screw3 <- lapply(input_screw3[,"n_v_x"], function(x) rep(0.165,x)) # x_v_eff = Bei Kerven die Kervenbreite [m] oder effektive Einbindetiefe der Schraube ins Holz (immer als Vektor mit einem Wert pro VM-Reihe) [m]
l_v_screw3 <- lapply(input_screw3[,"n_v_x"], function(x) rep(1,x)) # l_v = Kervenlaenge [m], fuer Schrauben fuer jede Reihe zu 1.0 setzen Angabe als Vector: Fuer jede Verbindungsmittelreihe einen wert angeben
a_v_screw3 <- lapply(input_screw3[,"n_v_x"], function(x) cumsum(rep(0.12,x))) # a_v = Abstaende der Verbindungsmittel immer bezogen auf das Traegerende [m]. cumsum() Schreibweise empfohlen


input_screw3[,"f_v_ax_k"]  <-  16  # f_v_ax_k = Ausziehwiderstand der Schrauben in N/mm?
input_screw3[,"f_v_tens_k"]  <- 17 # f_v_tens_k = Zugwiderstand der Schrauben in kN
input_screw3[,"alpha_v"] <- 45   # alpha_v = Einschraubwinkel in deg
input_screw3[,"mu_v"]   <- 0.25     # mu_v = Reibbeiwert (nur fuer Schraubenverbindungen)


##### Zusammenfuehren

input <- rbind(input_screw1,input_screw2,input_screw3)

n_v <- c(n_v_screw1,n_v_screw2,n_v_screw3)

k_s_pm <- c(k_s_pm_screw1,k_s_pm_screw2,k_s_pm_screw3)
x_v_eff <- c(x_v_eff_screw1,x_v_eff_screw2,x_v_eff_screw3)
l_v <- c(l_v_screw1,l_v_screw2,l_v_screw3)
a_v <- c(a_v_screw1,a_v_screw2,a_v_screw3)

###################################################
input[,"n_fem"] <- 0.1

# Allgemeine Werte
input[,"g_1"] <- 2        # g_1 = Ausbaulast in kN/m2
input[,"q_1"]  <- 3.5       # q_1 = Nutzlast in kN/m2
input[,"q_cat"]  <- "Q_B"     # q_cat = "Q_B" fuer Nutzlast Kategorie B, "Q_A" Kategorie A usw. (Sofistik Nomenklatur)

input[,"a_s_l"] <- 1.88 # cm?/m L?ngsbewehrung Beton

# Beiwerte zum Betonschwinden
input[,"CEM"] <- "R"
input[,"rh"] <- 65 # Luftfeuchte
input[,"t0_creep"] <- 30 # Zeitpukt der ersten Belastung
input[,"t_creep"] <- 50*365
input[,"t0_shrink"] <- 2 # Befinn der Nachbehandlung; 
input[,"t_shrink"] <- 50*365 # Endzeitpunkt
# Einlesen der Materialkennwerte f?r Holz und Beton
input[,"gamma_t"] <- 1.25
input[,"gamma_c"] <- 1.5
input[,"alpha_cc"] <- 0.85
#Sicherheitsbeiwerte
input[,"gam_g"] <- 1.35
input[,"gam_q"] <- 1.5
input[,"fakt_g_t37"] <- 1.0 # Ueberhoehung der Spannung, damit auf Nachweis t=3...7 Jahre verzichtet werden kann, entfaellt weil stattdessen Nachweis gefuehrt wird fuer t37
input[,"gam_sh"] <- 1.35
input[,"gam_sh_t37"] <- 0.6
input[,"gam_sh_fin"] <- 0.9

input[,"k_mod_t_perm"] <- 0.6 # BSH mittlere Einwirkungsdauer NKL 1 bzw. 2
input[,"k_mod_t_medi"] <- 0.8
input[,"k_mod_t_long"] <- 0.7
input[,"k_mod_t_shor"] <- 0.9
input[,"k_mod_t_inst"] <- 1.1

input[,"alpha_cc"] <- rep(0.85,nrow(input)) # EN 1992 zw. 0.8 und 1.0, empf. Wert 1.0

input[,"gam_t"] <- rep(1.25,nrow(input)) # BSH EN 1995
input[,"gam_c"] <- rep(1.5,nrow(input)) # EN 1992 st?ndig u vor?bergehend, au?ergew?hnlich 1.2
input[,"gam_s"] <- rep(1.15,nrow(input)) # En 1992 au?ergew?hnlich 1.0 Sicherheitsbeiwert STAHL, nicht Schwinden!

input[,"k_def_t"] <- rep(0.6,nrow(input)) # NKL 1 = 0.6, NKL 2= 0.8 ACHTUNG: Einige Zulassungen (bspw. W?rth schreibt f?r NKL2 kdef 2.0 vor!)

input[,"k_mod_v_perm"] <- signif(sqrt(input[,"k_mod_t_perm"]*input[,"alpha_cc"]),2) # Nach technical Specification 
input[,"k_mod_v_medi"] <- signif(sqrt(input[,"k_mod_t_medi"]*input[,"alpha_cc"]),2) # Nach technical Specification 
input[,"k_mod_v_long"] <- signif(sqrt(input[,"k_mod_t_long"]*input[,"alpha_cc"]),2) # Nach technical Specification 
input[,"k_mod_v_shor"] <- signif(sqrt(input[,"k_mod_t_shor"]*input[,"alpha_cc"]),2) # Nach technical Specification 
input[,"k_mod_v_inst"] <- signif(sqrt(input[,"k_mod_t_inst"]*input[,"alpha_cc"]),2) # Nach technical Specification 

input[,"gam_v"] <- rep(1.25,nrow(input)) # EN 1992 st?ndig u vor?bergehend, au?ergew?hnlich 1.2
input[,"k_def_v"] <- 2*input[,"k_def_t"] 

input[,"w_lim_inst"] <- input[,"l_x"]/300*1000
input[,"w_lim_fin"] <- input[,"l_x"]/200*1000

# Werte Heissbemessung
input[,"beta0"] <- rep(0.65,nrow(input)) # Abbrandrate 0.65 BSH NH [mm/min]
input[,"betan"] <- rep(0.7,nrow(input)) # idelle Abbrandrate 0.7 BSH NH, 0.8 VH NH, 0.7 VH/BSH Laub rho <290, 0.55 VH/BSH Laub rho>450 Abbrandrate [mm/min]
input[,"t_fi"] <- rep(90,nrow(input)) # Brandwiderstandsdauer 90 min
input[,"d_char_0"]<- (input[,"t_fi"]*input[,"beta0"])/1000
input[,"d_char_n"]<- (input[,"t_fi"]*input[,"betan"])/1000
input[,"d_fi_ef"] <- input[,"d_char_n"]+7/1000

input[,"b_t_fi"] <- input[,"b_t"]-(2*input[,"d_fi_ef"]) # seitlich beidseitig, mit erh?htem Abbrand 1,0 x 0.7 f?r t>20 min
input[,"h_t_fi"] <- input[,"h_t"]-input[,"d_fi_ef"] # von unten einseitig, mit erh?htem Abbrand 1,0 x 0.7 f?r t>20 min
input[,"u_t_fi"] <- input[,"h_t_fi"]*2+input[,"b_t_fi"] # Umfang des dem Feuer ausgesetzten Rest-QS
input[,"a_t_fi"] <- input[,"h_t_fi"]*input[,"b_t_fi"] # Fl?che desselben
input[,"k_mod_fi_c"] <- rep(1,nrow(input))
input[,"k_mod_fi_m"] <- rep(1,nrow(input))
input[,"k_mod_fi_e_t"] <- rep(1,nrow(input))

input[,"w_t_y_fi"] <- input[,"h_t_fi"]^2*input[,"b_t_fi"]/6
#input[,"w_c_y_fi"] <- input[,"wy_c"]
#input[,"a_c_fi"] <- input[,"a_c"]


# Bewehrungs Beiwerte
input[,"delta_c_dur_add"] <- 0 # Verringerung durch Schutzma?nahmen
input[,"delta_c_dur_st"] <- 0 # nur bei Verwendung durch nicht rostenden Stahl
input[,"delta_c_dur_gam"] <- 0# Sicherheitselement
input[,"c_min_dur"] <- 15 # Dauerhaftigkeitsanforderung
input[,"delta_c_dev"] <- 10  # Vorhaltsma?
input[,"d_s_u"] <- 6
input[,"c_min"] <- max(10,input[,"d_s_u"],input[,"c_min_dur"]+input[,"delta_c_dur_gam"]-input[,"delta_c_dur_st"]-input[,"delta_c_dur_add"])
input[,"c_nom"] <- input[,"c_min"]+input[,"delta_c_dev"]
input[,"d_c"] <- input[,"h_c"]-input[,"c_nom"]/1000-input[,"d_s_u"]/2/1000


