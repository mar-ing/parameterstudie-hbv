##### Abh?ngige Werte -> Quasi keine Eingabe mehr notwendig
#t <- get_timber(input[,"mat_t"],gamma_t=input[,"gamma_t"],h_t=input[,"h_t"]*1000)
t <- lapply(seq_along(input[,1]),function(x) get_timber(input[x,"mat_t"],gamma_t=input[x,"gamma_t"],h_t=input[x,"h_t"]))
t <- do.call("rbind",t)

c <- get_concrete(input[,"mat_c"],gamma_c=input[,"gamma_c"],alpha_cc=input[,"alpha_cc"])
psi <- get_psi(input[,"q_cat"])
b_c <- get_beff(b_0=input[,"b_t"],b_1=(input[,"l_y"]-input[,"b_t"])/2,l_e=0.85*input[,"l_x"])
qs_t <- get_qs(h=input[,"h_t"],b=input[,"b_t"],suffix="t")
qs_c <- get_qs(h=input[,"h_c"],b=b_c,suffix="c")

input <- cbind(input,t,c,psi,b_c=b_c,qs_t,qs_c)

input[,"gkla"] <- gsub("[a-z]*[A-Z]*","",input[,"mat_t"]) # f?r Weiterverarbeitung in Sofistik

# Festigkeit Heissbemessung
input[,"f_t_t0_k_fi"] <- input[,"f_t_t0_k"]*1.15 # 20% Fraktil der Festigkeitswerte, Faktor 1.15 f?r BSH
input[,"f_t_c0_k_fi"] <- input[,"f_t_c0_k"]*1.15 # 20% Fraktil der Festigkeitswerte, Faktor 1.15 f?r BSH
input[,"f_t_v_k_fi"] <- input[,"f_t_v_k"]*1.15 # 20% Fraktil der Festigkeitswerte, Faktor 1.15 f?r BSH
input[,"f_t_m_k_fi"] <- input[,"f_t_m_k"]*1.15 # 20% Fraktil der Festigkeitswerte, Faktor 1.15 f?r BSH

# Methode mit reduzierten Eigenschaften mit ausgerundeten Ecken
#input[,"b_t_fi"] <- input[,"b_t"]-(2*input[,"d_char_0"]) # seitlich beigseitig
#input[,"h_t_fi"] <- input[,"h_t"]-input[,"d_char_0"] # von unten einseitig
#input[,"u_t_fi"] <- input[,"h_t_fi"]*2+input[,"b_t_fi"]-4*input[,"d_char_0"]+pi*input[,"d_char_0"] # Umfang des dem Feuer ausgesetzten Rest-QS
#input[,"a_t_fi"] <- input[,"h_t_fi"]*input[,"b_t_fi"]-(input[,"d_char_0"]^2-input[,"d_char_0"]^2*pi*1/4) # Fl?che desselben
#input[,"kmod_fi_c"] <- 1-1/125*input[,"u_t_fi"]/input[,"a_t_fi"] # Kmod f?r Druckfestigkeit
#input[,"kmod_fi_m"] <- 1-1/200*input[,"u_t_fi"]/input[,"a_t_fi"] # kmod f?r Biegefestigkeit
#input[,"kmod_fi_e_t"] <- 1-1/330*input[,"u_t_fi"]/input[,"a_t_fi"] # kmod f?r Steifigkeit und Zugfestigkeit


# Kriechwert Beton ermitteln
# t0 erste Beanspruchung, tt End-Alter , in [d]
#input[,"h_null"] <- mapply(function(x) 2*input[x,"a_c"]/((input[x,"b_c"]+input[x,"b_c"]-ifelse(input[x,"doppelbalken"],2,1)*input[x,"b_t"]))*1000,x=seq_along(input[,"b_c"]))
input[,"h_null"] <- 2*input[,"a_c"]/(input[,"b_c"]*2)

phi <- get_phi(input[,"f_c_c_m"],input[,"t0_creep"],input[,"t_creep"],input[,"rh"],input[,"h_null"])
phi <- mapply(function(x) ifelse(x<2.5,2.5,3.5),x=phi) # Aufteilung in entweder 2.5 oder 3.5 (aktuell noch ohne Zwischenwerte)
input[,"phi"] <- phi

# Schwinden: Temp bzw. Normalkraft Ersatzlast
# In NKL 1 und teilweise auch NKL 2 duerfen Einfluesse aus Temperatur und Feuchte vernachlaesigt werden!
# Betonschwinden muss IMMER beruecksichtigt werden!
# Nach Technical Spec darf fuer den Nachweis t=3-7 das Schwinden auf 60% reduziert werden und im Endzustand auf 90%
# Das ist bereits im Bemessungscode so implementiert

eps <- get_shrink(input[,"rh"],input[,"CEM"],input[,"f_c_c_m"],input[,"f_c_c_k"],input[,"h_null"]) #tt und ts nur eingeben, wenn Spezifischer Zeitpunkt betrachtet
input <- cbind(input,eps*1000)
temp <- eps/(12*10^-6)

###################################################################
# Umrechnung der Verschiebungsmoduln und Abstaende fuer Sofistik #
##################################################################
sofi_vm <- lapply(seq_along(k_s_pm),function(x) sofistify_vm(vm_vals=data.frame(k_s_pm=k_s_pm[[x]],
                                                                                x_v_eff=x_v_eff[[x]],
                                                                                n_v=n_v[[x]],
                                                                                l_v=l_v[[x]],
                                                                                a_v=a_v[[x]]),input[x,"connection"]))
sofi_vm <- do.call(rbind,sofi_vm)
input <- cbind(input,sofi_vm)
# Netto Abstaende zwischen den Verbindungsmitteln zur Berechnung der Verbundsteifigkeit nach Gamma Verfahren und Verbindungsmitteltragfaehigkeit (Kervenvorholzversagen)
a_v_net <- lapply(seq_along(a_v),function(x) get_nettodist(vm_vals=data.frame(k_s_pm=k_s_pm[[x]],
                                                                              x_v_eff=x_v_eff[[x]],
                                                                              n_v=n_v[[x]],
                                                                              l_v=l_v[[x]],
                                                                              a_v=a_v[[x]]),input[x,"connection"]))

# Tragfaehigkeit des Verbindungsmittels
f_v_r_k <- lapply(seq_along(input[,"connection"]),function(x) {
  
  if(input[x,"connection"] %in% names_notches) {
    f_v_r_k <- get_f_v_r_k_notch(b_n=x_v_eff[[x]]*1000,
                                 t_n=input[x,"t_v"]*1000,
                                 l_v=l_v[[x]]*1000,
                                 a_v=a_v_net[[x]]*1000, # Netto Abstaende
                                 f_t_c0_d=input[x,"f_t_c0_d"],
                                 f_t_v_d=input[x,"f_t_v_d"],
                                 f_c_c_d=input[x,"f_c_c_d"],
                                 f_c_c_k=input[x,"f_c_c_k"],
                                 k_cr=input[x,"k_cr"],
                                 h_c=input[x,"h_c"])
  } 
  if(input[x,"connection"] %in% names_screws) {
    f_v_r_k <- get_f_v_r_k_screw(l_ef=mapply(function(y) ifelse(y>0.5,0.5,y),y=x_v_eff[[x]])*1000,
                                 d=input[x,"t_v"]*1000,
                                 f_t_c0_k=input[x,"f_t_c0_k"],
                                 f_t_v_k=input[x,"f_t_v_k"],
                                 f_c_c_k=input[x,"f_c_c_k"],
                                 f_v_tens_k=input[x,"f_v_tens_k"],
                                 f_v_ax_k=input[x,"f_v_ax_k"],
                                 alpha_v=input[x,"alpha_v"],
                                 rho_t=input[x,"rho_t_k"],
                                 mu_v=input[x,"mu_v"],
                                 gam_v=input[x,"gam_v"])
  }
  if(input[x,"connection"] %in% names_glue) {f_v_r_k <- NA}
  return(f_v_r_k)
})

# Berechnug des Design-Werts des Widerstands# der Scritt entfällt jetzt. Rd wird direkt berechnet
#f_v_r_k <- lapply(seq_along(f_v_r_k),function(xi) {
#  if(input[xi,"connection"] %in% names_mech_con) {
#    f_v_r_k[[xi]][,"min_rd"] <- f_v_r_k[[xi]][,"min"]/input[xi,"gam_v"]
#    
#    return(f_v_r_k[[xi]])}
#  else  {return(NA)}
#})#

# Spiegeln der Werte für die Zweite Trägerhälfte
f_v_r_k <- lapply(seq_along(f_v_r_k),function(x) {
  if(input[x,"connection"] %in% names_mech_con) {
    rbind(f_v_r_k[[x]],f_v_r_k[[x]])
  } else {f_v_r_k[[x]]}
})

#Querkraft Beiwerte Beton
#Noch einmal ?berpr?fen, Normalkraft im Beton einrechnen? Querkraft nachweis nachstellen? Oder auf sicherer Seite ohne Druck rechnen. Vermutlich das.
input[,"vcrd_k"] <- mapply(function(x) min(2,1+sqrt(200/(x*1000))),x=input[,"d_c"])
input[,"vcrdmin_vmin"] <- 0.035*input[,"vcrd_k"]^(3/2)*input[,"f_c_c_k"]^(1/2)
input[,"vcrdmin"] <- 1/1000*(input[,"vcrdmin_vmin"]+0)*input[,"b_c"]*1000*input[,"d_c"]*1000 # (vcrdmin+ 0.15*0.2*input[,"f_c_c_d"])k1 = 0.15; sig_cp = ned/ac < 0.2*fcd
input[,"crdc"] <- 0.18/input[,"gam_c"]
input[,"rho_c_l"] <- input[,"a_s_l"]*1/10000/input[,"d_c"]
input[,"sig_n_c_ed"] <- 0 #min(abs(input[,"n_c_ed"]/input[,"a_c"]),0.2*input[,"a_c"])
input[,"vcrd"] <- max(input[,"vcrdmin"],1/1000*(input[,"crdc"]*input[,"vcrd_k"]*(100*input[,"f_c_c_k"]*input[,"rho_c_l"])^(1/3)+0.15*input[,"sig_n_c_ed"])*input[,"b_c"]*1000*input[,"d_c"]*1000)

