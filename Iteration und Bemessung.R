###############################################################################################
#### STABWERKBERECHNUNG UND BEMESSUNG VON HBV-DECKEN MIT NACHGIEBIGEM  UND STARREM VERBUND ####
###############################################################################################

# Lokaler Mathjax Ordner muss festgelegt werden, damit Latex in den Plots verwendet werden kann
mathjaxdir <-  "C:\\Users\\Susi\\Downloads\\MathJax-2.7.7\\MathJax-2.7.7" #choose.dir() # Heim-PC
#mathjaxdir <- "\\\\Bv.tu-berlin.de\\files\\Verbund\\marie.breidenbach\\Eigene Dateien\\MathJax-2.7.7\\MathJax-2.7.7" # TU Berlin
Sys.setenv('PLOTLY_MATHJAX_PATH'=mathjaxdir)

# Der Pfad zu wps.exe von Sofistik
sofipath <- "C:\\\"Program Files\"\\SOFiSTiK\\2018\\\"SOFiSTiK 2018\"\\wps.exe"
sofipath <- "C:\\\"Program Files\"\\SOFiSTiK\\2020\\\"SOFiSTiK 2020\"\\wps.exe"

##########################
##### Inputs öffnen? #####
##########################
titl <- readline(prompt="Titel der Eingabe (nur Teil hinter dem Unterstrich):")
setwd(choose.dir()) # Auswahl des Working Directories (Achtung, die Eingabedatei und die Funktionen müssen dort liegen!)
source("Funktionen_Bemess.R") # L?dt die Bemessungsfunktionen
source(paste("Eingabe_",titl,".R",sep="")) # Lädt die Eingabedatei
source("Bemessung_Preprocess.R") # Preprocess beinhaltet Berechnung VM-Festigkeit, Umwandlung in Teddy-Format, laden von Materialfestigkeit etc.
if(!(dir.exists("sofidata"))) {dir.create("sofidata")} # erstellt den Ordner sofidata, falls nicht vorhanden

iteration_bool <- readline(prompt="Soll iteriert werden? j/n")
calculation_bool <- readline(prompt="Sollen Bemessungsdateien gerechnet werden? j/n")
if(iteration_bool %in% c("n","nein")) {
  # Einleisen von Ergebnissen + Weiterverarbeitung
  
  # Einlesen der ersten Eingabe (Vor Iteration)
  input_alt <- read.table(paste(titl,"_post_iteration.txt",sep=""),sep=";",dec=".",header = TRUE)
  # Zum Öffnen der Iterationsergebnisse
  res_iter <- read.table(paste(titl,"_res_iteration.txt",sep=""),sep=";",dec=".",header=TRUE)
  
}

# Einlesen der Sofistik Templates
template_files <- unique(input[,"template"]) 
template <- load_template(template_files)

#############################
##### Iterationsschritt #####
#############################
if(iteration_bool %in% c("j","ja")) {
  
  # Erzeugen der Ordner und Dateinamen
  input[,"titl"] <- mapply(function(x) paste(titl,x,sep="_"),seq(1,nrow(input),1)) # Erzeugt fuer jeden Fall einen speziellen Titel
  teddydir <- file.path(paste("sofidata/",titl,"_",seq_along(input[,1]),sep="")) # Erzeugt Ordnernamen
  lapply(teddydir,dir.create)  # Erzeugt Ordner
  
  res_iter <- lapply(seq_along(input[,1]),function(x) {
    res <- data.frame(h_iter=c(),eta_gzt_mitt=c(),eta_gzg_mitt=c(),sig_gzt_mitt=c(),sig_gzg_mitt=c(),
                      w_iter=c(),gam_ks=c(),gam_ku=c(),ei_eff_gzg=c(),ei_eff_gzt=c())
    return(res)
  })
  
  # ERZEUGEN DER .DAT UND ORDNER
  
  neu_iter <- rep(TRUE,nrow(input)) # boolean Wert, der festlegt, ob eine Datei neu iteriert werden muss, Zu Beginn alle True
  input[,"iter_count"] <- 1 # Zaehlt die Iterationsschritte
  h_c_eff_neu <- input[,"h_c"] # Setzt die effektive Betonhoehe zunaechst auf die tatsaechlich vorhandene
  
  while(sum(neu_iter)>0) { 
    input[,"h_c_eff_iter"] <- h_c_eff_neu 
    
    zust <- lapply(seq_along(input[,1]),function(x) { c("iter") })
    teddytitl <- make_teddyfile(input,template,zust,c(1:nrow(input)),write=TRUE,subfolder = "sofidata") # Erzeugt die .dat Dateien und speichter die Namen der Dateien in die Variable teddytitl
    
    # BERECHNUNG IN SOFISTIK 
    rows_to_calculate <- which(neu_iter) # Filtert die Dateien, die noch nicht fertig iteriert sind
    for(i in rows_to_calculate) {
      
      filepath <- teddytitl[i]
      
      # Erzeugt Code, den die Commando-Zeile versteht und die Rechnung aufruft
      code <- gsub("([^/]+\\s[^/]+)", "\\\\\"\\1\"",teddytitl[i],perl=TRUE)
      code <- gsub("/","\\",code,fixed=TRUE)
      code <- paste("",code,suffix <- " -b",sep="") # Warten bis Befehl ausgef?rt wird
      quotes <- "\""
      cmd <- paste("start /WAIT",sofipath,code,sep=" ")
      
      shell(cmd) # Lässt die Datei über ShellCommand laufen
      
      #file.remove(cdb_file[spcmn])
      input[i,"iter_count"] <- input[i,"iter_count"]+1
    }
    
    # Einlesen der .xlsx Dateien, die Sofistik im prog results erstellt hat
    
    files_c <- lapply(seq_along(input[,1]),function(x) {
      if(input[x,"connection"] %in% names_glue)  {
        zust <- c("iter")
        lf <- c("g","q","s") # Eigengewicht, Nutzlast, Schwinden
        ort <- "res_c" # results f?r c=Beton
        k <- "x" # F?r Klebungen "x" (Bei kerven wird in ks und ku unterschieden)
        komb <- apply(expand.grid(ort,zust,k,lf),1,paste,collapse="_") # expand grit erstellt alle m?glichen Kombinationen der Werte in den gegebenen Vektoren
      }
      
      else {
        zust <- c("iter")
        lf <- c("g","q","s") # s.o.
        ort <- "res_c" # s.o.
        k <- c("ku","ks") # Bezieht sich auf die verschiedenen Verschiebungsmoduln (GZT/GZG)
        komb <- apply(expand.grid(ort,zust,k,lf),1,paste,collapse="_") # s.o.
      }
      return(komb)})
    
    files_w <- lapply(files_c,function(x) gsub("_c_","_w_",x)) # w= Durchbiegungen
    
    n_dirs <- 1:nrow(input) # einzulesende Dateien (indizes)
    
    # einlesen
    res_c <- einlesen_sofi_mult_folders_to_list("sofidata",titl,files_c,n_dirs) 
    res_w <- einlesen_sofi_mult_folders_to_list("sofidata",titl,files_w,n_dirs)
    
    #sortieren
    if(exists("x")) {remove(x)} # Darf nicht im Envir vorhanden sein, sonst kann es zu Schwierigkeiten kommen
    if(exists("y")) {remove(y)} # Darf nicht im Envir vorhanden sein, sonst kann es zu Schwierigkeiten kommen
    
    # durch alle listen innerhalb einer liste gehen und einzeln die versch. resultate suchen
    
    # Nach diesen Groessen werden die Resultate umsortiert
    sort_cols_t <- lapply(seq_along(input[,1]),function(x)  c("My","Mz","N","Vz","tau","sig_x_o","sig_x_u"))
    
    sort_cols_c <- lapply(seq_along(input[,1]),function(x) 
      c("My","Mz","N","Vz","tau","sig_x_o","sig_x_u"))
    
    sort_cols_w <- lapply(seq_along(input[,1]),function(x)  c("w"))
    
    res_c <- sofistik_sortieren(res_c,sort_cols_c,3,beiwerte=input)
    res_w <- sofistik_sortieren(res_w,sort_cols_w,3,beiwerte=input)
    
    # Neuberechnung der Koordinaten beim Betonbalken (Sofistik gibt immer nur die Koordinaten entlang eines Stabelements an,
    # sodass f?r die eigentlichen Koordinaten die kumulative Summe aller Balkenstuecke gebildt werden muss)
    
    res_c <- lapply(seq_along(res_c),function(xi) {
      res_c[[xi]] <- lapply(res_c[[xi]],function(yi) {
        yi[,"x"] <- cumsum(yi[,"x"])
        return(yi)
      })
      return(res_c[[xi]])
    })
    
    # Lastfallkombination
    
    res_c <- lastfallkombi_iteration(res_c,input,names_glue,names_mech_con)
    res_w <- lastfallkombi_iteration(res_w,input,names_glue,names_mech_con)
    
    # Iterations-Infos in oben angelegte Listen einspeichern
    # Liest aus den Nachweisen fuer jede LF-Kombination die Auslastung und Durchbiegung in Feldmitte aus
    # Aufgrund von Sprungen im Verlauf des Stabwerkmodells kann es vorkommen, dass die max. belasteten Stelle nicht in der Mitte lieg
    sig_c_mitt <- lapply(res_c,mitt_eta_pro_nachweis,"sig_x_u","gz")
    
    # Nachweis Beton Unterseite
    res_c <- lapply(seq_along(res_c),function(xi) { 
      
      sig_i<- c("sig_x_u") # an welcher Stelle wird der Nachweis gef?hrt?
      f_c_d <- c(input[xi,"f_c_c_d"],input[xi,"f_c_t_d"])
      
      if(input[xi,"connection"] %in% c(names_mech_con,names_glue)) {
        if (input[xi,"connection"] %in% names_mech_con) {
          k_j <- c("ku","ks")
        }
        if (input[xi,"connection"] %in% names_glue) {
          k_j <- c("x") 
        }
        
        for(j in k_j) {
          
          lf_gzt_iter <- paste("iter_gzt",j,sep="_")
          lf_gzg_iter <- paste("iter_gzg",j,sep="_")
          
          for(i in c(1:length(sig_i))) {
            ## Biegung und Normalkraft
            
            #inst unten
            eta_gzt_iter <- paste("eta",lf_gzt_iter,sep="_")
            eta_gzg_iter <- paste("eta",lf_gzg_iter,sep="_")
            
            f_c_c_d <- input[xi,"f_c_c_d"]
            f_c_t_d <- input[xi,"f_c_t_d"]
            # Hier wird entlang jeder Nachweisstelle entlang des Balkens (also fuer jeden Punkt x) geprueft, ob Zugspannungen oder Druckspannungen nachgewiesen werden
            # Beim Stabwerkmodell kommt es hauefiger zu Spruengen, sodass dieser Schritt auf jeden Fall durchgefuehrt werden muss!
            # Es wird in diesem SChritt ein Vektor fuer jeden Fall erstellt, der fuer jeden Punkt x entweder die Beton Zug oder Druckfestigkeit ablegt, je nach dem was nachgewiesen wird
            # Einmal fuer GZT und einmal fuer GZG Ueberlagerungen (auch wenn es wahrscheinlicher ist, dass der Beton im GZT weiter reisst, wird auch der GZG betrachtet)
            f_c_d_iter_gzt <- mapply(function(z) ifelse(z>0,f_c_t_d,-f_c_c_d),z=res_c[[xi]][[sig_i[[i]]]][,lf_gzt_iter[[1]]])
            f_c_d_iter_gzg <- mapply(function(z) ifelse(z>0,f_c_t_d,-f_c_c_d),z=res_c[[xi]][[sig_i[[i]]]][,lf_gzg_iter[[1]]])
            
            # Eigentliche Nachweisfueherung
            res_c[[xi]][[sig_i[i]]][,eta_gzt_iter[1]] <- res_c[[xi]][[sig_i[i]]][,lf_gzt_iter[1]]/(f_c_d_iter_gzt)
            res_c[[xi]][[sig_i[i]]][,eta_gzg_iter[1]] <- res_c[[xi]][[sig_i[i]]][,lf_gzg_iter[1]]/(f_c_d_iter_gzg)
          } 
        }
      } else {}
      return(res_c[[xi]])
    }) 
    
    # Iterations-Infos in oben angelegte Listen einspeichern
    # Liest aus den Nachweisen fuer jede LF-Kombination die Auslastung und Durchbiegung in Feldmitte aus
    # Aufgrund von Sprungen im Verlauf des Stabwerkmodells kann es vorkommen, dass die max. belasteten Stelle nicht in der Mitte lieg
    eta_c_mitt <- lapply(res_c,mitt_eta_pro_nachweis,"sig_x_u","eta") # Liste mit einem Data-Frame pro Fall, enth?lt pro Lastfall Auslastung in Feldmitte und Koordinaten
    
    w_mitt <- lapply(res_w,mitt_eta_pro_nachweis,c("w"),"gzg") # Liste mit einem Data-Frame pro Fall, enthaelt pro Lastfall Durchbiegung in Feldmitte und Koordinaten
    
    # Haengt die Durchbiegung im GZG an einen Vektor mit bereits iterierten Durchbiegungen an, immer Feldmitte
    #(soll zeigen, ob sich der GZG nachweis durch reissen der Betonhoehe verschlechtert oder verbessert)
    # gleiches fuer die iterierten Betonhoehen
    row_gzg <- lapply(seq_along(w_mitt),function(x) {
      if(input[x,"connection"] %in% names_mech_con) {r <- grep("gzg_ks",row.names(w_mitt[[x]]))}
      if(input[x,"connection"] %in% names_glue) {r <- grep("gzg",row.names(w_mitt[[x]]))}
      return(r)
    })
    
    w_iter <- lapply(seq_along(input[,1]),function(x) w_mitt[[x]][row_gzg[[x]],"eta"]) # Liste mit einem Vektor pro Fall
    h_iter <- lapply(seq_along(input[,1]),function(x) input[x,"h_c_eff_iter"]) # Liste mit einem Vektor pro Fall
    
    # liest zusaetzlich die max. Auslastung an der Beton-UK aus (mit Angabe der jeweiligen Stelle x im Balken)
    # max_sig_c <- lapply(res_c,max_eta_pro_nachweis,"sig_x_u","eta")  # Liste mit einem Data-Frame pro Fall, enth?lt pro Lastfall Auslastung in Feldmitte und Koordinaten
    
    # liest den Bemessungszustand (GZT/GZG) sowie den Lastfall aus, der zum Riss gefuehrt hat
    #zust_max_sig_mitt <- do.call(rbind,lapply(sig_c_mitt,function(x) rownames(x)[which.max(x[,"eta"])])) # Vektor mit einer Zeile pro Fall, enthaelt den Namen des fuer den Riss massgebenden LF
    #max_sig_mitt <- do.call(rbind,lapply(sig_c_mitt,function(x) max(x[,"eta"]))) # Vektor mit einer Zeile pro Fall, enthaelt die Auslastung in Feldmitte an der Beton UK
    
    # haengt die  hinter bereits iterierte Werte:
    # Es handelt sich um Listen, mit einem Vektor pro Fall, jede Vektorzeile enthaelt das Ergebnis einer Iteration
    #max_zust_iter <- lapply(seq_along(max_zust_iter),function(x) c(max_zust_iter[[x]],zust_max_sig_mitt[x]))
    #max_eta_mitt_iter <- lapply(seq_along(max_eta_mitt_iter),function(x) c(max_eta_mitt_iter[[x]],max_sig_mitt[x]))
    
    # Auslesen der Zeile der relevanten Ergebnisse 
    row_gzg <- lapply(seq_along(eta_c_mitt),function(x) {
      if(input[x,"connection"] %in% names_mech_con) {r <- grep("gzg_ks",row.names(eta_c_mitt[[x]]))}
      if(input[x,"connection"] %in% names_glue) {r <- grep("gzg",row.names(eta_c_mitt[[x]]))}
      return(r)
    })
    row_gzt <- lapply(seq_along(eta_c_mitt),function(x) {
      if(input[x,"connection"] %in% names_mech_con) {r <- grep("gzt_ku",row.names(eta_c_mitt[[x]]))}
      if(input[x,"connection"] %in% names_glue) {r <- grep("gzg",row.names(eta_c_mitt[[x]]))}
      return(r)
    })
    
    # Anhand der Zeilen row_gzg und row_gzt die relevanten Zeilen filtern (auftrennen in zwei verschiedene Liste der obigen Daten)
    eta_gzg_mitt <- lapply(seq_along(input[,1]),function(x) eta_c_mitt[[x]][row_gzg[[x]],"eta"]) # Liste mit einem Vektor pro Fall, enthaelt Auslastung Beton UK im GZG
    eta_gzt_mitt <- lapply(seq_along(input[,1]),function(x) eta_c_mitt[[x]][row_gzt[[x]],"eta"]) # Liste mit einem Vektor pro Fall, enthaelt Auslastung Beton UK im GZG
    
    # gleiches Prozedere wie oben nur für sig_c_mitt statt eta_c_mitt:
    row_gzg <- lapply(seq_along(sig_c_mitt),function(x) {
      if(input[x,"connection"] %in% names_mech_con) {r <- grep("gzg_ks",row.names(sig_c_mitt[[x]]))}
      if(input[x,"connection"] %in% names_glue) {r <- grep("gzg",row.names(sig_c_mitt[[x]]))}
      return(r)
    })
    row_gzt <- lapply(seq_along(sig_c_mitt),function(x) {
      if(input[x,"connection"] %in% names_mech_con) {r <- grep("gzt_ku",row.names(sig_c_mitt[[x]]))}
      if(input[x,"connection"] %in% names_glue) {r <- grep("gzg",row.names(sig_c_mitt[[x]]))}
      return(r)
    })
    
    sig_gzg_mitt <- lapply(seq_along(input[,1]),function(x) sig_c_mitt[[x]][row_gzg[[x]],"eta"])
    sig_gzt_mitt <- lapply(seq_along(input[,1]),function(x) sig_c_mitt[[x]][row_gzt[[x]],"eta"])
    
    
    # Zur Ermittlung der Verbundkriechwerte muss die Verbundsteifigkeit bekannt sein
    # Diese wird aus den Schnittgroessen der letzten Iteration ermittelt (vereinfachend!)
    # Dazu sind in erster Linie die Schnittgroessen im holz notwendig
    
    files_t <- lapply(seq_along(input[,1]),function(x) {
      if(input[x,"connection"] %in% names_glue)  {
        zust <- c("iter")
        lf <- c("g","q","s")
        ort <- "res_t"
        k <- "x"
        komb <- apply(expand.grid(ort,zust,k,lf),1,paste,collapse="_")
      }
      
      else {
        zust <- c("iter")
        lf <- c("g","q","s")
        ort <- "res_t"
        k <- c("ku","ks")
        komb <- apply(expand.grid(ort,zust,k,lf),1,paste,collapse="_")
      }
      return(komb)})
    
    n_dirs <- seq_along(input[,1])
    
    #einlesen
    res_t <- einlesen_sofi_mult_folders_to_list("sofidata",titl,files_t,n_dirs) 
    
    #sortieren
    if(exists("x")) {remove(x)}
    if(exists("y")) {remove(y)}
    
    sort_cols_t <- lapply(seq_along(input[,1]),function(x)  c("My","Mz","N","Vz","tau","sig_x_o","sig_x_u"))
    
    res_t <- sofistik_sortieren(res_t,sort_cols_t,3,beiwerte=input)
    
    # Neuberechnung der Koordinaten (kummulative Summe der Koordinaten entlang der einzelnen Balkenstuecke = globale Koordinaten)
    res_t <- lapply(seq_along(res_t),function(xi) {
      
      res_t[[xi]] <- lapply(res_t[[xi]],function(yi) {
        yi[,"x"] <- cumsum(yi[,"x"])
        return(yi)
      })
      return(res_t[[xi]])
    })
    
    # Filtern der fuer die Berechnung notwendigen Schnittgroessen
    input_iter <- input
    input_iter$a_c_eff <- input$b_c*h_c_eff_neu
    input_iter$h_c_eff <- h_c_eff_neu
    input_iter <- reverse_gamma(res_t,input_iter)
    
    # DF mit Iterationsschritten
    res_iter <- lapply(seq_along(res_iter),function(x) {
      res <- rbind(res_iter[[x]],c(
        h_iter=h_iter[[x]],
        eta_gzt_mitt=eta_gzt_mitt[[x]],
        eta_gzg_mitt=eta_gzg_mitt[[x]],
        sig_gzt_mitt=sig_gzt_mitt[[x]],
        sig_gzg_mitt=sig_gzg_mitt[[x]],
        w_iter=w_iter[[x]],
        gam_ks=input_iter[x,"gam_ks"],
        gam_ku=input_iter[x,"gam_ku"],
        ei_eff_gzg=input_iter[x,"ei_eff_ks"],
        ei_eff_gzt=input_iter[x,"ei_eff_ku"]))
      colnames(res) <- c("h_iter","eta_gzt_mitt","eta_gzg_mitt","sig_gzt_mitt","sig_gzg_mitt","w_iter","gam_ks","gam_ku","ei_eff_gzg","ei_eff_gzt")
      return(res)
    })
    
    
    # Ermittelt fuer jeden Fall, ob neu iteriert werden muss, oder nicht
    neu_iter <- mapply(function(xi)     ifelse(max(eta_gzt_mitt[[xi]],eta_gzg_mitt[[xi]])<1,FALSE,TRUE),xi=seq_along(input[,1]))
    # Legt die Betonhoehe fuer den naechsten Iterationsschritt fest
    h_c_eff_neu <- mapply(function(xi)  ifelse(!neu_iter[xi],input[xi,"h_c_eff_iter"],signif(input[xi,"h_c_eff_iter"]-0.005,3)),xi=seq_along(input[,1]))
    
    # Ueberprueft wie viel Beton noch uebrig ist; wird die halbe Betonhoehe unterschritten, wird die Iteration abgebrochen
    neu_iter_probe <- mapply(function(xi) ifelse(input[xi,"h_c_eff_iter"]<=input[xi,"h_c"]*0.5,FALSE,neu_iter[xi]),xi=seq_along(input[,1])) 
    # Speichert in einen Vektor, ob die Iteration abgebrochen wurde (diese Datei wird dann im weiteren Bemessungsverlauf verworfen)
    abbruch <- mapply(function(xi) ifelse(input[xi,"h_c_eff_iter"]<=input[xi,"h_c"]*0.5,TRUE,FALSE),xi=seq_along(input[,1]))
    
    neu_iter <- neu_iter_probe 
    
  }
  # Aufraeumen: Loescht alles ausser .dat und .xlsx Dateien
  files <- mapply(function(x) as.vector(list.files(x,full.names = TRUE)),x=teddydir)
  files <- unlist(files)
  files <- files[which(grepl("([.]dat)|([.]xlsx)",files)==FALSE)]
  file.remove(files)
  
  # Auslesen der Risshöhen
  
  h_c_eff_gzg <- unlist(lapply(res_iter,function(x) x[which(x[,"eta_gzg_mitt"]<1)[1],"h_iter"]))  # Liest den Wert aus Spalte h_iter, bei dem eta_gzt_mitt gerade unter 1 fällt
  h_c_eff_gzt <- unlist(lapply(res_iter,function(x) x[which(x[,"eta_gzt_mitt"]<1)[1],"h_iter"])) # Liest den Wert aus Spalte h_iter, bei dem eta_gzt_mitt gerade unter 1 fällt
  ei_eff_gzg <- unlist(lapply(res_iter,function(x) x[which(x[,"eta_gzg_mitt"]<1)[1],"ei_eff_gzg"]))  # Liest den Wert aus Spalte ei_eff_gzg , bei dem eta_gzt_mitt gerade unter 1 fällt
  ei_eff_gzt <- unlist(lapply(res_iter,function(x) x[which(x[,"eta_gzt_mitt"]<1)[1],"ei_eff_gzt"]))  # Liest den Wert aus Spalte ei_ef_gzt, bei dem eta_gzt_mitt gerade unter 1 fällt
  
  # Wandelt die Daten der Iterationsschritte aus einer Liste in einen Long Data Frame um
  # Die Daten sind jetzt 2D
  # Um zwischen Daten verschiedener Elemente zu unterschieden wird in der Zeile spec die Nummer des Elements hinterlegt.
  res_iter_long <- cbind(spec=rep(paste("SPEC",seq_along(res_iter),sep="_"),sapply(res_iter,nrow)),do.call("rbind",res_iter)) 
  
  # Schreibt die Iterationsdaten
  write.table(res_iter_long,paste(titl,"res_iteration.txt",sep="_"),row.names=FALSE,sep=";",dec=".")
  
  # Nach abgeschlossener Iteration
  # @ Marius: Hier könnte man für einen Vergleich verschiedener Risshöhen und die Auswirkung auf die Zustände bestimmen, was wo angesetzt wird.
  
  input[,"h_c_eff_gzg"] <- h_c_eff_gzg # wird aktuell nicht verwendet in den templates
  input[,"h_c_eff_gzt"] <- h_c_eff_gzt # ebenso
  
  # Diese Werte werden aktuell angesetzt in den Templates
  # Ich habe jetzt der einfachheit alle Zeiten mit gleichen Werten belegt
  # Wollte aber die Option lassen, dass man das theoretisch gesehen sehr fein differenzieren kann
  input[,"h_c_eff_inst_gzg"] <- input[,"h_c_eff_gzg"] 
  input[,"h_c_eff_t37_gzg"] <- input[,"h_c_eff_gzg"] # s.o.
  input[,"h_c_eff_fin_gzg"] <- input[,"h_c_eff_gzg"] # s.o.
  
  input[,"h_c_eff_inst_gzt"] <- input[,"h_c_eff_gzt"] # s.o.
  input[,"h_c_eff_t37_gzt"] <- input[,"h_c_eff_gzt"] # s.o.
  input[,"h_c_eff_fin_gzt"] <- input[,"h_c_eff_gzt"] # s.o.
  
  input[,"abbruch"] <- abbruch
  
  input[,"a_c_eff_gzg"] <- input[,"h_c_eff_gzg"]*input[,"b_c"]
  input[,"a_c_eff_gzt"] <- input[,"h_c_eff_gzt"]*input[,"b_c"]
  
  input[,"w_c_y_eff_gzg"] <- input[,"h_c_eff_gzg"]^2*input[,"b_c"]/6
  input[,"w_c_y_eff_gzt"] <- input[,"h_c_eff_gzt"]^2*input[,"b_c"]/6
  
  input[,"gam_ks"] <- input_iter$gam_ks
  input[,"gam_ku"] <- input_iter$gam_ku
  input[,"ei_eff_gzg"] <- ei_eff_gzg
  input[,"ei_eff_gzt"] <- ei_eff_gzt
  
  # Langzeitwerte ks
  
  ks <- lapply(seq_along(k_s_pm),function(x) round(k_s_pm[[x]]*x_v_eff[[x]]*n_v[[x]],1)) # x_eff = Breite der Kerve oder Einbindelaenge der Schraube, n_v = Anzahl Schrauben/Kerven
  ks <- lapply(ks,function(x) c(x,x))
  ku <- lapply(seq_along(ks),function(x) {if(input[x,"connection"] %in% names_screws) {
    round(2/3*ks[[x]],1)} else {ks[[x]]}})
  
  psi_ks <- get_psi_comp(input[,"gam_ks"],input[,"phi"],input[,"k_def_t"]) # Beiwert zur Berechnung der zeitabhaengigen E-Moduln 
  
  input[,"e_t_fin_ks"] <- round(input[,"e_t_p"]/(1+psi_ks[,"t_tinf"]*input[,"k_def_t"]),2) # Werte zum zeitpunkt t = oo
  input[,"e_c_fin_ks"] <- round(input[,"e_c_m"]/(1+psi_ks[,"c_tinf"]*input[,"phi"]),2) # Werte zum zeitpunkt t = oo
  ks_fin <- lapply(seq_along(ks),function(xi) round(ks[[xi]]/(1+psi_ks[xi,"k_tinf"]*input[xi,"k_def_v"]),1)) # Werte zum zeitpunkt t = oo
  
  input[,"k_s_fin"] <- do.call("rbind",lapply(ks_fin,function(x) paste(x,collapse=","))) # Fuer Sofistik
  
  # Werte zum  Zeitpunkt t = 3....7 Jahre - werden nur benoetigt, wenn Nachweise mit t = oo bei 25% zusaetzlicher Last nicht eingehalten 
  input[,"e_t_t37_ks"] <- round(input[,"e_t_p"]/(1+psi_ks[,"t_t37"]*input[,"k_def_t"]),2)
  input[,"e_c_t37_ks"] <- round(input[,"e_c_m"]/(1+psi_ks[,"c_t37"]*input[,"phi"]),2)
  ks_t37 <- lapply(seq_along(ks),function(xi) round(ks[[xi]]/(1+psi_ks[xi,"k_t37"]*input[xi,"k_def_v"]),1))
  
  input[,"k_s_t37"] <- do.call("rbind",lapply(ks_t37,function(x) paste(x,collapse=","))) # Fuer Sofistik
  
  # Langzeitwerte ku
  psi_ku <- get_psi_comp(input[,"gam_ku"],input[,"phi"],input[,"k_def_t"]) # Beiwerte psi zur Berechnung der zeitabhaengigen E-Moduln
  
  input[,"e_t_fin_ku"] <- round(input[,"e_t_p"]/(1+psi_ku[,"t_tinf"]*input[,"k_def_t"]),2) # Werte zum zeitpunkt t = oo
  input[,"e_c_fin_ku"] <- round(input[,"e_c_m"]/(1+psi_ku[,"c_tinf"]*input[,"phi"]),2) # Werte zum zeitpunkt t = oo
  ku_fin <- lapply(seq_along(ku),function(xi) {
    if(input[xi,"connection"] %in% names_glue) {NA} else {round(ku[[xi]]/(1+psi_ku[xi,"k_tinf"]*input[xi,"k_def_v"]),1)}
  }) # Werte zum zeitpunkt t = oo
  
  input[,"k_u_fin"] <- do.call("rbind",lapply(ku_fin,function(x) paste(x,collapse=","))) # F?r Sofistik   
  
  # Werte zum  Zeitpunkt t = 3....7 Jahre - werden nur ben?tigt, wenn Nachweise mit t = oo bei 25% zus?tzlicher Last nicht eingehalten 
  input[,"e_t_t37_ku"] <- round(input[,"e_t_p"]/(1+psi_ku[,"t_t37"]*input[,"k_def_t"]),2)
  input["e_c_t37_ku"] <- round(input[,"e_c_m"]/(1+psi_ku[,"c_t37"]*input[,"phi"]),2)
  ku_t37 <- lapply(seq_along(ku),function(xi) {
    if(input[xi,"connection"] %in% names_glue) {NA} else {round(ku[[xi]]/(1+psi_ku[xi,"k_t37"]*input[xi,"k_def_v"]),1)}
  }) # Werte zum zeitpunkt t = oo
  
  input[,"k_u_t37"] <- do.call("rbind",lapply(ku_t37,function(x) paste(x,collapse=","))) # Fuer Sofistik
  
  #
  input[,"e_c_t37"] <- input[,"e_c_t37_ks"]
  input[,"e_c_fin"] <- input[,"e_c_fin_ks"]
  input[,"e_t_t37"] <- input[,"e_t_t37_ks"]
  input[,"e_t_fin"] <- input[,"e_t_fin_ks"]
  
  # Der alte Input wird hier gesichert, damit man ihn spaeter noch einmal anschauen kann, da
  # fuer die weitere Berechnung die Faelle verworfen werden, deren Iteration abgebrochen wurde
  input_alt <- input
  input_alt[,"abbruch"] <- abbruch
  input_alt$old_index <- seq(1:nrow(input_alt)) # Wichtig!!!! Damit die Dateien wieder in ihrem korrekten Ordner landen muss der alte Index beibehalten werden: 
  
  write.table(input_alt,paste(titl,"_post_iteration.txt",sep=""),quote=FALSE,row.names=FALSE,col.names=TRUE,sep=";",dec=".")
}

################################
##### STATISCHE BERECHNUNG #####
################################

# Ausfiltern der Abbruch-Dateien
input <- input_alt[which(input_alt$abbruch==FALSE),]
n_dirs <- input$old_index


# Speichern der Teddy-Dateien fuer die zu bemessenden Zustaende
zust <- lapply(seq_along(input[,1]),function(x) {
  if(input[x,"connection"] %in% names_glue) {
    c("inst_hgzt","t37_hgzt","fin_hgzt","inst_hgzg","t37_hgzg","fin_hgzg") # hier wird zwischen GZG und GZT Risshöhe unterschieden (unbedingt entsprechende template hbv_bemess_adhesive2 nutzen)
    # c("inst_x","t37_x","fin_x") # Diese Zustände für hbv_bemess_adhesive verwenden (hier wird nicht zwischen GZT und GZG Risshöhe unterschieden
    # sondern mit einer einheitlichen Risshöhe gerechnet)
  } else {
    c("inst_ku","t37_ku","fin_ku","inst_ks","t37_ks","fin_ks")
  }
})
# Erzeugen der Dateien


##### BERECHNUNG IN SOFISTIK #####

# Manuell (klappt nicht gut)
#teddytitl  <- teddytitl[which(files_to_calculate)]
#teddytitl_neu <- gsub("([^/]+\\s[^/]+)", "\\\\\"\\1\"",teddytitl,perl=TRUE)
#teddytitl_neu <- gsub("/","\\",teddytitl_neu,fixed=TRUE)
#neu <- paste("",teddytitl_neu,suffix <- " -b",sep="")
#cmd <- paste("start /WAIT",sofipath,neu,sep=" ")
#cmd <- paste(cmd, collapse=" && ")

#writeClipboard(cmd)

if(calculation_bool %in% c("j","ja")) {
  teddytitl <- make_teddyfile(input,template,zust,n_dirs,write=TRUE,subfolder="sofidata")
  for(xi in teddytitl) {
    
    code <- gsub("([^/]+\\s[^/]+)", "\\\\\"\\1\"",xi,perl=TRUE)
    code <- gsub("/","\\",code,fixed=TRUE)
    code <- paste("",code,suffix <- " -b",sep="")
    quotes <- "\""
    cmd <- paste("start /WAIT",sofipath,code,sep=" ")
    
    shell(cmd)
    
  }
  # Aufraeumen: Loescht alles ausser .dat und .xlsx Dateien
  files <- mapply(function(x) as.vector(list.files(x,full.names = TRUE)),x=teddydir)
  
  files <- unlist(files)
  files <- files[which(grepl("([.]dat)|([.]xlsx)",files)==FALSE)]
  file.remove(files)
}

# Einlesen der Teddy Ergebnisse
files_c <- lapply(seq_along(input[,1]),function(x) {
  if(input[x,"connection"] %in% names_glue)  {
    zust <- c("inst","t37","fin")
    lf <- c("g","q","s")
    ort <- "res_c"
    k <- c("hgzt","hgzg") #"x"
    komb <- apply(expand.grid(ort,zust,k,lf),1,paste,collapse="_")
  }
  
  else {
    zust <- c("inst","t37","fin")
    lf <- c("g","q","s")
    ort <- "res_c"
    k <- c("ku","ks")
    komb <- apply(expand.grid(ort,zust,k,lf),1,paste,collapse="_")
  }
  return(komb)})
files_t <- lapply(files_c,function(x) gsub("_c_","_t_",x))
files_w <- lapply(files_c,function(x) gsub("_c_","_w_",x))
files_v <- lapply(files_c,function(x) gsub("_c_","_v_",x))
files_wdyn <- lapply(seq_along(input[,1]),function(x) {
  if(input[x,"connection"] %in% names_glue)  {
    zust <- c("inst","fin")
    lf <- c("wdyn")
    ort <- "res_w" 
    k <- c("hgzt","hgzg") #"x" für template hbv_bemess_adhesive
    komb <- apply(expand.grid(ort,zust,k,lf),1,paste,collapse="_")
  }
  
  else {
    zust <- c("inst","fin")
    lf <- c("wdyn")
    ort <- "res_w"
    k <- c("ks")
    komb <- apply(expand.grid(ort,zust,k,lf),1,paste,collapse="_")
  }
})
files_eige <- lapply(seq_along(input[,1]),function(x) {
  if(input[x,"connection"] %in% names_glue)  {
    zust <- c("inst","fin")
    lf <- c("eige")
    ort <- "res_w" 
    k <- c("hgzt","hgzg") #"x"
    komb <- apply(expand.grid(ort,zust,k,lf),1,paste,collapse="_")
  }
  
  else {
    zust <- c("inst","fin")
    lf <- c("eige")
    ort <- "res_w"
    k <- c("ks")
    komb <- apply(expand.grid(ort,zust,k,lf),1,paste,collapse="_")
  }
})

# Einlesen
res_c <- einlesen_sofi_mult_folders_to_list("sofidata",titl,files_c,n_dirs) 
res_t <- einlesen_sofi_mult_folders_to_list("sofidata",titl,files_t,n_dirs)
res_w <- einlesen_sofi_mult_folders_to_list("sofidata",titl,files_w,n_dirs)
res_v <- einlesen_sofi_mult_folders_to_list("sofidata",titl,files_v,n_dirs) 
res_wdyn <- einlesen_sofi_mult_folders_to_list("sofidata",titl,files_wdyn,n_dirs)
res_eige <- einlesen_sofi_mult_folders_to_list("sofidata",titl,files_eige,n_dirs)

res_eige <- lapply(res_eige,function(y) lapply(y,function(z) {
  z$x <- seq_along(z[,1])
  return(z)}))
# Umsortieren
if(exists("x")) {remove(x)}
if(exists("y")) {remove(y)}

# durch alle listen innerhalb einer liste gehen und einzeln die versch. resultate suchen

sort_cols_t <- lapply(seq_along(input[,1]),function(x)  c("My","Mz","N","Vz","tau","sig_x_o","sig_x_u"))
sort_cols_c <- lapply(seq_along(input[,1]),function(x)  c("My","Mz","N","Vz","tau","sig_x_o","sig_x_u"))
sort_cols_w <- lapply(seq_along(input[,1]),function(x)  c("w"))
sort_cols_v <- lapply(seq_along(input[,1]),function(x)  c("Vy"))
sort_cols_wdyn <- lapply(seq_along(input[,1]),function(x)  c("w"))
sort_cols_eige <- lapply(seq_along(input[,1]),function(x)  c("freq"))

res_t <- sofistik_sortieren(res_t,sort_cols_t,3,beiwerte=input)
res_c <- sofistik_sortieren(res_c,sort_cols_c,3,beiwerte=input)
res_w <- sofistik_sortieren(res_w,sort_cols_w,3,beiwerte=input)
res_v <- sofistik_sortieren(res_v,sort_cols_v,3,beiwerte=input)
res_wdyn <- sofistik_sortieren(res_wdyn,sort_cols_wdyn,3,beiwerte=input)
res_eige <- sofistik_sortieren(res_eige,sort_cols_eige,3,beiwerte=input)

# Neuberechnung der x-Koordinaten
res_t <- lapply(res_t,function(xi) {
  xi <- lapply(xi,function(yi) {
    yi[,"x"] <- cumsum(yi[,"x"])
    return(yi)
  })
  return(xi)
})
# Die Berechnung der Spannungen aufgeloest nach Schnittkraeften ist notwendig fuer die Nachweisfuehrung bei kombinierter Normal-Biegebeanspruchung!
res_t <- lapply(seq_along(res_t),function(x) {
  
  non_coordinates <- grep("^[xy]{1}",colnames(res_t[[x]][["My"]]),perl=TRUE,invert=TRUE) # MY nur exemplarisch
  non_fire <- grep("fire",colnames(res_t[[x]][["My"]]),invert=TRUE) 
  non_fire <- intersect(non_coordinates,non_fire)
  
  fire <- grep("fire",colnames(res_t[[x]][["My"]]))
  fire <- intersect(non_coordinates,fire)
  
  res_t[[x]][["sig_x_u_probe"]] <- res_t[[x]][["My"]]
  res_t[[x]][["sig_x_o_probe"]] <- res_t[[x]][["My"]]
  
  res_t[[x]][["sig_x_u_probe"]][,non_fire] <- +(res_t[[x]][["My"]][,non_fire]/input[x,"wy_t"]+res_t[[x]][["N"]][,non_fire]/input[x,"a_t"])/1000
  res_t[[x]][["sig_x_o_probe"]][,non_fire] <- (-res_t[[x]][["My"]][,non_fire]/input[x,"wy_t"]+res_t[[x]][["N"]][,non_fire]/input[x,"a_t"])/1000
  
  res_t[[x]][["sig_x_u_probe"]][,fire] <- +(res_t[[x]][["My"]][,fire]/input[x,"wy_t_fi"]+res_t[[x]][["N"]][,fire]/input[x,"a_t_fi"])/1000
  res_t[[x]][["sig_x_o_probe"]][,fire] <- (-res_t[[x]][["My"]][,fire]/input[x,"wy_t_fi"]+res_t[[x]][["N"]][,fire]/input[x,"a_t_fi"])/1000
  
  
  # Getrennt nach Schnittkraeften My (fuer den nach schnittkraeften getrennten Nachweis nach EC5)
  
  res_t[[x]][["sig_x_u_my"]] <- res_t[[x]][["My"]] # Liste wird zuerst kopiert, damit Koordinaten usw. erhalten bleiben
  res_t[[x]][["sig_x_o_my"]] <- res_t[[x]][["My"]]
  
  res_t[[x]][["sig_x_u_my"]][,non_fire] <- +(res_t[[x]][["My"]][,non_fire]/input[x,"wy_t"])/1000
  res_t[[x]][["sig_x_o_my"]][,non_fire] <- (-res_t[[x]][["My"]][,non_fire]/input[x,"wy_t"])/1000
  
  res_t[[x]][["sig_x_u_my"]][,fire] <- +(res_t[[x]][["My"]][,fire]/input[x,"wy_t_fi"])/1000
  res_t[[x]][["sig_x_o_my"]][,fire] <- (-res_t[[x]][["My"]][,fire]/input[x,"wy_t_fi"])/1000
  
  #getrennt nach Schnittkr?ften N
  
  res_t[[x]][["sig_x_n"]] <- res_t[[x]][["N"]]
  
  res_t[[x]][["sig_x_n"]][,non_fire] <- res_t[[x]][["N"]][,non_fire]/input[x,"a_t"]/1000
  
  res_t[[x]][["sig_x_n"]][,fire] <- res_t[[x]][["N"]][,fire]/input[x,"a_t_fi"]/1000
  
  return(res_t[[x]])
})

# spannungsberechnung Beton
# Neuberechnung der Koordinaten
res_c<- lapply(seq_along(res_c),function(xi) {   
  res_c[[xi]] <- lapply(res_c[[xi]],function(yi) {
    yi[,"x"] <- cumsum(yi[,"x"])
    return(yi)
  })
  return(res_c[[xi]])
})
# Verbindungsmittel löschen, deren Dateien abgebrochen wurden

f_v_r_k <- lapply(input$old_index,function(x) f_v_r_k[[x]])
n_v <- lapply(input$old_index,function(x) n_v[[x]])
n_v <- lapply(seq_along(n_v),function(x) {
  if(input$connection[x] %in% names_glue) {
    input[x,"b_t"]
  } else {n_v[[x]]}
})

# Belastung pro Verbindungsmittel berechnen
# In Sofistik pro Schraubenreihe nur ein Element, daher wird auch eine Kraft pro Verbindungsmittelreihe ausgegeben
# Die Kraft muss bei mehreren Schrauben in einer Reihe als wieder gesplittet werden
res_v <- lapply(seq_along(res_v),function(xi) {
  if(input[xi,"connection"] %in% names_glue) {} else {
    res_v[[xi]] <- lapply(res_v[[xi]],function(yi) {
      
      cols <- grep("^[xy]{1}",colnames(yi),perl=TRUE,invert=TRUE)
      yi[,cols] <- yi[,cols]/n_v[[xi]]
      return(yi)
    })
  }
  return(res_v[[xi]])
})
# Nachweis der Verbindungsmittel
# Bei Klebung Umrechnung der Gurtkräfte in eine Schubbeanspruchung der Fuge
input[which(is.na(input[,"n_v_x"])),"n_v_x"] <- input[which(is.na(input[,"n_v_x"])),"b_t"]

res_v <- lapply(seq_along(res_v),function(xi) {
  if(input[xi,"connection"] %in% names_glue) {
    cols_to_add <- grep("^[x]{1}",colnames(res_c[[xi]][["Vz"]]),perl=TRUE,invert=TRUE)
    cols_to_add_2 <- grep("sep",colnames(res_t[[xi]][["Vz"]]),perl=TRUE,invert=TRUE)
    cols_to_add_3 <- grep("eta",colnames(res_t[[xi]][["Vz"]]),perl=TRUE,invert=TRUE)
    cols_to_add_4 <- grep("_gzt",colnames(res_t[[xi]][["Vz"]]),perl=TRUE,invert=TRUE)
    cols_to_add_5 <- grep("_gzg",colnames(res_t[[xi]][["Vz"]]),perl=TRUE,invert=TRUE)
    cols_to_add <- intersect(cols_to_add,cols_to_add_2) %>% intersect(cols_to_add_3) %>% intersect(cols_to_add_4) %>% intersect(cols_to_add_5)
    Vy <- (res_t[[xi]][["Vz"]][,cols_to_add]+res_c[[xi]][["Vz"]][cols_to_add])
    Vy <- Vy*1000/(input[xi,"n_v_x"]*1000*(input[xi,"h_t"]/2+input[xi,"h_c"]/2)*1000)
    
    Vy <- cbind(x=res_c[[xi]][["Vz"]][,"x"],Vy)
    res_v[[xi]] <- list(Vy=Vy)
  }
  return(res_v[[xi]])
}
)

# Lastfallkombination
res_t <- lastfallkombi(res_t,input,names_glue,names_mech_con)
res_c <- lastfallkombi(res_c,input,names_glue,names_mech_con)
res_w <- lastfallkombi(res_w,input,names_glue,names_mech_con)
res_v <- lastfallkombi(res_v,input,names_glue,names_mech_con) 
# weil keine Schnittgroessen fuer Verbindungsmittel bei Klebstoff vorhanden sind, wir der Vektor mit der Klebstoffzuordnung leer gelassen, so erfolgt keine Zuordnung und die Kombination wird uebersprungen

# Nachweis Holz
res_t <- lapply(seq_along(res_t),function(xi) { 
  
  sig_i<- c("sig_x_o","sig_x_u") # Es werden Spannungen an der Ober- und Unterseite nachgewiesen
  # Fuer welche Verschiebungsmoduln wird nachgewiesen? (GZT/GZG, KU/KS)
  if(input[xi,"connection"] %in% c(names_mech_con,names_glue)) {
    if (input[xi,"connection"] %in% names_mech_con) {
      k_j <- c("ku") #"ks") 
    }
    if (input[xi,"connection"] %in% names_glue) {
      k_j <- c("hgzt") 
    }
    
    k_mod_1 <- input[xi,"k_mod_t_perm"] # K-Mod fuer Lastfallkombi 1
    k_mod_2 <- input[xi,"k_mod_t_medi"] # K-Mod fuer Lastfallkombi 2
    k_mod_3 <- input[xi,"k_mod_t_perm"] # K-Mod fuer Lastfallkombi 3
    k_mod_4 <- input[xi,"k_mod_t_medi"] # K-Mod fuer Lastfallkombi 4
    
    # Speichern in temporaere Variablen, zur Uebersichtlichkeit
    f_t_m_d <- input[xi,"f_t_m_d"]
    f_t_t0_d <- input[xi,"f_t_t0_d"]
    f_t_v_d <- input[xi,"f_t_v_d"]
    k_cr <- input[xi,"k_cr"]
    
    # Schleife uber die Zustande
    for(j in k_j) {
      
      # Erzeugen der Lastfallbezeichnungen
      lf_gzt_inst <- paste("inst_gzt",j,c(1,2),sep="_") # Namen fuer Lastfallkombis 1 und 2 im Zustand t=0
      lf_gzt_fin <- paste("fin_gzt",j,c(1:4),sep="_") # Namen fuer Lastfallkombis 1-4 im Zustand t=00
      lf_gzt_t37 <- paste("t37_gzt",j,c(1:4),sep="_") # Namen fuer Lastfallkombis 1-4 im Zustand t=00
      
      # Schleife ueber die Nachweisorte (Spannung oben/ Spannung unten)
      
      for(i in sig_i) {
        # Biegung und Normalkraft
        # inst unten
        
        # Aus den Lastfallnamen werden nun die Namen fuer die Vektoren mit den Auslastungen gebildet
        eta_gzt_fin <- paste("eta",lf_gzt_fin,sep="_")
        eta_gzt_inst <- paste("eta",lf_gzt_inst,sep="_")
        eta_gzt_t37 <- paste("eta",lf_gzt_t37,sep="_")
        
        # Nachweis-Fuehrung
        res_t[[xi]][[i]][,eta_gzt_inst[1]] <- abs(res_t[[xi]][[i]][,lf_gzt_inst[1]]/(f_t_m_d*k_mod_1))
        res_t[[xi]][[i]][,eta_gzt_inst[2]] <- abs(res_t[[xi]][[i]][,lf_gzt_inst[2]]/(f_t_m_d*k_mod_2))
        
        res_t[[xi]][[i]][,eta_gzt_fin[1]] <- abs(res_t[[xi]][[i]][,lf_gzt_fin[1]]/(f_t_m_d*k_mod_1))
        res_t[[xi]][[i]][,eta_gzt_fin[2]] <- abs(res_t[[xi]][[i]][,lf_gzt_fin[2]]/(f_t_m_d*k_mod_2))
        res_t[[xi]][[i]][,eta_gzt_fin[3]] <- abs(res_t[[xi]][[i]][,lf_gzt_fin[3]]/(f_t_m_d*k_mod_3))
        res_t[[xi]][[i]][,eta_gzt_fin[4]] <- abs(res_t[[xi]][[i]][,lf_gzt_fin[4]]/(f_t_m_d*k_mod_4))
        
        res_t[[xi]][[i]][,eta_gzt_t37[1]] <- abs(res_t[[xi]][[i]][,lf_gzt_t37[1]]/(f_t_m_d*k_mod_1))
        res_t[[xi]][[i]][,eta_gzt_t37[2]] <- abs(res_t[[xi]][[i]][,lf_gzt_t37[2]]/(f_t_m_d*k_mod_2))
        res_t[[xi]][[i]][,eta_gzt_t37[3]] <- abs(res_t[[xi]][[i]][,lf_gzt_t37[3]]/(f_t_m_d*k_mod_3))
        res_t[[xi]][[i]][,eta_gzt_t37[4]] <- abs(res_t[[xi]][[i]][,lf_gzt_t37[4]]/(f_t_m_d*k_mod_4))
        
        
        # Nach einzelnen SChnittkraeften (bei kombinierter Beanspruchung nach EC5 durchzufuehren)
        # Zug Kombi 
        
        eta_gzt_inst <- paste("eta",lf_gzt_inst,"sep",sep="_")
        eta_gzt_fin <- paste("eta",lf_gzt_fin,"sep",sep="_")
        eta_gzt_t37 <- paste("eta",lf_gzt_t37,"sep",sep="_")
        
        # Zur einfacherern Indizierung
        my_i <- paste(i,"my",sep="_") # erzeugt entweder sig_o_my (Oberkante) oder sig_my_u (Unterkante)
        n_i <- "sig_x_n"
        
        f_t_c_d <- input[xi,"f_t_c0_d"] 
        f_t_t_d <- input[xi,"f_t_t0_d"]
        
        # Hier werden die Widerstaende fuer jedes x entlang des Balkens als Vektor abgelegt (Zugwiderstand, wenn Zugspannung herrscht, Druckwiderstand bei Druckspannung)
        # Wird Druck und Biegung oder Zug und Biegugn nachgewiesen? Druckwiderstand wird quadriert (siehe Nachweisformat Holz unter Druck und Biegung)
        f_t_d_inst_1 <- mapply(function(z) ifelse(z>0,k_mod_1*f_t_t_d,(k_mod_1*f_t_c_d^2)),z=res_t[[xi]][[n_i]][,lf_gzt_inst[1]])
        f_t_d_inst_2 <- mapply(function(z) ifelse(z>0,k_mod_2*f_t_t_d,(k_mod_2*f_t_c_d^2)),z=res_t[[xi]][[n_i]][,lf_gzt_inst[2]])
        
        f_t_d_fin_1 <- mapply(function(z) ifelse(z>0,k_mod_1*f_t_t_d,(k_mod_1*f_t_c_d^2)),z=res_t[[xi]][[n_i]][,lf_gzt_fin[1]])
        f_t_d_fin_2 <- mapply(function(z) ifelse(z>0,k_mod_2*f_t_t_d,(k_mod_2*f_t_c_d^2)),z=res_t[[xi]][[n_i]][,lf_gzt_fin[2]])
        f_t_d_fin_3 <- mapply(function(z) ifelse(z>0,k_mod_3*f_t_t_d,(k_mod_3*f_t_c_d^2)),z=res_t[[xi]][[n_i]][,lf_gzt_fin[3]])
        f_t_d_fin_4 <- mapply(function(z) ifelse(z>0,k_mod_4*f_t_t_d,(k_mod_4*f_t_c_d^2)),z=res_t[[xi]][[n_i]][,lf_gzt_fin[4]])
        
        f_t_d_t37_1 <- mapply(function(z) ifelse(z>0,k_mod_1*f_t_t_d,(k_mod_1*f_t_c_d^2)),z=res_t[[xi]][[n_i]][,lf_gzt_t37[1]])
        f_t_d_t37_2 <- mapply(function(z) ifelse(z>0,k_mod_2*f_t_t_d,(k_mod_2*f_t_c_d^2)),z=res_t[[xi]][[n_i]][,lf_gzt_t37[2]])
        f_t_d_t37_3 <- mapply(function(z) ifelse(z>0,k_mod_3*f_t_t_d,(k_mod_3*f_t_c_d^2)),z=res_t[[xi]][[n_i]][,lf_gzt_t37[3]])
        f_t_d_t37_4 <- mapply(function(z) ifelse(z>0,k_mod_4*f_t_t_d,(k_mod_4*f_t_c_d^2)),z=res_t[[xi]][[n_i]][,lf_gzt_t37[4]])
        
        # Auslesen der Spannungen aus Normalkraft fuer jedes x (als temporaeren Vector)
        # Herrscht Zug-Normalkraft oder Drucknormalkraft? Wenn Druck, werden Spannungen quadriert (siehe Nachweisformat Holz unter Druck und Biegung)
        f_t_n_inst_1 <- mapply(function(z) ifelse(z>0,z,z^2),z=res_t[[xi]][[n_i]][,lf_gzt_inst[1]])
        f_t_n_inst_2 <- mapply(function(z) ifelse(z>0,z,z^2),z=res_t[[xi]][[n_i]][,lf_gzt_inst[2]])
        
        f_t_n_fin_1 <- mapply(function(z) ifelse(z>0,z,z^2),z=res_t[[xi]][[n_i]][,lf_gzt_fin[1]])
        f_t_n_fin_2 <- mapply(function(z) ifelse(z>0,z,z^2),z=res_t[[xi]][[n_i]][,lf_gzt_fin[2]])
        f_t_n_fin_3 <- mapply(function(z) ifelse(z>0,z,z^2),z=res_t[[xi]][[n_i]][,lf_gzt_fin[3]])
        f_t_n_fin_4 <- mapply(function(z) ifelse(z>0,z,z^2),z=res_t[[xi]][[n_i]][,lf_gzt_fin[4]])
        
        f_t_n_t37_1 <- mapply(function(z) ifelse(z>0,z,z^2),z=res_t[[xi]][[n_i]][,lf_gzt_t37[1]])
        f_t_n_t37_2 <- mapply(function(z) ifelse(z>0,z,z^2),z=res_t[[xi]][[n_i]][,lf_gzt_t37[2]])
        f_t_n_t37_3 <- mapply(function(z) ifelse(z>0,z,z^2),z=res_t[[xi]][[n_i]][,lf_gzt_t37[3]])
        f_t_n_t37_4 <- mapply(function(z) ifelse(z>0,z,z^2),z=res_t[[xi]][[n_i]][,lf_gzt_t37[4]])
        
        # Nachweisfuehrung
        res_t[[xi]][[i]][,eta_gzt_inst[1]] <- f_t_n_inst_1/f_t_d_inst_1+abs(res_t[[xi]][[my_i]][,lf_gzt_inst[1]]/(f_t_m_d*k_mod_1))
        res_t[[xi]][[i]][,eta_gzt_inst[2]] <- f_t_n_inst_2/f_t_d_inst_2+abs(res_t[[xi]][[my_i]][,lf_gzt_inst[2]]/(f_t_m_d*k_mod_2))
        
        res_t[[xi]][[i]][,eta_gzt_fin[1]] <- f_t_n_fin_1/f_t_d_fin_1+abs(res_t[[xi]][[my_i]][,lf_gzt_fin[1]]/(f_t_m_d*k_mod_1))
        res_t[[xi]][[i]][,eta_gzt_fin[2]] <- f_t_n_fin_2/f_t_d_fin_2+abs(res_t[[xi]][[my_i]][,lf_gzt_fin[2]]/(f_t_m_d*k_mod_2))
        res_t[[xi]][[i]][,eta_gzt_fin[3]] <- f_t_n_fin_3/f_t_d_fin_3+abs(res_t[[xi]][[my_i]][,lf_gzt_fin[3]]/(f_t_m_d*k_mod_3))
        res_t[[xi]][[i]][,eta_gzt_fin[4]] <- f_t_n_fin_4/f_t_d_fin_4+abs(res_t[[xi]][[my_i]][,lf_gzt_fin[4]]/(f_t_m_d*k_mod_4))
        
        res_t[[xi]][[i]][,eta_gzt_t37[1]] <- f_t_n_t37_1/f_t_d_t37_1+abs(res_t[[xi]][[my_i]][,lf_gzt_t37[1]]/(f_t_m_d*k_mod_1))
        res_t[[xi]][[i]][,eta_gzt_t37[2]] <- f_t_n_t37_2/f_t_d_t37_2+abs(res_t[[xi]][[my_i]][,lf_gzt_t37[2]]/(f_t_m_d*k_mod_2))
        res_t[[xi]][[i]][,eta_gzt_t37[3]] <- f_t_n_t37_3/f_t_d_t37_3+abs(res_t[[xi]][[my_i]][,lf_gzt_t37[3]]/(f_t_m_d*k_mod_3))
        res_t[[xi]][[i]][,eta_gzt_t37[4]] <- f_t_n_t37_4/f_t_d_t37_4+abs(res_t[[xi]][[my_i]][,lf_gzt_t37[4]]/(f_t_m_d*k_mod_4))
        
      } 
      
      ## Querkraft-Nachweis
      eta_gzt_fin <- paste("eta",lf_gzt_fin,sep="_")
      eta_gzt_inst <- paste("eta",lf_gzt_inst,sep="_")
      eta_gzt_t37 <- paste("eta",lf_gzt_t37,sep="_")
      
      res_t[[xi]][["tau"]][,eta_gzt_inst[1]] <- res_t[[xi]][["tau"]][,lf_gzt_inst[1]]/(f_t_v_d*k_mod_1*k_cr)
      res_t[[xi]][["tau"]][,eta_gzt_inst[2]] <- res_t[[xi]][["tau"]][,lf_gzt_inst[2]]/(f_t_v_d*k_mod_2*k_cr)
      
      res_t[[xi]][["tau"]][,eta_gzt_fin[1]] <- res_t[[xi]][["tau"]][,lf_gzt_fin[1]]/(f_t_v_d*k_mod_1*k_cr)
      res_t[[xi]][["tau"]][,eta_gzt_fin[2]] <- res_t[[xi]][["tau"]][,lf_gzt_fin[2]]/(f_t_v_d*k_mod_2*k_cr)
      res_t[[xi]][["tau"]][,eta_gzt_fin[3]] <- res_t[[xi]][["tau"]][,lf_gzt_fin[3]]/(f_t_v_d*k_mod_3*k_cr)
      res_t[[xi]][["tau"]][,eta_gzt_fin[4]] <- res_t[[xi]][["tau"]][,lf_gzt_fin[4]]/(f_t_v_d*k_mod_4*k_cr)
      
      
      res_t[[xi]][["tau"]][,eta_gzt_t37[1]] <- res_t[[xi]][["tau"]][,lf_gzt_t37[1]]/(f_t_v_d*k_mod_1*k_cr)
      res_t[[xi]][["tau"]][,eta_gzt_t37[2]] <- res_t[[xi]][["tau"]][,lf_gzt_t37[2]]/(f_t_v_d*k_mod_2*k_cr)
      res_t[[xi]][["tau"]][,eta_gzt_t37[3]] <- res_t[[xi]][["tau"]][,lf_gzt_t37[3]]/(f_t_v_d*k_mod_3*k_cr)
      res_t[[xi]][["tau"]][,eta_gzt_t37[4]] <- res_t[[xi]][["tau"]][,lf_gzt_t37[4]]/(f_t_v_d*k_mod_4*k_cr)
      
    }
    #Hier koennte noch ein Brandnachweis folgen
    #res_t[[xi]][["tau"]][,"eta_inst_fire_1"] <- res_t[[xi]][["tau"]][,"inst_fire_1"]/input[xi,"f_t_v_k_fi"]/input[xi,"k_cr"] 
  } else {}
  
  return(res_t[[xi]])
}) 

# Nachweis Beton
res_c <- lapply(seq_along(res_c),function(xi) { 
  sig_i<- c("sig_x_o","sig_x_u")
  #f_c_d <- c(input[xi,"f_c_c_d"],input[xi,"f_c_t_d"])
  
  if(input[xi,"connection"] %in% c(names_mech_con,names_glue)) {
    if (input[xi,"connection"] %in% names_mech_con) {
      k_j <- c("ku") #,"ksfin") 
    }
    if (input[xi,"connection"] %in% names_glue) {
      k_j <- c("hgzt") 
    }
    
    for(j in k_j) {
      
      lf_gzt_inst <- paste("inst_gzt",j,c(1,2),sep="_")
      lf_gzt_fin <- paste("fin_gzt",j,c(1:4),sep="_")
      lf_gzt_t37 <- paste("t37_gzt",j,c(1:4),sep="_")
      
      for(i in c(1:length(sig_i))) {
        ## Biegung und Normalkraft
        #inst unten
        eta_gzt_fin <- paste("eta",lf_gzt_fin,sep="_")
        eta_gzt_inst <- paste("eta",lf_gzt_inst,sep="_")
        eta_gzt_t37 <- paste("eta",lf_gzt_t37,sep="_")
        
        #res_c[[xi]][[sig_i[i]]][,eta_gzt_inst[1]] <- abs(res_c[[xi]][[sig_i[i]]][,lf_gzt_inst[1]]/(f_c_d[i]))
        #res_c[[xi]][[sig_i[i]]][,eta_gzt_inst[2]] <- abs(res_c[[xi]][[sig_i[i]]][,lf_gzt_inst[2]]/(f_c_d[i]))
        
        #res_c[[xi]][[sig_i[i]]][,eta_gzt_fin[1]] <- abs(res_c[[xi]][[sig_i[i]]][,lf_gzt_fin[1]]/(f_c_d[i]))
        #res_c[[xi]][[sig_i[i]]][,eta_gzt_fin[2]] <- abs(res_c[[xi]][[sig_i[i]]][,lf_gzt_fin[2]]/(f_c_d[i]))
        #res_c[[xi]][[sig_i[i]]][,eta_gzt_fin[3]] <- abs(res_c[[xi]][[sig_i[i]]][,lf_gzt_fin[3]]/(f_c_d[i]))
        #res_c[[xi]][[sig_i[i]]][,eta_gzt_fin[4]] <- abs(res_c[[xi]][[sig_i[i]]][,lf_gzt_fin[4]]/(f_c_d[i]))
        
        #res_c[[xi]][[sig_i[i]]][,eta_gzt_t37[1]] <- abs(res_c[[xi]][[sig_i[i]]][,lf_gzt_t37[1]]/(f_c_d[i]))
        #res_c[[xi]][[sig_i[i]]][,eta_gzt_t37[2]] <- abs(res_c[[xi]][[sig_i[i]]][,lf_gzt_t37[2]]/(f_c_d[i]))
        #res_c[[xi]][[sig_i[i]]][,eta_gzt_t37[3]] <- abs(res_c[[xi]][[sig_i[i]]][,lf_gzt_t37[3]]/(f_c_d[i]))
        #res_c[[xi]][[sig_i[i]]][,eta_gzt_t37[4]] <- abs(res_c[[xi]][[sig_i[i]]][,lf_gzt_t37[4]]/(f_c_d[i]))
        f_c_c_d <- input[xi,"f_c_c_d"]
        f_c_t_d <- input[xi,"f_c_t_d"]
        f_c_d_inst_1 <- mapply(function(z) ifelse(z>0,f_c_t_d,-f_c_c_d),z=res_c[[xi]][[sig_i[[i]]]][,lf_gzt_inst[[1]]])
        f_c_d_inst_2 <- mapply(function(z) ifelse(z>0,f_c_t_d,-f_c_c_d),z=res_c[[xi]][[sig_i[[i]]]][,lf_gzt_inst[[2]]])
        
        f_c_d_fin_1 <- mapply(function(z) ifelse(z>0,f_c_t_d,-f_c_c_d),z=res_c[[xi]][[sig_i[[i]]]][,lf_gzt_fin[[1]]])
        f_c_d_fin_2 <- mapply(function(z) ifelse(z>0,f_c_t_d,-f_c_c_d),z=res_c[[xi]][[sig_i[[i]]]][,lf_gzt_fin[[2]]])
        f_c_d_fin_3 <- mapply(function(z) ifelse(z>0,f_c_t_d,-f_c_c_d),z=res_c[[xi]][[sig_i[[i]]]][,lf_gzt_fin[[3]]])
        f_c_d_fin_4 <- mapply(function(z) ifelse(z>0,f_c_t_d,-f_c_c_d),z=res_c[[xi]][[sig_i[[i]]]][,lf_gzt_fin[[4]]])
        
        f_c_d_t37_1 <- mapply(function(z) ifelse(z>0,f_c_t_d,-f_c_c_d),z=res_c[[xi]][[sig_i[[i]]]][,lf_gzt_t37[[1]]])
        f_c_d_t37_2 <- mapply(function(z) ifelse(z>0,f_c_t_d,-f_c_c_d),z=res_c[[xi]][[sig_i[[i]]]][,lf_gzt_t37[[2]]])
        f_c_d_t37_3 <- mapply(function(z) ifelse(z>0,f_c_t_d,-f_c_c_d),z=res_c[[xi]][[sig_i[[i]]]][,lf_gzt_t37[[3]]])
        f_c_d_t37_4 <- mapply(function(z) ifelse(z>0,f_c_t_d,-f_c_c_d),z=res_c[[xi]][[sig_i[[i]]]][,lf_gzt_t37[[4]]])
        
        res_c[[xi]][[sig_i[i]]][,eta_gzt_inst[1]] <- res_c[[xi]][[sig_i[i]]][,lf_gzt_inst[1]]/(f_c_d_inst_1)
        res_c[[xi]][[sig_i[i]]][,eta_gzt_inst[2]] <- res_c[[xi]][[sig_i[i]]][,lf_gzt_inst[2]]/(f_c_d_inst_2)
        
        res_c[[xi]][[sig_i[i]]][,eta_gzt_fin[1]] <- res_c[[xi]][[sig_i[i]]][,lf_gzt_fin[1]]/(f_c_d_fin_1)
        res_c[[xi]][[sig_i[i]]][,eta_gzt_fin[2]] <- res_c[[xi]][[sig_i[i]]][,lf_gzt_fin[2]]/(f_c_d_fin_2)
        res_c[[xi]][[sig_i[i]]][,eta_gzt_fin[3]] <- res_c[[xi]][[sig_i[i]]][,lf_gzt_fin[3]]/(f_c_d_fin_3)
        res_c[[xi]][[sig_i[i]]][,eta_gzt_fin[4]] <- res_c[[xi]][[sig_i[i]]][,lf_gzt_fin[4]]/(f_c_d_fin_4)
        
        res_c[[xi]][[sig_i[i]]][,eta_gzt_t37[1]] <- res_c[[xi]][[sig_i[i]]][,lf_gzt_t37[1]]/(f_c_d_t37_1)
        res_c[[xi]][[sig_i[i]]][,eta_gzt_t37[2]] <- res_c[[xi]][[sig_i[i]]][,lf_gzt_t37[2]]/(f_c_d_t37_2)
        res_c[[xi]][[sig_i[i]]][,eta_gzt_t37[3]] <- res_c[[xi]][[sig_i[i]]][,lf_gzt_t37[3]]/(f_c_d_t37_3)
        res_c[[xi]][[sig_i[i]]][,eta_gzt_t37[4]] <- res_c[[xi]][[sig_i[i]]][,lf_gzt_t37[4]]/(f_c_d_t37_4)
        
      }
      
      ## Querkraft
      eta_gzt_fin <- paste("eta",lf_gzt_fin,sep="_")
      eta_gzt_inst <- paste("eta",lf_gzt_inst,sep="_")
      eta_gzt_t37 <- paste("eta",lf_gzt_t37,sep="_")
      
      res_c[[xi]][["Vz"]][,eta_gzt_inst[1]] <- res_c[[xi]][["Vz"]][,lf_gzt_inst[1]]/(input[xi,"vcrd"])
      res_c[[xi]][["Vz"]][,eta_gzt_inst[2]] <- res_c[[xi]][["Vz"]][,lf_gzt_inst[2]]/(input[xi,"vcrd"])
      
      res_c[[xi]][["Vz"]][,eta_gzt_fin[1]] <- res_c[[xi]][["Vz"]][,lf_gzt_fin[1]]/(input[xi,"vcrd"])
      res_c[[xi]][["Vz"]][,eta_gzt_fin[2]] <- res_c[[xi]][["Vz"]][,lf_gzt_fin[2]]/(input[xi,"vcrd"])
      res_c[[xi]][["Vz"]][,eta_gzt_fin[3]] <- res_c[[xi]][["Vz"]][,lf_gzt_fin[3]]/(input[xi,"vcrd"])
      res_c[[xi]][["Vz"]][,eta_gzt_fin[4]] <- res_c[[xi]][["Vz"]][,lf_gzt_fin[4]]/(input[xi,"vcrd"])
      
      
      res_c[[xi]][["Vz"]][,eta_gzt_t37[1]] <- res_c[[xi]][["Vz"]][,lf_gzt_t37[1]]/(input[xi,"vcrd"])
      res_c[[xi]][["Vz"]][,eta_gzt_t37[2]] <- res_c[[xi]][["Vz"]][,lf_gzt_t37[2]]/(input[xi,"vcrd"])
      res_c[[xi]][["Vz"]][,eta_gzt_t37[3]] <- res_c[[xi]][["Vz"]][,lf_gzt_t37[3]]/(input[xi,"vcrd"])
      res_c[[xi]][["Vz"]][,eta_gzt_t37[4]] <- res_c[[xi]][["Vz"]][,lf_gzt_t37[4]]/(input[xi,"vcrd"])
      
    }
    #res_c[[xi]][["tau"]][,"eta_inst_fire_1"] <- res_c[[xi]][["tau"]][,"inst_fire_1"]/input[xi,"f_t_v_k_fi"]/input[xi,"k_cr"] 
  } else {}
  
  return(res_c[[xi]])
}) 

# Nachweis Durchbiegungen
res_w <- lapply(seq_along(res_w),function(xi) {
  
  if (input[xi,"connection"] %in% names_mech_con) {
    k_j <- c("ks") #,"ksfin") 
  }
  if (input[xi,"connection"] %in% names_glue) {
    k_j <- c("hgzg") 
  }
  for(j in k_j) {
    lf_gzg_inst <- paste("inst_gzg",j,sep="_")
    lf_gzg_fin <- paste("fin_gzg",j,sep="_")
    eta_gzg_fin <- paste("eta",lf_gzg_fin,sep="_")
    eta_gzg_inst <- paste("eta",lf_gzg_inst,sep="_")
    
    res_w[[xi]][["w"]][eta_gzg_inst] <- res_w[[xi]][["w"]][,lf_gzg_inst]/input[xi,"w_lim_inst"]
    res_w[[xi]][["w"]][eta_gzg_fin] <-  res_w[[xi]][["w"]][,lf_gzg_fin]/input[xi,"w_lim_fin"]
  }
  return(res_w[[xi]])
})


# Klebfugen-Festigkeit in die Liste der Verbindungsmittelfestigkeiten einarbeiten
f_v_r_k <- lapply(seq_along(f_v_r_k),function(xi) {
  if(input[xi,"connection"] %in% names_glue) {
    min_rd <- rep(input[xi,"f_t_v_d"],nrow(res_v[[xi]][[1]]))
    f_v_r_k[[xi]] <- data.frame(min_rd)
  }
  return(f_v_r_k[[xi]])
})

res_v <- lapply(seq_along(res_v),function(xi) { 
  # im Falle, dass kein VM existiert, weil es sich um eine Klebung handelt, Abfrage, ob die Liste = NA ist
  if(is.na(res_v[[xi]])) {return(NA)} else {
    
    if(input[xi,"connection"] %in% names_glue) {
      k_j <- "hgzt"
    } else {
      k_j <- c("ku") #,"ksfin")
    }
    
    k_mod_1 <- input[xi,"k_mod_v_perm"]
    k_mod_2 <- input[xi,"k_mod_v_medi"]
    k_mod_3 <- input[xi,"k_mod_v_perm"]
    k_mod_4 <- input[xi,"k_mod_v_medi"]
    
    f_v_r_d <- f_v_r_k[[xi]][,"min_rd"]
    
    
    for(j in k_j) {
      
      lf_gzt_inst <- paste("inst_gzt",j,c(1,2),sep="_")
      lf_gzt_fin <- paste("fin_gzt",j,c(1:4),sep="_")
      lf_gzt_t37 <- paste("t37_gzt",j,c(1:4),sep="_")
      
      ## Biegung und Normalkraft
      #inst unten
      eta_gzt_fin <- paste("eta",lf_gzt_fin,sep="_")
      eta_gzt_inst <- paste("eta",lf_gzt_inst,sep="_")
      eta_gzt_t37 <- paste("eta",lf_gzt_t37,sep="_")
      
      i <- "Vy"
      
      res_v[[xi]][[i]][,eta_gzt_inst[1]] <- abs(res_v[[xi]][[i]][,lf_gzt_inst[1]]/(f_v_r_d*k_mod_1))
      res_v[[xi]][[i]][,eta_gzt_inst[2]] <- abs(res_v[[xi]][[i]][,lf_gzt_inst[2]]/(f_v_r_d*k_mod_2))
      
      res_v[[xi]][[i]][,eta_gzt_fin[1]] <- abs(res_v[[xi]][[i]][,lf_gzt_fin[1]]/(f_v_r_d*k_mod_1))
      res_v[[xi]][[i]][,eta_gzt_fin[2]] <- abs(res_v[[xi]][[i]][,lf_gzt_fin[2]]/(f_v_r_d*k_mod_2))
      res_v[[xi]][[i]][,eta_gzt_fin[3]] <- abs(res_v[[xi]][[i]][,lf_gzt_fin[3]]/(f_v_r_d*k_mod_3))
      res_v[[xi]][[i]][,eta_gzt_fin[4]] <- abs(res_v[[xi]][[i]][,lf_gzt_fin[4]]/(f_v_r_d*k_mod_4))
      
      res_v[[xi]][[i]][,eta_gzt_t37[1]] <- abs(res_v[[xi]][[i]][,lf_gzt_t37[1]]/(f_v_r_d*k_mod_1))
      res_v[[xi]][[i]][,eta_gzt_t37[2]] <- abs(res_v[[xi]][[i]][,lf_gzt_t37[2]]/(f_v_r_d*k_mod_2))
      res_v[[xi]][[i]][,eta_gzt_t37[3]] <- abs(res_v[[xi]][[i]][,lf_gzt_t37[3]]/(f_v_r_d*k_mod_3))
      res_v[[xi]][[i]][,eta_gzt_t37[4]] <- abs(res_v[[xi]][[i]][,lf_gzt_t37[4]]/(f_v_r_d*k_mod_4))
      
    }
  }
  
  return(res_v[[xi]])
}) 

#### SCHWINGUNGEN ####
# Untersuchung mit F-min und Beschleunigung nur, wenn Schwingungsnachweis nicht erbracht wird
# Bez?glich des Aufbaus bestehen bei HBV Decken ueber 300 kg/m? keine Anforderungen nach EC 5
# Die Nachgiebigkeit des Unterzugs wird hier nicht beruecksichtigt! Der Nachweis ist also auf der unsicheren Seite und gibt nur Aufschluss darueber, 
# ob die Decke von "vorneherein" nicht funktioniert! ggf. k?nnte man das ueber einen Sicherheitsfaktor einfli?en lassen
input[,"f_grenz"] <- 8 # [Hz]  - Fremde Nutzungseinheiten
input[,"f_min"] <- 4.5

input[,"w_grenz"] <- 0.5 # [mm] - Fremde Nutzungseinheiten, kann in Absprache um 1.5 erh?ht werden

m <- 2500*input[,"h_c"]+input[,"g_1"]/9.81+(input$rho_t_mean*input$b_t*input$h_t)/input$l_y # in kg/m?
#m <- 2500*input[,"h_c"] # input[,"g_1"]/9.81+(input$rho_t_mean*input$b_t*input$h_t)/input$l_y # in kg/m?

#wird von Richter Hamm Winter nicht empfohlen anzuwenden, da der Nachweis kaum eingehalten werden kann
#input[,"a_grenz"] <- 0.05 # m/s?
#d <-  0.025 # D?mpfung HBV Rohdecke nach Hamm 2018 0.025 = 2.5%
#dyn_f <- mapply(function(x) ifelse(x>1.5 && x<2.5,280,ifelse(x>3 && x<5,140,ifelse(x>5,70,NA))),x=input[,"f_eige"]) #N; ggf. auch 140 N (kleinerer Wert auf der sicheren Seite), siehe Hamm 2018 S.5
#b_resonanz <- mapply(function(x) min(x,3),x=input[,"l_x"]) # nach Hamm 2018 soll der kleinere Wert der 1.5fachen Spannweite bzw der tats?chlichen RAUM Breite angenommen werden, MOdul Breite ist auf sehr sicherer Seite!
#a_prov <- 0.4*dyn_f/(m*0.5*input[,"l_x"]*0.5*b_resonanz*2*d)
#input[,"a_grenz"] > a_prov
#input[,"a_prov"] <- a_prov

# Steifigkeitskriterium
b_ef <- input[,"l_x"]/1.1*((input[,"h_c"]^3*1/12*1000*1000*input[,"e_c_m"])/(input[,"ei_eff_gzg"]/input[,"b_c"]))^(1/4)
bw <- sapply(seq_along(b_ef),function(x) min(b_ef[x],input$b_c[x]))

res_eige <- lapply(seq_along(res_eige),function(x) {
  if(input[x,"connection"] %in% names_glue) {
    # Masse Betonplatte 2500 kg/m3* h Beton in m -> kg/m2, EI muss in Nm2/m, l in m 
    res_eige[[x]][["freq"]][,"probe_inst_hgzt_eige"] <- pi/(2*input[x,"l_x"]^2)*sqrt(input[x,"ei_eff_gzt"]/(m[x])) # Mehr Last auf unsicherer Seite
    res_eige[[x]][["freq"]][,"probe_inst_hgzg_eige"] <- pi/(2*input[x,"l_x"]^2)*sqrt(input[x,"ei_eff_gzg"]/(m[x])) # Mehr Last auf unsicherer Seite
  }
  else {
    res_eige[[x]][["freq"]][,"probe_inst_ku_eige"] <- pi/(2*input[x,"l_x"]^2)*sqrt(input[x,"ei_eff_gzt"]/(m[x])) # Mehr Last auf unsicherer Seite
    res_eige[[x]][["freq"]][,"probe_inst_ks_eige"] <- pi/(2*input[x,"l_x"]^2)*sqrt(input[x,"ei_eff_gzg"]/(m[x])) # Mehr Last auf unsicherer Seite
  }
  return(res_eige[[x]])})

res_wdyn <- lapply(seq_along(res_wdyn),function(x) {
  if(input[x,"connection"] %in% names_glue) {
    res_wdyn[[x]][["w"]][,"probe_inst_hgzt_wdyn"] <- 2000*(input[x,"l_x"])^3/(48*input[x,"ei_eff_gzt"]*bw[x]/input[x,"b_c"])*1000 # mm
    res_wdyn[[x]][["w"]][,"probe_inst_hgzg_wdyn"] <- 2000*(input[x,"l_x"])^3/(48*input[x,"ei_eff_gzg"]*bw[x]/input[x,"b_c"])*1000 # mm
  }
  else {
    res_wdyn[[x]][["w"]][,"probe_inst_ku_wdyn"] <- 2000*(input[x,"l_x"])^3/(48*input[x,"ei_eff_gzt"]*bw[x]/input[x,"b_c"])*1000 # mm
    res_wdyn[[x]][["w"]][,"probe_inst_ks_wdyn"] <- 2000*(input[x,"l_x"])^3/(48*input[x,"ei_eff_gzg"]*bw[x]/input[x,"b_c"])*1000 # mm
  }
  return(res_wdyn[[x]])})

res_eige <- lapply(seq_along(res_eige),function(xi) { 
  # im Falle, dass kein VM existiert, weil es sich um eine Klebung handelt, Abfrage, ob die Liste = NA ist
  if(is.na(res_eige[[xi]])) {return(NA)} else {
    
    if(input[xi,"connection"] %in% names_glue) {
      k_j <- c("hgzt","hgzg")
    } else {
      k_j <- c("ks") #,"ksfin")
    }
    
    for(j in k_j) {
      
      lf_inst <- paste("inst",j,"eige",sep="_")
      lf_fin <- paste("fin",j,"eige",sep="_")
      lf_probe <- paste("probe_inst",j,"eige",sep="_")
      
      ## Biegung und Normalkraft
      #inst unten
      eta_fin <- paste("eta",lf_fin,sep="_")
      eta_inst <- paste("eta",lf_inst,sep="_")
      eta_probe <- paste("eta",lf_probe,sep="_")
      
      i <- "freq"
      
      # nur für die erste Eigenfrequenz!
      res_eige[[xi]][[i]][,eta_inst[1]] <- res_eige[[xi]][[i]][,lf_inst[1]]/input[xi,"f_grenz"]
      res_eige[[xi]][[i]][,eta_fin[1]]  <- res_eige[[xi]][[i]][,lf_fin[1]]/input[xi,"f_grenz"]
      res_eige[[xi]][[i]][,eta_probe[1]]  <- res_eige[[xi]][[i]][,lf_probe[1]]/input[xi,"f_grenz"]
    }
  }
  
  return(res_eige[[xi]])
}) 

res_wdyn <- lapply(seq_along(res_wdyn),function(xi) { 
  # im Falle, dass kein VM existiert, weil es sich um eine Klebung handelt, Abfrage, ob die Liste = NA ist
  if(is.na(res_wdyn[[xi]])) {return(NA)} else {
    
    if(input[xi,"connection"] %in% names_glue) {
      k_j <- c("hgzt","hgzg")
    } else {
      k_j <- c("ks") #,"ksfin")
    }
    
    for(j in k_j) {
      
      lf_inst <- paste("inst",j,"wdyn",sep="_")
      lf_fin <- paste("fin",j,"wdyn",sep="_")
      lf_probe <- paste("probe_inst",j,"wdyn",sep="_")
      
      ## Biegung und Normalkraft
      #inst unten
      eta_fin <- paste("eta",lf_fin,sep="_")
      eta_inst <- paste("eta",lf_inst,sep="_")
      eta_probe <- paste("eta",lf_probe,sep="_")
      
      i <- "w"
      
      res_wdyn[[xi]][[i]][,eta_inst[1]] <- res_wdyn[[xi]][[i]][,lf_inst[1]]/input[xi,"w_grenz"]
      res_wdyn[[xi]][[i]][,eta_fin[1]]  <- res_wdyn[[xi]][[i]][,lf_fin[1]]/input[xi,"w_grenz"]
      res_wdyn[[xi]][[i]][,eta_probe[1]]  <- res_wdyn[[xi]][[i]][,lf_probe[1]]/input[xi,"w_grenz"]
    }
  }
  
  return(res_wdyn[[xi]])
}) 


##### AUSLASTUNGEN AUSWERTEN #######
# maximale Auslastung entlang des Balkens
nachweisorte <- c("sig_x_u","sig_x_o","tau")
max_eta_t <- lapply(res_t,max_eta_pro_nachweis,nachweisorte,"eta")
max_eta_t <- lapply(max_eta_t,function(x) {rownames(x) <- paste("t.",rownames(x),sep="");return(x)}) # stellt ein t vornean, zur Unterscheidung sp?ter

nachweisorte <- c("w")
max_eta_w <- lapply(res_w,max_eta_pro_nachweis,nachweisorte,"eta")
max_eta_w <- lapply(max_eta_w,function(x) {rownames(x) <- paste("g.",rownames(x),sep="");return(x)}) # stellt ein g vornean, zur Unterscheidung sp?ter

max_w <- lapply(res_w,max_eta_pro_nachweis,"w","gzg")

nachweisorte <- c("sig_x_o")
max_eta_c <- lapply(res_c,max_eta_pro_nachweis,nachweisorte,"eta")
max_eta_c <- lapply(max_eta_c,function(x) {rownames(x) <- paste("c.",rownames(x),sep="");return(x)}) # stellt ein c vornean, zur Unterscheidung sp?ter

# Alternativ in Mitte die Spannungen auslesen, da zum Teil durch Spannungsspitzen am VM entstehen
mitt_eta_c <- lapply(res_c,mitt_eta_pro_nachweis,"sig_x_o","eta")
mitt_eta_c <- lapply(mitt_eta_c,function(x) {rownames(x) <- paste("c.",rownames(x),sep="");return(x)}) # stellt ein c vornean, zur Unterscheidung sp?ter

# Einfluss auf das Verbindungsmittel
max_eta_v <- lapply(res_v,max_eta_pro_nachweis,"Vy","eta")
max_eta_v <- lapply(max_eta_v,function(x) {rownames(x) <- paste("v.",rownames(x),sep="");return(x)}) # stellt ein c vornean, zur Unterscheidung sp?ter

eta_eige <- lapply(seq_along(res_eige),function(x) {
  if(input[x,"connection"] %in% names_glue) {
    temp <- data.frame(eta=res_eige[[x]][["freq"]][1,"eta_inst_hgzg_eige"],x=1)
    row.names(temp) <- "g.eta_inst_hgzg_eige"
  } else {
    temp <- data.frame(eta=res_eige[[x]][["freq"]][1,"eta_inst_ks_eige"],x=1)
    row.names(temp) <- "g.eta_inst_hgzg_eige"
  }
  return(temp)
})

max_eta_wdyn <- lapply(res_wdyn,max_eta_pro_nachweis,"w","eta")

max_eta_eige <- lapply(res_eige,max_eta_pro_nachweis,"freq","eta")

max_eta <- lapply(seq_along(max_eta_t),function(x) {
  rbind(max_eta_t[[x]],mitt_eta_c[[x]],max_eta_w[[x]],max_eta_v[[x]],eta_eige[[x]],make.row.names=TRUE)}
)

# Auflagerkraft für weitere Bemessung:
res_a <- lapply(seq_along(res_c),function(xi) {
  cols_to_add <- grep("^[x]{1}",colnames(res_t[[xi]][["Vz"]]),perl=TRUE,invert=TRUE)
  cols_to_add_2 <- grep("sep",colnames(res_t[[xi]][["Vz"]]),perl=TRUE,invert=TRUE)
  cols_to_add_3 <- grep("eta",colnames(res_t[[xi]][["Vz"]]),perl=TRUE,invert=TRUE)
  cols_to_add <- intersect(cols_to_add,cols_to_add_2) %>% intersect(cols_to_add_3)
  Vz <- (res_t[[xi]][["Vz"]][,cols_to_add]+res_c[[xi]][["Vz"]][cols_to_add])
  
  return(list(Vz=Vz))
}
)
max_a <- lapply(res_a,max_eta_pro_nachweis,"Vz","gzt")
max_a <- lapply(max_a,function(x) {rownames(x) <- paste("a.",rownames(x),sep="");return(x)}) # stellt ein t vornean, zur Unterscheidung sp?ter
tot_max_a <- do.call(rbind,lapply(max_a,function(x) max(x[,"eta"])))
max_nachweis_a <- do.call(rbind,lapply(max_a,function(x) rownames(x)[which.max(x[,"eta"])]))
input[,"tot_max_a"] <- tot_max_a
input[,"max_nachweis_a"] <- max_nachweis_a


# Auslesen welcher Nachweis insgesamt massgebend wurde
max_nachweis <- do.call(rbind,lapply(max_eta,function(x) rownames(x)[which.max(x[,"eta"])]))
# Maximale Auslastung beim insgesamt massgebenden Nachweis
tot_max_eta <- do.call(rbind,lapply(max_eta,function(x) max(x[,"eta"])))

input[,"tot_max_eta"] <- as.vector(tot_max_eta)
input[,"max_nachweis"] <- as.vector(max_nachweis)

# Auslesen welcher Nachweis im VM massgebend wurde
max_nachweis_v <- do.call(rbind,lapply(max_eta_v,function(x) rownames(x)[which.max(x[,"eta"])]))
# Maximale Auslastung beim im VM massgebenden Nachweis
tot_max_eta_v <- do.call(rbind,lapply(max_eta_v,function(x) max(x[,"eta"])))

input[,"tot_max_eta_v"] <- as.vector(tot_max_eta_v)
input[,"max_nachweis_v"] <- as.vector(max_nachweis_v)

# Auslesen welcher Nachweis im Holz massgebend wurde
max_nachweis_t <- do.call(rbind,lapply(max_eta_t,function(x) rownames(x)[which.max(x[,"eta"])]))
# Maximale Auslastung beim im Holz massgebenden Nachweis
tot_max_eta_t <- do.call(rbind,lapply(max_eta_t,function(x) max(x[,"eta"])))

input[,"tot_max_eta_t"] <- as.vector(tot_max_eta_t)
input[,"max_nachweis_t"] <- as.vector(max_nachweis_t)

# Auslesen welcher Nachweis im Beton massgebend wurde
max_nachweis_c <- do.call(rbind,lapply(max_eta_c,function(x) rownames(x)[which.max(x[,"eta"])]))
# Maximale Auslastung beim im Holz massgebenden Nachweis
tot_max_eta_c <- do.call(rbind,lapply(max_eta_c,function(x) max(x[,"eta"])))

input[,"tot_max_eta_c"] <- as.vector(tot_max_eta_c)
input[,"max_nachweis_c"] <- as.vector(max_nachweis_c)


#Umformen in Matrix mit einer Zeile pro Fall und den verschiedenen Nachweisen als Spalte
nachweis <- lapply(max_eta,function(x) t(x[1]))
nachweis <- do.call("rbind",nachweis)

nachweis_t_inst <- nachweis[,grep("[t]{1}[.]+.+inst.+sig",colnames(nachweis))]
nachweis_t_t37 <- nachweis[,grep("[t]{1}[.]+.+t37.+sig",colnames(nachweis))]
nachweis_t_fin <- nachweis[,grep("[t]{1}[.]+.+fin.+sig",colnames(nachweis))]

nachweis_inst <- nachweis[,grep("inst",colnames(nachweis))]
nachweis_t37 <- nachweis[,grep("t37",colnames(nachweis))]
nachweis_fin <- nachweis[,grep("fin",colnames(nachweis))]

nachweis_v_inst <- nachweis[,grep("[v]{1}[.].+inst",colnames(nachweis))]
nachweis_v_t37 <- nachweis[,grep("[v]{1}[.].+t37",colnames(nachweis))]
nachweis_v_fin <- nachweis[,grep("[v]{1}[.].+fin",colnames(nachweis))]


input[,"max_t_inst"] <- mapply(function(x) max(nachweis_t_inst[x,]),x=seq_along(nachweis_t_inst[,1]))
input[,"max_t_t37"] <- mapply(function(x) max(nachweis_t_t37[x,]),x=seq_along(nachweis_t_t37[,1]))
input[,"max_t_fin"] <- mapply(function(x) max(nachweis_t_fin[x,]),x=seq_along(nachweis_t_fin[,1]))

input[,"max_v_inst"] <- mapply(function(x) max(nachweis_v_inst[x,]),x=seq_along(nachweis_v_inst[,1]))
input[,"max_v_t37"] <- mapply(function(x) max(nachweis_v_t37[x,]),x=seq_along(nachweis_v_t37[,1]))
input[,"max_v_fin"] <- mapply(function(x) max(nachweis_v_fin[x,]),x=seq_along(nachweis_v_fin[,1]))

input[,"max_inst"] <- mapply(function(x) max(nachweis_inst[x,]),x=seq_along(nachweis_inst[,1]))
input[,"max_t37"] <- mapply(function(x) max(nachweis_t37[x,]),x=seq_along(nachweis_t37[,1]))
input[,"max_fin"] <- mapply(function(x) max(nachweis_fin[x,]),x=seq_along(nachweis_fin[,1]))

input[,"max_fin_inst"] <- mapply(function(x) max(input[x,"max_inst"],input[x,"max_fin"]),x=seq_along(input[,"max_inst"]))

delt_nachweis <- input[,"max_t37"]/input[,"max_fin_inst"]

delt_inst <- input[,"max_inst"]/input[,"max_t37"]
delt_fin  <- input[,"max_fin"]/input[,"max_t37"]
max_delt <- mapply(function(x) max(delt_inst[x],delt_fin[x]),x=seq_along(delt_inst))
input[,"max_delt"] <- max_delt
input[,"delt_inst"] <- delt_inst
input[,"delt_fin"] <- delt_fin

input[,"delt_nachweis"] <- delt_nachweis

input$nachweis_ok <- mapply(function(x) ifelse(x>=1,FALSE,TRUE),x=input$tot_max_eta)




###### LONG DATA ERZEUGEN #####
# create long data
#df_t <- nestedist_to_df(res_t)
#df_t <- cbind(df_t,material=rep("timber",nrow(df_t)))
#df_c <- nestedist_to_df(res_c)
#df_c <- cbind(df_c,material=rep("concrete",nrow(df_c)))
#df_w <- nestedist_to_df(res_w)
#df_w <- cbind(df_w,material=rep("deformation",nrow(df_w)))
#df_v <- nestedist_to_df(res_v)
#df_v <- cbind(df_v,material=rep("connection",nrow(df_v)))

#df <- rbind(df_t,df_c,fill=TRUE) %>% rbind(df_w,fill=TRUE) %>% rbind(df_v,fill=TRUE)
#write.table(df,paste(titl,"_longdata.txt",sep=""),sep=";",dec=".")

write.table(input,paste(titl,"_post_bemess.txt",sep=""),sep=";",dec=".")


# Ergebnisse speichern, dazu werden alle Eingabeparameter zusammen mit den Ausnutzungsgraden der Bemessung gespeichert.
# Liegt im Ordner bereits eine Ergebnisliste mit dem entsprechenden Namen, wird die csv um die neuen Ergebnisse erweitert
results <- cbind(input,nachweis)
if (file.exists("results.csv")) {
  write.table(results,"results.csv",append=TRUE,quote=FALSE,row.names=FALSE,col.names=T,sep=";",dec=".")  
}  else{
  write.table(results,"results.csv",quote=FALSE,row.names=FALSE,col.names=TRUE,sep=";",dec=".")
}
