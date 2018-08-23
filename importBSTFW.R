setwd("./Dokumente/ISSplott.r/")
library(XML)
library(data.table)

folders <- list.files("./INFRA", full.names = T)

counter <- 0
betriebsstellenfahrwege <- data.table()

for(i in 1:length(folders)){
  files <- list.files(path = folders[i], full.names = T, pattern = 'Spurplan.*\\.xml')
  for(j in 1:length(files)){
    print(files[j])
    spurplan <- readLines(files[j], encoding = "latin1")
    tree <- xmlTreeParse(spurplan, asText=TRUE)
    
    bts <- tree[[1]][["XmlIssDaten"]][["Spurplanbetriebsstellen"]]
    num_bts <- length(bts)
    for(k in 1:num_bts){
      bts_name <- xmlValue(bts[[k]][["Betriebsstelle"]])
      btsfw <- bts[[k]][["Betriebsstellenfahrwege"]]
      if(length(btsfw) <= 0){next()}
      for(m in 1:length(btsfw)){
        counter <- counter + 1
        name <- xmlValue(btsfw[[m]][["Name"]])
        start_typ <- xmlGetAttr(btsfw[[m]][["Anfang"]], "Typ")
        start_id <- xmlGetAttr(btsfw[[m]][["Anfang"]], "ID")
        end_typ <- xmlGetAttr(btsfw[[m]][["Ende"]], "Typ")
        end_id <- xmlGetAttr(btsfw[[m]][["Ende"]], "ID")
        prio <- -1
        if(!is.null(btsfw[[m]][["Prioritaeten"]])){
          prios <- btsfw[[m]][["Prioritaeten"]]
          for(n in 1:length(prios)){
            if(xmlGetAttr(prios[[n]], "Typ") == "FGz"){
              prio <- xmlValue(prios[[n]])
              break()
            }
          }
        }
        verlauf <- paste(as.character(unlist(xmlChildren(btsfw[[m]][["Verlauf"]]))), collapse = "#")
        attribute <- ""
        if(!is.null(btsfw[[m]][["Attribute"]])){
          attribute <- paste(as.character(unlist(xmlChildren(btsfw[[m]][["Attribute"]]))), collapse = "#")
        }
        fzmp_id <- ""
        if(!is.null(btsfw[[m]][["Fahrzeitmesspunkt"]])){
          fzmp_id <- xmlGetAttr(btsfw[[m]][["Fahrzeitmesspunkt"]], "ID")
        }
        
        abschnitte <- ""
        betriebsstellenfahrwege <- rbind(betriebsstellenfahrwege, data.table(ID = counter, BTS_NAME = bts_name,
                                                                             FW_NAME = name, PRIO = prio,
                                                                             START_ID = start_id, START_TYP = start_typ,
                                                                             END_ID = end_id, END_TYP = end_typ,
                                                                             VERLAUF = verlauf, ATTRIBUTE = attribute,
                                                                             FZMP_ID = fzmp_id, ABSCHNITTE = abschnitte))
      }
    }
  }
}

redVerlauf <- c("Weichenstamm", "WeichenabzweigLinks", "WeichenabzweigRechts", "KreuzungsweicheAnfangLinks", 
                "KreuzungsweicheEndeLinks")



for(b in 6800:length(betriebsstellenfahrwege$ID)){
  print(b)
  tmp_fw <- betriebsstellenfahrwege[b,]
  v <- unlist(strsplit(tmp_fw$VERLAUF, "#"))
  abschnitte <- ""
  id <- spurplanKnoten$SP_AB_ID[spurplanKnoten$NODE_ID == tmp_fw$START_ID & spurplanKnoten$BTS_NAME == tmp_fw$BTS_NAME]
  if(length(id) != 1){stop("error - no unique abschnitt")}
  abschnitte <- id
  ab <- spurplanKnoten$CTR[spurplanKnoten$SP_AB_ID == id]
  end_ab <- ""
  if(spurplanKnoten$NODE_ID[spurplanKnoten$CTR == ab[1]] == tmp_fw$START_ID){
    end_ab <- spurplanKnoten[CTR == ab[length(ab)],]
  }else{
    end_ab <- spurplanKnoten[CTR == ab[1],]
  }
  if(tmp_fw$START_TYP == "Betriebsstellengrenze"){
    if(tmp_fw$END_TYP == "Betriebsstellengrenze"){
      #check if endNode is Betriebsstellengrenze
      while (end_ab$TYPE != "Betriebsstellengrenze") {
        p <- unlist(strsplit(end_ab$PARTNER, "#"))
        if(length(p) == 0){stop("error - no partner found")}
        if(length(p)> 1){
          #decide which direction and go to correct partner
          if(v[1] == "Stamm"){
            id <- spurplanKnoten$SP_AB_ID[spurplanKnoten$NODE_ID == p[1] & spurplanKnoten$BTS_NAME == tmp_fw$BTS_NAME]
            p <- p[1]
          }else{
            id <- spurplanKnoten$SP_AB_ID[spurplanKnoten$NODE_ID == p[2] & spurplanKnoten$BTS_NAME == tmp_fw$BTS_NAME]
            p <- p[2]
          }
          if(length(id) != 1){stop("error - no unique abschnitt")}
          v <- v[-1]
          abschnitte <- paste(abschnitte, id, sep = "#")
          ab <- spurplanKnoten$CTR[spurplanKnoten$SP_AB_ID == id]
          if(spurplanKnoten$NODE_ID[spurplanKnoten$CTR == ab[1]] == p){
            end_ab <- spurplanKnoten[CTR == ab[length(ab)],]
          }else{
            end_ab <- spurplanKnoten[CTR == ab[1],]
          }
        }else{
          # go directly to partner
          if(end_ab$TYPE %in% redVerlauf){v <- v[-1]}
          id <- spurplanKnoten$SP_AB_ID[spurplanKnoten$NODE_ID == p[1] & spurplanKnoten$BTS_NAME == tmp_fw$BTS_NAME]
          if(length(id) != 1){stop("error - no unique abschnitt")}
          abschnitte <- paste(abschnitte, id, sep = "#")
          ab <- spurplanKnoten$CTR[spurplanKnoten$SP_AB_ID == id]
          if(spurplanKnoten$NODE_ID[spurplanKnoten$CTR == ab[1]] == p){
            end_ab <- spurplanKnoten[CTR == ab[length(ab)],]
          }else{
            end_ab <- spurplanKnoten[CTR == ab[1],]
          }
        }
      }
      if(length(v) > 0){stop("wrong path of btsfw!")}
      if(end_ab$NODE_ID != tmp_fw$END_ID){stop("wrong exit of btsfw!")}
    }else{
      # check if Hp, Prellbock, Gleisende is on abschnitt and stop
      while(!(tmp_fw$END_ID %in% spurplanKnoten$NODE_ID[spurplanKnoten$SP_AB_ID == id])){
        p <- unlist(strsplit(end_ab$PARTNER, "#"))
        if(length(p) == 0){stop("error - no partner found")}
        if(length(p)> 1){
          #decide which direction and go to correct partner
          if(v[1] == "Stamm"){
            id <- spurplanKnoten$SP_AB_ID[spurplanKnoten$NODE_ID == p[1] & spurplanKnoten$BTS_NAME == tmp_fw$BTS_NAME]
            p <- p[1]
          }else{
            id <- spurplanKnoten$SP_AB_ID[spurplanKnoten$NODE_ID == p[2] & spurplanKnoten$BTS_NAME == tmp_fw$BTS_NAME]
            p <- p[2]
          }
          if(length(id) != 1){stop("error - no unique abschnitt")}
          v <- v[-1]
          abschnitte <- paste(abschnitte, id, sep = "#")
          ab <- spurplanKnoten$CTR[spurplanKnoten$SP_AB_ID == id]
          if(spurplanKnoten$NODE_ID[spurplanKnoten$CTR == ab[1]] == p){
            end_ab <- spurplanKnoten[CTR == ab[length(ab)],]
          }else{
            end_ab <- spurplanKnoten[CTR == ab[1],]
          }
        }else{
          # go directly to partner
          if(end_ab$TYPE %in% redVerlauf){v <- v[-1]}
          id <- spurplanKnoten$SP_AB_ID[spurplanKnoten$NODE_ID == p[1] & spurplanKnoten$BTS_NAME == tmp_fw$BTS_NAME]
          if(length(id) != 1){stop("error - no unique abschnitt")}
          abschnitte <- paste(abschnitte, id, sep = "#")
          ab <- spurplanKnoten$CTR[spurplanKnoten$SP_AB_ID == id]
          if(spurplanKnoten$NODE_ID[spurplanKnoten$CTR == ab[1]] == p){
            end_ab <- spurplanKnoten[CTR == ab[length(ab)],]
          }else{
            end_ab <- spurplanKnoten[CTR == ab[1],]
          }
        }
      }
      if(length(v) > 0){stop("wrong path of btsfw!")}
      if(end_ab$NODE_ID != tmp_fw$END_ID && 
         spurplanKnoten$X[spurplanKnoten$NODE_ID == tmp_fw$END_ID & spurplanKnoten$BTS_NAME == tmp_fw$BTS_NAME] != ""){stop("wrong exit of btsfw!")}
    }
  }else{
    if(tmp_fw$END_TYP == "Betriebsstellengrenze"){
      if(end_ab$TYPE == "Betriebsstellengrenze" && end_ab$NODE_ID != tmp_fw$END_ID){
        if(grepl("Halteplatz", tmp_fw$START_TYP)){
          v <- unlist(strsplit(tmp_fw$VERLAUF, "#"))
          abschnitte <- ""
          id <- spurplanKnoten$SP_AB_ID[spurplanKnoten$NODE_ID == tmp_fw$START_ID & spurplanKnoten$BTS_NAME == tmp_fw$BTS_NAME]
          ab <- spurplanKnoten$CTR[spurplanKnoten$SP_AB_ID == id]
          end_ab <- spurplanKnoten[CTR == ab[length(ab)],]
        }
      }
      while (end_ab$TYPE != "Betriebsstellengrenze") {
        p <- unlist(strsplit(end_ab$PARTNER, "#"))
        if(length(p) == 0){
          if(grepl("Halteplatz", tmp_fw$START_TYP)){
            v <- unlist(strsplit(tmp_fw$VERLAUF, "#"))
            abschnitte <- ""
            id <- spurplanKnoten$SP_AB_ID[spurplanKnoten$NODE_ID == tmp_fw$START_ID & spurplanKnoten$BTS_NAME == tmp_fw$BTS_NAME]
            ab <- spurplanKnoten$CTR[spurplanKnoten$SP_AB_ID == id]
            end_ab <- spurplanKnoten[CTR == ab[length(ab)],]
            next()
          }else{
            stop("error - no partner found")
          }
        }
        if(length(p)> 1){
          #decide which direction and go to correct partner
          if(length(v) < 1){
            if(grepl("Halteplatz", tmp_fw$START_TYP)){
              v <- unlist(strsplit(tmp_fw$VERLAUF, "#"))
              abschnitte <- ""
              id <- spurplanKnoten$SP_AB_ID[spurplanKnoten$NODE_ID == tmp_fw$START_ID & spurplanKnoten$BTS_NAME == tmp_fw$BTS_NAME]
              ab <- spurplanKnoten$CTR[spurplanKnoten$SP_AB_ID == id]
              end_ab <- spurplanKnoten[CTR == ab[length(ab)],]
              next()
            }else{
              stop("error - no partner found")
            }
          }
          if(v[1] == "Stamm"){
            id <- spurplanKnoten$SP_AB_ID[spurplanKnoten$NODE_ID == p[1] & spurplanKnoten$BTS_NAME == tmp_fw$BTS_NAME]
            p <- p[1]
          }else{
            id <- spurplanKnoten$SP_AB_ID[spurplanKnoten$NODE_ID == p[2] & spurplanKnoten$BTS_NAME == tmp_fw$BTS_NAME]
            p <- p[2]
          }
          if(length(id) != 1){stop("error - no unique abschnitt")}
          v <- v[-1]
          abschnitte <- paste(abschnitte, id, sep = "#")
          ab <- spurplanKnoten$CTR[spurplanKnoten$SP_AB_ID == id]
          if(spurplanKnoten$NODE_ID[spurplanKnoten$CTR == ab[1]] == p){
            end_ab <- spurplanKnoten[CTR == ab[length(ab)],]
          }else{
            end_ab <- spurplanKnoten[CTR == ab[1],]
          }
        }else{
          # go directly to partner
          if(end_ab$TYPE %in% redVerlauf){v <- v[-1]}
          id <- spurplanKnoten$SP_AB_ID[spurplanKnoten$NODE_ID == p[1] & spurplanKnoten$BTS_NAME == tmp_fw$BTS_NAME]
          if(length(id) != 1){stop("error - no unique abschnitt")}
          abschnitte <- paste(abschnitte, id, sep = "#")
          ab <- spurplanKnoten$CTR[spurplanKnoten$SP_AB_ID == id]
          if(spurplanKnoten$NODE_ID[spurplanKnoten$CTR == ab[1]] == p){
            end_ab <- spurplanKnoten[CTR == ab[length(ab)],]
          }else{
            end_ab <- spurplanKnoten[CTR == ab[1],]
          }
        }
        if(end_ab$TYPE == "Betriebsstellengrenze" && end_ab$NODE_ID != tmp_fw$END_ID){
          if(grepl("Halteplatz", tmp_fw$START_TYP)){
            v <- unlist(strsplit(tmp_fw$VERLAUF, "#"))
            abschnitte <- ""
            id <- spurplanKnoten$SP_AB_ID[spurplanKnoten$NODE_ID == tmp_fw$START_ID & spurplanKnoten$BTS_NAME == tmp_fw$BTS_NAME]
            ab <- spurplanKnoten$CTR[spurplanKnoten$SP_AB_ID == id]
            end_ab <- spurplanKnoten[CTR == ab[length(ab)],]
            next()
          }
        }
      }
      if(length(v) > 0){stop("wrong path of btsfw!")}
      if(end_ab$NODE_ID != tmp_fw$END_ID){stop("wrong exit of btsfw!")}
      }else{
        abschnitte <- ""
      }
  }
  betriebsstellenfahrwege$ABSCHNITTE[b] <- abschnitte
}



write.csv2(betriebsstellenfahrwege, file = "./BTSFW-2013_46_DW_v02.csv", row.names = F)
