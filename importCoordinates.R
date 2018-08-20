setwd("./Dokumente/ISSplott.r/")
library(XML)
library(data.table)

folders <- list.files("./INFRA", full.names = T)

counter <- 0
ctr_node <- 0
spurplanKnoten <- data.table()

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
      sp_abschn <- bts[[k]][["Spurplanabschnitte"]]
      for(n in 1:length(sp_abschn)){
        counter <- counter + 1
        sp_ab_id <- counter
        strecke <- xmlValue(sp_abschn[[n]][["Strecke"]])
        knoten <- sp_abschn[[n]][["Spurplanknoten"]]
        for(m in 1: length(knoten)){
          ctr_node <- ctr_node + 1
          node_ctr <- ctr_node
          type <- xmlName(knoten[[m]])
          node_id <- xmlValue(knoten[[m]][["ID"]])
          if(!is.null(knoten[[m]][["Bildkoordinaten"]])){
            # node with coordinates exist
            x <- as.numeric(gsub(",", "\\.", xmlValue(knoten[[m]][["Bildkoordinaten"]][["X"]])))
            y <- as.numeric(gsub(",", "\\.", xmlValue(knoten[[m]][["Bildkoordinaten"]][["Y"]])))
            coordParent <- ""
            if(type == "Weichenanfang"){
              partner <- paste(xmlGetAttr(knoten[[m]][["Stamm"]], "ID"), 
                               xmlGetAttr(knoten[[m]][["Abzweig"]], "ID"), 
                               sep = "#")
              node_name <- xmlValue(knoten[[m]][["Stamm"]])
              if(length(node_name) <= 0){node_name <- ""}
            }
            if(type == "Betriebsstellengrenze"){
              partner <- xmlValue(knoten[[m]][["PartnerBtrst"]])
              node_name <- xmlValue(knoten[[m]][["Gleiskennzeichen"]])
              if(length(node_name) <= 0){node_name <- ""}
            }
            if(type == "Bildpunkt" || type == "Prellbock"){
              partner <- ""
              node_name <- ""
            }
            if(type == "KmSprungAnfang"){
              partner <- xmlGetAttr(knoten[[m]][["Partnerknoten"]], "ID")
              node_name <- ""
            }
            if(type == "Gleisende"){
              partner <- ""
              node_name <- xmlValue(knoten[[m]][["Name"]])
              if(length(node_name) <= 0){node_name <- ""}
            }
            if(type == "KreuzungAnfangLinks"){
              node_name <- xmlValue(knoten[[m]][["NachbarGeradeaus"]])
              if(length(node_name) <= 0){node_name <- ""}
              partner <- xmlGetAttr(knoten[[m]][["NachbarGeradeaus"]], "ID")
            }
            if(type == "KreuzungsweicheAnfangLinks"){
              node_name <- xmlValue(knoten[[m]][["NachbarGeradeaus"]])
              if(length(node_name) <= 0){node_name <- ""}
              if(!is.null(knoten[[m]][["NachbarAbzweig"]])){
                partner <- paste(xmlGetAttr(knoten[[m]][["NachbarGeradeaus"]], "ID"), 
                                 xmlGetAttr(knoten[[m]][["NachbarAbzweig"]], "ID"), 
                                 sep = "#")
              }else{
                partner <- xmlGetAttr(knoten[[m]][["NachbarGeradeaus"]], "ID")
              }
            }
            if(type == "Streckenwechsel0"){
              partner <- xmlGetAttr(knoten[[m]][["Partnerknoten"]], "ID")
              node_name <- ""
            }
            
            spurplanKnoten <- rbind(spurplanKnoten, data.table(CTR = node_ctr, NODE_ID = node_id, SP_AB_ID = sp_ab_id,
                                                               BTS_NAME = bts_name, STRECKE = strecke,
                                                               TYPE = type, NODE_NAME = node_name, PARTNER = partner, 
                                                               COORD_PAR = coordParent, X = x, Y = y))
          }else{
            x <- ""
            y <- ""
            addToFrame <- F
            if(type == "Fahrzeitmesspunkt" || type == "HalteplatzRzLinksF" || type == "HalteplatzRzLinksS" ||
               type == "HalteplatzRzRechtsF" || type == "HalteplatzRzRechtsS" || type == "HalteplatzGzF" || type == "HalteplatzGzS"){
              # get FZMP node for annotations
              node_name <- xmlValue(knoten[[m]][["Name"]])
              if(length(node_name) <= 0){node_name <- ""}
              partner <- ""
              coordParent <- ""
              addToFrame <- T
            }
            if(type == "WeichenabzweigLinks" || type == "WeichenabzweigRechts" || type == "Weichenstamm"){
              partner <- xmlGetAttr(knoten[[m]][["Partner"]], "ID")
              node_name <- xmlValue(knoten[[m]][["Partner"]])
              if(length(node_name) <= 0){node_name <- ""}
              coordParent <- partner
              addToFrame <- T
            }
            if(type == "KmSprungEnde"){
              partner <- xmlGetAttr(knoten[[m]][["Partnerknoten"]], "ID")
              node_name <- ""
              coordParent <- partner
              addToFrame <- T
            }
            if(type == "KreuzungsweicheEndeLinks"){
              node_name <- xmlValue(knoten[[m]][["NachbarGeradeaus"]])
              if(length(node_name) <= 0){node_name <- ""}
              if(!is.null(knoten[[m]][["NachbarAbzweig"]])){
                partner <- paste(xmlGetAttr(knoten[[m]][["NachbarGeradeaus"]], "ID"), 
                                 xmlGetAttr(knoten[[m]][["NachbarAbzweig"]], "ID"), 
                                 sep = "#")
              }else{
                partner <- xmlGetAttr(knoten[[m]][["NachbarGeradeaus"]], "ID")
              }
              coordParent <- xmlGetAttr(knoten[[m]][["NachbarGeradeaus"]], "ID")
              addToFrame <- T
            }
            if(type == "KreuzungsweicheEndeRechts"){
              node_name <- xmlValue(knoten[[m]][["NachbarGeradeaus"]])
              if(length(node_name) <= 0){node_name <- ""}
              partner <- paste(xmlGetAttr(knoten[[m]][["NachbarGeradeaus"]], "ID"), 
                               xmlGetAttr(knoten[[m]][["HauptknotenAnfang"]], "ID"), 
                               sep = "#")
              coordParent <- xmlGetAttr(knoten[[m]][["HauptknotenAnfang"]], "ID")
              addToFrame <- T
            }
            if(type == "KreuzungEndeLinks"){
              node_name <- xmlValue(knoten[[m]][["NachbarGeradeaus"]])
              if(length(node_name) <= 0){node_name <- ""}
              partner <- xmlGetAttr(knoten[[m]][["NachbarGeradeaus"]], "ID")
              coordParent <- partner
              addToFrame <- T
            }
            if(type == "KreuzungEndeRechts" || type == "KreuzungAnfangRechts"){
              node_name <- xmlValue(knoten[[m]][["NachbarGeradeaus"]])
              if(length(node_name) <= 0){node_name <- ""}
              partner <- xmlGetAttr(knoten[[m]][["NachbarGeradeaus"]], "ID")
              coordParent <- xmlGetAttr(knoten[[m]][["HauptknotenAnfang"]], "ID")
              addToFrame <- T
            }
            if(type == "KreuzungsweicheAnfangRechts"){
              node_name <- xmlValue(knoten[[m]][["NachbarGeradeaus"]])
              if(length(node_name) <= 0){node_name <- ""}
              partner <- paste(xmlGetAttr(knoten[[m]][["NachbarGeradeaus"]], "ID"), 
                               xmlGetAttr(knoten[[m]][["HauptknotenEnde"]], "ID"), 
                               sep = "#")
              coordParent <- xmlGetAttr(knoten[[m]][["HauptknotenAnfang"]], "ID")
              addToFrame <- T
            }
            if(type == "Streckenwechsel1"){
              partner <- xmlGetAttr(knoten[[m]][["Partnerknoten"]], "ID")
              node_name <- ""
              coordParent <- partner
              addToFrame <- T
            }
            if(addToFrame){
              spurplanKnoten <- rbind(spurplanKnoten, data.table(CTR = node_ctr, NODE_ID = node_id, SP_AB_ID = sp_ab_id,
                                                                 BTS_NAME = bts_name, STRECKE = strecke,
                                                                 TYPE = type, NODE_NAME = node_name, PARTNER = partner, 
                                                                 COORD_PAR = coordParent, X = x, Y = y))
            }
          }
        }
      }
    }
  }
}

#calculate x/y for FZMP and Kreuzungen, Kreuzungsweichen
for(i in 1:length(spurplanKnoten$CTR)){
  if(i %% 1000 == 0){print(i)}
  if(spurplanKnoten$COORD_PAR[i] == ""){next()}
  spurplanKnoten$X[i] <- spurplanKnoten$X[which(spurplanKnoten$COORD_PAR[i] == spurplanKnoten$NODE_ID)]
  spurplanKnoten$Y[i] <- spurplanKnoten$Y[which(spurplanKnoten$COORD_PAR[i] == spurplanKnoten$NODE_ID)]
}

spurplanabschnitte <- unique(spurplanKnoten$SP_AB_ID)
for(ab in spurplanabschnitte){
  if(ab %% 500 == 0){print(paste("SP_AB:", ab))}
  tmp <- spurplanKnoten[SP_AB_ID == ab,]
  if(tmp$X[1] == "" || tmp$X[length(tmp$X)] == ""){stop("missing coordinates")}
  fzmp_ctr <- tmp$CTR[which(tmp$TYPE == "Fahrzeitmesspunkt")]
  if(length(fzmp_ctr)<=0){next()}
  
  before <- max(tmp$CTR[tmp$CTR < fzmp_ctr & tmp$X != ""])
  after <- min(tmp$CTR[tmp$CTR > fzmp_ctr & tmp$X != ""])
  
  spurplanKnoten$X[spurplanKnoten$CTR == fzmp_ctr] <- mean(c(as.numeric(tmp$X[tmp$CTR == before]), as.numeric(tmp$X[tmp$CTR == after])))
  spurplanKnoten$Y[spurplanKnoten$CTR == fzmp_ctr] <- mean(c(as.numeric(tmp$Y[tmp$CTR == before]), as.numeric(tmp$Y[tmp$CTR == after])))
}

write.csv2(spurplanKnoten, file = "./INFRA/D2013_46.csv", row.names = F)
