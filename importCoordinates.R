setwd("./Dokumente/ISSplott.r/")
library(XML)

folders <- list.files("./INFRA", full.names = T)

sp_ab_ID <- 0

for(i in 1:length(folders)){
  files <- list.files(path = folders[i], full.names = T, pattern = 'Spurplan.*\\.xml')
  for(j in 1:length(files)){
    spurplan <- readLines(files[j], encoding = "latin1")
    tree <- xmlTreeParse(spurplan, asText=TRUE)
    
    bts <- tree[[1]][["XmlIssDaten"]][["Spurplanbetriebsstellen"]]
    num_bts <- length(bts)
    for(k in 1:num_bts){
      name <- xmlValue(bts[[k]][["Betriebsstelle"]])
      sp_abschn <- bts[[k]][["Spurplanabschnitte"]]
      for(n in 1:length(sp_abschn)){
        id <- sp_ab_ID + 1
        sp_ab_ID <- id
        strecke <- xmlValue(sp_abschn[[n]][["Strecke"]])
        knoten <- sp_abschn[[n]][["Spurplanknoten"]]
        for(m in 1: length(knoten)){
          if(!is.null(knoten[[m]][["Bildkoordinaten"]])){
            # node with coordinates exist
            type <- xmlName(knoten[[m]])
            node_id <- xmlValue(knoten[[m]][["ID"]])
            partner <- xmlValue(knoten[[m]][["PartnerBtrst"]])
            x <- as.numeric(gsub(",", "\\.", xmlValue(knoten[[m]][["Bildkoordinaten"]][["X"]])))
            y <- as.numeric(gsub(",", "\\.", xmlValue(knoten[[m]][["Bildkoordinaten"]][["Y"]])))
          }
          if(xmlName(knoten[[m]]) == "Fahrzeitmesspunkt"){
            # get FZMP node for annotations
          }
        }
        #calculate x/y for FZMP if it exists
      }
    }
  }
}