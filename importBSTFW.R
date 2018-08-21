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

write.csv2(betriebsstellenfahrwege, file = "./BTSFW-2013_46_DW.csv", row.names = F)
