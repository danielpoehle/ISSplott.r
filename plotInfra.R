setwd("./Dokumente/ISSplott.r/")
library(data.table)
library(ggplot2)

options(expressions=500000)
spurplanFileName <- "./D2013_46_v02.csv"
btsfwFileName <- "./BTSFW-2013_46_DW_v02.csv"
folderName <- "./WEBSERVICE"


bts_list <- c("ABCH", "ABCHG", "ASTT", "A465M", "A464S", "AMUE", "A467M", "A466M",
              "A469S", "A468S", "ASWA", "A471F", "A470S", "A473F", "A472S", "A475F",
              "A474S", "A477W", "A476W", "A977W", "A577W", "AFRD", "A976A", "A576W",
              "AAH A", "A479A", "A480R", "A481S", "A482A", "A483R", "A484R", "AQRB",
              "ABG", "ABG G")
# fileName <- list.files("./WEBSERVICE", pattern = "*.csv", full.names = T)[2]
# fName <- list.files("./WEBSERVICE", pattern = "*.csv", full.names = F)[2]

plotAllRoutes <- function(folderName, spurplanFileName, btsfwFileName){
  spurplanKnoten <- as.data.table(read.csv2(file = spurplanFileName, stringsAsFactors = F))
  betriebsstellenfahrwege <- as.data.table(read.csv2(file = btsfwFileName, stringsAsFactors = F))
  files <- list.files(folderName, pattern = "*.csv", full.names = T)
  fileNames <- list.files(folderName, pattern = "*.csv", full.names = F)
  for(i in 1:length(files)){
    print(i)
    plotWayWebservice(files[i], fileNames[i], spurplanKnoten, betriebsstellenfahrwege)
  }
}

plotWayWebservice <-function(file, fileName, spurplanKnoten, betriebsstellenfahrwege){
  df <- read.csv2(file, stringsAsFactors = F)
  bts_list <- df$RIL100
  fw_list <- df$DurchfahrtFahrweg
  fw_list[1] <- df$AbfahrtFahrweg[1]
  fw_list[length(fw_list)] <- df$AnkunftFahrweg[length(fw_list)]
  if(any(fw_list == "")){stop("one fw is empty")}
  ab_list <- list()
  for(j in 1:length(fw_list)){
    ab <- betriebsstellenfahrwege$ABSCHNITTE[which(betriebsstellenfahrwege$FW_NAME == fw_list[j])]
    if(length(ab) <=0){stop("no btsfw with matching name")}
    ab_list[[j]] <- ab
  }
  p <- plotBTS(spurplanKnoten, bts_list, fw_list)
  wd <- min(200, 4*length(bts_list))
  he <- min(30, 7+length(bts_list)*0.1)
  ggsave(filename = paste0("./WEBSERVICE/PLOTS/", unlist(strsplit(fileName, "\\."))[1], ".jpg"), 
         plot = p, width = wd, height = he, units = "cm", limitsize = F)
}


plotBTS <- function(spurplanKnoten, bts_list, fw_list){
  p <- ggplot()
  b_frame <- data.frame(BTS = bts_list, FW = fw_list , SHIFT_X = 0, SHIFT_Y = 0, stringsAsFactors = F)
  tmp_list <- list(generateTMPshift(spurplanKnoten, b_frame$BTS[1]))
  
  for (j in 2:length(b_frame$BTS)){
    tmp_old <- generateTMPshift(spurplanKnoten, b_frame$BTS[j-1], b_frame$SHIFT_X[j-1], b_frame$SHIFT_Y[j-1])
    tmp_new <- generateTMPshift(spurplanKnoten, b_frame$BTS[j], 0, 0)
    res <- calcShift(tmp_old, tmp_new)
    if(is.null(res)){stop(paste(j, "error in calcShift"))}
    b_frame$SHIFT_X[j] <- res[1]
    b_frame$SHIFT_Y[j] <- res[2]
    tmp_list[[j]] <- generateTMPshift(spurplanKnoten, b_frame$BTS[j], b_frame$SHIFT_X[j], b_frame$SHIFT_Y[j])
  }
  p <- plotInfra(p, tmp_list)
  p
}

calcShift <- function(tmp_old, tmp_new){
  bts_old <- tmp_old$BTS_NAME[1]
  bts_new <- tmp_new$BTS_NAME[1]
  gr_old <- tmp_old[TYPE == "Betriebsstellengrenze" & PARTNER == bts_new]
  gr_new <- tmp_new[TYPE == "Betriebsstellengrenze" & PARTNER == bts_old]
  if(length(gr_old) <=0 || length(gr_new) <=0){return()}
  
  #find matching Betriebsstellengrenze
  for(i in 1:length(gr_old)){
    m <- gr_new[NODE_NAME == gr_old$NODE_NAME[i] & STRECKE == gr_old$STRECKE[i]]
    if(length(m$CTR) == 1){
      delta_x <- m$X - gr_old$X[i]
      delta_y <- m$Y - gr_old$Y[i]
      
      if(length(tmp_new$CTR) <= 10 || length(tmp_old$CTR) <= 10){
        return(c(delta_x, delta_y))
      }
      
      # if neighbour bts overlap too much --> shift them horizontally
      x_max_new <- quantile(tmp_new$X - delta_x, probs = 0.9)
      x_min_new <- quantile(tmp_new$X - delta_x, probs = 0.1)
      x_max_old <- quantile(tmp_old$X, probs = 0.9)
      x_min_old <- quantile(tmp_old$X, probs = 0.1)
      if((x_max_old >= x_min_new && x_max_old <= x_max_new) ||
         (x_max_new >= x_min_old && x_max_new <= x_max_old)){
        print(paste("x_min_old", x_min_old))
        print(paste("x_max_old", x_max_old))
        print(paste("x_min_new", x_min_new))
        print(paste("x_max_new", x_max_new))
        
        delta_y <- min(tmp_old$Y) - max(tmp_new$Y)
      }
      
      return(c(delta_x, delta_y))
    }
  }
  #no unique match or no match
  return()
}

generateTMPshift <- function(spurplanKnoten, bts, shift_x = 0, shift_y = 0){
  tmp <- spurplanKnoten[BTS_NAME == bts]
  tmp <- tmp[X != ""]
  if(length(tmp$CTR) <= 0){return()}
  tmp$X <- as.numeric(tmp$X) - shift_x
  tmp$Y <- as.numeric(tmp$Y) - shift_y
  
  tmp
}

generateFWshift <- function(spurplanKnoten, abschnitte, shift_x = 0, shift_y = 0){
  ab <- unlist(strsplit(abschnitte, "#"))
  tmp <- spurplanKnoten[SP_AB_ID %in% ab,]
  tmp <- tmp[X != "" & TYPE != "Fahrzeitmesspunkt"]
  if(length(tmp$CTR) <= 0){return()}
  tmp$X <- as.numeric(tmp$X) - shift_x
  tmp$Y <- as.numeric(tmp$Y) - shift_y
  
  tmp
}

plotBTSFW <- function(p, tmp){
  tmp$GR <- 0
  p <- p + geom_line(data=tmp, aes(x=X, y=Y, group=SP_AB_ID, colour = "red", size = 2))
  
  p
}

plotInfra <- function(p, tmp){
  
  for(j in 1:length(tmp_list)){
    fzmp <- tmp[[j]][TYPE == "Fahrzeitmesspunkt"]
    fzmp$GR <- 0
    tmp[[j]] <- tmp[[j]][TYPE != "Fahrzeitmesspunkt"]
    tmp[[j]]$GR <- 0
    tmp[[j]]$GR[tmp[[j]]$TYPE == "Betriebsstellengrenze"] <- 1
    tmp[[j]]$GR[tmp[[j]]$TYPE == "Gleisende" | tmp[[j]]$TYPE == "Prellbock"] <- 2
    
    p <- p +
      annotate("text", x=mean(tmp[[j]]$X), y=mean(tmp[[j]]$Y), label=tmp[[j]]$BTS_NAME[1], size = 3, color="grey") +
      geom_line(data=tmp[[j]], aes(x=X, y=Y, group=SP_AB_ID)) +
      geom_point(data=tmp[[j]], aes(x=X, y=Y, shape=as.factor(GR), size = 0)) +
      theme_minimal() + theme(legend.position="none")
    if(length(fzmp$CTR) > 0){
      p <- p + 
        geom_point(data=fzmp, aes(x=X, y=Y, color = "red", size = 0)) + 
        geom_text(data=fzmp, aes(x=X, y=Y, label=NODE_NAME)) 
    }else{
      fzmp <- tmp[[j]][1]
      p <- p + geom_point(data=fzmp, aes(x=X, y=Y, color = "red", size = 0))
    }
  }
  
  
  p
}

p <- plotBTS(spurplanKnoten, bts_list)

ggsave(filename = "./tmp.jpg", plot = p, width = 200, height = 100, units = "cm", limitsize = F)


abschnitte <- "9#4#12#5#7#3"
p <- plotBTSFW(ggplot(), generateFWshift(spurplanKnoten, abschnitte))
plotInfra(p, generateTMPshift(spurplanKnoten, "ELSP"))

abschnitte <- "7819#7818"
p <- plotBTSFW(ggplot(), generateFWshift(spurplanKnoten, abschnitte))
plotInfra(p, generateTMPshift(spurplanKnoten, "FKRO"))

abschnitte <- "15966#15970#16009#15888#15895#15968#15906#15917#15910#15884#15918"
p <- plotBTSFW(ggplot(), generateFWshift(spurplanKnoten, abschnitte))
plotInfra(p, generateTMPshift(spurplanKnoten, "AA"))

abschnitte <- "#15932#15952#15918"
p <- plotBTSFW(ggplot(), generateFWshift(spurplanKnoten, abschnitte))
plotInfra(p, generateTMPshift(spurplanKnoten, "AA"))

abschnitte <- "1594#1573#1515#1555#1568#1558#1625#1620#1485#1519#1532#1531"
p <- plotBTSFW(ggplot(), generateFWshift(spurplanKnoten, abschnitte))
plotInfra(p, generateTMPshift(spurplanKnoten, "FB"))

abschnitte <- "5046#4997#5058#5073#5087#5078#4994#5056#5066#4996#5075#5029#5061#5002"
p <- plotBTSFW(ggplot(), generateFWshift(spurplanKnoten, abschnitte))
bts_list <- c("FFU G", "FFU")
p <- plotBTS(spurplanKnoten, bts_list)
plotInfra(p, generateTMPshift(spurplanKnoten, "FFU"))

plotInfra(ggplot(), generateTMPshift(spurplanKnoten, "FALZ"))
