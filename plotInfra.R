#setwd("./Dokumente/ISSplott.r/")
library(data.table)
library(ggplot2)

options(expressions=500000)



spurplanKnoten <- as.data.table(read.csv2(file = spkFile, stringsAsFactors = F))
betriebsstellenfahrwege <- as.data.table(read.csv2(file = btsfwFile, stringsAsFactors = F))


# bts_list <- c("ABCH", "ABCHG", "ASTT", "A465M", "A464S", "AMUE", "A467M", "A466M",
#               "A469S", "A468S", "ASWA", "A471F", "A470S", "A473F", "A472S", "A475F",
#               "A474S", "A477W", "A476W", "A977W", "A577W", "AFRD", "A976A", "A576W",
#               "AAH A", "A479A", "A480R", "A481S", "A482A", "A483R", "A484R", "AQRB",
#               "ABG", "ABG G")
# fileName <- list.files("./WEBSERVICE", pattern = "*.csv", full.names = T)[2]
# fName <- list.files("./WEBSERVICE", pattern = "*.csv", full.names = F)[2]

plotAllRoutes <- function(folderName, targetFolder){
  files <- list.files(folderName, pattern = "*.csv", full.names = T)
  fileNames <- list.files(folderName, pattern = "*.csv", full.names = F)
  for(i in 1:length(files)){
    print(fileNames[i])
    plotWayWebservice(files[i], fileNames[i], spurplanKnoten, betriebsstellenfahrwege, targetFolder)
  }
}

plotWayWebservice <-function(file, fileName, spurplanKnoten, betriebsstellenfahrwege, targetFolder){
  df <- read.csv2(file, stringsAsFactors = F)
  bts_list <- df$RIL100
  fw_list <- df$DurchfahrtFahrweg
  fw_list[which(fw_list == "")] <- df$AbfahrtFahrweg[which(fw_list == "")]
  fw_list[which(fw_list == "")] <- df$AnkunftFahrweg[which(fw_list == "")]
  if(any(fw_list == "")){stop(paste(j, "one fw is empty"))}
  ab_list <- list()
  for(j in 1:length(fw_list)){
    fw <- betriebsstellenfahrwege[which(betriebsstellenfahrwege$FW_NAME == fw_list[j] &
                                                     betriebsstellenfahrwege$BTS_NAME == bts_list[j]),]
    ab <- fw$ABSCHNITTE
    if(length(ab) <=0){
      print(paste(j, "no btsfw with matching name:", bts_list[j], fw_list[j]))
      ab_list[[j]]$ab <- list("")
    }else{
      tmp_ab <- list(ab = ab, color = getColorsForBTSFW(fw$PRIO))
      ab_list[[j]] <- tmp_ab
      }
  }
  anz <- ceiling(length(bts_list)/150.0)

  for(k in 1:anz){
    range <- (1+(k-1)*150):(150+(k-1)*150)
    if(k == anz){
      range <- (1+(k-1)*150):length(bts_list)
    }
    prt_bts_list <- bts_list[range]
    prt_fw_list <- fw_list[range]
    prt_ab_list <- ab_list[range]

    p <- plotBTS(spurplanKnoten, prt_bts_list, prt_fw_list, prt_ab_list)
    wd <- min(600, 6*length(prt_bts_list))
    he <- min(30, 7+length(prt_bts_list)*0.1)
    ggsave(filename = paste0(targetFolder, unlist(strsplit(fileName, "\\."))[1], "_", sprintf("%03d", k), ".pdf"),
           plot = p, width = wd, height = he, units = "cm", limitsize = F)
  }


}

getColorsForBTSFW <- function(prio){
  if(prio >80){
    return("red3")
  }else if(prio > 60){
    return("royalblue3")
  }else if(prio > 40){
    return("springgreen4")
  }else if(prio > 20){
    return("darkmagenta")
  }else if(prio > 0){
    return("lightcyan3")
  }else{
    return("black")
  }
}


plotBTS <- function(spurplanKnoten, bts_list, fw_list, ab_list){
  b_frame <- data.frame(BTS = bts_list, FW = fw_list , SHIFT_X = 0, SHIFT_Y = 0, ROTATE_X = F, ROTATE_Y = F, stringsAsFactors = F)
  tmp_list <- list(generateTMPshift(spurplanKnoten, b_frame$BTS[1]))

  st_fw <- betriebsstellenfahrwege[which(betriebsstellenfahrwege$FW_NAME == fw_list[1] & betriebsstellenfahrwege$BTS_NAME == bts_list[1]),]
  node_end <- spurplanKnoten[spurplanKnoten$NODE_ID == st_fw$END_ID & spurplanKnoten$BTS_NAME == st_fw$BTS_NAME,]
  node_start <- spurplanKnoten[spurplanKnoten$NODE_ID == st_fw$START_ID & spurplanKnoten$BTS_NAME == st_fw$BTS_NAME,]
  if(node_start$X == ""){
    node_start <- spurplanKnoten[spurplanKnoten$SP_AB_ID == node_start$SP_AB_ID & spurplanKnoten$TYPE == "Fahrzeitmesspunkt",]
  }
  is_steigend <- (as.numeric(node_end$X) - as.numeric(node_start$X)) >= 0
  tmp_list[[1]]$Y <- -tmp_list[[1]]$Y

  kopfmachen_last <- F
  if(length(b_frame$BTS) > 1){
    for (j in 2:length(b_frame$BTS)){
      #for (j in 2:11){
      tmp_old <- tmp_list[[j-1]]
      tmp_new <- generateTMPshift(spurplanKnoten, b_frame$BTS[j], 0, 0)
      res <- calcShift(tmp_old, tmp_new)
      if(is.null(res)){stop(paste(j, "error in calcShift"))}
      b_frame$SHIFT_X[j] <- res[1]
      b_frame$SHIFT_Y[j] <- res[2]
      tmp_new <- generateTMPshift(spurplanKnoten, b_frame$BTS[j], b_frame$SHIFT_X[j], b_frame$SHIFT_Y[j])
      gr_start <- tmp_new[TYPE == "Betriebsstellengrenze" & PARTNER == b_frame$BTS[j-1]]
      if(j < length(b_frame$BTS)){
        gr_end <- tmp_new[TYPE == "Betriebsstellengrenze" & PARTNER == b_frame$BTS[j+1]]
      }else{
        gr_end <- tmp_new[TYPE == "Fahrzeitmesspunkt"]
        if(length(gr_end$CTR) < 1){
          #last BTS with no FZMP :-/
          gr_end <- tmp_new[TYPE == "Betriebsstellengrenze" & PARTNER != gr_start$PARTNER[1],]
        }
      }
      if(gr_start$PARTNER[1] == gr_end$PARTNER[1]){
        # kopfmachen in j
        gr_end <- tmp_new[TYPE == "Fahrzeitmesspunkt"]
        kopfmachen_last <- T
      }
      if(((gr_end$X[1] - gr_start$X[1] ) >= 0) != is_steigend){
        # rotate by x
        fix_x <- tmp_new$X[tmp_new$PARTNER == b_frame$BTS[j-1]][1]
        distance <- abs(tmp_new$X - fix_x)
        #print(paste(j, "fix_x", fix_x, "dist", distance))
        tmp_new$X <- fix_x + (-1 + 2*is_steigend) * distance
        b_frame$ROTATE_X[j] <- T
      }
      # check rotate y
      gr_old <- tmp_old[TYPE == "Betriebsstellengrenze" & PARTNER == b_frame$BTS[j]]
      gr_new <- tmp_new[TYPE == "Betriebsstellengrenze" & PARTNER == b_frame$BTS[j-1]]
      distance <- numeric(0)
      for(i in 1:length(gr_old$CTR)){
        d <- gr_new$Y[gr_new$NODE_NAME == gr_old$NODE_NAME[i] & gr_new$STRECKE == gr_old$STRECKE[i]]
        if(length(d) == 1){
          distance <- c(distance, d - gr_old$Y[i])
        }else{
          distance <- c(distance, 0)
        }

      }
      if(sum(abs(distance)) > 0.1){
        fix_y <- gr_old$Y[which.min(abs(distance))]
        distance <- tmp_new$Y - fix_y
        tmp_new$Y <- tmp_new$Y - (2*distance)
        b_frame$ROTATE_Y[j] <- T
      }
      res <- calcShift(tmp_old, tmp_new)
      if(is.null(res)){stop(paste(j, "error in calcShift"))}
      b_frame$SHIFT_X[j] <- b_frame$SHIFT_X[j] + res[1]
      b_frame$SHIFT_Y[j] <- b_frame$SHIFT_Y[j] + res[2]
      tmp_new <- correctTMP(tmp_new, res)

      tmp_list[[j]] <- tmp_new
      #tmp_fw <- generateFWshift(tmp_list, ab_list[1:length(tmp_list)])
      #p <- ggplot()
      #p <- plotBTSFW(p, tmp_fw)
      #p <- plotInfra(p, tmp_list)
      #ggsave(filename = paste0("./WEBSERVICE/PLOTS/parts/", sprintf("%04d", j-1), ".jpg"),
      #       plot = p, width = 200, height = 30, units = "cm", limitsize = F)
      if(kopfmachen_last){
        is_steigend <- !is_steigend
        kopfmachen_last <- F
      }
    }
  }

  tmp_fw <- generateFWshift(tmp_list, ab_list)
  p <- plotBTSFW(ggplot(), tmp_fw)
  plotInfra(p, tmp_list)
}

plotOnlyInfra <- function(spurplanKnoten, bts_list){
  b_frame <- data.frame(BTS = bts_list, SHIFT_X = 0, SHIFT_Y = 0, ROTATE_X = F, ROTATE_Y = F, stringsAsFactors = F)
  tmp_list <- list(generateTMPshift(spurplanKnoten, b_frame$BTS[1]))

  is_steigend <- T
  tmp_list[[1]]$Y <- -tmp_list[[1]]$Y
  kopfmachen_last <- F
  if(length(b_frame$BTS) > 1){
    for (j in 2:length(b_frame$BTS)){
      #for (j in 2:11){
      tmp_old <- tmp_list[[j-1]]
      tmp_new <- generateTMPshift(spurplanKnoten, b_frame$BTS[j], 0, 0)
      res <- calcShift(tmp_old, tmp_new)
      if(is.null(res)){stop(paste(j, "error in calcShift"))}
      b_frame$SHIFT_X[j] <- res[1]
      b_frame$SHIFT_Y[j] <- res[2]
      tmp_new <- generateTMPshift(spurplanKnoten, b_frame$BTS[j], b_frame$SHIFT_X[j], b_frame$SHIFT_Y[j])
      gr_start <- tmp_new[TYPE == "Betriebsstellengrenze" & PARTNER == b_frame$BTS[j-1]]
      if(j < length(b_frame$BTS)){
        gr_end <- tmp_new[TYPE == "Betriebsstellengrenze" & PARTNER == b_frame$BTS[j+1]]
      }else{
        gr_end <- tmp_new[TYPE == "Fahrzeitmesspunkt"]
        if(length(gr_end$CTR) < 1){
          #last BTS with no FZMP :-/
          gr_end <- tmp_new[TYPE == "Betriebsstellengrenze" & PARTNER != gr_start$PARTNER[1],]
        }
      }
      if(gr_start$PARTNER[1] == gr_end$PARTNER[1]){
        # kopfmachen in j
        gr_end <- tmp_new[TYPE == "Fahrzeitmesspunkt"]
        kopfmachen_last <- T
      }
      if(((gr_end$X[1] - gr_start$X[1] ) >= 0) != is_steigend){
        # rotate by x
        fix_x <- tmp_new$X[tmp_new$PARTNER == b_frame$BTS[j-1]][1]
        distance <- abs(tmp_new$X - fix_x)
        #print(paste(j, "fix_x", fix_x, "dist", distance))
        tmp_new$X <- fix_x + (-1 + 2*is_steigend) * distance
        b_frame$ROTATE_X[j] <- T
      }
      # check rotate y
      gr_old <- tmp_old[TYPE == "Betriebsstellengrenze" & PARTNER == b_frame$BTS[j]]
      gr_new <- tmp_new[TYPE == "Betriebsstellengrenze" & PARTNER == b_frame$BTS[j-1]]
      distance <- numeric(0)
      for(i in 1:length(gr_old$CTR)){
        d <- gr_new$Y[gr_new$NODE_NAME == gr_old$NODE_NAME[i] & gr_new$STRECKE == gr_old$STRECKE[i]]
        if(length(d) == 1){
          distance <- c(distance, d - gr_old$Y[i])
        }else{
          distance <- c(distance, 0)
        }

      }
      if(sum(abs(distance)) > 0.1){
        fix_y <- gr_old$Y[which.min(abs(distance))]
        distance <- tmp_new$Y - fix_y
        tmp_new$Y <- tmp_new$Y - (2*distance)
        b_frame$ROTATE_Y[j] <- T
      }
      res <- calcShift(tmp_old, tmp_new)
      if(is.null(res)){stop(paste(j, "error in calcShift"))}
      b_frame$SHIFT_X[j] <- b_frame$SHIFT_X[j] + res[1]
      b_frame$SHIFT_Y[j] <- b_frame$SHIFT_Y[j] + res[2]
      tmp_new <- correctTMP(tmp_new, res)

      tmp_list[[j]] <- tmp_new
      #tmp_fw <- generateFWshift(tmp_list, ab_list[1:length(tmp_list)])
      #p <- ggplot()
      #p <- plotBTSFW(p, tmp_fw)
      #p <- plotInfra(p, tmp_list)
      #ggsave(filename = paste0("./WEBSERVICE/PLOTS/parts/", sprintf("%04d", j-1), ".jpg"),
      #       plot = p, width = 200, height = 30, units = "cm", limitsize = F)
      if(kopfmachen_last){
        is_steigend <- !is_steigend
        kopfmachen_last <- F
      }
    }
  }

  plotInfra(ggplot(), tmp_list)
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
      return(c(delta_x, delta_y))
    }
  }
  if(length(delta_x) <= 0){
    #no unique match or no match
    return()
  }
}

generateTMPshift <- function(spurplanKnoten, bts, shift_x = 0, shift_y = 0){
  tmp <- spurplanKnoten[BTS_NAME == bts]
  tmp <- tmp[X != ""]
  if(length(tmp$CTR) <= 0){return()}
  tmp$X <- as.numeric(tmp$X) - shift_x
  tmp$Y <- as.numeric(tmp$Y) - shift_y

  tmp
}

generateFWshift <- function(tmp_list, ab_list){
  tmp_fw <- list()
  for(j in 1:length(ab_list)){
    if(ab_list[[j]]$ab == ""){
      tmp_fw[[j]] <- NULL
      next()
      }
    ab <- unlist(strsplit(ab_list[[j]]$ab, "#"))
    tmp <- tmp_list[[j]][SP_AB_ID %in% ab,]
    tmp <- tmp[X != "" & TYPE != "Fahrzeitmesspunkt"]
    if(length(tmp$CTR) <= 0){
      tmp_fw[[j]] <- NULL
      next()
      }
    tmp_fw[[j]] <- tmp
    tmp_fw[[j]]$color <- ab_list[[j]]$color
  }
  tmp_fw
}

correctTMP <- function(tmp_new, res){
  tmp_new$X <- tmp_new$X - res[1]
  tmp_new$Y <- tmp_new$Y - res[2]
  tmp_new
}

plotBTSFW <- function(p, tmp){
  for(j in 1:length(tmp)){
    if(is.null(tmp[[j]])){next()}
    tmp[[j]]$GR <- 0
    p <- p + geom_line(data=tmp[[j]], aes(x=X, y=Y, group=SP_AB_ID), colour = tmp[[j]]$color, size = 3)
  }
  p
}

plotInfra <- function(p, tmp){

  for(j in 1:length(tmp)){
    fzmp <- tmp[[j]][TYPE == "Fahrzeitmesspunkt"]
    fzmp$GR <- 0
    tmp[[j]] <- tmp[[j]][TYPE != "Fahrzeitmesspunkt"]
    tmp[[j]]$GR <- 0
    tmp[[j]]$GR[tmp[[j]]$TYPE == "Betriebsstellengrenze"] <- 1
    tmp[[j]]$GR[tmp[[j]]$TYPE == "Gleisende" | tmp[[j]]$TYPE == "Prellbock"] <- 2

    p <- p +
      annotate("text", x=mean(tmp[[j]]$X), y=mean(tmp[[j]]$Y), label=tmp[[j]]$BTS_NAME[1], size = 3, color="grey") +
      geom_line(data=tmp[[j]], aes(x=X, y=Y, group=SP_AB_ID)) +
      geom_point(data=tmp[[j]], aes(x=X, y=Y, shape=as.factor(GR)), size = 1) +
      theme_minimal() + theme(legend.position="none")
    if(length(fzmp$CTR) > 0){
      p <- p +
        geom_point(data=fzmp, aes(x=X, y=Y), color = "red", size = 2) +
        geom_text(data=fzmp, aes(x=X, y=Y, label=NODE_NAME))
    }else{
      fzmp <- tmp[[j]][1]
      p <- p + geom_point(data=fzmp, aes(x=X, y=Y), color = "red", size = 0)
    }
  }


  p
}

# p <- plotBTS(spurplanKnoten, bts_list)
#
# ggsave(filename = "./tmp.jpg", plot = p, width = 200, height = 100, units = "cm", limitsize = F)
#
#
# abschnitte <- "9#4#12#5#7#3"
# p <- plotBTSFW(ggplot(), generateFWshift(spurplanKnoten, abschnitte))
# plotInfra(p, generateTMPshift(spurplanKnoten, "ELSP"))
#
# abschnitte <- "7819#7818"
# p <- plotBTSFW(ggplot(), generateFWshift(spurplanKnoten, abschnitte))
# plotInfra(p, generateTMPshift(spurplanKnoten, "FKRO"))
#
# abschnitte <- "15966#15970#16009#15888#15895#15968#15906#15917#15910#15884#15918"
# p <- plotBTSFW(ggplot(), generateFWshift(spurplanKnoten, abschnitte))
# plotInfra(p, generateTMPshift(spurplanKnoten, "AA"))
#
# abschnitte <- "#15932#15952#15918"
# p <- plotBTSFW(ggplot(), generateFWshift(spurplanKnoten, abschnitte))
# plotInfra(p, generateTMPshift(spurplanKnoten, "AA"))
#
# abschnitte <- "1594#1573#1515#1555#1568#1558#1625#1620#1485#1519#1532#1531"
# p <- plotBTSFW(ggplot(), generateFWshift(spurplanKnoten, abschnitte))
# plotInfra(p, generateTMPshift(spurplanKnoten, "FB"))
#
# abschnitte <- "5046#4997#5058#5073#5087#5078#4994#5056#5066#4996#5075#5029#5061#5002"
# p <- plotBTSFW(ggplot(), generateFWshift(spurplanKnoten, abschnitte))
# bts_list <- c("FFU G", "FFU")
# p <- plotBTS(spurplanKnoten, bts_list)
# plotInfra(p, generateTMPshift(spurplanKnoten, "FFU"))
#
# abschnitte <- list("71095#71098#71106#71107#71104#71103#71096#71097#71101#71111")
# tmp_list <- list(generateTMPshift(spurplanKnoten, "LHL"))
# p <- plotBTSFW(ggplot(), generateFWshift(tmp_list, abschnitte))
# plotInfra(p, tmp_list)
#
# plotInfra(ggplot(), generateTMPshift(spurplanKnoten, "FALZ"))
