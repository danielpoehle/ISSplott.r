setwd("./Dokumente/ISSplott.r/")
library(data.table)
library(ggplot2)

spurplanKnoten <- as.data.table(read.csv2(file = "./tmpRBNord.csv", stringsAsFactors = F))
# bts_list <- c("ABCH", "ABCHG", "ASTT", "A465M", "A464S", "AMUE")

plotBTS <- function(spurplanKnoten, bts_list){
  p <- ggplot()
  b_frame <- data.frame(BTS = bts_list, SHIFT_X = 0, SHIFT_Y = 0)
  tmp <- generateTMPshift(spurplanKnoten, b_frame$BTS[1])
  p <- plotInfra(p, tmp)
  for (j in 2:length(b_frame$BTS)){
    tmp_old <- generateTMPshift(spurplanKnoten, b_frame$BTS[j-1], b_frame$SHIFT_X[j-1], b_frame$SHIFT_Y[j-1])
    tmp_new <- generateTMPshift(spurplanKnoten, b_frame$BTS[j], b_frame$SHIFT_X[j], b_frame$SHIFT_Y[j])
    res <- calcShift(tmp_old, tmp_new, b_frame$SHIFT_X[j-1], b_frame$SHIFT_Y[j-1], b_frame$SHIFT_X[j], b_frame$SHIFT_Y[j])
    if(is.null(res)){stop(paste(j, "error in calcShift"))}
    b_frame$SHIFT_X[j] <- res[1]
    b_frame$SHIFT_Y[j] <- res[2]
    tmp <- generateTMPshift(spurplanKnoten, b_frame$BTS[j], b_frame$SHIFT_X[j], b_frame$SHIFT_Y[j])
    p <- plotInfra(p, tmp)
  }
  p
}

calcShift <- function(tmp_old, tmp_new, x_old, y_old, x_new, y_new){
  bts_old <- tmp_old$BTS_NAME[1]
  bts_new <- tmp_new$BTS_NAME[1]
  gr_old <- tmp_old[TYPE == "Betriebsstellengrenze" & PARTNER == bts_new]
  gr_new <- tmp_new[TYPE == "Betriebsstellengrenze" & PARTNER == bts_old]
  if(length(gr_old) <=0 || length(gr_new) <=0){return()}
  
  #find matching Betriebsstellengrenze
  for(i in 1:length(gr_old)){
    m <- gr_new[NODE_NAME == gr_old$NODE_NAME[i]]
    if(length(m$CTR) == 1){
      delta_x <- m$X - gr_old$X[i]
      delta_y <- m$Y - gr_old$Y[i]
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

plotInfra <- function(p, tmp){
  
  fzmp <- tmp[TYPE == "Fahrzeitmesspunkt"]
  fzmp$GR <- 0
  tmp <- tmp[TYPE != "Fahrzeitmesspunkt"]
  tmp$GR <- 0
  tmp$GR[tmp$TYPE == "Betriebsstellengrenze"] <- 1
  tmp$GR[tmp$TYPE == "Gleisende" | tmp$TYPE == "Prellbock"] <- 2
  
  p <- p +
    annotate("text", x=mean(tmp$X), y=mean(tmp$Y), label=tmp$BTS_NAME[1], size = 10, color="grey") +
    geom_line(data=tmp, aes(x=X, y=Y, group=SP_AB_ID)) +
    geom_point(data=tmp, aes(x=X, y=Y, size=3, shape=as.factor(GR))) +
    theme_minimal() + theme(legend.position="none")
  if(length(fzmp$CTR) > 0){
    p <- p + 
      geom_point(data=fzmp, aes(x=X, y=Y, color = "red", size = 3)) + 
      geom_text(data=fzmp, aes(x=X, y=Y, label=NODE_NAME)) 
  }else{
    fzmp <- tmp[1]
    p <- p + geom_point(data=fzmp, aes(x=X, y=Y, color = "red", size = 0))
  }
  p
}

p <- plotBTS(spurplanKnoten, bts_list)
ggsave(filename = "./BTSLIST.jpg", plot = p, width = 10*length(bts_list), units = "cm")
