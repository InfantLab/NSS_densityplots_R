goldposition <- function(thisYear,lastYear, Q) {

  #get the info for 2017
  thisQ17<-dplyr::filter(lastYear, Question == Q)
  Gold<-dplyr::filter(thisQ17,Provider == 'Goldsmiths College')
  #individual goldsmiths scores
  gscore17<-Gold$Satisfaction
  #text of question (with 2017 rank at end)
  qtext<-paste(Q, ':  ',Gold$QuText)
  #extract rank
  pos1 = str_locate(qtext,fixed("("))[1]
  pos2 = str_locate(qtext,fixed(")"))[1]
  rank17 = substr(qtext,pos1+1,pos2-1)
  #shorten question
  qtext = substr(qtext, 0, pos1-1)
  
  #Goldsmiths rank this year?
  rnk<-rank(-thisQ17$Satisfaction, ties.method = "min",)
  wh<-which(thisQ17$Provider == "Goldsmiths College", arr.ind=TRUE)
  rank17<-toString(rnk[wh])
  if((rnk[wh+1]=rnk[wh]) || (rnk[wh-1]=rnk[wh])){
    rank17 = paste("=", rank17, sep ="")
  }
  
  
  #get the info 2018, the column names are different
  thisQ18<-dplyr::filter(thisYear, `Question Number` == Q)
  Gold<-dplyr::filter(thisQ18,Provider == "Goldsmiths College")
  
  gscore18<-Gold$`Actual value`
  #qtext<-Q  #paste(Q, ':  ',Gold$QuText)
  
  #Goldsmiths rank this year?
  #note that we multiply by -1 to rank from highest to lowest
  rnk<-rank(-thisQ18$'Actual value', ties.method = "min",)
  wh<-which(thisQ18$Provider == "Goldsmiths College", arr.ind=TRUE)
  rank18<-toString(rnk[wh])
  if((rnk[wh+1]=rnk[wh]) || (rnk[wh-1]=rnk[wh])){
    rank18 = paste("=", rank18, sep ="")
  }
  r18 = paste("2018: ",rank18,sep="")
  r17 = paste("2017: ",rank17,sep="")
  ranks = paste("RANK\n2018: ",rank18,"\n2017: ",rank17,sep="")
  s18 = paste("2018: ",toString(gscore18*100), "%",sep="")
  s17 = paste("2017: ",toString(gscore17*100), "%",sep="")
  
  ggplot(data=thisQ18)+
    geom_density(data=thisQ17,aes(Satisfaction),color="gold") +
    geom_segment(aes(x=gscore17,xend=gscore17,y=0,yend=9), color="gold") +
    geom_density(data=thisQ18,aes(`Actual value`)) +
    geom_segment(aes(x=gscore18,xend=gscore18,y=0,yend=9)) +
    ggtitle(qtext) +
    geom_text(label = "Rank", x = .1, y = 8, size = 4, color = "black")+
    geom_text(label = r18, x = .1, y = 7.2, size = 6, color = "black")+
    geom_text(label = r17, x = .1, y = 6.4, size = 4, color = "gold")+
    geom_text(label = "Score", x = .1, y = 4, size = 4, color = "black")+
    geom_text(label = s18, x = .1, y = 3.2, size = 6, color = "black")+
    geom_text(label = s17, x = .1, y = 2.4, size = 4, color = "gold")+
    xlim(0,1)
    qpng<-paste(Q, '.png')
    ggsave(qpng,width= 10, height = 6)
}
