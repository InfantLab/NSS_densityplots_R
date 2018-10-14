library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)

setwd('C:\\Users\\Caspar\\Dropbox\\Goldsmiths\\NSS')


fulldata<-read_excel('NSS_taught_all17_rankings.xlsx',sheet='Sheet1')

fulldata<-tbl_df(fulldata)

theme_set(theme_classic())
goldposition <- function(allQs, Q) {
  
  thisQ<-dplyr::filter(allQs, Question == Q)
  Gold<-dplyr::filter(thisQ,INStitution == 'Goldsmiths College')
  
  gscore<-Gold$Satisfaction
  qtext<-paste(Q, ':  ',Gold$QuText)
  
  
  ggplot(thisQ,aes(Satisfaction))+geom_density() +
    geom_segment(aes(x=gscore,xend=gscore,y=0,yend=9)) +
    ggtitle(qtext) +
    xlim(0,1)
  qpng<-paste(Q, '.png')
  ggsave(qpng,width= 10, height = 6)
}


goldposition(fulldata,'Q01')
goldposition(fulldata,'Q02')
goldposition(fulldata,'Q03')
goldposition(fulldata,'Q04')
goldposition(fulldata,'Q05')
goldposition(fulldata,'Q06')
goldposition(fulldata,'Q07')
goldposition(fulldata,'Q08')
goldposition(fulldata,'Q09')
goldposition(fulldata,'Q10')
goldposition(fulldata,'Q11')
goldposition(fulldata,'Q12')
goldposition(fulldata,'Q13')
goldposition(fulldata,'Q14')
goldposition(fulldata,'Q15')
goldposition(fulldata,'Q16')
goldposition(fulldata,'Q17')
goldposition(fulldata,'Q18')
goldposition(fulldata,'Q19')
goldposition(fulldata,'Q20')
goldposition(fulldata,'Q21')
goldposition(fulldata,'Q22')
goldposition(fulldata,'Q23')
goldposition(fulldata,'Q24')
goldposition(fulldata,'Q25')
goldposition(fulldata,'Q26')
goldposition(fulldata,'Q27')
