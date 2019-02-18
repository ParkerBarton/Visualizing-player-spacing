library(RCurl)
library(jsonlite)
library(dplyr)
library(sp)
library(ggplot2)
library(data.table)
library(gganimate)

source("PBP_functions.R")
source("gganimate-0.1.1")

all.movements <- sportvu_convert_json("/Users/parkerbarton/Desktop/UNZIPPED/0021500001.json")
str(all.movements)

gameid = "0021500001"
pbp <- get_pbp(gameid)
str(pbp)

pbp <- pbp[-1,] #first row is NAs
colnames(pbp)[2] <- c('event.id') #will use this to merge on all.movements df
pbp0 <- pbp %>% select (event.id,EVENTMSGTYPE,EVENTMSGACTIONTYPE,SCORE)
pbp0$event.id <- as.numeric(levels(pbp0$event.id))[pbp0$event.id]
head(pbp0)
tail(pbp0)

id305 <- all.movements[which(all.movements$event.id == 305),]
dim(id305)
head(id305)
#length(table(id304$game_clock))

playerdf <- player_position1(df=id305, eventid=305,gameclock=290.63) 
playerdf
chulldf <- chull_plot(df=id305, eventid=305,gameclock=290.63)
chulldf
ballposdf <- ball_position1(df=id305, eventid=305,gameclock=290.63)
ballposdf

fullcourt() + 
  geom_point(data=playerdf,aes(x=X,y=Y,group=ID,color=factor(ID)),size=6) +       #players
  geom_text(data=playerdf,aes(x=X,y=Y,group=ID,label=jersey),color='black') +     #jersey number
  geom_polygon(data=chulldf,aes(x=X,y=Y,group=ID,fill=factor(ID)),alpha = 0.2) +  #convex hull
  geom_point(data=ballposdf,aes(x=X,y=Y),color='darkorange',size=3) +             #ball
  scale_color_manual(values=c("lightsteelblue2","orangered2")) +
  scale_fill_manual(values=c("lightsteelblue2","orangered2")) +
  theme(legend.position="none")

clocktimes= rev(sort(unique(id305$game_clock)))

fulldf=list()

for(i in seq_along(clocktimes)){
  
  dplayer <- player_position1(df=id305, 305,clocktimes[i]) #Gets positions of players
  dchull <- chull_plot(df=id305, 305,clocktimes[i])       #Gets area of convex hull
  ballpos <- ball_position1(df=id305, 305,clocktimes[i])  #Gets position of ball
  dchull$jersey = "NA"
  dplayer$valx = 'player'
  dchull$valx = 'hull'
  ballpos$valx  = 'ball'
  fulldf[[i]] = rbind(dplayer,dchull,ballpos)
}

length(fulldf)  #419 elements

fulldf = Map(cbind,fulldf,timebin=1:length(fulldf))  #add time unit 
table(lapply(fulldf,nrow) %>% unlist)
playdf = data.table::rbindlist(fulldf)
playdf2 = playdf %>% filter(timebin!=1) %>% filter(timebin<418)

p = fullcourt() + 
  geom_point(data=playdf2 %>% filter(valx=="player"),aes(x=X,y=Y,group=ID,color=factor(ID),frame=timebin),size=6) +
  geom_text(data=playdf2 %>% filter(valx=="player"),aes(x=X,y=Y,group=ID,frame=timebin,label=jersey),color='black') +
  geom_polygon(data=playdf2 %>% filter(valx=="hull"),aes(x=X,y=Y,group=ID,fill=factor(ID),frame=timebin),alpha = 0.2) + 
  geom_point(data=playdf2 %>% filter(valx=="ball"),aes(x=X,y=Y,frame=timebin),color='darkorange',size=3) +
  scale_color_manual(values=c("lightsteelblue2","orangered2")) +
  scale_fill_manual(values=c("lightsteelblue2","orangered2")) +
  theme(legend.position="none")
gganimate(p, "nbaplot.gif", title_frame =F, ani.width = 600, ani.height = 450, interval=0.1) 
