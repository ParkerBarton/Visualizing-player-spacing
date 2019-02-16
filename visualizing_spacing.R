library(RCurl)
library(jsonlite)
library(dplyr)
library(sp)
library(ggplot2)
source("_functions.R")
source("_function_halfcourt.R")
all.movements <- sportvu_convert_json("/Users/parkerbarton/Desktop/UNZIPPED/0021500001.json")
str(all.movements)
gameid = "0021500001"
pbp <- get_pbp(gameid) 
head(pbp)

pbp <- pbp[-1,]
colnames(pbp)[2] <- c('event.id')
#Trying to limit the fiels to join to keep the overall size manageable
pbp <- pbp %>% select (event.id,EVENTMSGTYPE,EVENTMSGACTIONTYPE,SCORE)
pbp$event.id <- as.numeric(levels(pbp$event.id))[pbp$event.id]
all.movements.merged <- merge(x = all.movements, y = pbp, by = "event.id", all.x = TRUE)

id304 <- all.movements.merged[which(all.movements.merged$event.id == 304),]

head(id304)
#Capture the first time they get to 28'
balltime <- id304 %>% group_by(event.id) %>% filter(lastname=="ball")  %>% 
  summarise(clock28 = max(game_clock[x_loc<28])) %>% print(event.id,clock28)

#Find the positions of the players for each team at time 373.4 for event 303
dfall <- id304 %>% filter(game_clock == balltime$clock28)  %>% 
  filter(lastname!="ball") %>% select (team_id,x_loc,y_loc)
colnames(dfall) <- c('ID','X','Y')
head(dfall)

#Calculate the Convex Hull
df_hull2 <- dfall %>% filter(ID == min(ID)) %>% select(X,Y)
c.hull2 <- chull(df_hull2)  #Calculates convex hull#
c.hull3 <- c(c.hull2, c.hull2[1]) #You need five points to draw four line segments, so we add the first set of points at the end
df2 <- as.data.frame(cbind(1,df_hull2[c.hull3 ,]$X,df_hull2[c.hull3 ,]$Y))
colnames(df2) <- c('ID','X','Y')
df2 # The points of the convex hull

ggplot(df2, aes(x=X, y=Y)) + geom_polygon()  
#get area of plot
chull.coords <- df_hull2[c.hull3 ,]
chull.poly <- Polygon(chull.coords, hole=F)  #From the package sp
chull.area <- chull.poly@area
chull.area

#centroid of hull
dfcentroid <- c(mean(df_hull2[c.hull2 ,]$X),mean(df_hull2[c.hull2 ,]$Y))
dfcentroid 

##These functions assume you have all the movement data in a data frame called total

#Convert data into suitable format
total <-id304
total$x_loc_r <- total$x_loc
total$y_loc_r <- total$y_loc

#Get data for building graphic
dplayer <- player_position(304,297.48) #Gets positions of players
dchull <- chull_plot(304,297.48)       #Gets area of convex hull
dcentroid <- chull_plot_centroid(304,297.48)  #Gets centroid of convex hull
head(dplayer)

#Plot graphic
halfcourt() + 
  ##Add players
  geom_point(data=dplayer,aes(x=X,y=Y,group=ID),color=dense_rank(dplayer$ID),size=5) + scale_colour_brewer() +
  ##Add Convex hull areas
  geom_polygon(data=dchull,aes(x=X,y=Y,group=ID),fill=dense_rank(dchull$ID),alpha = 0.2) + scale_fill_brewer() + 
  ##Add Centroids
  scale_shape_identity() + geom_point(data=dcentroid,aes(x=X,y=Y,group=dcentroid$ID),color=(dcentroid$ID),size=3,shape=8) 
