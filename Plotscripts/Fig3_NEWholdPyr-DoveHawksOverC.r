rm ( list = ls() )
par ( mar =  c ( 4,4,4,4), mfcol = c( 3,5) )
library(ggplot2)
library(gghalves)
load( file.path (PROJHOME , "Plots_Videos" , "PlotData" , "pieData.rda"))
defaults = list ( max = c(  70),
                  n.gr =200,
                  rc = c(2),
                  skew = c(2),
                  pyr =c(0.15),
                  mig = c(0.02),
                  mut = 0.01,
                  C = c(6) )


## 1

#### CONDITIONS  - favour fol fight

whi2 = which ( Dat$max %in% 50 &
                 Dat$n.gr %in% defaults$n.gr &
                 Dat$rc %in% 3 &
                 Dat$skew %in% 4 &
                 #Dat$pyr %in% defaults$pyr &
                 Dat$mig %in% 0.01 &
                 Dat$mut %in% defaults$mut
               # Dat$C %in% defaults$C
)
Dat2 = Dat[whi2,]

# PLOT
unq = unique ( Dat2$pyr)
for ( i in 1:length (unq)){
  #i=1
 Dat3 = Dat2[ Dat2$pyr  == unq[i], ]
plot ( Dat3$hawk.leaders ~ Dat3$C , ylim = c(  0 ,1 ), main = paste ( "pyr = " , unq [i]))
plot ( Dat3$ff           ~ Dat3$C , ylim = c(  0 ,1 ))
plot ( Dat3$hawk.leaders ~ Dat3$ff , ylim = c(  0 ,1 ),xlim = c(0,1))
abline ( 0, 1)
 }

## 2

#### CONDITIONS  - favour NO fol fight

whi3 = which ( Dat$max %in% 90 &
                 Dat$n.gr %in% 1000 &
                 Dat$rc %in% 0.5 &
                 Dat$skew %in% 1 &
                 # Dat$pyr %in% defaults$pyr &
                 Dat$mig %in% 0.04 &
                 Dat$mut %in% 0.01#
               # Dat$C %in% defaults$C
)
Dat2 = Dat[whi3,]

# PLOT
unq = unique ( Dat2$pyr)
for ( i in 1:length (unq)){
  #i=1
  Dat3 = Dat2[ Dat2$pyr  == unq[i], ]
  plot ( Dat3$hawk.leaders ~ Dat3$C , ylim = c(  0 ,1 ), main = paste ( "pyr = " , unq [i]))
  plot ( Dat3$ff           ~ Dat3$C , ylim = c(  0 ,1 ))
  plot ( Dat3$hawk.leaders ~ Dat3$ff , ylim = c(  0 ,1 ),xlim = c(0,1))
  abline ( 0, 1)
}


############ 3.

# Focused on Pyr = 0.15,

whi2 = which ( Dat$max %in% 50 &
                 Dat$n.gr %in% defaults$n.gr &
                 Dat$rc %in% 3 &
                 Dat$skew %in% 4 &
                 Dat$pyr %in% defaults$pyr &
                 Dat$mig %in% 0.01 &
                 Dat$mut %in% defaults$mut
               # Dat$C %in% defaults$C
)
whi3 = which ( Dat$max %in% 90 &
                 Dat$n.gr %in% 200&
                 Dat$rc %in% 1 &
                 Dat$skew %in% 1 &
                 Dat$pyr %in% defaults$pyr &
                 Dat$mig %in% 0.04 &
                 Dat$mut %in% 0.01#
               # Dat$C %in% defaults$C
)
whi4 = which (   Dat$max  %in% defaults$max  &
                 Dat$n.gr %in% defaults$n.gr &
                 Dat$rc   %in% defaults$rc   &
                 Dat$skew %in% defaults$skew &
                 Dat$pyr  %in% defaults$pyr  &
                 Dat$mig  %in% defaults$mig  &
                 Dat$mut  %in% defaults$mut
               # Dat$C %in% defaults$C
)
par ( mfcol = c( 2,2))

Dat2 = Dat [ whi2, ]
Dat3 = Dat [ whi3, ]
Dat4 = Dat [ whi4, ]
Dat2$folPush = "a_high"
Dat3$folPush = "c_low"
Dat4$folPush = "b_med"
Dat5 = rbind ( Dat2 , Dat3, Dat4)
Dat6 = Dat5[  Dat5$C == 6,]


table(Dat6$folPush)
plot ( Dat2$hawk.leaders ~ Dat2$C , ylim = c(  0 ,1 ))
points  ( Dat3$hawk.leaders ~ Dat3$C , ylim = c(  0 ,1 ), col = "red")
points  ( Dat4$hawk.leaders ~ Dat4$C , ylim = c(  0 ,1 ), col = "blue")
plot ( Dat2$ff ~ Dat2$C , ylim = c(  0 ,1 ))
points  ( Dat3$ff ~ Dat3$C , ylim = c(  0 ,1 ), col = "red")
points  ( Dat4$ff ~ Dat4$C , ylim = c(  0 ,1 ), col = "blue" )
plot ( Dat2$hawk.leaders ~ Dat2$ff , ylim = c(  0 ,1 ), xlim = c(0,1))
points  ( Dat3$hawk.leaders ~ Dat3$ff , ylim = c(  0 ,1 ), xlim = c(0,1), col = "red")
points  ( Dat4$hawk.leaders ~ Dat4$ff , ylim = c(  0 ,1 ), xlim = c(0,1), col = "blue")

plot  ( Dat4$hawk.leaders ~ Dat4$C , ylim = c(  0 ,1 ), col = "blue")
plot  ( Dat4$ff ~ Dat4$C , ylim = c(  0 ,1 ), col = "blue" )


abline ( 0,1 )


unq = unique ( Dat5$C)
for  ( i in 1:length ( unq)){
  #i=4
Dat6 = Dat5[  Dat5$C == unq[i],]
  boxplot ( Dat6$hawk.leaders ~ Dat6$folPush, ylim =  c( 0,1),main = unq[i])
  boxplot ( Dat6$ff           ~ Dat6$folPush)
}

par( mfcol = c( 2,1))
Dat6 = Dat5[  Dat5$C == 6,]
plot  ( Dat4$ff ~ Dat4$C , ylim = c(  0 ,1 ), col = "blue" )
plot  ( Dat4$hawk.leaders ~ Dat4$C , ylim = c(  0 ,1 ), col = "blue")


size.pts = 0.009
dotsize = 2
max.mods = 15

table ( Dat4$C)
Dat7=Dat4
unq = unique( Dat7$C)
for( i in 1:length(unq)){
  whi = which (Dat7$C == unq[i])
  Dat7 = Dat7 [ - whi [ (max.mods+1):length(whi)], ]
}
table ( Dat7$C)

g1 =  ggplot ( Dat7 , aes ( x =  C  , y = hawk.leaders)) +
  stat_summary(fun = mean, geom = "point") +
  stat_summary(fun = mean, geom = "line")+
  stat_summary(fun.data =  mean_se, geom = "errorbar", width = 0)+
  ylab ( expression ( paste ( "Leaders' hawk-dove score ( ", italic (HD) , ")")))+
  xlab ( expression( paste ( "Cost of escalated fighting (",italic( C ) ,")")))+
  ylim(c(0,1))+
  theme ( axis.text = element_text(size =  12.1), axis.title = element_text(size  = 13))
g2  =  ggplot ( Dat7 , aes ( x = C  , y = ff)) +
  stat_summary(fun = mean, geom = "point") +
  stat_summary(fun = mean, geom = "line")+
  stat_summary(fun.data =  mean_se, geom = "errorbar", width = 0)+
  ylab ( expression( paste ( "Followers' fight contribution ( "  , italic( FC) ,")")))+
  xlab ( expression( paste ( "Cost of escalated fighting (",italic( C ) ,")")))+
  ylim(c(0,1))+
  theme ( axis.text = element_text(size =  12.1), axis.title = element_text(size  = 13))



table ( Dat6$folPush)
Dat8=Dat6
unq = unique( Dat8$folPush)
for( i in 1:length(unq)){
  whi = which (Dat8$folPush == unq[i])
  Dat8 = Dat8 [ - whi [ (max.mods+1):length(whi)], ]
}
table ( Dat8$folPush)

Dat8$folPush [ Dat8$folPush == "c_low"] = "Low FC"
Dat8$folPush [ Dat8$folPush == "b_med"] =  "Medium FC (default values)"
Dat8$folPush [ Dat8$folPush == "a_high"] = "High FC"

Dat8$folPush <- factor(Dat8$folPush ,levels = c("Low FC","Medium FC (default values)","High FC"))
g3 =  ggplot ( Dat8 , aes ( x =  as.numeric( folPush)  , y = hawk.leaders)) +
  stat_summary(fun = mean, geom = "point") +
  stat_summary(fun = mean, geom = "line")+
  stat_summary(fun.data =  mean_se, geom = "errorbar", width = 0)+
  ylab ( expression ( paste ( "Leaders' hawk-dove score ( ", italic (HD) , ")")))+
  ylim(c(0,1))+
  xlab ("Parameter values in favour of...")+
 scale_x_continuous(breaks = c(1,2,3) ,

                    labels= c( expression ( paste ( "Low "   , italic (     FC))),
                               expression ( paste ( "Medium ", italic (     FC) , " (default values)")),
                               expression ( paste ( "High "  , italic (     FC)))))+
  theme ( axis.text = element_text(size =  12.1), axis.title = element_text(size  = 13))


#expression(paste("No. of ", italic("bacteria X"), " isolates with corresponding types")



g1 <- arrangeGrob(g1, top = textGrob("A", x = unit(0, "npc"), y   = unit(1, "npc"), just=c("left","top"),gp=gpar(col="black", fontsize=18)))
g2 <- arrangeGrob(g2, top = textGrob("B", x = unit(0, "npc"), y   = unit(1, "npc"), just=c("left","top"),gp=gpar(col="black", fontsize=18)))
g3 <- arrangeGrob(g3, top = textGrob("C", x = unit(0, "npc"), y   = unit(1, "npc"), just=c("left","top"),gp=gpar(col="black", fontsize=18)))



lay = rbind ( c ( 1,1,3,3),
              c ( 2,2,3,3))
gridExtra::grid.arrange(g1,g2,g3,layout_matrix = lay)



 cb =  cbind ( c ( 2 , 7 ,7 ,3 , 4 ,5),
               c ( 3 , 3 ,4 ,3 , 4, 5))
 sam = sample (1:6)
 cb
 cb[sam,]
cor( cb[sam ,1] , cb [sam,2])

sum (c( 23.45,
   2.18,
   23,
   2,18,
   50,
   50,
   30,
   9,
   16.50,
   12,
   101.5,
   14.25,
   6.3,
   45.9,
   2,
   32.5,
   42,
   160,
   21.5,
   6.6,
   3,
   24,
   3.2,
   2.4,
   27,
   13,
   3)
)
