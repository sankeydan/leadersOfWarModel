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

# Focused on C = 6,

whi2 = which ( Dat$max %in% 50 &
                 Dat$n.gr %in% defaults$n.gr &
                 Dat$rc %in% 3 &
                 Dat$skew %in% 4 &
                 #Dat$pyr %in% defaults$pyr &
                 Dat$mig %in% 0.01 &
                 Dat$mut %in% defaults$mut&
                Dat$C %in% defaults$C
)
whi3 = which ( Dat$max %in% 90 &
                 Dat$n.gr %in% 200&
                 Dat$rc %in% 1 &
                 Dat$skew %in% 1 &
               #  Dat$pyr %in% defaults$pyr &
                 Dat$mig %in% 0.04 &
                 Dat$mut %in% 0.01&
                Dat$C %in% defaults$C
)
whi4 = which (   Dat$max  %in% defaults$max  &
                   Dat$n.gr %in% defaults$n.gr &
                   Dat$rc   %in% defaults$rc   &
                   Dat$skew %in% defaults$skew &
                  # Dat$pyr  %in% defaults$pyr  &
                   Dat$mig  %in% defaults$mig  &
                   Dat$mut  %in% defaults$mut&
                 Dat$C %in% defaults$C
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

table ( Dat4$pyr)
Dat7=Dat4
unq = unique( Dat7$pyr)
for( i in 1:length(unq)){
  whi = which (Dat7$pyr == unq[i])
  Dat7 = Dat7 [ - whi [ (max.mods+1):length(whi)], ]
}


g1 =  ggplot ( Dat7 , aes ( x = factor ( pyr)  , y = hawk.leaders)) +
  geom_half_boxplot(outlier.shape = NA)+
  ylab ( "Leaders' dove-hawk score (DH)")+
  xlab ("")+
  ylim(c(0,1))+
  geom_half_dotplot(binwidth = size.pts,fill="white", dotsize= dotsize )
g2  =  ggplot ( Dat7 , aes ( x = factor ( pyr)  , y = ff)) +
  geom_half_boxplot(outlier.shape = NA)+
  ylab ( "Followers' contribution (FC)")+
  xlab ("Costly victory parameter (Pyr)")+
  ylim(c(0,1))+
  geom_half_dotplot(binwidth = size.pts,fill="white", dotsize= dotsize )
g1 <- arrangeGrob(g1, top = textGrob("A", x = unit(0, "npc"), y   = unit(1, "npc"), just=c("left","top"),gp=gpar(col="black", fontsize=18, fontfamily="Arial")))
g2 <- arrangeGrob(g2, top = textGrob("B", x = unit(0, "npc"), y   = unit(1, "npc"), just=c("left","top"),gp=gpar(col="black", fontsize=18, fontfamily="Arial")))

gridExtra::grid.arrange(g1,g2, ncol = 1)




