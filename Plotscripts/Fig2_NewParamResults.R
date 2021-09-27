#Housekeeping
rm ( list = ls())
load(file.path (PROJHOME , "Plots_Videos" , "PlotData" , "paramResultsData.rda"))
library ( ggplot2 )
library( gghalves)
library(grid)
library(gridExtra)

# remove 600 as not enough data
Dat = Dat [ -which ( Dat$n.gr >=600 ),]
Dat = Dat [ -which ( Dat$skew >=6   ),]
Dat = Dat [ -which ( Dat$rc %in% c( 1.5, 2.5, 3.5)),]

## DEFAULTS
defaults = data.frame ( max = c(  70),
                        n.gr =200,
                        rc = c(2),
                        skew = c(2),
                        pyr =c(0.15),
                        mig = c(0.02),
                        mut = 0.01,
                        C = c(6))

# Add rows to keep boxplots true on x-axis
add  = function ( var = NULL , val= NULL){
  # var = "n.gr"
  # val = 600
  whi =   which ( names(Dat) == var)
  Dat = rbind( Dat , Dat[1,])
  Dat [ nrow( Dat) , ] = NA
  Dat$max [nrow(Dat)] = defaults$max
  Dat$n.gr[nrow(Dat)] = defaults$n.gr
  Dat$rc  [nrow(Dat)] = defaults$rc
  Dat$skew[nrow(Dat)] = defaults$skew
  Dat$pyr [nrow(Dat)] = defaults$pyr
  Dat$mig [nrow(Dat)] = defaults$mig
  Dat$mut [nrow(Dat)] = defaults$mut
  Dat$C   [nrow(Dat)] = defaults$C
  Dat[ nrow(Dat),whi] = val
  # Dat[nrow(Dat),]
  return(Dat)
}

#Dat =add ( "skew", 1.5)
#Dat =add ( "skew", 2.5)
#Dat =add ( "skew", 3)
#Dat =add ( "skew", 3.5)

## DEFAULTS
defaults = list ( max  = c(  70),
                  n.gr =200,
                  rc   = c(2),
                  skew = c(2),
                  pyr  = c(0.15),
                  mig  = c(0.02),
                  mut  = 0.01,
                  C    = c(6))
whi = which (   Dat$max  %in% defaults$max  &
                  Dat$n.gr %in% defaults$n.gr &
                  Dat$rc   %in% defaults$rc   &
                  Dat$skew %in% defaults$skew &
                  Dat$pyr  %in% defaults$pyr  &
                  Dat$mig  %in% defaults$mig  &
                  Dat$mut  %in% defaults$mut  &
                  Dat$C    %in% defaults$C
)







plott.var = function ( x , xlabb = "" , size.pts =NULL,dotsize = NULL, bold.text.num = NULL, legend = T, ylabb = "Contribution"){
  # x= c( "n.gr")
  # xlabb = NULL
  # ylabb = "Contribution"
  # legend = T
  # size.pts =0.01
  # dotsize =2
  # bold.text.num = 2

  def2 = defaults
  for ( j in 1:length(x)){
    def2[[which (names (def2) ==  x[j])]] = unique(Dat[,x[j]])
  }
  whi = which (
    Dat$max    %in% def2$max  &
      Dat$n.gr %in% def2$n.gr &
      Dat$rc   %in% def2$rc   &
      Dat$skew %in% def2$skew &
      Dat$pyr  %in% def2$pyr  &
      Dat$mig  %in% def2$mig  &
      Dat$mut  %in% def2$mut  &
      Dat$C    %in% def2$C
  )
  Dat2 = Dat[whi,]
  tb  = table(Dat2[,x])
  whi.tb = which ( tb > 15)
  whi.tb = as.numeric( names (whi.tb))
  if ( length (whi.tb ) >0){
    for ( i in whi.tb){
      #i=600
      whi.over = which ( Dat2[,x] ==  i   )
      Dat2 = Dat2 [ - whi.over[16:length(whi.over)],]
    }
  }
  print( table ( Dat2[,x]))


  Dat3 = Dat2
  Dat3$Xvar = as.factor (  Dat3[,x] )

  plainbold = rep ( "plain" , length ( unique ( Dat3$Xvar) ))
 # plainbold[bold.text.num] = "bold"

  Dat4 = rbind ( data.frame (x = Dat3 [, x] , Contribution = Dat3$ff , type = "Follower contribution"),
                 data.frame (x = Dat3 [, x] , Contribution = Dat3$eh , type = "Leader contribution"))




  g1 = ggplot ( Dat4, aes (y = Contribution, x =  x ,col = type))+

    stat_summary(fun = mean, geom = "point") +
    stat_summary(fun = mean, geom = "line")+
    stat_summary(fun.data =  mean_se, geom = "errorbar", width = 0)+
    xlab(xlabb)+ylim(c(0,1))+
   guides(col=guide_legend(title=""))+
    ylab ( "Fighting contribution" )+
   theme ( axis.text = element_text(size =  12.1), axis.title = element_text(size  = 11))
  if ( legend ==F){
    g1 = g1 + theme(legend.position = "none")
  }
  if( legend == T){
    g1 = g1 + theme( legend.position = c(0.75,0.89))
  }
  g1
  return ( g1)
}

g1 = ggplotify::as.grob( plott.var( "max" ,size.pts = 0.005,bold.text.num = 4,dotsize = 1.99,legend= T , xlabb= expression( paste ( "Max group size: "            , italic(Max)))))
g2 = ggplotify::as.grob( plott.var( "n.gr",size.pts = 0.016,bold.text.num = 2,dotsize = 2.99,legend= F , xlabb= expression( paste ( "N groups in starting pop.: " , italic(Ngr)))))
g3 = ggplotify::as.grob( plott.var( "rc"  ,size.pts = 0.016,bold.text.num = 2,dotsize = 2.99,legend= F , xlabb= expression( paste ( "Encounter rate: "            , italic(Er)))))
g4 = ggplotify::as.grob( plott.var( "skew",size.pts = 0.016,bold.text.num = 3,dotsize = 2.99,legend= F , xlabb= expression( paste ( "Reproductive skew: "         , italic(Skew)))))
g5 = ggplotify::as.grob( plott.var( "mig" ,size.pts = 0.016,bold.text.num = 3,dotsize = 2.99,legend= F , xlabb= expression( paste ( "Migration rate: "            , italic(Mig)))))
g6 = ggplotify::as.grob( plott.var( "mut" ,size.pts = 0.016,bold.text.num = 2,dotsize = 2.99,legend= F , xlabb= expression( paste ( "Mutation strength: "         , italic(Mut)))))
g8 = ggplotify::as.grob( plott.var( "pyr" ,size.pts = 0.016,bold.text.num = 2,dotsize = 2.99,legend= F , xlabb= expression( paste ( "Pyrrhic constant: "          , italic(Pyr)))))
g7 = ggplotify::as.grob( plott.var( "C"   ,size.pts = 0.016,bold.text.num = 3,dotsize = 2.99,legend= F , xlabb= expression( paste ( "Cost of escalated conflict: ", italic(C)))))

g1 <- arrangeGrob(g1, top = textGrob("A", x = unit(0, "npc"), y   = unit(1, "npc"), just=c("left","top"),gp=gpar(col="black", fontsize=18)))
g7 <- arrangeGrob(g7, top = textGrob("B", x = unit(0, "npc"), y   = unit(1, "npc"), just=c("left","top"),gp=gpar(col="black", fontsize=18)))
g8 <- arrangeGrob(g8, top = textGrob("D", x = unit(0, "npc"), y   = unit(1, "npc"), just=c("left","top"),gp=gpar(col="black", fontsize=18)))
g2 <- arrangeGrob(g2, top = textGrob("G", x = unit(0, "npc"), y   = unit(1, "npc"), just=c("left","top"),gp=gpar(col="black", fontsize=18)))
g3 <- arrangeGrob(g3, top = textGrob("D", x = unit(0, "npc"), y   = unit(1, "npc"), just=c("left","top"),gp=gpar(col="black", fontsize=18)))
g4 <- arrangeGrob(g4, top = textGrob("E", x = unit(0, "npc"), y   = unit(1, "npc"), just=c("left","top"),gp=gpar(col="black", fontsize=18)))
g5 <- arrangeGrob(g5, top = textGrob("C", x = unit(0, "npc"), y   = unit(1, "npc"), just=c("left","top"),gp=gpar(col="black", fontsize=18)))
g6 <- arrangeGrob(g6, top = textGrob("H", x = unit(0, "npc"), y   = unit(1, "npc"), just=c("left","top"),gp=gpar(col="black", fontsize=18)))

lay <- rbind(c(1,1,1,1,3,3,3),
             c(1,1,1,1,4,4,4),
             c(2,2,2,2,5,5,5)#,
           #  c(1,1,1,1,NA,NA,NA)
            # c(7,7,7,2,2),
           #  c(8,8,8,6,6)
           )

gs = list (g1, #g2 ,
           g7, g5,g3 ,g4#, g6, g7, g8
           )


gridExtra:: grid.arrange ( grobs=gs, layout_matrix=lay)


dev.copy(pdf,file.path (  PROJHOME , "Plots_videos" , "pdfs" , "p15.pdf"))
dev.off()
dev.off()
gridExtra:: grid.arrange ( grobs=gs, layout_matrix=lay)
