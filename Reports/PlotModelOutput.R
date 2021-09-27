{
  rm ( list =  ls())
  library (HeroicOrExploit)
  library (ggplot2)
  library(vcd)
  library(diptest)
  fold = file.path ( PROJHOME , "Output" , "Rserver_output" )
  files = list.files(fold)
  files
  load  ( file.path ( fold , "DancombinedData2021_06_01.rda"  ))
  li2 = li
  load  ( file.path ( fold , "KingsleycombinedData2021_06_02.rda"  ))
  li2.2 = li
  load  ( file.path ( fold , "KingsleycombinedData2021_06_04.rda"  ))
  li2.3 = li
  load  ( file.path ( fold , "KingsleycombinedData2021_06_05.rda"  ))
  li2.4 = li
  load  ( file.path ( fold , "KingsleycombinedData2021_06_08.rda"  ))
  li2.5 = li
  load  ( file.path ( fold , "KingsleycombinedData2021_07_21.rda"  ))
  li2.6 = li
  load  ( file.path ( fold , "DancombinedData2021_07_26.rda"  ))
  li2.7 = li
  load  ( file.path ( fold , "DancombinedData2021_07_27.rda"  ))
  li2.8 = li
  load  ( file.path ( fold , "DancombinedData2021_07_29.rda"  ))

  #length ( li ) + length( li2.2)
  li3 = combine.lists( li, li2  )
  li3 = combine.lists(li3, li2.2)
  li3 = combine.lists(li3, li2.3)
  li3 = combine.lists(li3, li2.4)
  li3 = combine.lists(li3, li2.5)
  li3 = combine.lists(li3, li2.6)
  li3 = combine.lists(li3, li2.7)
  li3 = combine.lists(li3, li2.8)
  #li3=li2.4

  rm ( li )
  rm ( li2)
  rm ( li2.2)
  rm ( li2.3)
  rm ( li2.4)
  rm ( li2.5)
  rm(li2.6)
  rm(li2.7)
  rm(li2.8)
  rb = NULL
  vars = NULL
  hawkdovetype =NULL
  i=8
  par(mfrow= c(2,2))
}

li3[[20]]
"max n.gr rc skew  pyr C  mig  mut"

la = lapply(li3,function ( x) paste(x$vars, collapse = "-"))

li4 = list (
s50 = li3[which( la %in% "50-200-2-2-0.15-6-0.02-0.01")[1:15]],
s70 = li3[which( la %in% "70-200-2-2-0.15-6-0.02-0.01")[1:15]],
s90 = li3[which( la %in% "90-200-2-2-0.15-6-0.02-0.01")[1:15]]
)

save( li4,file = file.path(PROJHOME , "Plots_Videos", "plotData" , "P17.rda"))
maxs=NULL
biggrp=NULL
for ( i in 1:length ( li3)){
  #i=8
  if ( i == 1 & !is.null( rb )){
    stop()
  }

  dat = li3 [[i]]
  if ( ! is.null(dat)){
  vars = rbind ( vars , dat[[2]])
  dat = as.data.frame(dat[[1]])

  maxs =c(maxs, max(dat$n.per.group))
  if(max ( dat$n.per.group)>80 ){
    biggrp = c(biggrp, i)
  }
  #  plot ( dat$leader.dovehawk ~ dat$t)

  dat = dat [ order ( dat[,"t"]),]
  dat = dat [   dat [ ,"t"] %in% ( max( dat[,"t"]) - 5000):max( dat[,"t"])   ,]
  dat.m.dove = dat[ dat[,"leader.dovehawk"] >0.5,]

  # hist ( dat.m.dove[,"leader.dovehawk"], xlim = c(0,1))
  # hist ( dat.m.dove[,"follower.mean.dovehawk"])
  #dat.m.dove = dat.m.dove[ dat.m.dove [ , "follower.mean.dovehawk"]>0.5,]
  # hist ( dat.m.dove[,"follower.mean.dovehawk"], xlim = c( 0,1))
  #par(mfrow =c (2,2))
  eh = c( dat[ , "leader.explhero"] )
  ff = c( dat[,"follower.mean.folfight"])
  gs = c( dat[,"n.per.group"])
  # gsdh = c(dat[,"n.per.group"],0,1)
  # dh = c( dat[,"leader.dovehawk"],0,1)
  # smoothScatter(eh~ff,nbin = 50,nrpoints = 0,ylim = c(0,1), xlim=c(0,1), ylab = "Exploit Hero Score", xlab = "Follower Fight Score",colramp = colorRampPalette(viridis::viridis(12)))
  # smoothScatter(gs~ff,nbin = 50,nrpoints = 0, xlim=c(0,1), ylab = "gs", xlab = "Follower Fight Score",colramp = colorRampPalette(viridis::viridis(12)))
  # smoothScatter(gs~eh,nbin = 50,nrpoints = 0, xlim=c(0,1), ylab = "gs", xlab = "Exploit Hero Score",colramp = colorRampPalette(viridis::viridis(12)))
  # smoothScatter(gsdh~dh,nbin = 50,nrpoints = 0, xlim=c(0,1), ylab = "gs", xlab = "dovehawk",colramp = colorRampPalette(viridis::viridis(12)))

  hawk.leaders= mean( dat$leader.dovehawk)
  hawk.followers = mean ( dat$follower.mean.dovehawk)
  prop.hawk.leaders   = length ( which (dat$leader.dovehawk        > 0.5) )/nrow( dat)
  prop.hawk.followers = length ( which (dat$follower.mean.dovehawk > 0.5) )/nrow( dat)

  dove0.4 = length ( which (dat$leader.dovehawk        > 0.3) )/nrow( dat)
  if ( dove0.4 ==1){
    type=  "allhawk"
  } else {
    type = ifelse ( prop.hawk.leaders > 0.5 , "majhawk", "majdove")
  }
  # hist ( dat$leader.dovehawk)
  # legend ( "center" , legend = c(       type))


  hawkdovetype = c(hawkdovetype , type)



  dove.leaders=1-hawk.leaders
  dove.followers = 1 - hawk.followers
  rb=rbind ( rb ,  c( eh = mean( eh),
                      ff = mean ( ff),
                     # gs = gs,
                      dove.leaders   = dove.leaders,
                      hawk.leaders   = hawk.leaders,
                      prop.hawk.leaders = prop.hawk.leaders,#
                      prop.hawk.followers = prop.hawk.followers,
                      dove.followers = dove.followers,
                      hawk.followers = hawk.followers

  ))
  }
}
maxs
{
  rb = as.data.frame( rb)

  # hist ( rb$eh,xlim = c(0,1))
  # hist ( rb$ff,xlim = c(0,1))
  rb$diff =  apply ( rb [,1:2], 1, diff)
  rb$fol.con = rb$ff > 0.5
  rb$led.con = rb$eh > 0.5
  rb$type = "cooperative"
  rb$type [ rb$fol.con & !rb$led.con] = "Johnstone"
  rb$type [ !rb$fol.con & rb$led.con] = "Gavrilets"
  rb$type [ !rb$fol.con &!rb$led.con] = "selfish"
  rb [ rb$type == "Johnstone",]
  Dat = cbind ( vars, rb, hawkdovetype)

  ## DEFAULTS
  defaults = data.frame ( max = c(  70),
                          n.gr =200,
                          rc = c(2),
                          skew = c(2),
                          pyr =c(0.15),
                          mig = c(0.02),
                          mut = 0.01,
                          C = c(6))
  whi = which ( Dat$max %in% defaults$max &
                  Dat$n.gr %in% defaults$n.gr &
                  Dat$rc %in% defaults$rc &
                  Dat$skew %in% defaults$skew &
                  Dat$pyr %in% defaults$pyr &
                  Dat$mig %in% defaults$mig &
                  Dat$mut %in% defaults$mut &
                  Dat$C %in% defaults$C
  )
}


plott.var =
  function ( x , xlabb = "" , ylabb = "Contribution"){
    # x="C"
    # xlabb = NULL
    # ylabb = "Contribution"
    Dat2 = Dat[,x] != defaults[, x]
    Dat2[ whi ]= T
    Dat3 = Dat [ Dat2, ]
    Dat3$Xvar = Dat3[,x]
    Dat4 = rbind ( data.frame (x = Dat3 [, x] , contribution = Dat3$ff , type = "ff"),
                   data.frame (x = Dat3 [, x] , contribution = Dat3$eh , type = "eh"))
    g1 = ggplot ( Dat4, aes (y = contribution, x = x, col = type))+
      geom_point()+
      geom_smooth(method  = "loess",se=F)+
      xlab(x)+ylim(c(0,1))

    # boxplot ( Dat3$hawk.leaders ~ Dat3$Xvar, ylim = c(0,1),ylab = "dove-hawk score" , xlab = x)
    # par (new = T)
    # plot ( jitter( Dat3$Xvar,1) , Dat3$hawk.leaders, ylim = c(0,1),xaxt = "n" ,yaxt = "n", ylab = "", xlab = "")
    #
    #  plot ( Dat3$hawk.leaders ~ Dat3$eh)
    #  plot ( Dat3$hawk.leaders ~ Dat3$ff)

    g2 = ggplot ( Dat3, aes( as.factor( Xvar), hawk.leaders)) +  geom_violin() + geom_point() + xlab( x)+ylim(c(0,1))

    mod = glm ( hawk.leaders ~
                  eh * ff
                +Xvar
                , data = Dat3)
    print(summary( mod))

    mod = glm ( hawk.leaders ~
                  eh * ff
                # +Xvar
                , data = Dat3)
    print(summary( mod))
    df =  data.frame ( y =  mod$residuals , x =  Dat3$Xvar)
    g3 = ggplot ( df , aes  (x,y))+geom_point () + geom_smooth(method="lm") + xlab( x)


    gridExtra::grid.arrange(g1,g2,g3)


  }

par ( mfrow = c ( 1,1 ))
# plott.var( "max" , "max group size")
# plott.var( "n.gr", "n groups")
# plott.var( "rc"  , "rate of conflict")
# plott.var( "skew", "reproductive skew")
# plott.var( "pyr" , "Pyrrhic const.")
# plott.var( "mig" , "Migration rate")
# plott.var( "mut" , "Mutation strength")
# plott.var( "C"   , "Cost of escalated conflict")


hist( Dat$prop.hawk.leaders)
mj=1
if ( mj ==1){

  Dat.save = Dat
  Dat = Dat.save # run this only 2nd time around
  mj=2
}

save( Dat, file=  file.path (PROJHOME , "Plots_Videos" , "PlotData" , "pieData.rda"))
save( Dat, file=  file.path (PROJHOME , "Plots_Videos" , "PlotData" , "paramResultsData.rda"))

Dat = Dat [ - which (Dat$C   == 2) , ]
Dat = Dat [ - which (Dat$pyr == 1) , ]
rownames(Dat) = NULL
whi2 = which ( Dat$max %in% 50 &
                 Dat$n.gr %in% defaults$n.gr &
                 Dat$rc %in% 3 &
                 Dat$skew %in% 4 &
                 # Dat$pyr %in% defaults$pyr &
                 Dat$mig %in% 0.01 &
                 Dat$mut %in% defaults$mut
               # Dat$C %in% defaults$C
)
Dat2 = Dat[whi2,]
pies = table ( Dat2$hawkdovetype, Dat2$C, Dat2$pyr)
main = "majDove vs Inbetween vs Allhawk"
pies
dims = dim( pies)
# par ( mfrow = c( nrow(pies), ncol (pies)))
par ( mfrow = c( dims[2], dims[3]))

for ( i in 1:dims[2]){
  for( j in 1:dims[3]){
    x = pies[,i,j]
    # x= c( 1-x , x)
    #names ( x) = c( "maj. dove" , "all hawk")
    names( x) = c("","","")
    if( i == 1 & j ==1 ) {
      pie(x,main = main,col = c( "darkgrey" , "white", "lightgrey"))
    } else {
      pie(x , col = c( "darkgrey" , "white", "lightgrey"))
    }
  }
}
unqPyr = unique( Dat2$pyr)
unqC   = sort(unique( Dat2$C))
par ( mfrow = c( length(unqPyr), length(unqC)) )
for( i in 1:length(unqC)){
  for ( j in 1:length(unqPyr)){
    d = Dat2[ Dat2$pyr == unqPyr [j] &
                Dat2$C   == unqC   [i] ,]
    # hist(d$prop.hawk.leaders, xlim = c(0,1),breaks = 3000)
  }
}

whi2 = which ( Dat$max %in% 90 &
                 Dat$n.gr %in% defaults$n.gr &
                 Dat$rc %in% 1 &
                 Dat$skew %in% 1 &
                 # Dat$pyr %in% defaults$pyr &
                 Dat$mig %in% 0.04 &
                 Dat$mut %in% defaults$mut
               # Dat$C %in% defaults$C
)
Dat2 = Dat[whi2,]
pies = table ( Dat2$hawkdovetype, Dat2$C, Dat2$pyr)
main = "majDove vs Inbetween vs Allhawk"
pies
dims = dim( pies)
# par ( mfrow = c( nrow(pies), ncol (pies)))
par ( mfrow = c( dims[2], dims[3]))
for ( i in 1:dims[2]){
  for( j in 1:dims[3]){
    x = pies[,i,j]
    names( x) = c("","","")
    # x= c( 1-x , x)
    #names ( x) = c( "maj. dove" , "all hawk")
    if( i == 1 & j ==1 ) {
      pie(x,main = main, col = c( "darkgrey" , "white", "lightgrey"))
    } else {
      pie(x, col = c( "darkgrey" , "white", "lightgrey"))
    }
  }
}
unqPyr = unique( Dat2$pyr)
unqC   = sort(unique( Dat2$C))
par ( mfrow = c( length(unqPyr), length(unqC)) )
for( i in 1:length(unqC)){
  for ( j in 1:length(unqPyr)){
    d = Dat2[ Dat2$pyr == unqPyr [j] &
                Dat2$C   == unqC   [i] ,]
    # hist(d$prop.hawk.leaders, xlim = c(0,1),breaks = 3000)
  }
}


table ( Dat2$type , Dat2$C , Dat2$pyr)
?tapply
#Dat2 = Dat [ Dat$type %in% c(  "Gavrilets" ,  "Johnstone"),]
Dat2 = Dat [ Dat$C + ( Dat$C * Dat$pyr) > 6,]
Dat2 = Dat [ Dat$C = 6 & Dat$pyr == 0.15   ,]

ggplot ( Dat2 , aes (x = ff , y = hawk.leaders))+
  geom_smooth()+
  geom_point()+
  ylim ( c(0,1))

ggplot ( Dat2 , aes (x = eh , y = hawk.leaders))+
  geom_smooth()+
  geom_point()+
  ylim ( c(0,1))

ggplot ( Dat2 , aes (x = eh , y = ff))+
  geom_smooth()+
  geom_point()+
  ylim ( c(0,1))



Dat$hawk.binary     = Dat$hawk.leaders > 0.5
Dat$hero.binary     = Dat$eh > 0.5
Dat$folfight.binary = Dat$ff > 0.5

Dat2 = Dat[Dat$C == 6 & Dat$pyr == 0.15,]

par ( mfrow = c(2,2))
Dat2$hawkdovetype = as.factor ( Dat2$hawkdovetype)
x = table (  Dat2$hawkdovetype [ Dat2$folfight.binary == F & Dat2$hero.binary == F])
pie(  x, col = c( "darkgrey" , "white", "lightgrey"))
x = table (  Dat2$hawkdovetype [ Dat2$folfight.binary == T & Dat2$hero.binary == F])
pie(  x, col = c( "darkgrey" , "white", "lightgrey"))
x = table (  Dat2$hawkdovetype [ Dat2$folfight.binary == F & Dat2$hero.binary == T])
pie(  x, col = c( "darkgrey" , "white", "lightgrey"))
x = table (  Dat2$hawkdovetype [ Dat2$folfight.binary == T & Dat2$hero.binary == T])
pie(  x, col = c( "darkgrey" , "white", "lightgrey"))


hist (Dat2$hawk.leaders [ Dat2$folfight.binary == F & Dat2$hero.binary == F] , xlim = c(0,1), main = "" , xlab = "")
hist (Dat2$hawk.leaders [ Dat2$folfight.binary == T & Dat2$hero.binary == F] , xlim = c(0,1), main = "" , xlab = "")
hist (Dat2$hawk.leaders [ Dat2$folfight.binary == F & Dat2$hero.binary == T] , xlim = c(0,1), main = "" , xlab = "")
hist (Dat2$hawk.leaders [ Dat2$folfight.binary == T & Dat2$hero.binary == T] , xlim = c(0,1), main = "" , xlab = "")

# Whatever the variables, does not affect hawk-score beyond the fol fight. E.g. If you end up with folfight binary < 0.5, and leader binary > 0.5 you end up with XX hawk score. There is little variation  around the means of these histograms, despite they represent a huge variance in model parameters.

table ( Dat$skew  , Dat$type)





mod1 = lm ( Dat$hawk.leaders ~
              Dat$eh * Dat$ff +
              Dat$max +
              Dat$n.gr +
              Dat$rc +
              Dat$C *
              Dat$pyr +
              Dat$mig +
              Dat$mut)
plot(mod1)

summary ( mod1)
