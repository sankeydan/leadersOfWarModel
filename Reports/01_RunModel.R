# Run model

# Housekeeping
rm( list= ls())
library ( LeadersOfWarModel)
#source ( file.path (PROJHOME , "R", "model2.r"))
folderDS(c(  "Output" , "ModelOutput2"))


# Variables
vars = expand.grid(
  max = c( 40, 60 , 80,  100),
  n.gr = c( 200 ),
  siz = c(  20 ),
  rc = c( 1,5 ) ,
  unc = c( 0.1,  0),
  mig = c( 0.01,0.05 ),
  mut = c( 0.01 ),
  skew = c(10, 1 ),
  V = c(6),
  pyr = c( 0,  0.5 ),
  C = c ( 4, 6, 8 ),
  n.ge = 50000)



# Run the model with each combination of variables
for (  i in 1:nrow ( vars)){
  #i=1

  # Filename
  fileName = paste0 ( paste (  c (paste ( as.vector( t(cbind( names ( vars ) , as.numeric(    vars [i , ])))), collapse = "-"), datetimeDS("date")), collapse = "_"), ".rda")

  # Model
  dat = try ( model4 (
    n.groups =             vars$n.gr [i],
    size.of.groups =       vars$siz  [i],
    mig.prob          =    vars$mig  [i],
    max.group.size       = vars$max  [i],
    mut.str =              vars$mut  [i],
    n.generations =        vars$n.ge [i],
    rate.of.conflict =     vars$rc   [i],
    V =                    vars$V    [i],
    C =                    vars$C    [i],
    pyr =                  vars$pyr  [i],
    method.starting.pop = "0.5",
    plott = T,
    plott.sq = 100
  ))
  if ( class ( dat )== "try-error"){
    plot.new ()
    mtext (  fileName , 3)
    mtext ( "had error" , 3 , line= -1)
    # Save
    dat2 = list ( data = NA, vars = vars[i,])
    save ( dat2 , file = file.path ( PROJHOME , "Output" , "ModelOutput2", fileName))
  }  else {


    # plot
    d = dat[ seq( 1, nrow( dat) , 1000),]

    {
    pdf ( file.path ( PROJHOME , "Plots_Videos" , "running_model_plots", paste ( "runplot" , i , fileName , datetimeDS( spec = "date") ,".pdf")))
      par ( mfrow = c( 2, 3))
    plot  ( d[,"leader.dovehawk"] ~ d[,"t"], ylim = c ( 0 , 1) , ylab = "Leader Hawk score")
    plot  ( d[,"leader.explhero"] ~ d[,"t"], ylim = c ( 0 , 1) , ylab = "Leader Hero score" )
    plot  ( d[,"n.per.group"] ~ d[,"t"],  ylab = "N in group" )
    dend = dat [   dat[ , "t"] == max( dat[,"t"]),]
    seq = round ( nrow( dat)/2):nrow(dat)
    smoothScatter(dat [  seq , "leader.dovehawk"],dat[seq,"n.per.group"],nbin = 50,nrpoints = 0, xlim = c(0,1), colramp = colorRampPalette(viridis::viridis(12)))
    smoothScatter(dat [  seq , "leader.dovehawk"],dat[seq,"leader.explhero"],nbin = 50,nrpoints = 0,ylim = c(0,1), xlim = c(0,1), colramp = colorRampPalette(viridis::viridis(12)))
    smoothScatter(dat [  seq , "leader.explhero"],dat[seq,"n.per.group"],nbin = 50,nrpoints = 0, xlim = c(0,1), colramp = colorRampPalette(viridis::viridis(12)))
    par ( new = T ,mfrow = c(1,1))
    mtext ( 3, text =  fileName, line = 3)
    par ( new = F)
    dev.off()
    }
    par ( mfrow = c(2,3))
    plot  ( d[,"leader.dovehawk"] ~ d[,"t"], ylim = c ( 0 , 1) , ylab = "Leader Hawk score")
    plot  ( d[,"leader.explhero"] ~ d[,"t"], ylim = c ( 0 , 1) , ylab = "Leader Hero score" )
    plot  ( d[,"n.per.group"] ~ d[,"t"],  ylab = "N in group" )
    seq = round ( nrow( dat)/2):nrow(dat)
    smoothScatter(dat [  seq , "leader.dovehawk"],dat[seq,"n.per.group"],nbin = 50,nrpoints = 0, xlim = c(0,1), colramp = colorRampPalette(viridis::viridis(12)))
    smoothScatter(dat [  seq , "leader.dovehawk"],dat[seq,"leader.explhero"],nbin = 50,nrpoints = 0,ylim = c(0,1), xlim = c(0,1), colramp = colorRampPalette(viridis::viridis(12)))
    smoothScatter(dat [  seq , "leader.explhero"],dat[seq,"n.per.group"],nbin = 50,nrpoints = 0, xlim = c(0,1), colramp = colorRampPalette(viridis::viridis(12)))
      par ( new = T ,mfrow = c(1,1))
    mtext ( 3, text =  fileName, line = 3)
    par ( new = F)

    # Save
    dat2 = list ( data = dat, vars = vars[i,])
    save ( dat2 , file = file.path ( PROJHOME , "Output" , "ModelOutput2", fileName))

  } # end of if( class ( dat) == "try-error" )

  # Takestock
  print ( paste ( i , "/" , nrow( vars)))
}


