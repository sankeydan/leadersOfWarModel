# Run model
{
# Housekeeping
rm( list= ls())
library( HeroicOrExploit)

# Output method
output.method = "all.follower" #  "mean.follower" , "all.follower"

# Variables
vars = expand.grid(
  max  = c( 40)   ,
  n.gr = c( 200 ) ,
  siz  = c(  20 ) ,
  rc   = c( 1)   ,
  unc  = c( 0)    ,
  mig  = c( 0.01 ),
  mut  = c( 0.01 ),
  skew = c( 1)   ,
  V    = c(6)     ,
  pyr  = c(0)   ,
  C    = c(6)     ,
  n.ge = 20000)


# Run the model with each combination of variables
i=1
}
for (  i in 1:nrow ( vars)){


  # Filename
  fileName = paste0 ( paste (  c (paste ( as.vector( t(cbind( names ( vars ) , as.numeric( vars [i , ])))), collapse = "-"), datetimeDS("datehourmin")), collapse = "_"), ".rda")

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
    uncertainty.param =    vars$unc  [i],
    skew =                 vars$skew [i],
    method.starting.pop = "0.5",
    plott = T                  ,
    plott.sq = 100             ,
    output.method = output.method
  ))
  if ( class ( dat )== "try-error"){

    # Save
    dat2 = list ( data = NA, vars = vars[i,],surplusOrDeficit = NA  , groupsplitdata = NA)
    save ( dat2 , file = file.path (PROJHOME, "Output" , "ModelOutput-AllFollower", fileName))
  }  else {

    # Save
    dat2 = list ( data = dat$dat, vars = vars[i,], surplusOrDeficit = dat$surplusOrDeficit  , groupsplitdata = dat$groupsplitdata   )
    save ( dat2 , file = file.path ( fold , "Output" , "ModelOutput-AllFollower", fileName))

  } # end of if( class ( dat) == "try-error" )

  # Takestock
  print ( paste ( i , "/" , nrow( vars)))
}


