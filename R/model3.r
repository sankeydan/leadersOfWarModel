#library(HeroicOrExploit)

model3 =   function  (
  n.groups = 50 ,
  size.of.groups = 100 ,
  uncertainty.param =  .00 ,
  max.group.size = 500 ,
  mut.str = 0.01 ,
  inf = 5 ,
  skew = 2 ,
  n.generations = 100000,
  rate.of.conflict = 1,
  mig.prob = 0.01,
  V = 1 ,
  C = 2 ,
  method.starting.pop = "uniform", # c( "uniform , "0.5" )
  plott = F,
  plott.sq = 5000,
  main = NULL
){

  ## VARIABLES

  # n.groups =             vars$n.gr [1]
  # size.of.groups =       vars$siz  [1]
  # uncertainty.param =    vars$unc  [1]
  # max.group.size       = 50
  # mut.str =              vars$mut  [1]
  # inf =                  vars$inf  [1]
  # n.generations =        vars$n.ge [1]
  # rate.of.conflict =     vars$rc   [1]
  # V =                    vars$V    [1]
  # C =                    vars$C    [1]
  # skew                 = vars$skew [1]
  # method.starting.pop = "0.5"
  # mig.prob = 0.1
  # plott = T
  # plott.sq = 1000
  # main = "eat my shorts"

  { # initialise conditions , run from here

    # library
    #library (HeroicOrExploit)

    # Stop if not
    if ( !(C/2)%%1==0) stop ( "C/2 must be an integer")
    if ( !(V/2)%%1==0) stop ( "V/2 must be an integer")

    # Initialise
    groups = list ()
    for ( i in 1:n.groups){
      if ( method.starting.pop == "uniform"){
        groups[[i]] =   cbind  ( dovehawk = runif ( size.of.groups),
                                 explhero = runif ( size.of.groups))
      }
      if ( method.starting.pop == "0.5") {
        groups[[i]] =  cbind ( dovehawk = rep( 0.5, size.of.groups),
                               explhero = rep( 0.5, size.of.groups))
      }
    }
    group.id = 1:n.groups
    pop.size = n.groups * size.of.groups
    m = n.groups

    # exponential distribution for group splits
    EXPS = rexp ( 100000 )
    exps = EXPS /10
    hist (exps)
    exps = ifelse ( exps > 1, 1, exps )
    hist(exps)
    exps =   4 +
      ( exps *( max.group.size  - 4 ))
    exps = max.group.size + 4 - exps
    #hist ( exps, breaks = 1000)

    # empty objects
    li = list ()
    dat = NULL
  }

  # run simulation
  for ( t in 1: n.generations ){
    #t=1


    n.conflicts = round ( length(groups)*rate.of.conflict * 0.5)
    sam.mat = NULL
    for ( i in 1:n.conflicts){
      x = sample ( 1:length (groups), size = 2, replace = F)
      sam.mat= rbind ( sam.mat , x)
    }

    # Fighting loop
    no.fight = c()
    for ( i in 1: nrow(sam.mat)){
      #i=7

      id1 = sam.mat[i,1]
      id2 = sam.mat[i,2]
      g1 = groups[[id1]]
      g2 = groups[[id2]]

      # extinction
      if ( !is.na  ( g1[1])){
        if ( nrow(g1) < 2){
          g1 = NA
          no.fight = c( no.fight , which (apply ( sam.mat , 1 , function(x) id1 %in% x  )))
        }
      } else  {
        no.fight = c( no.fight , which (apply ( sam.mat , 1 , function(x) id1 %in% x  )))
      }
      if ( !is.na ( g2[1])){
        if ( nrow(g2) < 2){
          g2 = NA
          no.fight = c( no.fight , which (apply ( sam.mat , 1 , function(x) id2 %in% x  )))
        }
      } else {
        no.fight = c( no.fight , which (apply ( sam.mat , 1 , function(x) id2 %in% x  )))
      }

      if (!  i %in% no.fight){

        # Will they fight?
        g1.hawk = as.logical ( rbinom( 1,1,prob = g1[1,"dovehawk"]) )
        g2.hawk = as.logical ( rbinom( 1,1,prob = g2[1,"dovehawk"]) )


        ##### HAwK dove GAMES ####
        #source( file= file.path ( PROJHOME , "R" , "reprod.mort.funct.r"))
        {
          # if both hawks
          if ( g1.hawk & g2.hawk ){

            # How much will the leader contrbute?
            g1.heroscore = g1[1,"explhero"]
            g2.heroscore = g2[1,"explhero"]
            names(g1.heroscore) = NULL
            names(g2.heroscore) = NULL
            leader.fight.contribution.g1 = g1.heroscore * inf
            leader.fight.contribution.g2 = g2.heroscore * inf
            g1.str = (nrow( g1)-1)+leader.fight.contribution.g1
            g2.str = (nrow( g2)-1)+leader.fight.contribution.g2

            # Do team 1 win?
            win1 =
              rnorm ( 1, mean = g1.str , sd = nrow( g1)* uncertainty.param) >
              rnorm ( 1, mean = g2.str , sd = nrow( g2)* uncertainty.param)

            # Fitness - mortalities
            g1.mort=  reprod.mort.funct( group =  g1,
                                         reprod.mort = "mort",
                                         win = win1,
                                         C = C,
                                         leader.fight.contribution= leader.fight.contribution.g1,
                                         group.fight.strength = g1.str)
            g2.mort =  reprod.mort.funct( group =  g2,
                                          reprod.mort = "mort",
                                          win = !win1,
                                          C = C,
                                          leader.fight.contribution= leader.fight.contribution.g2,
                                          group.fight.strength = g2.str)
            g1 = g1.mort$group
            g2 = g2.mort$group

            # Fitness - Reproductions
            if ( !is.na(g1[1])){
              g1.reprod =  reprod.mort.funct( group =  g1,
                                              reprod.mort = "reprod",
                                              win = win1,
                                              V = V,
                                              skew = skew,
                                              mut.str = mut.str,
                                              group.leader.survives = g1.mort$group.leader.survives)
            }
            if ( !is.na(g2[1])){
              g2.reprod = reprod.mort.funct( group = g2 ,
                                             reprod.mort =  "reprod",
                                             win = !win1 ,
                                             V = V,
                                             skew = skew,
                                             mut.str = mut.str,
                                             group.leader.survives =  g2.mort$group.leader.survives )
              g1 = g1.reprod$group
              g2 = g2.reprod$group
            }
          }

          if ( g1.hawk &  ! g2.hawk){
            g1.reprod = reprod.mort.funct( group= g1,
                                           reprod.mort = "reprod",
                                           win = T,
                                           V = V,
                                           skew = skew,
                                           mut.str = mut.str,
                                           group.leader.survives = T
            )
            g1 = g1.reprod$group
          }
          if ( g2.hawk &  ! g1.hawk){
            g2.reprod = reprod.mort.funct( group= g2,
                                           reprod.mort = "reprod",
                                           win = T,
                                           V = V,
                                           skew = skew,
                                           mut.str = mut.str,
                                           group.leader.survives = T
            )
            g2 = g2.reprod$group
          }
          if ( !g1.hawk & ! g2.hawk ){
            g1.reprod = reprod.mort.funct( group= g1,
                                           reprod.mort = "reprod",
                                           win = T,
                                           V = V/2,
                                           skew = skew,
                                           mut.str = mut.str,
                                           group.leader.survives = T)
            g2.reprod = reprod.mort.funct( group= g2,
                                           reprod.mort = "reprod",
                                           win = T,
                                           V = V/2,
                                           skew = skew,
                                           mut.str = mut.str,
                                           group.leader.survives = T)
            g1 = g1.reprod$group
            g2 = g2.reprod$group
          }
        }


        # back into population
        groups[[id1]] = g1
        groups[[id2]] = g2

      } # end of  if ( i  %in% no.fight)

    } # end of fighting loop, sam.mat


    #Population dynamics
    extinct.groups  = unlist (  lapply ( groups,   function (x) is.na(x[1]) ) )
    groups[ which (  extinct.groups )] = NULL
    group.id [ which( extinct.groups)] = NA
    group.id = na.omit ( group.id)

    #group splits?
    n.per.group = unlist ( lapply  ( groups , function (x) nrow ( x)))
    sam = sample ( exps,length( groups))
    spli = apply ( cbind ( n.per.group ,sam  ),1,which.max)
    if ( any ( spli == 1 ) ){
      whi = which ( spli == 1 )
      for ( i in 1:length ( whi)){
        #i=1
        j = whi[i]
        m = m+1
        group.id = c( group.id, m )#
        newgroup =  sample ( 2:n.per.group[j] ,size = round ( runif ( 1 , min = 2, max = floor(n.per.group[j]/2) )) ) # Leader cannot be in rebel faction. Rebel faction cannot be larger than original group
        newgroupies =   groups[[j]][newgroup,]
        groups[[length(groups)+1]] = newgroupies
        groups[[j]] = groups[[j]][-newgroup,] # they will continue to lead their successful group
      }
    }

    ##### POPULATION DYNAMICS ###  Indiviudals die with same probability. Slight possibility that population needs to gain size but unlikely
    n.per.group = unlist ( lapply  ( groups , function (x) nrow ( x)))
    allind = sum ( n.per.group)
    mat = cbind( group.num = rep (  c(  1: length ( n.per.group)) ,  n.per.group),
                 dovehawk = unlist ( lapply ( groups , function (x) x[,"dovehawk"])),
                 explhero = unlist ( lapply ( groups , function (x) x[,"explhero"])),
                 leader = F)
    leader = c(  1,  cumsum ( n.per.group[1:(length(n.per.group)-1)])+1)
    mat [ leader, "leader" ] = 1

    # deaths
    if ( allind - pop.size > 0 ){
    mat2  = mat [  - sample ( 1:nrow(mat) , size =  allind - pop.size) , ]
    }
    # births
    if ( allind - pop.size < 0) {
      reprods = mat [ sample ( 1 : nrow( mat), size = pop.size - allind),]
      if ( length (reprods) != ncol(mat)){
      newbies = t(apply ( reprods , 1 , function (x) {
        c( x[1], rnormWithMinMax(2, m =  x[2:3], s = mut.str, lwr = 0, upr = 1), 0)
      }))
      } else {
        newbies = t(apply ( t(reprods) , 1 , function (x) {
          c( x[1], rnormWithMinMax(2, m =  x[2:3], s = mut.str, lwr = 0, upr = 1), 0)
        }))
      }
      mat2 = rbind ( mat , newbies)
    }
    # no change
    if ( allind == pop.size ){
      mat2 = mat
    }

    # Migration
    mig = replicate( pop.size , rbinom(1,1,mig.prob))
    mig[ mat2[,"leader"] == 1] = 0
    mat2 = t ( apply ( cbind ( mat2 , mig) , 1 , function (x) {
      #x= cbind ( mat2, mig) [1,]
      if ( x ["mig"] != 0){
        x["group.num"] = sample ( c( 1:length(groups))[-x["group.num"]] , size = 1)
      }
      x
    }))
    tb = table ( mat2[,"group.num"])
    fulltb = rep ( 0, length( n.per.group))
    names ( fulltb ) = 1: length(n.per.group)
    fulltb[ names (tb) ] = tb
    which.extinct = NULL
    groups = list()
    for ( j in 1: length ( n.per.group)){
      #j=2
      whi = which ( mat2 [ , "group.num"]  == j)
      if ( length ( whi ) >1 ){ # if group 2 or more
        if ( any ( mat2[whi,"leader"] == 1)){
          whi2 = which ( mat2[whi,"leader"]==1)
          grou  = mat2[ whi , c ( "dovehawk" , "explhero")]
          groups[[j]] = rbind ( grou[whi2 ,] , grou[-whi2,])
        } else {
          groups[[j]] = mat2[ sample(whi), c ( "dovehawk", "explhero")]
        }
      } else {
        which.extinct = c( which.extinct , j)
      }
    }
    groups[  which.extinct ] = NULL
    group.id [which.extinct] = NA
    group.id = group.id [  complete.cases( group.id) ]


    #Save data
    n.per.group = unlist ( lapply ( groups, function(x) nrow ( x)))
    leader.scores =   t(   sapply ( groups, function(x) x[1,]) )
    follower.scores = t (  sapply ( groups, function(x)  {
      x2 = x[2:nrow(x),]
      if (length ( x2) == 2){
        x2
      } else {
        colMeans( x2 )
      }
    }) )
    generation.dat  = cbind ( n.per.group, leader.scores, follower.scores, group.id , t)
    dimnames ( generation.dat )[[2]] = c(  "n.per.group",  "leader.dovehawk",     "leader.explhero",    "follower.mean.dovehawk", "follower.mean.explhero",  "group.id", "t")
    dat = rbind ( dat, generation.dat)

    # plot data
    if ( plott ){
      if ( t %in% seq ( plott.sq, n.generations, plott.sq)){
        d = dat[ sample ( 1: nrow( dat), round ( nrow(dat)/400)),]
        par ( mfrow = c( 2, 3))
        plot  ( d[,"leader.dovehawk"] ~ d[,"t"], ylim = c ( 0 , 1) , ylab = "Leader Hawk score")
        plot  ( d[,"leader.explhero"] ~ d[,"t"], ylim = c ( 0 , 1) , ylab = "Leader Hero score" )
        # plot  ( d[,"follower.mean.dovehawk"] ~ d[,"t"], ylim = c ( 0 , 1) , ylab = "Follower mean Hawk score" )
        #  plot  ( d[,"follower.mean.explhero"] ~ d[,"t"], ylim = c ( 0 , 1) , ylab = "Follower mean Hero score" )
        plot  ( d[,"n.per.group"] ~ d[,"t"],  ylab = "N in group" )
        dend = dat [   dat[ , "t"] == max( dat[,"t"]),]
        plot ( dend[,"leader.dovehawk"], dend[ , "n.per.group"] , xlim = c( 0 ,1))
        plot ( dend[,"leader.dovehawk"], dend[ , "leader.explhero"], xlim = c( 0,1), ylim = c( 0,1))
        plot ( dend[,"leader.explhero"], dend[ , "n.per.group"], xlim = c( 0,1))

        par ( new = T ,mfrow = c(1,1))
        mtext ( 3, text =  main, line = 3)
        par ( new = F)
      }
    }
  }
  return ( dat )
}
