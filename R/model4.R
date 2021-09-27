#library(HeroicOrExploit)

model4 =   function  (
  n.groups =           NULL ,
  size.of.groups =     NULL ,
  uncertainty.param =  NULL ,
  max.group.size =     NULL ,
  mut.str =            NULL ,
  inf =                    1,
  vul =                    1,
  skew =                NULL,
  n.generations =       NULL,
  rate.of.conflict =    NULL,
  mig.prob =            NULL,
  V =                   NULL,
  C =                   NULL,
  pyr=                  NULL,
  method.starting.pop = "0.5", # c( "uniform , "0.5" , "choose" )
  groups = NULL,
  plott = F,
  plottred = F,
  plott.sq = NULL,
  plottreducer = 1,
  main = NULL,
  output.method = "mean.follower"
){

  ## VARIABLES

  # rm(list = ls())
  n.groups =  50
  size.of.groups =  30
  uncertainty.param = 0.1
  max.group.size       = 100
  mut.str =           0.01
  inf =                 5
  vul =                 1
  n.generations =        1000
  rate.of.conflict =     2
  V =                    8
  C =                    8
  pyr = 0
  skew                 = 2
  method.starting.pop = "uniform"
  mig.prob = 0.1
  plott = T
  plott.sq = 1000
  main = "eat my shorts"
  output.method = "all.follower"

#
  # inf =                    1
  # vul =                    1
  # output.method = "all.follower"
  # main = "eat my shorts"
  # i=1
  # n.groups =             vars$n.gr [i]
  # size.of.groups =       vars$siz  [i]
  # mig.prob          =    vars$mig  [i]
  # max.group.size       = vars$max  [i]
  # mut.str =              vars$mut  [i]
  # n.generations =        vars$n.ge [i]
  # rate.of.conflict =     vars$rc   [i]
  # V =                    vars$V    [i]
  # C =                    vars$C    [i]
  # uncertainty.param =    vars$unc  [i]
  # pyr =                  vars$pyr  [i]
  # skew =                 vars$skew [i]
  # method.starting.pop = "choose"
  # groups = groups
  # plott = T
  # plott.sq = 100
  # plottreducer = 1
  # library (HeroicOrExploit)


  { # initialise conditions , run from here

    # library
    #library (HeroicOrExploit)

    # Stop if not
    # if ( !( pyr * C)%%1==0) stop ( " pyr * C must be an integer")
    if ( !(V/2)%%1==0) stop ( "V/2 must be an integer")
    # if ( (  (1 + pyr) * C  ) < V) stop ( "1 +pyr * C should be >= V ")
    # Initialise

    if ( method.starting.pop != "choose" ){
      groups = list ()
      for ( i in 1:n.groups){
        if ( method.starting.pop == "uniform"){
          groups[[i]] =   cbind  ( dovehawk = runif ( size.of.groups),
                                   explhero = runif ( size.of.groups),
                                   folfight = runif (size.of.groups))
        }
        if ( method.starting.pop == "0.5") {
          groups[[i]] =  cbind ( dovehawk = rep( 0.5, size.of.groups),
                                 explhero = rep( 0.5, size.of.groups),
                                 folfight = rep(0.5, size.of.groups))
        }
      }
    }


    group.id = 1:length(groups)
    pop.size = sum ( unlist ( lapply ( groups , nrow)))
    m = length(groups)

    # exponential distribution for group splits
    EXPS = rexp ( 100000 )
    exps = EXPS /10
    #hist (exps)
    exps = ifelse ( exps > 1, 1, exps )
    #hist(exps)
    exps =   4 +
      ( exps *( max.group.size  - 4 ))
    exps = max.group.size + 4 - exps
    #hist ( exps, breaks = 1000)
    #min ( exps)

    # empty objects
    li = list ()
    dat = NULL
    groupsplitdata= NULL
    surplusOrDeficit = NULL
  }

  # run simulation
  t=1
  for ( t in 1: n.generations ){

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

      # IF neither group is extinct
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
            leader.vulnerability.g1 = g1.heroscore * vul
            leader.vulnerability.g2 = g2.heroscore * vul
            follower.contribution.g1 = sum(g1[-1,"folfight"])
            follower.contribution.g2 = sum(g2[-1,"folfight"])

            g1.str = follower.contribution.g1+leader.fight.contribution.g1
            g2.str = follower.contribution.g2+leader.fight.contribution.g2

            # Do team 1 win?
            if ( g1.str == g2.str ){
              win1 = as.logical(  rbinom(1,1,0.5) )
            } else {
              win1 =
                rnorm ( 1, mean = g1.str , sd = nrow( g1)* uncertainty.param) >
                rnorm ( 1, mean = g2.str , sd = nrow( g2)* uncertainty.param)
            }

            # Fitness - mortalities
            g1.mort=  reprod.mort.funct( group =  g1,
                                         reprod.mort = "mort",
                                         win = win1,
                                         C = C,
                                         pyr = pyr,
                                         follower.contribution = follower.contribution.g1,
                                         leader.vulnerability = leader.vulnerability.g1,
                                         group.fight.strength = g1.str)
            g2.mort =  reprod.mort.funct( group =  g2,
                                          reprod.mort = "mort",
                                          win = !win1,
                                          C = C,
                                          pyr = pyr,
                                          follower.contribution = follower.contribution.g2,
                                          leader.vulnerability = leader.vulnerability.g2,
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
              g1 = g1.reprod$group
            }
            if ( !is.na(g2[1])){
              g2.reprod = reprod.mort.funct( group = g2 ,
                                             reprod.mort =  "reprod",
                                             win = !win1 ,
                                             V = V,
                                             skew = skew,
                                             mut.str = mut.str,
                                             group.leader.survives =  g2.mort$group.leader.survives )
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
    spli = data.frame (spli)
    whi  = which ( apply ( spli , 1 , function ( x ){length( unique(  c( x, 1)))} ) == 1)

    # # # # # work # # #
    #if ( spli[1] == 1) stop()
    # # # # #
    while ( length(  whi ) > 0){

      for ( i in 1:length ( whi)){
        #i=1
        j = whi[i]
        m = m+1
        group.id = c( group.id, m )#
        groupsplitdata= rbind ( groupsplitdata  , c( mother = group.id[j] , daughter =  m, t = t))
        newgroup =  sample ( 2:n.per.group[j] ,size = round ( runif ( 1 , min = 2, max = floor(n.per.group[j]/2) )) ) # Leader cannot be in rebel faction. Rebel faction cannot be larger than original group
        newgroupies =   groups[[j]][newgroup,]
        groups[[length(groups)+1]] = newgroupies
        groups[[j]] = groups[[j]][-newgroup,] # they will continue to lead their successful group
      }
      n.per.group = unlist ( lapply  ( groups , function (x) nrow ( x)))
      sam = sample ( exps,length( groups))
      spli2 = apply ( cbind ( n.per.group ,sam  ),1,which.max)
spli = cbind ( spli , spli2[1:nrow(spli)] )
whi  = which ( apply ( spli , 1 , function ( x ){length( unique(  c( x, 1)))} ) == 1)
    }



    ##### POPULATION DYNAMICS ###  Indiviudals die or replicate with same probability. Slight possibility that population needs to gain size but unlikely
    n.per.group = unlist ( lapply  ( groups , function (x) nrow ( x)))
    allind = sum ( n.per.group)
    surplusOrDeficit =c ( surplusOrDeficit ,allind - pop.size)
    mat = cbind( group.num = rep (  c(  1: length ( n.per.group)) ,  n.per.group),
                 dovehawk = unlist ( lapply ( groups , function (x) x[,"dovehawk"])),
                 explhero = unlist ( lapply ( groups , function (x) x[,"explhero"])),
                 folfight = unlist ( lapply ( groups, function (x) x[,"folfight"])),
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
          #x = reprods [ 1, ]
          c( x[1], rnormWithMinMax(3, m =  x[c("dovehawk", "explhero","folfight")], s = mut.str, lwr = 0, upr = 1), 0)
        }))
      } else {
        newbies = t(apply ( t(reprods) , 1 , function (x) {
          c( x[1], rnormWithMinMax(3, m =  x[c("dovehawk", "explhero","folfight")], s = mut.str, lwr = 0, upr = 1), 0)
        }))
      }
      mat2 = rbind ( mat , newbies)
    }
    # no change
    if ( allind == pop.size ){
      mat2 = mat
    }

    # Migration
    mig = replicate( nrow (mat2), rbinom(1,1,mig.prob))
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
          grou  = mat2[ whi , c ( "dovehawk" , "explhero", "folfight")]
          groups[[j]] = rbind ( grou[whi2 ,] , grou[-whi2,])
        } else {
          groups[[j]] = mat2[ sample(whi), c ( "dovehawk", "explhero", "folfight")]
        }
      } else {
        which.extinct = c( which.extinct , j)
      }
    }
    groups[  which.extinct ] = NULL
    group.id [which.extinct] = NA
    group.id = na.omit ( group.id)
    # # # # work # # # # #
    # vec= 1
    # m2 = 1
    # while ( m2 > 0){
    #   lenvec1 = length (vec)
    #   vec2 = groupsplitdata [ which(  groupsplitdata[,1] %in% vec), 2]
    #   vec= c ( vec, vec2)
    #   vec = vec[ !duplicated(vec)]
    #   lenvec2 = length(vec)
    #   m2 = diff( c ( lenvec1 , lenvec2))
    # }
    # altruistic.followers = group.id [ which ( unlist ( lapply ( groups ,function (x) mean( x[,"folfight"]) ))>0.8)]
    # if (any ( !altruistic.followers %in% vec )) stop("1")
    # # # # # # # # # # # #

    n.per.group = unlist ( lapply ( groups, function(x) nrow ( x)))

    ########### Save data ###########
    if ( output.method == "mean.follower"){
      leader.scores =   t(   sapply ( groups, function(x) x[1,]) )
      follower.scores = t (  sapply ( groups, function(x)  {
        x2 = x[2:nrow(x),]
        if (length ( x2) == 3){
          x2
        } else {
          colMeans( x2 )
        }
      }) )

      generation.dat  = cbind ( n.per.group, leader.scores, follower.scores, group.id , t)
      dimnames ( generation.dat )[[2]] = c(  "n.per.group",  "leader.dovehawk",     "leader.explhero",  "leader.folfight",  "follower.mean.dovehawk", "follower.mean.explhero", "follower.mean.folfight" ,"group.id", "t")
      dat = rbind ( dat, generation.dat)


      # plot data
      if ( plott ){

        if ( t %in% seq ( plott.sq, n.generations, plott.sq)){
          print(t)
          d = dat[ sample ( 1: nrow( dat), plottreducer),]
          par ( mfrow = c( 3, 3))

          vec= 1
          m2 = 1
          while ( m2 > 0){
            lenvec1 = length (vec)
            vec2 = groupsplitdata [ which(  groupsplitdata[,"mother"] %in% vec), 2]
            vec= c ( vec, vec2)
            vec = vec[ !duplicated(vec)]
            lenvec2 = length(vec)
            m2 = diff( c ( lenvec1 , lenvec2))
          }
          d = as.data.frame(d)
          d$col = 1
          if ( plottred ){
            d$col[ d$group.id %in% vec] = 2
          }
          plot  ( d[,"leader.dovehawk"] ~ d[,"t"], ylim = c ( 0 , 1)    , col = d$col, ylab = "Leader Hawk score")
          plot  ( d[,"leader.explhero"] ~ d[,"t"], ylim = c ( 0 , 1)    , col = d$col, ylab = "Leader Hero score" )
          plot  ( d[, "follower.mean.folfight"] ~ d[,"t"], ylim = c(0,1), col = d$col, ylab = "Follower Fight Score")
          # plot  ( d[,"follower.mean.dovehawk"] ~ d[,"t"], ylim = c ( 0 , 1) , ylab = "Follower mean Hawk score" )
          #  plot  ( d[,"follower.mean.explhero"] ~ d[,"t"], ylim = c ( 0 , 1) , ylab = "Follower mean Hero score" )
          #plot  ( d[,"n.per.group"] ~ d[,"t"],  ylab = "N in group" )
          dend = dat [   dat[ , "t"] == max( dat[,"t"]),]
          plot ( dend[,"leader.dovehawk"], dend[ , "n.per.group"] , xlim = c( 0 ,1))
          plot ( dend[,"leader.dovehawk"], dend[ , "leader.explhero"], xlim = c( 0,1), ylim = c( 0,1))
          plot ( dend[,"leader.explhero"], dend[ , "follower.mean.folfight"], xlim = c( 0,1),ylim = c(0,1))
          plot ( dend[,"leader.dovehawk"], dend[ , "follower.mean.folfight"], xlim = c( 0,1),ylim = c(0,1))
          plot ( dend[,"follower.mean.folfight"], dend[ , "n.per.group"] , xlim = c( 0 ,1))


          par ( new = T ,mfrow = c(1,1))
          mtext ( 3, text =  main, line = 3)
          par ( new = F)
        }
      }
    }

    # Save data, different method
    if ( output.method == "all.follower"){


      doc = do.call ("rbind" , groups )
      gi = rep ( group.id, n.per.group)
      dat.temp = cbind (doc, group.id=gi, t = t ,class.l.f = 2)
      cums = cumsum(n.per.group)
      cums = cums [ 1:(length(cums)-1)]
      dat.temp [ c(1, cums+1), "class.l.f"] =1
      dat = rbind(dat , dat.temp)

      if ( plott ){
        if ( t %in% seq ( plott.sq, n.generations, plott.sq)){
          print(t)
          par(mfrow = c(1,3))
          d = dat[ sample ( 1: nrow( dat), plottreducer),]
          d2 = dat [ sample ( 1:nrow(dat), plottreducer*20),]
          plot  ( d2[d2[,"class.l.f"] == 1,"dovehawk"]  ~ d2[d2[,"class.l.f"] == 1,"t"], ylim = c ( 0 , 1)    , ylab = "Leader Hawk score")
          plot  ( d2[d2[,"class.l.f"] == 1,"explhero"]  ~ d2[d2[,"class.l.f"] == 1,"t"], ylim = c ( 0 , 1)    , ylab = "Leader Hero score" )
          plot  ( d[d[,"class.l.f"] == 2, "folfight"] ~ d[d[,"class.l.f"] == 2,"t"], ylim = c(0,1), ylab = "Follower Fight Score")

        }
      }

    }
  }
  return ( list (
    dat = dat  ,
    surplusOrDeficit = surplusOrDeficit,
    groupsplitdata = groupsplitdata
  ))
}
