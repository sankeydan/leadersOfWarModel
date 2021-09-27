model =   function  (
  n.groups = 50 ,
  size.of.groups = 100 ,
  uncertainty.param =  .005 ,
  max.group.size.param = 3 ,
  mut.str = 0.01 ,
  influence = 2 ,
  n.generations = 100000,
  method.starting.pop = "uniform",
  plott = F,
  plott.sq = 5000,
  main = NULL
){
  {
    ## VARIABLES

    # n.groups = 50
    # size.of.groups = 10
    # uncertainty.param =  .005
    # max.group.size.param = 3
    # mut.str = 0.01
    # influence = 2
    # n.generations = 100000
    # plott = T
    # main = "dickvandyke"
    # method.starting.pop = "uniform"


    # compound parameters
    uncertainty = uncertainty.param * size.of.groups
    max.group.size = max.group.size.param * size.of.groups

    # Initialise
    groups = list ()
    for ( i in 1:n.groups){
      if ( method.starting.pop == "uniform"){
        groups[[i]] = cbind  ( dovehawk = runif ( size.of.groups),
                               explhero = runif ( size.of.groups))
      } else {
        groups[[i]] =  cbind ( dovehawk = rep( 0.5, size.of.groups),
                               explhero = rep( 0.5, size.of.groups))
      }
    }
    pop.size = n.groups * size.of.groups

    # exponential distribution for group splits
    EXPS = rexp ( 100000 )
    exps=EXPS /10
    exps = ifelse ( exps > 1, 1, exps )
    exps =   size.of.groups +
      ( exps *( max.group.size  - size.of.groups ))
    exps = max.group.size + size.of.groups - exps
    #hist ( exps, breaks = 1000)

    # empty objects
    li = list ()
    dat = NULL
  }

  # run simulation
  for ( t in 1: n.generations ){

    # # # # #
    if ( any ( lapply ( groups,  length)<2))stop("1")
    # # # # #

    round.down.to.even = 2 * floor( length(groups)/2)
    sam.mat  = matrix ( sample ( 1:round.down.to.even ), ncol = 2)
    for ( i in 1: nrow(sam.mat)){
      #i=1
      id1 = sam.mat[i,1]
      id2 = sam.mat[i,2]
      g1 = groups[[id1]]
      g2 = groups[[id2]]

      # Will they fight?
      g1.hawk = as.logical ( rbinom( 1,1,prob = g1[1,"dovehawk"]) )
      g2.hawk = as.logical ( rbinom( 1,1,prob = g2[1,"dovehawk"]) )

      # if both hawks
      if ( g1.hawk & g2.hawk ){

        # Will the leader contrbute?
        g1.e = as.logical ( rbinom( 1,1,g1[1]) ) # probability leader will be heroic, .e represents effort, multiplied by their influence score
        g2.e = as.logical ( rbinom( 1,1,g2[1]) )
        g1.str = ifelse ( g1.e , length( g1) * influence, length(g1)-1)
        g2.str = ifelse ( g2.e , length( g2) * influence, length(g2)-1)


        # Do team 1 win?
        win1 =
          rnorm ( 1, mean = g1.str , sd = uncertainty) >
          rnorm ( 1, mean = g2.str , sd = uncertainty)

        # Fitness
        if ( win1 ){ # if team 1 wins
          if ( ! g1.e){ # and the leader exploited
            groups[[id1]] = c ( g1 ,
                                rnormWithMinMax(1,       g1[1],mut.str,lwr = 0,upr=1 ),  # Leader reproduces
                                rnormWithMinMax(1,sample(g1,1),mut.str,lwr = 0,upr=1 )  )# and Random member reproduces. Could be leader twice potentially
          } else { # If leader was heroic
            groups[[id1]] = c ( g1 , rnormWithMinMax(1,sample(g1,1),mut.str,lwr = 0,upr=1 )  )# Only Random member reproduces, could be leader!
          } # Team 2 get punished
          if (  g2.e){ # if the leader was heroic
            g2 = g2[ - c(  1,                                      # Off with his head!!
                           sampleWithoutSurprises ( 2:length(g2)))] # And one other random
            groups[[id2]] = sample(g2) # new leader assigned at random
          } else { # If the leader was exploitative
            groups[[id2]] = g2[ - sampleWithoutSurprises(2:length(g2))] # just random member dies
          }
        }
        if ( ! win1 ){ # if team 2 wins
          if ( ! g2.e){ # and the leader exploited
            groups[[id2]] = c ( g2 ,
                                rnormWithMinMax(1,       g2[1],mut.str,lwr = 0,upr=1 ),  # Leader reproduces
                                rnormWithMinMax(1,sample(g2,1),mut.str,lwr = 0,upr=1 )  )# and Random member reproduces. Could be leader twice potentially
          } else { # If leader was heroic
            groups[[id2]] = c ( g2 , rnormWithMinMax(1,sample(g2,1),mut.str,lwr = 0,upr=1 )  )# Only Random member reproduces, could be leader!
          } # Team 1 get punished
          if (  g1.e){ # if the leader was heroic
            g1 = g1[ - c(  1,                                      # Off with his head!!
                           sampleWithoutSurprises ( 2:length(g1)))] # And one other random
            groups[[id1]] = sample(g1) # new leader assigned at random
          } else { # If the leader was exploitative
            groups[[id1]] = g1[ - sampleWithoutSurprises(2:length(g1))]
          }
        }
      }

      # if g1 is hawk and g2 is dove
      if ( g1.hawk &  ! g2.hawk){

      }
    }

    # extinction
    n.per.group = unlist ( lapply ( groups, length) )
    if ( any ( n.per.group < 2)){
      groups = groups[-which ( n.per.group < 2)]
    }
    n.per.group = unlist ( lapply ( groups, length) )


    # Get population back to standard size
    while ( sum ( n.per.group) != pop.size){
      sig = sign ( pop.size  - sum( n.per.group) )
      n.per.group = ifelse( n.per.group <3,0,n.per.group)
      prob = n.per.group / sum( n.per.group) # probability of extra reproduction OR extra deaths are both a function of the group size
      whi = which ( as.vector ( rmultinom(1:length(groups), size = 1 , prob = prob)) == 1)
      if ( sig == 1){ # population size is too small
        rand.repr = groups[[whi]][sampleWithoutSurprises(1:length(groups[[whi]]))] # random reproducer
        groups[[ whi ]] = c(  groups[[ whi ]] , rnormWithMinMax(1,rand.repr,mut.str,0,1)) # replicates into their group, with variance mut.str
      } else { # population size is too large
        rand.deat = sampleWithoutSurprises(1:length(groups[[whi]])) # individual which will die
        if ( rand.deat == 1) { # if this random death is the leader
          groups[[ whi ]] =     groups[[ whi ]] [-1] # The King is Dead!
          groups[[ whi ]] = sample ( groups[[whi]]) # new leader allocated at random
        } else { # otherwise just remove the random soldier
          groups [[ whi]] = groups [[ whi ]] [ -rand.deat]
        }
      }
      n.per.group = unlist ( lapply ( groups, length) )
    }

    #group splits?
    sam = sample ( exps,length( groups))
    spli = apply ( cbind ( n.per.group ,sam  ),1,which.max)
    if ( any ( spli == 1 ) ){
      whi = which ( spli == 1 )
      for ( i in 1:length ( whi)){
        #i=1
        j = whi[i]
        newgroup =  sample ( 2:n.per.group[j] ,round ( runif ( 1 , min = 2, max = floor(n.per.group[j]/2) )) ) # Leader cannot be in rebel faction. Rebel faction cannot be larger than original group
        groups[[length(groups)+1]] = groups[[j]][newgroup] #
        groups[[j]] = groups[[j]][-newgroup] # they will continue to lead their successful group
      }
    }

    #Save data
    # li[[t]] = groups
    n.per.group =  unlist ( lapply ( groups, function(x) c(  length(x)) ))
    heroic.score =  unlist (  lapply ( groups, function(x) c(   x[1]) ))
    dat = rbind ( dat, cbind ( n.per.group, heroic.score , t))

    # plot data
    if ( plott ){
      if ( t %in% seq ( plott.sq, n.generations, plott.sq)){
        d = dat[ seq ( 1, nrow( dat), 1000),]
        plot  ( d[,"heroic.score"] ~ d[,"t"], ylim = c ( 0 , 1) , ylab = "Heroic score", main  = main )
      }
    }
  }
  return ( dat )
}
