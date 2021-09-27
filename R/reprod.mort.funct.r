reprod.mort.funct = function(
  group = NULL,
  reprod.mort = NULL,
  win = NULL,
  C = NULL,
  V = NULL,
  skew = NULL,
  mut.str = NULL,
  leader.vulnerability= NULL,
  follower.contribution = NULL,
  group.fight.strength = NULL,
  group.leader.survives = NULL,
  pyr = 0
){

  # Variables

  # group =  g1
  # reprod.mort = "mort"
  # win = win1
  # C = C
  # pyr = pyr
  # follower.contribution = follower.contribution.g1
  # leader.vulnerability = leader.vulnerability.g1
  # group.fight.strength = g1.str



  if ( reprod.mort == "mort"){
    if ( win ){
      #how many die?

      rbi=rbinom (1,1, prob=(C*pyr + C )- floor ( C*pyr + C) )
      morts = ifelse ( nrow( group) <= floor(C*pyr)+rbi+1, nrow ( group) , floor(pyr * C) +rbi )

    } else {
      morts =  ifelse ( nrow( group) <= C+1, nrow ( group) , C)
    }
    if ( morts != nrow(group)){
      group.vulnerability = follower.contribution + leader.vulnerability
      if (  group.vulnerability == 0 ){ # If they are all selfish fucks
        probs = rep (1/ nrow( group ) , nrow(group)) # they all have equal chance of dying
      } else {
        leader.prob = leader.vulnerability  /  group.vulnerability
        follower.probs = (group[-1,"folfight"]) / group.vulnerability
        probs = c( leader.prob , follower.probs)
      }

      # # # # work
      # probs = runif (  4 , 0 , 1)
      # morts = 3
      # group = matrix ( NA, nrow =  length ( probs))
      # probs = probs / sum ( probs)
      # # #

      sum ( probs)
      deaths = c()
      if ( morts != 0){
        for (  j in 1:morts){
          if (sum(probs) == 0){
            probs = rep(1/length(probs), length(probs))
          } else {
            probs = probs / sum( probs)
          }
          death = which (  as.logical (as.vector( rmultinom(1:nrow(group),size = 1, prob = probs))))
          probs [  death ]  = 0
          deaths = c( deaths , death)
        }
      }
      group.leader.survives = ifelse ( any( deaths ==1 ), F, T)
      if ( ! is.null ( deaths) ){
        group = group[-deaths,]
      }
      if ( ! group.leader.survives ){
        group = group [ sample ( 1:nrow(group)), ]
      }
    }  else {
      group = NA
      group.leader.survives = F
    }
    return (
      list ( group.leader.survives = group.leader.survives,
             group = group)
    )
  }
  if ( reprod.mort == "reprod"){
    if ( win){
      if ( group.leader.survives){
        leader.prob = ( skew ) / ( (nrow(group)-1 ) + skew  )
        follower.probs = rep ((1-leader.prob)/ (nrow(group)-1) , nrow(group)-1)
        probs = c( leader.prob , follower.probs)
      } else {
        probs = rep ( 1 /  nrow(group), nrow (group))
      }
      reprods = apply (  replicate (V, rmultinom(1:nrow(group), size =  1,prob = probs)),1,rbind)
      newbies = t(apply ( reprods , 1 , function (x) {
        rnormWithMinMax(3, m = group[which ( x == 1),], s = mut.str, lwr = 0, upr = 1)
      }))
      group = rbind ( group , newbies)
      return(list (   group = group ))
    } else {
      return ( list ( group = group))
    }
  }


}
