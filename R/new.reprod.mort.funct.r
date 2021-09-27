new.reprod.mort.funct = function(
  group = NULL,
  reprod.mort = NULL,
  C = NULL,
  V = NULL,
  skew = NULL,
  mut.str = NULL,
  leader.vulnerability= NULL,
  group.fight.strength = NULL,
  group.leader.survives = NULL
){

  # Variables

  # group =  groups[[i]]$members
  # reprod.mort = "reprod"
  # C = C
  # leader.vulnerability = 0
  # group.fight.strength = g2.str


  if ( reprod.mort == "mort"){
      morts = ifelse ( nrow( group) <= C+1, nrow ( group) , C)
    if ( morts != nrow(group)){
      leader.prob = leader.vulnerability  /  group.fight.strength
      follower.probs = (group[-1,"folfight"]) / group.fight.strength
      probs = c( leader.prob , follower.probs)
      deaths = c()
      for (  j in 1:morts){
        death = which (  as.logical (as.vector( rmultinom(1:nrow(group),size = 1, prob = probs))))
        probs [  death ]  = 0
        if (sum(probs) == 0){
          probs = rep(1/length(probs), length(probs))
        } else
        probs = probs / sum( probs)
        deaths = c( deaths , death)
      }
      group.leader.survives = ifelse ( any( deaths ==1 ), F, T)
      group = group[-deaths,]
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
