
vars =
  rbind(
    expand.grid(max, n.gr, rc, skew, pyr,C ,mig,mut),
    expand.grid(Max, n.gr, rc, skew, pyr,C ,mig,mut),
    expand.grid(max, N.gr, rc, skew, pyr,C ,mig,mut),
    expand.grid(max, n.gr, Rc, skew, pyr,C ,mig,mut),
    expand.grid(max, n.gr, rc, skew, pyr,C ,mig,mut),
    expand.grid(max, n.gr, rc, Skew, pyr,C ,mig,mut),
    expand.grid(max, n.gr, rc, skew, Pyr,Cs,mig,mut),
    expand.grid(max, n.gr, rc, skew, pyr,C ,Mig,mut),
    expand.grid(max, n.gr, rc, skew, pyr,C ,mig,Mut)
  )

vars  = do.call  ("rbind" ,replicate (n.iter ,   vars,simplify = F ))



names ( vars) = c ( "max",  "n.gr", "rc",  "skew",  "pyr",  "C", "mig",  "mut")

cei = ceiling ( nrow  ( vars ) / max.iters.per.tab)
re = rep ( NA , cei * max.iters.per.tab)

sams = NULL
maxsums = NULL
for ( j in 1:sampletrialnum){

  sam = sample ( 1:nrow(vars) , replace = F)
  sams = rbind( sams , sam)
  re [ 1:length(sam)] = sam
  mat = matrix ( re, max.iters.per.tab , cei)
  sums = c()
  for ( i  in 1:cei){
    v = vars [ mat [,i],]
    v = v [ complete.cases( v) , ]
    assign( paste0 ( "vars", i), v)
    sums =c (sums , ( sum ( v[,"n.gr"])))
  }
  maxsums = c ( maxsums , max ( sums ))
}

sam = sams [ which ( maxsums == min ( maxsums))[1] ,]
re [ 1:length(sam)] = sam
mat = matrix ( re, max.iters.per.tab , cei)
sums = c()
for ( i  in 1:cei){
  v = vars [ mat [,i],]
  v = v [ complete.cases( v) , ]
  print ( v)
  assign( paste0 ( "vars", i), v)
  sums =c (sums , ( sum ( v[,"n.gr"])))
}

print (cei)
vars = vars4
