
skip.to.end = T
if ( ! skip.to.end ){
rm( list = ls())
fold = file.path  (PROJHOME , "Output" , "SupplementaryAnalyses", "AllFollower-analyses")

list.files( fold)

load ( file.path ( fold,"max-70-n.gr-200-rc-2-skew-2-pyr-0.15-C-6-mig-0.02-mut-0.01_2021-07-27_10-48.rda"))

str(dat2)

dat = dat2$data

dat = as.data.frame ( dat)
dat2 = dat
dat2 = dat2 [ which ( dat$t == 15000)[1]:nrow(dat),]
dat3 = dat2[ sample ( 1:nrow(dat2),7000),]
plot ( dat3$folfight)
mean(dat3$folfight)
save( dat3 , file = file.path ( PROJHOME , "Output" , "SupplementaryAnalyses" , "allfollower.rda"))
length ( unique( dat2$t))
length ( unique( dat2$group.id))

dat2 = dat2 [ dat2$class.l.f == 2 , ]

tap = tapply ( dat2$folfight , list ( dat2$group.id,
                          dat2$t
                          ), "mean")

dat = dat [ dat$class.l.f == 2,]

xs = NULL
m=1
sq = round ( seq( 15000, 20000, length.out = 100))
for ( i in 15000:20000 ){
  datx = dat [ dat$t == i,]
  x = tapply (datx$folfight , datx$group.id, mean)
  names(x )  = NULL
  xs = rbind (  xs, cbind ( x,i) )
  if ( i %in% sq){
    print ( paste (  m , "%"))
    m=m+1
  }
}

plot(xs[,1])
save( xs , file = file.path ( PROJHOME , "output", "SupplementaryAnalyses" , "allfollower.comb.rda" ) )
load( file.path ( PROJHOME , "output", "SupplementaryAnalyses", "allfollower.comb.rda" ) )
x2 = xs [ sample ( 1:nrow(xs),5000),]
plot ( x2[,1] ~ x2[,2])

l.o. = 15
sq = seq( 1, nrow( dat) , length.out = l.o.)
sq2 = sq-1
sqs = rbind ( sq [1:(l.o.-1)],
              sq2[2:l.o.    ])
for ( i in 1:(l.o.-1)){
  #i=1
  sq = sqs[,i]
  datx = dat [  sq[1]:sq[2],]
tap = tapply ( datx$folfight , list ( datx$group.id,
                                      datx$t ), "mean")
str(tap)
}
head(dat)

data = dat [ sample( 1:nrow(dat),10000),]

plot ( data[,3])
mean ( data[,2])




}

#######################

{

par ( mfrow = c(1,2))

rm(list = ls())

load( file.path ( PROJHOME , "output", "SupplementaryAnalyses", "allfollower.rda" ) )
plot ( dat3$folfight ~ dat3$t , main = "Individual score" , xlab = "Nth round", ylab = "Follower contribution")
mean ( dat3$folfight)
mtext("A", side = 3, line = 1, adj = -0.2, cex = 2.0)

load( file.path ( PROJHOME , "output", "SupplementaryAnalyses", "allfollower.comb.rda" ) )
x2 = xs [ sample ( 1:nrow(xs),5000),]
plot ( x2[,1] ~ x2[,2], xlab = "Nth round", main = "Group score",ylab = "Follower contribution")
mean ( x2[,1])
mtext("B", side = 3, line = 1, adj = -0.2, cex = 2.0)
}


#######################



