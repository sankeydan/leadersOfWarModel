rm ( list = ls())
load(file.path(PROJHOME , "Plots_Videos", "plotData" , "P17.rda"))
i=3
dfs = NULL
for ( i in 1:length(li4)){
li  = li4[[i]]
j=1
for ( j in 1:length(li)){
li2 = li[[j]]
data = li2$data
if(!is.null(data)){
data = data [ data[,"t"]  >10000,]
df = data.frame (grpsize = data[,"n.per.group"],
dh.score = data[,"leader.dovehawk"],
i = i,
j = j)
dfs = rbind (dfs , df)
}
}
}

dfs$max = NULL
dfs$max [ dfs$i == 1] = "50"
dfs$max [ dfs$i == 2] = "70"
dfs$max [ dfs$i == 3] = "90"
library(ggplot2)
ggplot ( dfs , aes ( x=grpsize , dh.score,color =max ))+
  geom_smooth()+
  ylab ( expression(paste ( italic(DH), " score")))+
  xlab ( "Number of individuals in group")
