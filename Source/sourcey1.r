vars2 = var.funct( files2)
files2 = files2 [order ( vars2 [, 1 ],
                         vars2 [, 2 ],
                         vars2 [, 3 ])]
vars2 = var.funct(files2)
vars2 = vars2[, -which ( names (vars2) %in% c( "unc", "max" , "mut"))]

# load data and combine
li = list()
for ( i in 1:length ( files2)){
  load ( file.path (  folder , files2[i]))
  li[[i]] = dat
  print ( paste ( i , "/" , length(files2)))
}
