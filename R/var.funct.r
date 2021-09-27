var.funct = function  ( filesr){
  # filesr = files2
vars =   stringr::str_split_fixed ( filesr , "-",  16)
data.frame ( inf = vars[,2],
n.g = as.numeric ( vars[,4]),
siz = as.numeric ( vars[,6]),
unc = vars[,8],
max = vars[,10],
mut = vars[,12])
}
