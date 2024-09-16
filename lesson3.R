Response2Drug <- read.table('/Users/alex_aredov/Downloads/Response2Drug.txt', row.names = 'Case', header = T, sep='\t')
Response2Drug = Response2Drug[!is.na(Response2Drug$Age),]
Response2Drug['Age']
Response2Drug['Age'] = ifelse(Response2Drug['Age'] < 66, ifelse(Response2Drug['Age'] < 33, 'young', 'middle'), 'old')
write.csv(Response2Drug, file = "mytable.csv")

ff<-function(x) {
  if(x > 0){
    ifelse((x - 2) <= 0, return(x), return(ff(x-2)*x))
  } else {
    return(NA)
  }
  return(0)
}
ff(6)
ff(7)
ff(0)
ff(-2)

v = runif(25, -10, 50)
m = matrix(v, 5, 5)
col_means_m = colMeans(m)
col_sums_m = colSums(m)
row_means_m = rowMeans(m)
row_sums_m = rowSums(m)

logint = function(a,b,base=2) {
  h = (b - a)/10
  s = 0
  for(i in seq(a, b, by=h)){
    s = s + (1/log(i, base))
  }
  return(h*s)
}
logint(2,4,exp(1))




