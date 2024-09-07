v1 = runif(25)
v2 = runif(25)
mx_v1 = matrix(v1, 5, 5)
mx_v2 = matrix(v2, 5, 5)

col_means_mx_v1 = colMeans(mx_v1)
col_sums_mx_v1 = colSums(mx_v1)
col_means_mx_v2 = colMeans(mx_v2)
col_sums_mx_v2 = colSums(mx_v2)

add_mx = mx_v1 + mx_v2
sub_mx = mx_v1 - mx_v2
div_mx = mx_v1 / mx_v2
mult_mx = mx_v1 %*% mx_v2

rownames(mx_v1) = letters[0:5]
colnames(mx_v1) = letters[0:5]
rownames(mx_v2) = letters[0:5]
colnames(mx_v2) = letters[0:5]

a = mx_v1[0:3, 0:3]
b = mx_v1[,2]
c = mx_v1[4,]

main = matrix(c(4, 3, -1, 5, -2, 4, 1, 2, 0), nrow=3, ncol=3)
ext = matrix(c(4, 3, -1, 7, 5, -2, 4, 10, 1, 2, 0, 12), nrow=3, ncol=4)
n = 3
if(det(main) != 0){
  print('matrix has one solution')
}

det_m = det(main)

a = main
a[,1] <- ext[,4]
x1 = det(a)/det_m
x1

b = main
b[,2] <- ext[,4]
x2 = det(b)/det_m
x2

c = main
c[,3] <- ext[,4]
x3 = det(c)/det_m
x3

solve(main, ext[,4])

mytable = data.frame(
  age = round(runif(10, 0, 100)),
  gender = c('M', 'F', 'M', 'F', 'M', 'F', 'M', 'F', 'M', 'F'),
  disease = c(T, F, T, F, T, F, T, F, T, F)
)
mytable$gender = as.factor(mytable$gender)
mytable
library("xlsx")
write.csv(mytable, file = "mytable.csv")
getwd()

Response2Drug <- read.table('/Users/alex_aredov/Downloads/Response2Drug.txt', row.names = 'Case', header = T, sep='\t')
rownames(Response2Drug) <- paste('Patient ', rownames(Response2Drug))
Response2Drug = Response2Drug[!is.na(Response2Drug$Age),]
Response2Drug
transform(Response2Drug, Mutation = as.logical(Mutation))

summary(Response2Drug[c('Age', 'Response')])

summary(Response2Drug$Response[Response2Drug$Age < 35])
summary(Response2Drug$Response[Response2Drug$Age >35 & Response2Drug$Age < 60])
summary(Response2Drug$Response[Response2Drug$Age > 60])

summary(Response2Drug$Response[Response2Drug$Gender=='M'])
summary(Response2Drug$Response[Response2Drug$Gender=='F'])

summary(Response2Drug$Response[Response2Drug$Mutation==0])
summary(Response2Drug$Response[Response2Drug$Mutation==1])

AdverseReactions <- read.table('/Users/alex_aredov/Downloads/Adverse reactions.txt', header = T, row.names = 'Any.Treatment.Emergent.AE', sep='\t')
AdverseReactions

l = list(Response2Drug$Age, Response2Drug$Gender, AdverseReactions)
l

length(Response2Drug$Age[Response2Drug$Age>50 & Response2Drug$Gender=='M'])

AdverseReactions[AdverseReactions$TIVORBEX.20.mg.three.times.daily > AdverseReactions$Placebo, c('TIVORBEX.20.mg.three.times.daily', 'Placebo')]
AdverseReactions[AdverseReactions$TIVORBEX.40.mg.twice.daily > AdverseReactions$Placebo, c('TIVORBEX.20.mg.three.times.daily', 'Placebo')]
AdverseReactions[AdverseReactions$TIVORBEX.40.mg.three.times.daily > AdverseReactions$Placebo, c('TIVORBEX.20.mg.three.times.daily', 'Placebo')]

AdverseReactions[
  AdverseReactions$TIVORBEX.40.mg.three.times.daily > AdverseReactions$Placebo &
  AdverseReactions$TIVORBEX.20.mg.three.times.daily > AdverseReactions$Placebo &
  AdverseReactions$TIVORBEX.40.mg.twice.daily > AdverseReactions$Placebo, 
  c(
    'TIVORBEX.40.mg.three.times.daily', 
    'TIVORBEX.20.mg.three.times.daily',
    'TIVORBEX.40.mg.twice.daily',
    'Placebo')]







