comb = function(n, x) {
  return(factorial(n) / (factorial(x) * factorial(n-x)))
}

comb2 = function(n) {
  return(n*(n-1) /2 )
}
#Why this 
# comb = function(n, x) {
# 	if (n<x) {return 0}
#   return(factorial(n) / (factorial(x) * factorial(n-x)))
# }

number_clusters = 5
number_patitions = 3

for (m in seq(2,6)){
p <-rep(0,number_patitions*number_clusters)
dim(p)<- c(number_patitions,number_clusters)



for (i in seq(0,number_patitions-1)) {
	for (j in seq(0,number_clusters-1)) {
		p[i+1,j+1] = dim(par[which(par[,1]==i & par[,m]==j),])[1]/300
	}


}

TP = 0
FN = 0
FP = 0

for (i in seq(1,number_patitions)) {
	if(sum(p[i,])*300 >=2){FN = FN + comb2(sum(p[i,])*300)}
	
	for (j in seq(1,number_clusters)) {
		if(p[i,j]*300 >=2){TP = TP + comb2(p[i,j]*300)}
	}
}

for (m in seq(1,number_clusters)) {
	if(sum(p[,i])*300 >=2){FP = FP + comb2(sum(300*p[,m]))}
}

FN = FN - TP
FP = FP - TP
# print(TP)
# print(FN)
# print(FP)

I = 0
Ht = 0
Hc = 0
for (i in seq(1,number_patitions)) {
	if (sum(p[i,])!=0) {Ht = Ht - sum(p[i,])*log(sum(p[i,]),2)}
	for (j in seq(1,number_clusters)) {
		if (p[i,j]){
		I = I + p[i,j]*log(p[i,j]/(sum(p[i,])*sum(p[,j])),2)
		}
	}

}

for (k in seq(1,number_clusters)) {
	if (sum(p[,k])!=0) {Hc = Hc - sum(p[,k])*log(sum(p[,k]),2)}
}


print(I)
print(Ht)
print(Hc)
cat(paste(m-1,"NMI:"))
print(I/sqrt(Ht*Hc))
print(TP/(TP+FN+FP))
print(p*300)
}





