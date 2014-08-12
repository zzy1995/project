library(randomForest)

data.bin=read.table('binary-ling-data.data',header=TRUE)
ling.responses <- data.bin[,7:ncol(data.bin)]


data.bin.n= na.omit(data.bin)
ling.d <- data.bin.n[,-(c(1:4))]

diff <- function(size){
	samp=sample(1:nrow(data.bin.n),size)
	train.lat= ling.d[samp, -2]
	test.lat= ling.d[-samp, -2]
	train.long= ling.d[samp, -1]
	test.long = ling.d[-samp, -1]
	p.rf.lat= randomForest(lat~.,data= train.lat,ntree=100)
	pred.rf.lat=predict(p.rf.lat,test.lat)
	p.rf.long= randomForest(long~.,data= train.long,ntree=100)
	pred.rf.long=predict(p.rf.long,test.long)
	return(list(diff.lat=mean(abs(pred.rf.lat-test.lat[,1])),diff.long=mean(abs(pred.rf.long-test.long[,2]))))
}

result=replicate(100,diff(1000))
par(mfrow=c(1,2))
hist(unlist(result[1,]),main='latitude')
hist(unlist(result[2,]),main='longitude')