# Compare between answers to some questions
# 65 66 76

col.num.s=c(114:127,182:195)

data.bin=read.table('binary-ling-data.data',header=TRUE)

# take subset of these three questions

data.subset=data.bin[,col.num.s]

# answers for three questions
num=c(6,7,14)

# 65&66

count.6566=mat.or.vec(6,7)
for(i in 1:6){
	for(j in 1:7){
		count.6566[i,j]=sum(data.subset[,i] & data.subset[,6+j])
	}
}

# 65&76

count.6576=mat.or.vec(6,14)
for(i in 1:6){
	for(j in 1:14){
		count.6576[i,j]=sum(data.subset[,i] & data.subset[,13+j])
	}
}

# 66&76

count.6676=mat.or.vec(7,14)
for(i in 1:7){
	for(j in 1:14){
		count.6676[i,j]=sum(data.subset[,i+6] & data.subset[,13+j])
	}
}

# count conditional probability

condition.probs=function(count,rc=1){
	prob=mat.or.vec(nrow(count),ncol(count))
	if(rc==1){
		for(i in 1:nrow(count)){
			for(j in 1:ncol(count)){
				prob[i,j]=count[i,j]/sum(count[i,])
			}
		}
	}else{
		for(i in 1:nrow(count)){
			for(j in 1:ncol(count)){
				prob[i,j]=count[i,j]/sum(count[,j])
			}
		}
	}
	return(prob)
}
# 1 represents given row(former) 2 represents given column(latter)

prob=list()
prob$row6566=condition.probs(count.6566,1)
prob$col6566=condition.probs(count.6566,2)
prob$row6576=condition.probs(count.6576,1)
prob$col6576=condition.probs(count.6576,2)
prob$row6676=condition.probs(count.6676,1)
prob$col6676=condition.probs(count.6676,2)

# prob contains all the conditional probabilities
prob