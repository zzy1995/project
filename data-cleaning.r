# create a subset of the data in lingData.txt where all observations that
# omitted every question have been removed. Store the **number** of
# observations that you omitted as the variable <n.no.response>

data=read.table('lingdata.txt',header=TRUE)
idcs.nr=apply(sapply(data,function(x) x==0),1,function(x) sum(x,na.rm=TRUE)==67)
# n.no.response <- your code here
n.no.response=sum(idcs.nr)
data.subset=data[!idcs.nr,]

# plot a histogram of the number of omitted responses for each observation
# after removing observations that omitted all questions

n.omitted=apply(data.subset[,5:71]==0,1,function(x) sum(x,na.rm=TRUE))
hist(n.omitted,main="Number Omitted Questions",xlab="questions omitted")

# using your subset (with observations that responded to no questions
# removed) find the 99th percentile cutoff for number of questions
# omitted. Remove all observations with more than this number of omitted
# questions.

cutoff=quantile(n.omitted,0.99)
idcs.cutoff=n.omitted>cutoff
data.cutoff=data.subset[!idcs.cutoff,]
# save the subset of remaining observations in a file named
# "ling-data-clean.data" 

write.table(data.cutoff, file="ling-data-clean.data", row.names=F)
