library(RUnit)
errMsg <- function(err) print(err)
load('reformatting-tests.rda')

# Implement the makeBinary function.
# args:
# <response.row>: a vector of integers giving the response values for each
#   question 
# <n.responses>: a vector of integers (same length as <response.row>)
#   indicating the number of possible responses for each question
#
# returns:
# a binary vector that reformats the responses of <response.row> as
# described in project1.pdf

makeBinary <- function(response.row, n.responses) {

	if(length(response.row)!=length(n.responses)) stop('unesumqual lengths')
	x=rep(0,sum(n.responses))
	for(i in 1:length(response.row)){
		if(response.row[i]!=0){
			if(i==1){
				x[response.row[i]]=1
			} else{
				x[sum(n.responses[1:(i-1)])+response.row[i]]=1
				}
		}
	}
	return(x)
}


tryCatch(checkEquals(make.binary.test1, makeBinary(make.binary.rr1,
                                                   make.binary.nr))
         ,error=function(err) errMsg(err))

tryCatch(checkEquals(make.binary.test2, makeBinary(make.binary.rr2,
                                                   make.binary.nr))
         ,error=function(err) errMsg(err))

# use your "makeBinary" function to reformat your "ling-data-clean.data"
# dataset. Store this as a dataframe with variable names and order **as
# indicated in project1.pdf**. Save this dataframe as the file
# "binary-ling-data.data".
    
data.subset=read.table("ling-data-clean.data",header=TRUE)
data.num=data.subset[,5:71]
n.responses=apply(data.subset[,5:71],2,max)
data.bin=t(apply(data.num,1,makeBinary,n.responses=n.responses))
col.names=unlist(sapply(1:length(n.responses),function(x) paste(rep(names(n.responses)[x],n.responses[x]),1:n.responses[x],sep='.')))
colnames(data.bin)=col.names
final=data.frame(data.subset[,c(1,2,3,4,72,73)],data.bin)

write.table(final, file="binary-ling-data.data", row.names=F)

final.data=read.table('binary-ling-data.data',header=TRUE)

tryCatch(checkEquals(head.binary.data,final.data[1:10,]),error=function(err) errMsg(err))
