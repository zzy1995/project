
data.bin=read.table('binary-ling-data.data',header=TRUE)
ling.responses <- data.bin[,7:ncol(data.bin)]



## PCA

pca=prcomp(data.bin[,7:ncol(data.bin)],scale=TRUE,center=TRUE)


## PCA
##### PCA summary
pca.summary = summary(pca)

##### PC1-PC6 significant

pc1=pca$rotation[,1]
pc2=pca$rotation[,2]
pc3=pca$rotation[,3]
pc4=pca$rotation[,4]
pc5=pca$rotation[,5]
pc6=pca$rotation[,6]

PCs=list(pc1,pc2,pc3,pc4,pc5,pc6)

sort.PCs=lapply(1:6,function(i) sort(abs(PCs[[i]]),decreasing=TRUE))

# find the questions that matter
p=list()
for(i in 1:6){
p[[i]]=unique(unname(sapply(names(which(sort.PCs[[i]]>0.1)),function(x) substr(x,1,4))))
}

# Count the numbers that questions appear
table(unlist(p))

# questions concerned
q.concerned=names(which(table(unlist(p))>2))

# column numbers

data.origin=read.table("ling-data-clean.data",header=TRUE)
origin=colnames(data.origin[,c(-1,-2,-3,-4)])
origin.num=match(q.concerned,origin)
n.responses=apply(data.origin[,5:71],2,max)
colnum=vector()

start=paste(q.concerned,"1",sep='.')
col.start=match(start,colnames(data.bin))
for(i in 1:length(origin.num)){
colnum=c(colnum,(col.start[i]:(col.start[i]+n.responses[origin.num[i]]-1)))
}


# colnum after PCA

pca.r <- data.bin[,colnum]
