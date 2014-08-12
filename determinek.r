library(fpc)

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






## determine number of k
##### Running 2000 samples 4 times

par(mfrow=c(2,2), mar=0.1+c(4,4,1,1), oma=c(0,0,2,0))
k.replicate = 4
samp.size = 2000
replicate(k.replicate,{
sample <- list()
sample$idcs <- sample(1:nrow(data.bin), samp.size)
sample$responses  <- ling.responses[sample$idcs,]
r = sample$responses
d = dist (r)
ks = 2:8
ASW <- sapply(ks, FUN=function(k) {
			cluster.stats(d, kmeans(r, centers=k, nstart=5)$cluster)$avg.silwidth
			})
			
plot(ks, ASW, type="l")
title("Running 2000 samples 4 times", outer=TRUE) 
})



## determine number of clusters 
### 2000,3000,4000,5000 samples

mapply(function(s.s,title1) {
sample <- list()
samp.size <- s.s
sample$idcs <- sample(1:nrow(data.bin), samp.size)
sample$responses  <- ling.responses[sample$idcs,]
r = sample$responses
d = dist (r)
ks = 2:8
ASW <- sapply(ks, FUN=function(k) {
			cluster.stats(d, kmeans(r, centers=k, nstart=5)$cluster)$avg.silwidth
			})
			
plot(ks, ASW, type="l",main=title1)
    }, c(2000,3000,4000,5000),c('2000 samples', '3000 samples', '4000 samples', '5000 samples')) 
    









## determine number of clusters after PCA
##### Running 2000 samples 4 times



par(mfrow=c(2,2), mar=0.1+c(4,4,1,1), oma=c(0,0,2,0))
k.replicate = 4
samp.size <- 2000

replicate(k.replicate,{
sample <- list()
sample$idcs <- sample(1:nrow(data.bin), samp.size)
sample$responses  <- pca.r[sample$idcs,]
r = sample$responses
d = dist (r)
ks = 2:8
ASW <- sapply(ks, FUN=function(k) {
			cluster.stats(d, kmeans(r, centers=k, nstart=5)$cluster)$avg.silwidth
			})
			
plot(ks, ASW, type="l")
title("Running 2000 samples 4 times", outer=TRUE) 
})

## determine number of clusters after PCA
### 2000,3000,4000,5000 samples

mapply(function(s.s,title1) {
sample <- list()
samp.size <- s.s
sample$idcs <- sample(1:nrow(data.bin), samp.size)
sample$responses  <- pca.r[sample$idcs,]
r = sample$responses
d = dist (r)
ks = 2:8
ASW <- sapply(ks, FUN=function(k) {
			cluster.stats(d, kmeans(r, centers=k, nstart=5)$cluster)$avg.silwidth
			})
			
plot(ks, ASW, type="l",main=title1)
    }, c(2000,3000,4000,5000),c('2000 samples', '3000 samples', '4000 samples', '5000 samples')) 
    

