library("bnstruct")

# read data and set parameters
B <- 10
a <- read.delim("Child_data_na_5000.txt",na.strings="?",header=FALSE,sep="") + 1
node.sizes <- rep(0,ncol(a))
for( i in 1:ncol(a) )
  node.sizes[i] <- length(unique(a[,i]))

node.sizes <- c(2,6,3,2,3,4,3,3,2,2,3,3,5,2,2,3,3,2,5,2)
names(a) <- list("BirthAsphyxia","Disease","Age","LVH","DuctFlow","CardiacMixing",
                "LungParench","LungFlow","Sick","HypDistrib","HypoxiaInO2","CO2",
                "ChestXray","Grunting","LVHReport","LowerBodyO2","RUQO2","CO2Report",
                "XrayReport","GruntingReport")
cont.nodes <- c()
k.impute <- 10
layering   <- c(1,2,3,3,3,3,3,3,3,4,4,4,4,4,4,5,5,5,5,5)
max.fanin.layers <- as.matrix(read.table(header=F,text="
  0  1  1  1  1
  0  1  1  1  1
  0  0  8  7  7
  0  0  0 14  6
  0  0  0  0 19"))
max.fanin  <- 3 # threshold for the values of max.fanin.layers

# impute data 
impa <- knn.impute(as.matrix(a),k.impute,setdiff(1:length(node.sizes),cont.nodes))

# test SM alone
res.single <- sm(impa,node.sizes,cont.nodes,max.fanin,layering,max.fanin.layers)

# test SM with bootstrap
res.boot <- boot.bn(as.matrix(a), node.sizes, B, cont.nodes, max.fanin, layering,
               max.fanin.layers,iss=1, verbose=TRUE, k.impute=10)

# test SM with MMPC
cpc <- mmpc( impa, node.sizes, cont.nodes, 0.05 )
res.cpc <- sm(impa,node.sizes,cont.nodes,max.fanin,layering,max.fanin.layers,cpc.mat=cpc)

# test SM with MMPC and bootstrap
max.fanin <- 19
Rprof()
res.boot.cpc <- boot.bn(as.matrix(a), node.sizes, B, cont.nodes, max.fanin, layering,
               	max.fanin.layers,iss=1, verbose=TRUE, k.impute=10, chi.th = 0.05 ) 
Rprof(NULL)

# test HC with MMPC
res.hc.cpc <- hc(impa,node.sizes,cpc)
