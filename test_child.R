library("bnstruct")

# read data and set parameters
B <- 100
a <- read.delim("Child_data_na_5000.txt",na.strings="?",header=FALSE,sep="") + 1

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
  0  0  0  0 19")) # only used by SM
max.fanin  <- 3 # threshold for the values of max.fanin.layers

# impute data 
impa <- knn.impute(as.matrix(a),k.impute,setdiff(1:length(node.sizes),cont.nodes))

# test SM alone
res.sm <- sm(impa,node.sizes,cont.nodes,max.fanin,layering,max.fanin.layers)

# plot, requires package Rgraphviz
setEPS()
postscript("sm.eps")
plot.mat(dag.to.cpdag(res.sm),names(a))
dev.off()

# test SM with bootstrap
res.sm.boot <- boot.bn(as.matrix(a), node.sizes, B, cont.nodes, 
                       verbose=TRUE, k.impute=10, method = "sm",
                       max.fanin = max.fanin, layering = layering,
                       max.fanin.layers = max.fanin.layers )

# plot
postscript("sm.boot.eps")
plot.mat(res.sm.boot,names(a),0.25,B)
dev.off()

# test MMHC alone
cpc <- mmpc( impa, node.sizes, cont.nodes, 0.05, layering )
res.mmhc <- hc( impa, node.sizes, cpc, cont.nodes )

# plot
postscript("mmhc.eps")
plot.mat(dag.to.cpdag(res.mmhc),names(a))
dev.off()

# test MMHC with bootstrap
res.mmhc.boot <- boot.bn(as.matrix(a), node.sizes, B, cont.nodes, 
                         verbose=TRUE, k.impute=10, method = "mmhc",
                         layering=layering)

# plot
postscript("mmhc.boot.eps")
plot.mat(res.mmhc.boot,names(a),0.25,B)
dev.off()
