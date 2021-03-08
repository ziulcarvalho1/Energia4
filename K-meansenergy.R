library(caTools)
library(stringr)
setwd("/AI/energia4/")

data0 <- read.csv("dia-re.csv") ## Items
##data1[3]  <- lapply(data1[3], as.numeric)
data0[4]  <- lapply(data0[4], as.numeric)
data0[5]  <- lapply(data0[5], as.numeric)
data0[6]  <- lapply(data0[6], as.numeric)
data0[7]  <- lapply(data0[7], as.numeric)
data0[8]  <- lapply(data0[8], as.numeric)
data0[9]  <- lapply(data0[9], as.numeric)
data0[13] <- lapply(data0[13], as.numeric)
data0[14] <- lapply(data0[14], as.numeric)

var4  <- summary(data0[4])
var5  <- summary(data0[5])
var6  <- summary(data0[6])
var7  <- summary(data0[7])
var8  <- summary(data0[8])
var9  <- summary(data0[9])
var13  <- summary(data0[13])
var41 <- as.numeric(substr(var4[6],9,14))
var51 <- as.numeric(substr(var5[6],9,14))
var61 <- as.numeric(substr(var6[6],9,14))
var71 <- as.numeric(substr(var7[6],9,14))
var81 <- as.numeric(substr(var8[6],9,14))
var91 <- as.numeric(substr(var9[6],9,14))
var131 <- as.numeric(substr(var13[6],9,14))

print(var41)
print(var51)
print(var61)
print(var71)
print(var81)
print(var91)
print(var131)



print(var4[6])
class(var41)
class(data0)
class(var[6])
data0[4]  <- (data0[4])/var41
data0[5]  <- (data0[5])/var51
data0[6]  <- (data0[6])/var61
data0[7]  <- (data0[7])/var71
data0[8]  <- (data0[8])/var81
data0[9]  <- (data0[9])/var91
data0[13] <- (data0[13])/var131

print(data0)
##data0[14] <- scale(data0[14])

data_sample = sample.split(data0,SplitRatio=0.95) ##here we separate the file to be the nodes
data1 = subset(data0,data_sample==TRUE)  ##trainning data
data5 = subset(data0,data_sample==FALSE) ##test data


## Making sure all are numeric



print(data1)
data4 <- cbind(data1[4],data1[5],data1[6],data1[7],data1[8],data1[9],data1[13]) ## file adjusted to run k-means
##print(data3)

valor <- kmeans(data4, 150, iter.max = 150, nstart = 10,algorithm = c("Hartigan-Wong",
                                                                      "Lloyd",
                                                                      "Forgy",
                                                                      "MacQueen"), trace=FALSE)
# S3 method for kmeans

## cluster centers "fitted" to each obs.:
print(valor$centers)
data2 <- valor$centers ##  nodes
print(data2)
print(data2[2,1])
## Generates a vector indicating the node associated to each row
data3 <- fitted(valor, method = c("classes"))
print(data3)
print(data3[1])
str(data3)
class(data3)


### Here we add the node as a column in the dataset (ssociate a node to each record in the trainning data)
for (i in 1:nrow(data1)){
  data1[i,16]<-data3[i]
}
print(data1)
## Here we convert the Kmeans format into matrix adding the necessary columns
m <- matrix(0:0, nrow = nrow(data2), ncol = 12)
print(m)
str(m)
print(data2[,])
for (ix in 1:nrow(data2)){
  for (i in 1:7){
    ##print(data3[ix,i])
    m[ix,i]<-data2[ix,i]
  }
  m[ix,8]<-ix
}

print(m) 
data2 <- m ## Data2 is the nodes matrix where we are going to place positive and negative for reach and sell

for (i in 1:nrow(data1)){  ## All trainning data records
  
  for (ix in 1:nrow(data2)){  ## all nodes 
    
    if (data1[i,16]==data2[ix,8]){ ## if the node is the one associates with the item
      

      data2[ix,10] <-data2[ix,10]+data1[i,14]
      data2[ix,9] <- data2[ix,9]+1
      data2[ix,11]<-data2[ix,10]/data2[ix,9]
      ##print(ix)
    } ## if the node is the one associates with the item
  }  ## all nodes
  print(i)
} ## all trainning data records

print(data1)
print(data2)
write.csv(data2,"/AI/energia4/clustered.csv")
write.csv(data5,"/AI/energia4/test.csv")
str(data3)
str(data1)
str(data5)
#########Calcula o preço
len1 <- nrow(data5)
print(len1)
print(data5)
contador1 <- 1
page <- 100
contador0 <- 0 ## paginacao

while (contador1<=len1){ ## Loop items test set
  
  ##print(contador1) 
  
  
  
  
  if (contador0 == page){
    page = page+10
    print(contador1)
    print(Sys.time())
  }
  contador0=contador0+1
  
  tprec1   <- data5[contador1,4]
  ttmax1   <- data5[contador1,5]
  ttmin1   <- data5[contador1,6]
  tsol1    <- data5[contador1,7]
  ttmed1   <- data5[contador1,8]
  ttumid1  <- data5[contador1,9]
  ttena1   <- data5[contador1,13]

  
  
  
  
  menor <- 10000000
  len2 <- nrow(data2)
  ##print(len2)
  contador2 <- 1
  print("Inicio ciclo cluster")
  while (contador2 <= len2){ ## nodes
    ##print(contador2)
    tprec2   <- data2[contador2,1]
    ttmax2   <- data2[contador2,2]
    ttmin2   <- data2[contador2,3]
    tsol2    <- data2[contador2,4]
    ttmed2   <- data2[contador2,5]
    ttumid2  <- data2[contador2,6]
    ttena2   <- data2[contador2,7]
    
    ## Subtraction section 
    
    
    tprec3     <-  tprec1      - tprec2
    ttmax3     <-  ttmax1      - ttmax2
    ttmin3     <-  ttmin1      - ttmin2
    tsol3      <-  tsol1       - tsol2
    ttmed3     <-  ttmed1      - ttmed2
    ttumid3    <-  ttumid1     - ttumid2
    ttena3     <-  ttena1      - ttena2

    
    quadrado <- ((tprec3**2)+(ttmax3**2)+(ttmin3**2)+(tsol3**2)+(ttmed3**2)+(ttumid3**2)+(ttena3**2))
    
    d <- (quadrado**1/2)
    
    if (d<menor & d!=0){ ## selection of the best cluster (small distance)
      registro1 <- data2[contador2,8] 
      registro  <- contador2
      menor <- d
      print("menor")
      print(menor)
      print(registro)
      print(registro1)
    }
    
    contador2 <- contador2 + 1
  } ## end loop contador 2
  
  data2[registro,12]  <- data2[registro,12]+1
  data5[contador1,11] <- data2[registro,11] ## Preço PLD
  
  data5[contador1,12] <- registro
  contador1 <- contador1+1
  ##print(contador1)
  
} ## end loop items contador1



write.csv(data5,"/AI/energia4/classified.csv")