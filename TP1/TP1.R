
setwd("~/Dropbox/ITBA/Tecnicas_Algoritmos_Aprendizaje_Automatico/machine_learning_algorithms/TP1")
# Leo el set de datos de Optidigits
base=read.table("dataset-optical/optdigits.tra",sep=",")

# Ejemplo de una variable
head(base,1)

dim(base)

library(jpeg) #cargamos la libreria
vector=base[14,] #tomamos la fila 14 por separado
vector #visualizamos que la fila sigue siendo un data.frame con títulos de columnas
#visualice en la columna V65 qué número se va a dibujar
vector=vector[-65] #sacamos la columna V65 (variable a predecir – valor5)
vector=as.numeric(vector) #transformamos el data.frame a vector numérico
vector #visualizamos que la fila ahora es un vector sin títulos de columnas
vector=vector/16 #transformamos los valores de (1:16) a (0:1)
vector
imagen=array(vector,dim=c(8,8)) #creamos la imagen de 8x8
imagen=t(imagen) #rotamos la imagen
plot(as.raster(imagen))

inverted_imagen = 1 - imagen
plot(as.raster(inverted_imagen))

base[77,65]

vector=base[77,] #tomamos la fila 14 por separado
vector #visualizamos que la fila sigue siendo un data.frame con títulos de columnas
#visualice en la columna V65 qué número se va a dibujar
vector=vector[-65] #sacamos la columna V65 (variable a predecir – valor5)
vector=as.numeric(vector) #transformamos el data.frame a vector numérico
vector #visualizamos que la fila ahora es un vector sin títulos de columnas
vector=vector/16 #transformamos los valores de (1:16) a (0:1)
vector
imagen=array(vector,dim=c(8,8)) #creamos la imagen de 8x8
imagen=t(imagen) #rotamos la imagen
plot(as.raster(imagen))

inverted_imagen = 1 - imagen
plot(as.raster(inverted_imagen))


for (i in 1:64){
  var=paste("V",i,sep="")
  nuevo=paste("Pixel",i,sep="")
  names(base)[names(base)==var]=nuevo}

names(base)[names(base)=="V65"]="NumeroMostrado"
base$NumeroMostrado=as.factor(base$NumeroMostrado)

head(base,1)

plot(base$NumeroMostrado,main="Distribución de Clases",col="snow")

colors()

summary(base$NumeroMostrado)

nrow(base)

summary(base$NumeroMostrado)

vector_summary_NumeroMostrado = summary(base$NumeroMostrado)

min(vector_summary_NumeroMostrado)
which.min(vector_summary_NumeroMostrado)

max(vector_summary_NumeroMostrado)
which.max(vector_summary_NumeroMostrado)

######################################################
######################PARTE B#########################
######################################################

library(caret)

set.seed(39208777);particion=createDataPartition(y=base$NumeroMostrado,p=0.75,list=FALSE)
entreno=base[particion,]
testeo=base[-particion,]



head(entreno[,1:4]);head(entreno[,62:65])
summary(entreno[,1:4]);summary(entreno[,62:65])
head(testeo[,1:4]);head(testeo[,62:65])
summary(testeo[,1:4]);summary(testeo[,62:65])

table(entreno$NumeroMostrado)
table(testeo$NumeroMostrado)

dim(entreno)
dim(testeo)


######################################################
######################PARTE C#########################
######################################################

library(nnet)

set.seed(39208777);red=nnet(NumeroMostrado~.,entreno,size=30,maxit=20000,MaxNWts=20000)

sum(sapply(red$wts, length))

library(NeuralNetTools)
plotnet(red)

pred=predict(red,testeo,type="class")
confusionMatrix(factor(pred),testeo$NumeroMostrado)

resultado <- confusionMatrix(factor(pred),testeo$NumeroMostrado)
accuracy_confusionMatrix <- resultado$overall["Accuracy"]
accuracy_confusionMatrix

digitoAsignado=base[77,]
digitoAsignado

predict(red,digitoAsignado,type="class")


######################################################
######################PARTE D#########################
######################################################


set.seed(123);red=nnet(NumeroMostrado~.,entreno,size=30,maxit=20000,MaxNWts=20000)

sum(sapply(red$wts, length))

pred=predict(red,testeo,type="class")
confusionMatrix(factor(pred),testeo$NumeroMostrado)

resultado <- confusionMatrix(factor(pred),testeo$NumeroMostrado)
accuracy_confusionMatrix <- resultado$overall["Accuracy"]
accuracy_confusionMatrix

