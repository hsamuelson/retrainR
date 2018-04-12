#
# Henry Samuelson 4/9/18
#
# Scychonyis Retraining for Neural networks


higgsDat <- read.csv("C:/Users/hsamuelson/Desktop/R/Higgs/training/training.csv")
higgsDat <- higgsDat[,-1]

library(neuralnet)


asdd <- higgsDat[1:150,]
Label <- convertToBinary(asdd$Label)
asdd <- asdd[,-32]
asdd <- cbind(asdd, Label)



# build list
convertToBinary <- function(asd){
zxc <-numeric()
  for(i in 1:length(asd)){
    if(asd[i] == "s"){
      zxc[i] <- 1
    }else {
      zxc[i] <- 0
    }
  }
  return(zxc)
}


nn <-neuralnet(formula = model_formula, data = asdd, hidden = 30)
res <- round(abs(compute(nn, observations)$net.result))

obsBinary <- convertToBinary(observationAwsers)



library(nnCore)

trainingSet <- higgsDat[1:150,]
observations <- higgsDat[200:1000,]
observationAwsers <- observations$Label
observations <- observations[,-32]
testingSet <- higgsDat[2000:3000,]

higgsNN1 <- nnCoreV1$new(Label ~ ., data= trainingSet, hidden = 30)
higgsNN1$train(9999, trace = 1e3, learn_rate = .0001)

sum(higgsNN1$computeNN(observations) == observationAwsers)/ 801 #Accuracy initally
