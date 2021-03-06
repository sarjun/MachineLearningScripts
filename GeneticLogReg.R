library(rms)
library(pROC)
library(DMwR)
library(plyr)
library(parallel)

# Constants
phenotypesLoc = "C:/Users/Arjun/Documents/UVa/Sixth Semester/CS 6316/ResearchProject/GA/phenotypes.txt"
archiveDir = "C:/Users/Arjun/Documents/UVa/Sixth Semester/CS 6316/ResearchProject/GA/Archive/"
dataLoc = "C:/Users/Arjun/Documents/UVa/Sixth Semester/CS 6316/ResearchProject/DataSet2.csv"

# Set up cross validation function
# source('MachineLearningScripts/CrossValidationFunction.R')

# Read in the data and set up train/test sets
folds = 10
data<-read.table(dataLoc,header=T,sep=",")
cvFolds = rmCrossValidation(data, folds)

# Helpers for cross validation
attributeNames = colnames(data)[2:32]
bestPhenotypes = list()
bestPhenRocs = list()
mutationRate = 0.001
crossoverRate = 0.7
genCount = 1
genSize = 1500

readPhenotypes = function() {
  return(scan(phenotypesLoc, what="", sep="\n"))
}

attrsOfPhenotype = function(phenotype) {
  genes = strsplit(phenotype, '')[[1]]
  return(attributeNames[Filter(function(i){genes[i]=="1"}, 1:31)])
}

evaluatePhenotype = function(phenotype) {
  # Get formula from phenotype
  formula = as.formula(paste("Class~", paste(attrsOfPhenotype(phenotype), collapse="+")))
  
  # Build the logistic regression model and calculate ROC curve
  rocAvg = 0
  for(i in 1:folds) {
    testset = cvFolds[[i]][[2]]
    attach(cvFolds[[i]][[1]])
    optimal = lrm(formula,y=T,x=T)
    optimal = robcov(optimal,cluster=id)
    prob=predict(optimal,type=c("fitted"),testset)
    testset$prob = prob
    ROC = roc(Class==1 ~ prob, data = testset, auc=TRUE)
    rocAvg = rocAvg + as.numeric(ROC["auc"])
    detach()
  }
  
  rocAvg = rocAvg / folds
  print(rocAvg)
}

handleColinearity = function(phenotype) {
  for(i in 0:2) {
    if(as.numeric(substr(phenotype, 18+i, 18+i)) + as.numeric(substr(phenotype, 29+i, 29+i)) == 2) {
      phenotype = paste(substr(phenotype, 1, 17+i), 0, substr(phenotype, 19+i, 28+i), 1, substr(phenotype, 30+i, 31), sep="")
    }
  }

  return(phenotype)
}

handleColinearityInGeneration = function(generation) {
  return(lapply(generation, handleColinearity))
}

evaluateGeneration = function(phenotypes) {
  phenRocs = mclapply(phenotypes, evaluatePhenotype, mc.cores=8)
  bestPhenotypes <<- c(bestPhenotypes, phenotypes)
  bestPhenRocs <<- c(bestPhenRocs, phenRocs)
  
  ord = order(unlist(bestPhenRocs), decreasing = T)
  bestPhenRocs <<- bestPhenRocs[ord]
  bestPhenotypes <<- bestPhenotypes[ord]
  bestPhenRocs <<- bestPhenRocs[1:genSize]
  bestPhenotypes <<- bestPhenotypes[1:genSize]
}

writeArchive = function() {
  filename = paste(archiveDir, "HallOfFame", genCount, ".txt", sep="")
  for(i in 1:genSize) {
    write(paste(paste(i, bestPhenRocs[i], sep=": "), bestPhenotypes[i], paste(attrsOfPhenotype(bestPhenotypes[[i]]), collapse=", "), sep=","), file=filename, append=T)
  }
}

generateSelectionProb = function() {
  scores = bestPhenRocs
  scoreSum = sum(unlist(bestPhenRocs))
  currProb = 0
  selectionProbUpperBound = list()
  
  for(i in 1:genSize) {
    selectionProbUpperBound[i] = scores[[i]]/scoreSum + currProb
    currProb = currProb + scores[[i]]/scoreSum
  }
  
  return(selectionProbUpperBound)
}

getParentIndex = function(selProb) {
  sel = runif(1, 0, 1)
  
  for(i in 1:genSize) {
    if(sel < selProb[i]) return(i)
  }
  
  return(genSize)
}

crossover = function(parents) {
  chance = runif(1, 0, 1)
  
  if(chance <= crossoverRate) {
    children = list()
    crossAt = sample(1:30, 1)
    
    children[1] = paste(substr(parents[1], 1, crossAt), substr(parents[2], crossAt + 1, 31), sep="")
    children[2] = paste(substr(parents[2], 1, crossAt), substr(parents[1], crossAt + 1, 31), sep="")
    return(children)
  }
  
  return(parents)
}

mutateChildren = function(children) {
  chance1 = runif(31, 0, 1)
  chance2 = runif(31, 0, 1)
  
  for(i in 1:31) {
    if(chance1[i] <= mutationRate) {
      children[1] = paste(substr(children[1], 1, i-1), as.numeric(substr(children[1], i, i)) * -1 + 1, substr(children[1], i+1, 31), sep="")
    }
    if(chance2[i] <= mutationRate) {
      children[1] = paste(substr(children[2], 1, i-1), as.numeric(substr(children[2], i, i)) * -1 + 1, substr(children[2], i+1, 31), sep="")
    }
  }
  
  return(children)
}

formNextGeneration = function(currGen) {
  crossoverProbs = runif(genSize, 0, 1)
  nextGen = list()
  
  selectionProb = generateSelectionProb()
  
  for(i in 1:(genSize/2)) {
    parents = list()
    parents[1] = currGen[getParentIndex(selectionProb)]
    parents[2] = currGen[getParentIndex(selectionProb)]
    
    children = crossover(parents)
    children = mutateChildren(children)
    
    nextGen[length(nextGen)+1] = children[1]
    nextGen[length(nextGen)+1] = children[2]
  }
  
  return(nextGen)
}

currGen = readPhenotypes()
for(x in 1:10) {
  currGen = handleColinearityInGeneration(currGen)
  evaluateGeneration(currGen)
  writeArchive()
  currGen = formNextGeneration(currGen)
  genCount = genCount + 1
}