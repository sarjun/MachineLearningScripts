library(plyr)
library(graphics)

# Read in data
data<-read.table("C:/Users/Arjun/Documents/UVa/Sixth Semester/CS 6316/ResearchProject/DataSet2.csv",header=T,sep=",")

# Calculate risk in overall data set
dataSetRisk = (count(data, c("Class"))[,c("freq")][2])/nrow(data)

# Loop through the attributes
countAttr = ncol(data)-2
for(i in 2:(countAttr)) {
  # Get data frame with just the desired attribute
  attrData = data[,c(i+1, ncol(data))]
  attrData = attrData[order(attrData[,1]), ]
  
  # Calculate data points
  x = list()
  y = list()
  coords = list()
  for(j in 5:95) {
    dataRangeMin = trunc(nrow(attrData)*(j-5)/100)
    dataRangeMax = trunc(nrow(attrData)*(j+5)/100)
    countRare <- count(attrData[dataRangeMin:dataRangeMax,], c("Class"))[,c("freq")][2]
    x[length(x)+1] = as.numeric(j)
    y[length(y)+1] = (countRare/(nrow(attrData)))/dataSetRisk
    coords[length(coords) + 1] = as.numeric(j)
    coords[length(coords) + 1] = (countRare/(nrow(attrData)))/dataSetRisk
  }
  plotPoints = data.frame(matrix(coords, ncol = 2, byrow = TRUE))
  names(plotPoints) = c("Data Point Percentile", "Relative Risk")
  png(filename=paste("C:/Users/Arjun/Documents/UVa/Sixth Semester/CS 6316/ResearchProject/Univariate Risk Curves/", colnames(attrData)[1], ".png", sep="") )
  plot(plotPoints, main = paste(colnames(attrData)[1], "(Univariate Risk Curve)", sep=""))
  dev.off()
}

