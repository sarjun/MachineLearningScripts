rmCrossValidation <- function(data, cluster, class, folds = 10, evaluate) {
  sum0 = nrow(subset(data, Class == 0))
  sum1 = nrow(subset(data, Class == 1))
  class0List = subset(count(data, c("Class", "id")), Class==0)
  class1List = subset(count(data, c("Class", "id")), Class==1)
  class1List = class1List[with(class1List, order(-1 * freq)), ]
  foldHolder = list()
  obsCount = list()
  for(i in 1:folds) {
    foldHolder[[i]] = list()
    obsCount[i] = 0
  }
  
  for(i in 1:(nrow(class1List))) {
    foldIndex = 1
    assigned = T
    while(obsCount[[foldIndex]] + class1List[i, "freq"] > ceil(sum1/10)) {
      foldIndex = foldIndex + 1
      if(foldIndex > folds) {
        assigned = F
        break
      }
    }
    
    if(!assigned) {
      next
    }
    
    foldHolder[[foldIndex]][length(foldHolder[[foldIndex]]) + 1] = class1List[i, "id"]
    obsCount[foldIndex] = obsCount[[foldIndex]] + class1List[i, "freq"]
  }
  
  for(i in 1:folds) {
    obsCount[i] = obsCount[[i]] + sum(subset(class0List, id %in% foldHolder[[i]])[,"freq"])
    class0List = subset(class0List, !(id %in% foldHolder[[i]]))
  }
  
  for(i in 1:(nrow(class0List))) {
    foldIndex = 1
    assigned = T
    while(obsCount[[foldIndex]] + class0List[i, "freq"] > ceil(nrow(data)/10)) {
      foldIndex = foldIndex + 1
      if(foldIndex > folds) {
        assigned = F
        break
      }
    }
    
    if(!assigned) {
      next
    }
    
    foldHolder[[foldIndex]][length(foldHolder[[foldIndex]]) + 1] = class0List[i, "id"]
    obsCount[foldIndex] = obsCount[[foldIndex]] + class0List[i, "freq"]
  }
}