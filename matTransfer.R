randomMat <- function(vertexNum){
  
  
  lambda = 0.005
  
  res = matrix(data = 0, nrow = vertexNum, ncol = vertexNum)
  i = 1
  while(i <= vertexNum){
    j = 1
    while(j <= vertexNum){
      if(i == j){
        j = j + 1
        next;
      }
      rand = runif(1, 0, 1)
      if(rand < lambda){
        res[i,j] = 1
      }
      j = j + 1
    }
    i = i + 1
  }
  return(res)
}


resultMat <- function(SecondMat, First_Mat, StaticMat, vertexNum){
  res = matrix(data = 0, nrow = vertexNum, ncol = vertexNum)
  
  for(i in 1 : vertexNum){
    for(j in 1 : vertexNum){
      if(SecondMat[i,j] == 1 || First_Mat[i,j] == 1 || StaticMat[i,j] == 1){
        res[i,j] = 1
      }
    }
  }
  
  return(res)
}


threeMats2bn <- function(secondMat, first_Mat, staticMat, nodeName, vertexNum){
  
  
  ret = empty.graph(nodeName)
  
  i = 1
  while(i <= vertexNum){
    j = 1
    while(j <= vertexNum){
      if(secondMat[i, j] == 1){
        ret = set.arc(ret, nodeName[i], nodeName[2 * vertexNum + j])
      }
      j = j + 1
    }
    i = i + 1
  }
  
  
  i = 1
  while(i <= vertexNum){
    j = 1
    while(j <= vertexNum){
      if(first_Mat[i, j] == 1){
        ret = set.arc(ret, nodeName[1 * vertexNum + i], nodeName[2 * vertexNum + j])
      }
      j = j + 1
    }
    i = i + 1
  }
  
  i = 1
  while(i <= vertexNum){
    j = 1
    while(j <= vertexNum){
      if(staticMat[i, j] == 1){
        ret = set.arc(ret, nodeName[2 * vertexNum + i], nodeName[2 * vertexNum + j])
      }
      j = j + 1
    }
    i = i + 1
  }
  
  return(ret)
}


net2First_Mat <- function(net, vertexNum){
  
  
  res = matrix(0, nrow = vertexNum, ncol = vertexNum)
  len = length(net[["arcs"]]) / 2
  
  i = 1
  while(i <= len){
    from = net[["arcs"]][i]
    to = net[["arcs"]][i + len]
    
    nfrom = nchar(from)
    nto = nchar(to)
    # print(nfrom)
    # print(nto)
    
    if(substr(from, 1, 1) == "B"){
      from = substr(from, 2, nfrom)
      to = substr(to, 2, nto)
      index1 = which(c(1:vertexNum) == from)
      index2 = which(c(1:vertexNum) == to)
      res[index1, index2] = 1
    }
    i = i + 1
  }
  
  return(res)
}


net2SecondMat <- function(net, vertexNum){
  
  
  res = matrix(0, nrow = vertexNum, ncol = vertexNum)
  len = length(net[["arcs"]]) / 2
  
  i = 1
  while(i <= len){
    from = net[["arcs"]][i]
    to = net[["arcs"]][i + len]
    
    nfrom = nchar(from)
    nto = nchar(to)
    
    if(substr(from, 1, 1) == "A"){
      from = substr(from, 2, nfrom)
      to = substr(to, 2, nto)
      index1 = which(c(1:vertexNum) == from)
      index2 = which(c(1:vertexNum) == to)
      res[index1, index2] = 1
    }
    i = i + 1
  }
  
  return(res)
}


net2StaticMat <- function(net, vertexNum){
  
  
  res = matrix(0, nrow = vertexNum, ncol = vertexNum)
  len = length(net[["arcs"]]) / 2
  
  i = 1
  while(i <= len){
    from = net[["arcs"]][i]
    to = net[["arcs"]][i + len]
    
    nfrom = nchar(from)
    nto = nchar(to)
    
    if(substr(from, 1, 1) == "C"){
      from = substr(from, 2, nfrom)
      to = substr(to, 2, nto)
      index1 = which(c(1:vertexNum) == from)
      index2 = which(c(1:vertexNum) == to)
      res[index1, index2] = 1
    }
    i = i + 1
  }
  
  return(res)
}


calResult <- function(resMat, Goldmat, vertexNum){
  
  
  TP = 0
  TN = 0
  FP = 0
  FN = 0
  for(i in 1 : vertexNum){
    for(j in 1 :vertexNum){
      if(i == j){
        next
      }
      if(resMat[i,j] == 1 && Goldmat[i,j] == 1){
        TP = TP + 1
      }else if(resMat[i,j] == 1 && Goldmat[i,j] == 0){
        FP = FP + 1
      }else if(resMat[i,j] == 0 && Goldmat[i,j] == 0){
        TN = TN + 1
      }else{
        FN = FN + 1
      }
    }
  }
  
  Pre0 = round(TP/(TP+FP), 4)
  Recall0 = round(TP/(TP+FN), 4)
  Acc0 = round((TP+TN)/(TP+TN+FN+FP), 4)
  FScore0 = round(2 * Pre0 * Recall0 / (Pre0 + Recall0), 4)
  BAC = round((TP/(TP + FN) + TN/(TN + FP)) / 2, 4)
  res = c(TP, TN, FP, FN, Pre0, Recall0, Acc0, FScore0, BAC)
  mat0 = matrix(data = res, nrow = 1, ncol = 9, byrow = FALSE,dimnames = NULL)
  
  return(mat0)
}




