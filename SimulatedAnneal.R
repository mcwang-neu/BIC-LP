add <- function(net, SecondMat, First_Mat, StaticMat, nodeName, vertexNum){
  
  yy = sample(0:2, 1, replace = TRUE)
  if(yy == 2){
    from = sample(1: vertexNum, 1, replace = TRUE)
    to = sample(1: vertexNum, 1, replace = TRUE)
    time = 0
    while(from == to || SecondMat[from, to] == 1){
      from = sample(1: vertexNum, 1, replace = TRUE)
      to = sample(1: vertexNum, 1, replace = TRUE)
      time = time + 1
      if(time == 100){
        return(net)
      }
    }
    SecondMat[from, to] = 1
  }else if(yy == 1){
    from = sample(1: vertexNum, 1, replace = TRUE)
    to = sample(1: vertexNum, 1, replace = TRUE)
    time = 0
    while(from == to || First_Mat[from, to] == 1){
      from = sample(1: vertexNum, 1, replace = TRUE)
      to = sample(1: vertexNum, 1, replace = TRUE)
      time = time + 1
      if(time == 100){
        return(net)
      }
    }
    First_Mat[from, to] = 1
  }else{
    from = sample(1: vertexNum, 1, replace = TRUE)
    to = sample(1: vertexNum, 1, replace = TRUE)
    time = 0
    while(from == to || StaticMat[from, to] == 1){
      from = sample(1: vertexNum, 1, replace = TRUE)
      to = sample(1: vertexNum, 1, replace = TRUE)
      time = time + 1
      if(time == 100){
        return(net)
      }
    }
    StaticMat[from, to] = 1
    if(checkCircleInMat(StaticMat, vertexNum)){
      return(net)
    }
  }
  return(threeMats2bn(SecondMat, First_Mat, StaticMat, nodeName, vertexNum))
}


del <- function(net, nodeName, vertexNum){
  
  e = empty.graph(nodeName)
  len = length(net[["arcs"]]) / 2
  if(len == 0){
    return(net)
  }
  yy = sample(1 : len, 1, replace = TRUE)
  for(i in 1 : len){
    if(i == yy){
      next
    }
    #print(net[["arcs"]][i])
    #print(net[["arcs"]][i + len])
    e = set.arc(e, net[["arcs"]][i], net[["arcs"]][i + len])
  }
  
  return(e)
}


rev <- function(net, SecondMat0, First_Mat0, StaticMat0, nodeName, vertexNum){
  
  from = sample(1: vertexNum, 1, replace = TRUE)
  to = sample(1:vertexNum, 1, replace = TRUE)
  
  time = 0
  while(from == to || StaticMat0[from, to] == 0){
    from = sample(1: vertexNum, 1, replace = TRUE)
    to = sample(1:vertexNum, 1, replace = TRUE)
    time = time + 1
    if(time == 100){
      return(net)
    }
  }
  t = StaticMat0[from, to]
  StaticMat0[from, to] = StaticMat0[to, from]
  StaticMat0[to, from] = t
  
  if(checkCircleInMat(StaticMat0, vertexNum)){
    return(net)
  }
  
  return(threeMats2bn(SecondMat0, First_Mat0, StaticMat0, nodeName, vertexNum))
}


changeTarget <- function(net, SecondMat, First_Mat, StaticMat, nodeName, vertexNum){
  
  yy = sample(0:2, 1, replace = TRUE)
  # print(yy)
  if(yy == 2){
    from = sample(1: vertexNum, 1, replace = TRUE)
    to = sample(1: vertexNum, 1, replace = TRUE)
    time = 0
    while(from == to || SecondMat[from, to] == 0){
      from = sample(1: vertexNum, 1, replace = TRUE)
      to = sample(1: vertexNum, 1, replace = TRUE)
      time = time + 1
      if(time == 100){
        return(net)
      }
    }
    SecondMat[from, to] = 0
    target = sample(1: vertexNum, 1, replace = TRUE)
    # print(target)
    SecondMat[from, target] = 1
  }else if(yy == 1){
    from = sample(1: vertexNum, 1, replace = TRUE)
    to = sample(1: vertexNum, 1, replace = TRUE)
    time = 0
    while(from == to || First_Mat[from, to] == 0){
      from = sample(1: vertexNum, 1, replace = TRUE)
      to = sample(1: vertexNum, 1, replace = TRUE)
      time = time + 1
      if(time == 100){
        return(net)
      }
    }
    First_Mat[from, to] = 0
    target = sample(1: vertexNum, 1, replace = TRUE)
    # print(target)
    First_Mat[from, target] = 1
  }else{
    from = sample(1: vertexNum, 1, replace = TRUE)
    to = sample(1: vertexNum, 1, replace = TRUE)
    time = 0
    while(from == to || StaticMat[from, to] == 0){
      from = sample(1: vertexNum, 1, replace = TRUE)
      to = sample(1: vertexNum, 1, replace = TRUE)
      time = time + 1
      if(time == 100){
        return(net)
      }
    }
    StaticMat[from, to] = 0
    target = sample(1: vertexNum, 1, replace = TRUE)
    # print(target)
    StaticMat[from, target] = 1
    if(checkCircleInMat(StaticMat, vertexNum)){
      return(net)
    }
  }
  
  return(threeMats2bn(SecondMat, First_Mat, StaticMat, nodeName, vertexNum))
}


changeRegulator <- function(net, SecondMat, First_Mat, StaticMat, nodeName, vertexNum){
  
  yy = sample(0:2, 1, replace = TRUE)
  # print(yy)
  if(yy == 2){
    from = sample(1: vertexNum, 1, replace = TRUE)
    to = sample(1: vertexNum, 1, replace = TRUE)
    time = 0
    while(from == to || SecondMat[from, to] == 0){
      from = sample(1: vertexNum, 1, replace = TRUE)
      to = sample(1: vertexNum, 1, replace = TRUE)
      time = time + 1
      if(time == 100){
        return(net)
      }
    }
    SecondMat[from, to] = 0
    regulator = sample(1: vertexNum, 1, replace = TRUE)
    # print(regulator)
    # print(to)
    SecondMat[regulator, to] = 1
  }else if(yy == 1){
    from = sample(1: vertexNum, 1, replace = TRUE)
    to = sample(1: vertexNum, 1, replace = TRUE)
    time = 0
    while(from == to || First_Mat[from, to] == 0){
      from = sample(1: vertexNum, 1, replace = TRUE)
      to = sample(1: vertexNum, 1, replace = TRUE)
      time = time + 1
      if(time == 100){
        return(net)
      }
    }
    First_Mat[from, to] = 0
    regulator = sample(1: vertexNum, 1, replace = TRUE)
    # print(regulator)
    # print(to)
    First_Mat[regulator, to] = 1
  }else{
    from = sample(1: vertexNum, 1, replace = TRUE)
    to = sample(1: vertexNum, 1, replace = TRUE)
    time = 0
    while(from == to || StaticMat[from, to] == 0){
      from = sample(1: vertexNum, 1, replace = TRUE)
      to = sample(1: vertexNum, 1, replace = TRUE)
      time = time + 1
      if(time == 100){
        return(net)
      }
    }
    StaticMat[from, to] = 0
    regulator = sample(1: vertexNum, 1, replace = TRUE)
    # print(regulator)
    # print(to)
    StaticMat[regulator, to] = 1
    if(checkCircleInMat(StaticMat, vertexNum)){
      return(net)
    }
  }
  
  return(threeMats2bn(SecondMat, First_Mat, StaticMat, nodeName, vertexNum))
}



SA <- function(xdata, pearson, lasso, NodeName, vertexNum, m, gama){
  myres0 = SimulatedAnnealing(xdata, xdata_Corr, lassoCorr, NodeName, vertexNum, m, gama)
  print(myres0[['arcs']])
  resMat = resultMat(net2SecondMat(myres0, vertexNum), net2First_Mat(myres0, vertexNum), net2StaticMat(myres0, vertexNum), vertexNum)
  r0 = calResult(resMat, Goldmat, vertexNum)
  print(r0)
  return(myres0)
}


