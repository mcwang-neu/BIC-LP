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

SimulatedAnnealing <- function(xdata, pearson, lasso, NodeName, vertexNum, m, gama){
  
  L = 150     # 当前温度下的迭代次数
  k = 0.1   # 衰减参数
  Ts = 100
  Te = 0.0001
  
  
  SecondMat = randomMat(vertexNum)
  First_Mat = randomMat(vertexNum)
  StaticMat = randomMat(vertexNum)
  while(checkCircleInMat(StaticMat, vertexNum)){
    StaticMat = randomMat(vertexNum)
  }
  Net = threeMats2bn(SecondMat, First_Mat, StaticMat, NodeName, vertexNum)
  score0 = 0
  while(Ts > Te){
    
    Ts = Ts * k
    print(Ts)
    for(i in 1 : L){
      
      SecondMat = net2SecondMat(Net, vertexNum)
      First_Mat = net2First_Mat(Net, vertexNum)
      StaticMat = net2StaticMat(Net, vertexNum)
      
      score0 = IBIC(Net, xdata, pearson, lasso, vertexNum, m, gama)
      
      flag = sample(0:4, 1, replace = TRUE);
      if(flag == 0){
        # add
        addNet = add(Net, SecondMat, First_Mat, StaticMat, NodeName, vertexNum)
        scorea = IBIC(addNet, xdata, pearson, lasso, vertexNum, m, gama)
        if(scorea > score0){
          Net = addNet;
        }else{
          if(exp((scorea - score0) / Ts) > runif(1, 0, 1)){
            Net = addNet;
          }
        }
      }else if(flag == 1){
        # del
        delNet = del(Net, NodeName, vertexNum)
        scoreb = IBIC(delNet, xdata, pearson, lasso, vertexNum, m, gama)
        if(scoreb > score0){
          Net = delNet
        }else{
          if(exp((scoreb - score0) / Ts) > runif(1, 0, 1)){
            Net = delNet;
          }
        }
      }else if(flag == 2){
        # rev
        revNet = rev(Net, SecondMat, First_Mat, StaticMat, NodeName, vertexNum)
        scorec = IBIC(revNet, xdata, pearson, lasso, vertexNum, m, gama)
        if(scorec > score0){
          Net = revNet
        }else{
          if(exp((scorec - score0) / Ts) > runif(1, 0, 1)){
            Net = revNet;
          }
        }
      }else if(flag == 3){
        # cht
        chtNet = changeTarget(Net, SecondMat, First_Mat, StaticMat, NodeName, vertexNum)
        scored = IBIC(chtNet, xdata, pearson, lasso, vertexNum, m, gama)
        if(scored > score0){
          Net = chtNet
        }else{
          if(exp((scored - score0) / Ts) > runif(1, 0, 1)){
            Net = chtNet;
          }
        }
      }else{
        # chr
        chrNet = changeRegulator(Net, SecondMat, First_Mat, StaticMat, NodeName, vertexNum)
        scoree = IBIC(chrNet, xdata, pearson, lasso, vertexNum, m, gama)
        if(scoree > score0){
          Net = chrNet
        }else{
          if(exp((scoree - score0) / Ts) > runif(1, 0, 1)){
            Net = chrNet;
          }
        }
      }
    }
  }
  return(Net)
}

IBIC <- function(net, xdata, pearson_Corr, lasso_Corr, vertexNum, m, gama){
  
  sum = score(net, xdata, type = 'aic-g')
  if(m == 0){
    return(sum)
  }
  #转换片内矩阵
  staticMat = net2StaticMat(net, vertexNum)
  #片内计算pearsonbenefit
  for (i in 1 : vertexNum) {
    for(j in 1 : vertexNum){
      if(i == j){
        next;
      }
      if(staticMat[i,j] == 1){
        sum = sum + log(m * pearson_Corr[i,j] + gama)
      }else{
        sum = sum + log(m * (1 - pearson_Corr[i,j]) + gama) 
      }
    }
  }
  
  secondMat = net2SecondMat(net, vertexNum)
  first_Mat = net2First_Mat(net, vertexNum)
  for (i in 1:vertexNum) {
    for(j in 1 : vertexNum){
      if(i == j){
        next
      }
      if(secondMat[i,j] == 1){
        sum = sum + log(m * lasso_Corr[i,j] + gama)
      }else{
        sum = sum + log(m * (1 - lasso_Corr[i,j]) + gama)
      }
      if(first_Mat[i,j] == 1){
        sum = sum + log(m * lasso_Corr[i + vertexNum, j] + gama)
      }else{
        sum = sum + log(m * (1 - lasso_Corr[i + vertexNum, j]) + gama)
      }
    }
  }
  return(sum)
}


SA <- function(xdata, pearson, lasso, NodeName, vertexNum, m, gama){
  myres0 = SimulatedAnnealing(xdata, xdata_Corr, lassoCorr, NodeName, vertexNum, m, gama)
  print(myres0[['arcs']])
  resMat = resultMat(net2SecondMat(myres0, vertexNum), net2First_Mat(myres0, vertexNum), net2StaticMat(myres0, vertexNum), vertexNum)
  r0 = calResult(resMat, Goldmat, vertexNum)
  print(r0)
  return(myres0)
}


SA <- function(xdata, pearson, lasso, NodeName, vertexNum, m, gama){
  myres0 = SimulatedAnnealing(xdata, xdata_Corr, lassoCorr, NodeName, vertexNum, m, gama)
  print(myres0[['arcs']])
  resMat = resultMat(net2SecondMat(myres0, vertexNum), net2First_Mat(myres0, vertexNum), net2StaticMat(myres0, vertexNum), vertexNum)
  r0 = calResult(resMat, Goldmat, vertexNum)
  print(r0)
  return(myres0)
}


