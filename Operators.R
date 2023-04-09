addarc <- function(net, nodeName, vertexNum){
  from = sample(1 : vertexNum * 3, 1, replace = TRUE)
  to = sample((2 * vertexNum + 1) : (3 * vertexNum), 1, replace = TRUE)
  # from + 2 * vertexNum == to || from + vertexNum == to || 
  if(from == to){
    return(net)
  }

  if(from > 2 * vertexNum){
    StaticMat = net2StaticMat(net, vertexNum)
    StaticMat[from - 2 * vertexNum, to - 2 * vertexNum] = 1
    if(checkCircleInMat(StaticMat, vertexNum) == TRUE){
      return(net)
    }else{
      net = set.arc(net, nodeName[from], nodeName[to])
      return(net)
    }
  }else{
    net = set.arc(net, nodeName[from], nodeName[to])
    return(net)
  }
}

delarc <- function(net, nodeName, vertexNum){
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
    e = set.arc(e, net[["arcs"]][i], net[["arcs"]][i + len])
  }
  return(e)
}

revarc <- function(net, nodeName, vertexNum){
  
  from = sample(1: vertexNum, 1, replace = TRUE)
  to = sample(1:vertexNum, 1, replace = TRUE)
  
  STM = net2StaticMat(net, vertexNum)
  SM = net2SecondMat(net, vertexNum)
  FM = net2First_Mat(net, vertexNum)
  
  time = 0
  while(from == to || STM[from, to] == 0){
    from = sample(1: vertexNum, 1, replace = TRUE)
    to = sample(1:vertexNum, 1, replace = TRUE)
    time = time + 1
    if(time == 10){
      return(net)
    }
  }
  t = STM[from, to]
  STM[from, to] = STM[to, from]
  STM[to, from] = t
  # print(from)
  # print(to)
  if(checkCircleInMat(STM, vertexNum)){
    return(net)
  }
  return(threeMats2bn(SM, FM, STM, nodeName, vertexNum))
}

multiarc <- function(net, NodeName, vertexNum){
  net = delarc(net, NodeName, vertexNum)
  net = addarc(net, NodeName, vertexNum)
  net = revarc(net, NodeName, vertexNum)
  return(net)
}

SimulatedAnnealing <- function(xdata, pearson, lasso, NodeName, vertexNum, m, gama){
  L = 60      # 当前温度下的迭代次数
  k = 0.998   # 衰减参数
  Ts = 1000
  Te = 0.0001
  
  curNet = empty.graph(NodeName)
  score0 = 0
  while(Ts > Te){
    Ts = Ts * k
    print(c(score0, Ts))
    
    for (i in 1 : L) {
      
      score0 = IBIC(curNet, xdata, pearson, lasso, vertexNum, m, gama)
      
      flag = sample(0:3, 1, replace = TRUE)
      
      if(flag == 0){    
        # add
        addNet = addarc(curNet, NodeName, vertexNum)
        scorea = IBIC(addNet, xdata, pearson, lasso, vertexNum, m, gama)
        if(scorea > score0 || (exp((scorea - score0) / Ts) > runif(1, 0, 1))){
          curNet = addNet
        }
      }else if (flag == 1){
        # del 
        delNet = delarc(curNet, NodeName, vertexNum)
        scored = IBIC(delNet, xdata, pearson, lasso, vertexNum, m, gama)
        if(scored > score0 || (exp((scored - score0) / Ts) > runif(1, 0, 1))){
          curNet = delNet
        }
      }else if (flag == 2){
        revNet = revarc(curNet, NodeName, vertexNum)
        scorer = IBIC(revNet, xdata, pearson, lasso, vertexNum, m, gama)
        if(scorer > score0 || (exp((scorer - score0) / Ts) > runif(1, 0, 1))){
          curNet = revNet
        }
      }else{
        mulNet = multiarc(curNet, NodeName, vertexNum)
        scorem = IBIC(mulNet, xdata, pearson, lasso, vertexNum, m, gama)
        if(scorem > score0 || (exp((scorem - score0) / Ts) > runif(1, 0, 1))){
          curNet = mulNet
        }
      }
    }
  }
  return(curNet)
  
}

IBIC <- function(net, xdata, pearson_Corr, lasso_Corr, vertexNum, m, gama){
  
  sum = score(net, xdata, type = 'bic-g')
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
  print(resMat)
  r0 = calResult(resMat, Goldmat, vertexNum)
  print(r0)
  return(myres0)
}


