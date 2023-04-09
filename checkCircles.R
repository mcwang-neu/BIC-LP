checkCircleInMat <- function(graph, nNode){
  
  nParentNum = getParentsNum(graph, nNode)
  NodeStack = matrix(data = -1, nrow = 1, ncol = nNode)
  NodeVisit = matrix(data = 0, nrow = 1, ncol = nNode)
  top = 0
  i = 1
  while(i <= nNode){
    if(nParentNum[i] == 0){
      top = top + 1
      NodeVisit[i] = 1
      NodeStack[top] = i
    }
    i = i + 1
  }
  while(top != 0){
    flag = NodeStack[top]
    top = top - 1
    NodeVisit[flag] = 1
    
    j = 1
    while(j <= nNode){
      if(graph[flag, j] == 1){
        nParentNum[j] = nParentNum[j] - 1
        if(nParentNum[j] == 0 && NodeVisit[j] == 0){
          top = top + 1 
          NodeStack[top] = j
        }
      }
      j = j + 1
    }
  }
  i = 1
  while(i <= nNode){
    if(NodeVisit[i] == 0){
      return(TRUE)
    }
    i = i + 1
  }
  return(FALSE)
}

getParentsNum <- function(graphs, nNode)
{
  ret = matrix(data = 0, nrow = 1, ncol = nNode)
  
  j = 1
  while(j <= nNode){
    i = 1
    while(i <= nNode){
      if(graphs[i,j] == 1){
        ret[j] = ret[j] + 1
      }
      i = i + 1
    }
    j = j + 1
  }
  return(ret)
}
