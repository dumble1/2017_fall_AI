myDM = function(roads,car,packages) {
  hroads <- t(roads$hroads)
  vroads <- t(roads$vroads)
  xdim <- ncol(hroads)
  ydim <- nrow(vroads)
  print(hroads)
  print(vroads)
  print(xdim)
  print(ydim)
  costarr <- array(0,dim = c(xdim,ydim,4)) #cost  matrix
  costarr[2:10,,1] = hroads #cost to the left
  costarr[,2:10,2] = vroads #cost below
  costarr[1:9,,3] = hroads #cost to the right
  costarr[,1:9,4] = vroads #cost above
  print(costarr)
  print(astar(costarr,c(1,1),c(10,10)))
  
  
  

  # print(typeof(roads))
  # hroads = matrix(unlist(roads[1]), ncol = 9, byrow = TRUE)
  # vroads = roads[2]
  # print(typeof(hroads))
  # print(hroads)
  # print(is.matrix(hroads))
  # a <- ncol(hroads)
  # print(a)
  return(car)
}

hopcost = function(xdim,ydim,goal) {
  #returns matrix keeping the number of hops from every point (x,y) to the goal
  hopcostm <- matrix(0,xdim,ydim)
  for (i in 1:xdim) {
    for (j in 1:ydim) {
      hopcostm[i,j] <- abs(i - goal[1]) + abs(j - goal[2])
    }
  }
  return(hopcostm)
}


astar = function(costarr,source,goal) {
  xdim = dim(costarr)[1]
  ydim = dim(costarr)[2]
  visitedm <- matrix(FALSE, nrow = xdim, ncol = ydim)
  #stores the best predecessor of all the vistied nodes
  visited.costm <- matrix(Inf,xdim,ydim)
  visited.costm[source[1],source[2]] <- 0
  visited.previousarr <- array(0,dim=c(xdim,ydim,2))
  visiting <- source
  hopcostm <- hopcost(xdim,ydim,goal)
  #stores the expected cost to the nodes in the frontier
  frontier.costm <- matrix(Inf,xdim,ydim)
  #stores the best predecessor of the nodes in the frontier
  frontier.previousarr <- array(0,dim=c(xdim,ydim,2))
  #frontier.pathm[source] <- list("cost" = 0, "path" = list(source))
  
  while(visitedm[goal[1],goal[2]] == FALSE) {
    has.leftneigh <- !(visiting[1] == 1)
    has.lowerneigh <- !(visiting[2] == 1)
    has.rightneigh <- !(visiting[1] == xdim)
    has.upperneigh <- !(visiting[2] == ydim)
    if(has.leftneigh) {
      leftneigh <- visiting - c(1,0)
      left.expectedcost <- visited.costm[visiting[1],visiting[2]] + costarr[visiting[1],visiting[2],1] + hopcostm[leftneigh[1],leftneigh[2]]
    }
    if(has.lowerneigh) {
      lowerneigh <- visiting - c(0,1)
      lower.expectedcost <- visited.costm[visiting[1],visiting[2]] + costarr[visiting[1],visiting[2],2] + hopcostm[lowerneigh[1],lowerneigh[2]]
    }
    if(has.rightneigh) {
      rightneigh <- visiting + c(1,0)
      right.expectedcost <- visited.costm[visiting[1],visiting[2]] + costarr[visiting[1],visiting[2],3] + hopcostm[rightneigh[1],rightneigh[2]]
    }
    if(has.upperneigh) {
      upperneigh <- visiting + c(0,1)
      upper.expectedcost <- visited.costm[visiting[1],visiting[2]] + costarr[visiting[1],visiting[2],4] + hopcostm[upperneigh[1],upperneigh[2]]
    }

    if(has.leftneigh == TRUE && visitedm[leftneigh[1],leftneigh[2]] == FALSE &&
       left.expectedcost < frontier.costm[leftneigh[1],leftneigh[2]]) {
      frontier.costm[leftneigh[1],leftneigh[2]] <- left.expectedcost
      frontier.previousarr[leftneigh[1],leftneigh[2],] <- visiting
    }
    if(has.lowerneigh == TRUE && visitedm[lowerneigh[1],lowerneigh[2]] == FALSE &&
       lower.expectedcost < frontier.costm[lowerneigh[1],lowerneigh[2]]) {
      frontier.costm[lowerneigh[1],lowerneigh[2]] <- lower.expectedcost
      frontier.previousarr[lowerneigh[1],lowerneigh[2],] <- visiting
    }
    if(has.rightneigh == TRUE && visitedm[rightneigh[1],rightneigh[2]] == FALSE &&
       right.expectedcost < frontier.costm[rightneigh[1],rightneigh[2]]) {
      frontier.costm[rightneigh[1],rightneigh[2]] <- right.expectedcost
      frontier.previousarr[rightneigh[1],rightneigh[2],] <- visiting
    }
    if(has.upperneigh == TRUE && visitedm[upperneigh[1],upperneigh[2]] == FALSE &&
       upper.expectedcost < frontier.costm[upperneigh[1],upperneigh[2]]) {
      frontier.costm[upperneigh[1],upperneigh[2]] <- upper.expectedcost
      frontier.previousarr[upperneigh[1],upperneigh[2],] <- visiting
    }
    
    frontier.costm[visiting[1],visiting[2]] <- Inf
    visitedm[visiting[1],visiting[2]] <- TRUE
    
    #print(frontier.costm)
    #print(which(frontier.costm == min(frontier.costm), arr.ind = TRUE)[1,])
    visiting <- which(frontier.costm == min(frontier.costm), arr.ind = TRUE)[1,]
    visited.costm[visiting[1],visiting[2]] <- frontier.costm[visiting[1],visiting[2]] - hopcostm[visiting[1],visiting[2]]
    visited.previousarr[visiting[1],visiting[2],] <- frontier.previousarr[visiting[1],visiting[2],]
    
    # print("visiting")
    # print(visiting)
    # print("frontier.costm")
    # print(frontier.costm)
    
  }
  
  print(visited.costm)
  pathm <- matrix(0,xdim*ydim,2)
  pathm[1,] = goal
  where.onpath = goal
  i = 2
  while(!identical(where.onpath,source)) {
    where.onpath <- visited.previousarr[where.onpath[1],where.onpath[2],]
    pathm[i,] <- where.onpath
    i <- i+1
  }
  return(pathm)
}

extract.nextmove = function(pathm) {
  
}

stupid.randomm = function(nrows,ncols,maxvalue) {
  a <- matrix(1,nrow=nrows,ncol=ncols)
  for (i in 1:nrows) {
    for (j in 1:ncols) {
      a[i,j] <- sample(1:maxvalue,1)
    }
  }
  return(a)
}

test = function(nrows) {
  hroads <- stupid.randomm(nrows,nrows-1,5)
  vroads <- stupid.randomm(nrows-1,nrows,5)
  print(hroads)
  print(vroads)
  costarr <- array(0,dim = c(nrows,nrows,4)) #cost  matrix
  costarr[2:nrows,,1] = hroads #cost to the left
  costarr[,2:nrows,2] = vroads #cost below
  costarr[1:(nrows-1),,3] = hroads #cost to the right
  costarr[,1:(nrows-1),4] = vroads #cost above
  #print(costarr)
  print(astar(costarr,c(1,1),c(nrows,nrows)))
}
