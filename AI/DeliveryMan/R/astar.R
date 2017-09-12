#In all our functions we treat x as the row dimension, y as the coloumn dimension

myDM = function(roads,car,packages) {
#add
	if(length(car$mem)==0 ){							# first start(no pickup order and no finished delivery).
		car$mem = orderPickup(0,0,0,packages,list())					# store pickup order in mem.  
		car$mem[[1]] = NULL
		print(car$mem)
	}
	if(car$load !=0){
	 	toGo = car$load
	}else{
		toGo =	car$mem[[1]]
	}
	print ("turn")
 	print (car$mem)
	dest_x = NA
	dest_y = NA
	
	if(packages[toGo,5] == 2){										# delivered.
		car$mem[[1]] = NULL												# delete old target.
		print(car$mem)
		toGo = car$mem[[1]]
		if(length(car$mem)==0){											# game is finished. this instrucion may be not executed.
			return (car)
		}
	}
	if(car$mem[[1]] != car$load && car$load!=0){										# if pick other package 
		for(i in 1:length(car$mem)){
			if (car$load == car$mem[[i]]){				# move picked one to front of the car$mem
				print("wrong pick")
				print (car$mem)
				tmp = car$mem[[i]]
				car$mem[[i]] = NULL
				print(car$mem)
				car$mem = c(list(tmp), car$mem)
				print(car$mem)
				break;
			}		
		}
	}
	if(packages[toGo,5] == 0){									# target package is not picked.
		dest_x = packages[toGo,1]									# set destination as package.
		dest_y = packages[toGo,2]	
	}
	else if(packages[toGo,5] == 1){									# not delivered yet.
		dest_x = packages[toGo,3]									# set destination as where you should deliver it to.
		dest_y = packages[toGo,4]
	}
	else{
		print("what's wrong?? ERROR")								# Error case package state is strange.	 
}
#add
  
#transpose hroads and vroads to make dimensions fit
  hroads <- t(roads$hroads) 
  vroads <- t(roads$vroads)
  xdim <- nrow(hroads) + 1
  ydim <- ncol(hroads)
  #Initialise costarr, which contains the edge costs to every node's neighbours
  costarr <- array(0,dim = c(xdim,ydim,4))
  costarr[2:10,,1] = hroads #cost to the left
  costarr[,2:10,2] = vroads #cost below
  costarr[1:9,,3] = hroads #cost to the right
  costarr[,1:9,4] = vroads #cost above
  bestPath = astar(costarr,c(car$x,car$y),c(dest_x,dest_y))  ##find bestpath from current node to next package or destination.
 
#  print (bestPath)
# add order function.
	n=1
	while(TRUE){
		if(bestPath[n,1]==0&&bestPath[n,2]==0){
			break;
		}
		n = n+1
	}
	if (n==2){													# if next pack and last dest are on the same coordination then we need to stay once to pick up the package. 
		car$nextMove = 5										# In this case, best Path's length is only 1. so n-2 can make error.
	}
	else if(bestPath[n-2,1] - car$x == 1){									# go right.
		car$nextMove = 6
	}
	else if(bestPath[n-2,1] - car$x == -1){								# go left.
		car$nextMove = 4								
	}
	else if(bestPath[n-2,2] - car$y == 1){								# go up.
		car$nextMove = 8
	}
	else if(bestPath[n-2,2] - car$y == -1){								# go down.
		car$nextMove = 2
	}
	else if(bestPath[n-2,1] == car$x &&  bestPath[n-2,2] == car$y){			# stay 
		car$nextMove = 5
	}
	else{
		print ("no way?!")
	}		
#	
  return(car)
}



orderPickup = function(originx,originy,cost,packages,track){ 
	min = 2000
	mintrack= list()							
	for(i in 1:nrow(packages)){
		if (i %in% track)                                                    #already visited.
			next
		else{
			cost_tmp = cost + abs(packages[i,1]-originx) + 
							  abs(packages[i,2]-originy)	# origin to next package.
		 	 tmp = orderPickup(packages[i,3],packages[i,4], cost_tmp,packages, c(track,list(i)))
					#get the cost and track to the end.
			if(tmp[[1]]<min){		# find the minimum cost track.
				min = tmp[[1]]
				tmp[[1]]=NULL
				mintrack= tmp
			}
		}
	}
	if(length(mintrack)==0){			#if there is no where to go, then just return previous cost and 
	#print(cost)
	#print(track)
		answer = c(list(cost),track)
	}
	else{
	#	cat("this is ",cost, "    ",min)
 #print(mintrack)
		answer =c(list(min),mintrack)
	}
	return (answer)
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
  #Returns the shortest path (to be read backwards) from source to goul on a Manhattan grid. 
  #Costarr stores the edge cost from each node to its neighbours (left,lower,right,upper)
  #Source and goal are two-dimensional vectors of x- and y-coordinates
  
  #Local variables used
  
  #xdim, ydim: Store size of grid
  xdim = dim(costarr)[1]
  ydim = dim(costarr)[2]
  #visiting: Stores the node that is expanded in the current iteration
  visiting <- source
  #visitedm: Stores all the nodes that have been expanded or are expanded in the current iteration
  visitedm <- matrix(FALSE, nrow = xdim, ncol = ydim)
  visitedm[source[1],source[2]] <- TRUE
  #visited.previousarr: Stores the best predecessor of all the visited nodes.
# Used later to reconstruct the optimal path
  visited.previousarr <- array(0,dim=c(xdim,ydim,2))
  #visited.costm: Stores the cost of the optimal path to all expanded nodes
  visited.costm <- matrix(Inf,xdim,ydim)
  visited.costm[source[1],source[2]] <- 0
  
  #hopcostm: Stores the shortest number of hops from every node to the goal
  hopcostm <- hopcost(xdim,ydim,goal)
  #frontier.expectedcostm: stores the expected cost from source to goal
  # if the route over the node in the frontier is taken
  # Entry is infinitiy iff the node is not in the frontier
  frontier.expectedcostm <- matrix(Inf,xdim,ydim)
  #Stores the expected predecessor on the path from source to goal over the node in the frontier
  frontier.expectedpreviousarr <- array(0,dim=c(xdim,ydim,2))
  #(As long as a node is the frontier, its expected cost and its expected predecessor might be updated in any interation)
  
 
  
  while(visitedm[goal[1],goal[2]] == FALSE) {
    has.leftneigh <- !(visiting[1] == 1)
    has.lowerneigh <- !(visiting[2] == 1)
    has.rightneigh <- !(visiting[1] == xdim)
    has.upperneigh <- !(visiting[2] == ydim)
    
    #Identify coordinates and expected path cost of neighbours of current node
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

    #If the neighbours have not been expanded yet, add them to the frontier (if they already are in the frontier,
    #update their expected costs and predecessors if appropiate)
    if(has.leftneigh == TRUE && visitedm[leftneigh[1],leftneigh[2]] == FALSE &&
       left.expectedcost < frontier.expectedcostm[leftneigh[1],leftneigh[2]]) {
      frontier.expectedcostm[leftneigh[1],leftneigh[2]] <- left.expectedcost
      frontier.expectedpreviousarr[leftneigh[1],leftneigh[2],] <- visiting
    }
    if(has.lowerneigh == TRUE && visitedm[lowerneigh[1],lowerneigh[2]] == FALSE &&
       lower.expectedcost < frontier.expectedcostm[lowerneigh[1],lowerneigh[2]]) {
      frontier.expectedcostm[lowerneigh[1],lowerneigh[2]] <- lower.expectedcost
      frontier.expectedpreviousarr[lowerneigh[1],lowerneigh[2],] <- visiting
    }
    if(has.rightneigh == TRUE && visitedm[rightneigh[1],rightneigh[2]] == FALSE &&
       right.expectedcost < frontier.expectedcostm[rightneigh[1],rightneigh[2]]) {
      frontier.expectedcostm[rightneigh[1],rightneigh[2]] <- right.expectedcost
      frontier.expectedpreviousarr[rightneigh[1],rightneigh[2],] <- visiting
    }
    if(has.upperneigh == TRUE && visitedm[upperneigh[1],upperneigh[2]] == FALSE &&
       upper.expectedcost < frontier.expectedcostm[upperneigh[1],upperneigh[2]]) {
      frontier.expectedcostm[upperneigh[1],upperneigh[2]] <- upper.expectedcost
      frontier.expectedpreviousarr[upperneigh[1],upperneigh[2],] <- visiting
    }
    
    frontier.expectedcostm[visiting[1],visiting[2]] <- Inf #delete expanded node from frontier
    
    #choose one of the nodes with minimal expected cost in the frontier to be expanded next
    visiting <- which(frontier.expectedcostm == min(frontier.expectedcostm), arr.ind = TRUE)[1,]
    visitedm[visiting[1],visiting[2]] <- TRUE
    
    #By the properties of A*, we have found the optimal path to the node that is expanded
    visited.costm[visiting[1],visiting[2]] <- frontier.expectedcostm[visiting[1],visiting[2]] - hopcostm[visiting[1],visiting[2]]
    visited.previousarr[visiting[1],visiting[2],] <- frontier.expectedpreviousarr[visiting[1],visiting[2],]
    
  }
  
  #Reconstruct path starting at the goal
  pathm <- matrix(0,xdim*ydim,2)
  pathm[1,] = goal
  where.onpath = goal
  i = 2
  while(!identical(where.onpath,source)) {
    where.onpath <- visited.previousarr[where.onpath[1],where.onpath[2],]
    pathm[i,] <- where.onpath
    i <- i+1
  }
  # #Activate if you want to cut off zeros and reverse the path
  # pathx <- pathm[,1]
  # pathy <- pathm[,2]
  # pathx <- pathx[which(pathx != 0)]
  # pathy <- pathy[which(pathy != 0)]
  # pathm <- cbind(pathx,pathy)
  # pathm <- apply(pathm, 2, rev)
  
  return(pathm)
}


stupid.randomm = function(nrows,ncols,maxvalue) {
  #Creates a matrix of size nrows x ncols with random integers in {1,...,maxvalue}
  #Probably slow
  a <- matrix(1,nrow=nrows,ncol=ncols)
  for (i in 1:nrows) {
    for (j in 1:ncols) {
      a[i,j] <- sample(1:maxvalue,1)
    }
  }
  return(a)
}

test = function(nrows) {
  #Runs A* on a grid of size nrows x nrows with random edge costs between 1 and 5
  #to obtain the shortest path between the points (1,1) and (nrows,nrows)
  
  #randomly generate edges and print them
  hroads <- stupid.randomm(nrows-1,nrows,5)
  vroads <- stupid.randomm(nrows,nrows-1,5)
  print(hroads)
  print(vroads)
  
  #Initialise cost array
  costarr <- array(0,dim = c(nrows,nrows,4))
  costarr[2:nrows,,1] = hroads #cost to the left
  costarr[,2:nrows,2] = vroads #cost below
  costarr[1:(nrows-1),,3] = hroads #cost to the right
  costarr[,1:(nrows-1),4] = vroads #cost above

  print(astar(costarr,c(1,1),c(nrows,nrows)))
}
