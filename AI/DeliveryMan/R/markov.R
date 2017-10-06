## Number of states
NSTATES=40

#' Models WheresCroc as a hidden Markov model.
#'
#' @param moveInfo list of attributes saved between executions.
#' The moves will be written to moveInfo$moves, as a pair.
#' @param readings salinity, phosphor and nitrogen levels observed
#' in this step
#' @param edges the edges of the water-hole system, as a vector
#' of pairs
#' @param probs the mean and standard deviation for the emission probability
#' distribution for each lake
markovWC=function(moveInfo,readings,positions,edges,probs) {
  
  # Initial run?
  if (length(moveInfo$mem) == 0) {
    ## Generate adjacency matrix
    adj.mtrx = generate.adjacency.mtrx(edges)
    ## Generate transition matrix
    t.mtrx = generate.transition.matrix(adj.mtrx)

    ## Initialize the memory list
    moveInfo$mem = list(s.old = matrix(1/NSTATES,nrow=NSTATES,ncol=1),   # Initial state estimation (all states equally probable)
                        t.mtrx = t.mtrx,                                 # Transition matrix
                        adj.mtrx = adj.mtrx                              # Adjacency matrix
                        )
  }

  # Extract data
  s.old = moveInfo$mem$s.old
  t.mtrx = moveInfo$mem$t.mtrx
  adj.mtrx = moveInfo$mem$adj.mtrx

  # Positions must be numeric type
  positions = as.numeric(positions)

  # Generate the state estimation
  if (!is.na(positions[1]) && positions[1] < 0) {
    ## Hiker 1 is eaten
    s.est = rep(0,NSTATES)
    s.est[-positions[1]] = 1
  } else if (!is.na(positions[2]) && positions[2] < 0) {
    ## Hiker 2 is eaten
    s.est = rep(0,NSTATES)
    s.est[-positions[2]] = 1
  } else {
    # Estimate state with forward algorithm
    s.est = state.estimate(s.old, t.mtrx, readings, probs)
        
    # If hikers are alive, then Croc is not at their positions
    if (!is.na(positions[1])) {
      s.est[positions[1]] = 0
    } 
    if (!is.na(positions[2])) {
      s.est[positions[2]] = 0
    }

    # Normalize to real probabilities
    s.est = normalize(s.est)
  }
  
  # Current position
  cur = positions[3]
  # Most probable location for Croc
  goal = which(s.est == max(s.est),arr.ind = TRUE)[1]
  
  # Find shortest path to goal
  path = bfs(cur,goal,adj.mtrx)
  
  if (length(path) == 1) {
    ## We are at the goal!
    mv1 = 0

    ## Find second best state
    s.copy = s.est
    s.copy[goal] = -1
    second.best = which(s.copy == max(s.copy),arr.ind = TRUE)[1]
    second.path = bfs(cur,second.best,adj.mtrx)

    ## Take a step in the second best direction
    mv2 = second.path[2]
  } else if (length(path) == 2) {
    ## One step left to goal
    mv1 = path[2]
    mv2 = 0
  } else {
    ## At least 2 steps left to the goal
    mv1 = path[2]
    mv2 = path[3]
  }
  
  mv1 = as.numeric(mv1)
  mv2 = as.numeric(mv2)
  
  moveInfo$moves=c(mv1,mv2)
  
  # Mark nodes as searched
  if (mv1 == 0) {
    ## Current state is searched
    s.est[cur] = 0
  } else if (mv2 == 0) {
    ## State mv1 is searched
    s.est[mv1] = 0 
  }

  s.est = normalize(s.est)
  moveInfo$mem$s.old = s.est

  return(moveInfo)
}

#' Generates an adjacency matrix from the edges
#'
#' @param edges the edges of the graph as a vector of pairs
#' @return an adjacency matrix for the graph
generate.adjacency.mtrx <- function(edges) {
  mtrx = matrix(0,nrow=NSTATES,ncol=NSTATES)

  for (i in 1:nrow(edges)) {
    from = edges[i,1]
    to = edges[i,2]
    
    ## Mark 'from' and 'to' as being neighbors
    mtrx[from,to] = 1
    mtrx[to,from] = 1
    mtrx[to,to] = 1
    mtrx[from,from] = 1
  }

  return (mtrx)
}

#' Generates a transition matrix T, where T[i,j] is the probability
#' that we transition to state j given that we are in state i,
#' assuming any move to a neighbor is equally probable
#'
#' @param adj.mtrx adjacency matrix of the graph
#' @return A transition matrix for the graph described by the
#' adjacency matrix, assuming transitions to all neighbors are
#' equally probable
generate.transition.matrix <- function(adj.mtrx) {
  # Initialise the transistion matrix as the adjacency matrix
  transition.matrix = adj.mtrx

  n = nrow(adj.mtrx)
  
  # Normalize the '1':s in the adjacency matrix to real probabilities
  for (i in 1:n) {
    # Number of neighbors of i
    neighbors = length(which(transition.matrix[i,] == 1))
    for (j in 1:n) {
      transition.matrix[i,j] = transition.matrix[i,j] / neighbors
    }
  }
  
  return (transition.matrix)
}

#' Performs BFS to find the shortest path from 'from' to 'to'
#' in the graph defined by the adjacency matrix 'adj.mtrx'
#'
#' @param from the start vertex
#' @param to the end vertex
#' @param adj.mtrx adjacency matrix for the graph
#' @return A shortest path from 'from' to 'to' in the
#' graph described by the adjacency matrix
bfs <- function(from,to,adj.mtrx) {
  # Number of vertices
  n = nrow(adj.mtrx)

  # FIFO queue
  queue = matrix(c(FALSE,Inf), nrow=n, ncol=2, byrow = TRUE)
  # Visited vertices
  visited = rep(FALSE,n)
  # Parent of every vertex
  parent = rep(NA,n)

  # Add 'from' to queue and visited
  queue[from,] = c(TRUE,0)
  visited[from] = TRUE

  # Perform search
  while (TRUE %in% queue[,1]) {
    min = min(queue[,2])

  # Extract the elemetn with the minimum depth from the queue
    current = which(queue[,2] == min)[1]
    queue[current,] = c(FALSE,Inf)

    # Add all unvisited adjacent vertices to the queue
    for (adj in which(adj.mtrx[current,] == 1)) {
      if (!visited[adj]) {
        visited[adj] = TRUE
        queue[adj,] = c(TRUE,min+1)
        parent[adj] = current

        if (adj == to) {
          break
        }
      }
    }
  }

  # Help function to create a path from the parent array
  reconstruct.path <- function(start,end,p) {
    path = c(end)
    current = end
    while (current != start) {
      path = c(p[current],path)
      current = p[current]
    }
    return (path)
  } 
  
  return (reconstruct.path(from,to,parent))
}

#' Estimate the current state from the previous one, using
#' the forward algorithm
#'
#' @param s.old previous state estimation
#' @param t.mtrx transition matrix
#' @param obs observations in this step
#' @param probs means and standard deviations for the emission probability
#' distributions
#' @return The probability distribution generated by the forward
#' algorithm
state.estimate <- function(s.old, t.mtrx, obs, probs) {
  prob <- function(x,mu,sigma) {
    return (dnorm(x,mu,sigma))
  }

  e.sal = prob(obs[1],probs$salinity[,1],probs$salinity[,2])
  e.pho = prob(obs[2],probs$phosphate[,1],probs$phosphate[,2])
  e.nit = prob(obs[3],probs$nitrogen[,1],probs$nitrogen[,2])

  s.est = t(t(s.old) %*% t.mtrx) * e.sal * e.pho * e.nit

  return (s.est)
}

#' Normalizes a vector with numbers so that the sum of the numbers
#' is one
#'
#' @param v the vector to normalize
#' @return a normalized vector u, such that u[i] = v[i]/sum(v)
normalize <- function(v) {
  return (v/sum(v))
}

#' Run n times, ouput average, min, max and stddev
testRun <- function(n, makeMoves=markovWC, plot=F) {
  times = c()
  for (i in 1:n) {
    t = runWheresCroc(makeMoves=markovWC,plot=plot)
    times = c(times,t)
    if (i %% 10 == 0) {
      print(i)
      printStats(times)
    }
  }
  printStats(times)
}

#' Prints statistics about a vector
printStats <- function(v) {
  print(paste("mean:",mean(v)))
  print(paste("min:",min(v)))
  print(paste("max:",max(v)))
  print(paste("stddev:",sd(v)))
}
