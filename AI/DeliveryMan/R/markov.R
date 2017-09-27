#' Number of states
NSTATES=40

#' Interval sizes in emission matrices
MIN=50
MAX=250
DELTA=1

markovWC=function(moveInfo,readings,positions,edges,probs) {

  # Initial run?
  if (length(moveInfo$mem) == 0) {
    #' Generate emission matrices
    e.mtrx.sal = generate.emission.matrix(probs$salinity,MIN,MAX,DELTA)
    e.mtrx.pho = generate.emission.matrix(probs$phosphate,MIN,MAX,DELTA)
    e.mtrx.nit = generate.emission.matrix(probs$nitrogen,MIN,MAX,DELTA)

    #' Generate transition matrix
    t.mtrx = generate.transition.matrix(edges,NSTATES)

    #' Initialize the memory list
    moveInfo$mem = list(s.old = rep(1/NSTATES,NSTATES),   # Initial state estimation (all states equally probable)
                        t.mtrx = t.mtrx,                  # Transition matrix
                        e.mtrxs = list(salinity=e.mtrx.sal,phosphate=e.mtrx.pho,nitrogen=e.mtrx.nit) # Emission matrices
                        )
  }

  # Extract data
  s.old = moveInfo$mem$s.old
  t.mtrx = moveInfo$mem$t.mtrx
  e.mtrxs = moveInfo$mem$e.mtrxs

  # Had problems with positions not beeing numeric
  for (i in 1:length(positions)) {
    if (!is.na(positions[i])) {
      positions[i] = as.numeric(positions[i])
    }
  }

  # Generate the state estimation
  if (!is.na(positions[1]) && positions[1] < 0) {
    #' Hiker 1 is eaten
    print(paste("hiker 1 is eaten!"))
    s.est = rep(0,NSTATES)
    s.est[-positions[1]] = 1
  } else if (!is.na(positions[2]) && positions[2] < 0) {
    #' Hiker 2 is eaten
    print(paste("hiker 2 is eaten!"))
    s.est = rep(0,NSTATES)
    s.est[-positions[2]] = 1
  } else {
    # Estimate state with forward algorithm
    s.est = state.estimate(s.old, t.mtrx,e.mtrxs,readings)

    # If hikers are not eaten, Croc is not at their positions
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
  # Most probably location for Croc
  goal = which(s.est == max(s.est),arr.ind = TRUE)
  
  print(paste("most probable state:",goal))

  # Find shortest path to goal
  path = bfs(cur,goal,generate.adjacency.mtrx(edges))
  
  # Manual step or not
  manual = FALSE

  if (!manual) {
    if (length(path) == 1) {
      #' We are at the goal!
      #' TODO: something more clever as mv2? For example, path leading to the
      #' second most probable state?
      mv1 = 0
      mv2 = 0
    } else if (length(path) == 2) {
      # One step left to goal
      mv1 = path[2]
      mv2 = 0
    } else {
      # Take two steps
      mv1 = path[2]
      mv2 = path[3]
    }
  } else {
    #' Stolen from manualWC 
    options=getOptions(positions[3],edges)
    print("Move 1 options (plus 0 for search):")
    print(options)
    mv1=readline("Move 1: ")
    if (mv1=="q") {stop()}
    if (!mv1 %in% options && mv1 != 0) {
      warning ("Invalid move. Search ('0') specified.")
      mv1=0
    }
    if (mv1!=0) {
      options=getOptions(mv1,edges)
    }
    print("Move 2 options (plus 0 for search):")
    print(options)
    mv2=readline("Move 2: ")    
    if (mv2=="q") {stop()}
    if (!mv1 %in% options && mv1 != 0) {
      warning ("Invalid move. Search ('0') specified.")
      mv2=0
    }
  }
  
  mv1 = as.numeric(mv1)
  mv2 = as.numeric(mv2)
  
  moveInfo$moves=c(mv1,mv2)
  
  # Mark nodes as searched
  if (mv1 == 0) {
    #' Current state is searched
    s.est[positions[3]] = 0
  } else if (mv2 == 0) {
    #' State mv1 is searched
    stopifnot(mv1 <= length(s.est))
    stopifnot(mv1 > 0)
    s.est[mv1] = 0 
  }

  s.est = normalize(s.est)
  moveInfo$mem$s.old = s.est

  return(moveInfo)
}

#' Generates an adjacency matrix from the edges
generate.adjacency.mtrx <- function(edges) {
  mtrx = matrix(0,nrow=NSTATES,ncol=NSTATES)

  for (i in 1:nrow(edges)) {
    from = edges[i,1]
    to = edges[i,2]
    
    #' Mark 'from' and 'to' as being neighbors
    mtrx[from,to] = 1
    mtrx[to,from] = 1
    mtrx[to,to] = 1
    mtrx[from,from] = 1
  }

  return (mtrx)
}

#' Performs BFS to find the shortest path from 'from' to 'to'
#' in the graph defined by the adjacency matrix 'adj.mtrx'
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
#' @param e.mtrxs emission matrices for salinity, phosphate and nitrogen
#' @param obs observations in this step
state.estimate <- function(s.old, t.mtrx, e.mtrxs, obs) {
  # Initialize the state estimation
  s.est = rep(0,NSTATES)

  # Run the forward algorithm
  for (i in 1:NSTATES) {
    sum = 0
    for (j in 1:NSTATES) {
      sum = sum + s.old[j]*t.mtrx[j,i] 
    }

    #' Emission probabilities for salinity, phosphate and nitrogen
    e.sal = e.mtrxs$salinity[i,n.interval(obs[1],MIN,MAX,DELTA)]
    e.pho = e.mtrxs$phosphate[i,n.interval(obs[2],MIN,MAX,DELTA)]
    e.nit = e.mtrxs$nitrogen[i,n.interval(obs[3],MIN,MAX,DELTA)]

    #' Total emission probabilitiy is the product of the individual
    #' probabilities (assuming they are independent)
    e = e.sal*e.pho*e.nit

    s.est[i] = sum*e
  }
  
  return (s.est)
}

#' Normalizes a vector with numbers so that the sum of the numbers
#' is one
normalize <- function(v) {
  tot.sum = sum(v)
  for (i in 1:length(v)) {
    v[i] = v[i] / tot.sum
  }
  return (v)
}

#' Generates a transition matrix T, where T[i,j] is the probability
#' that we transition to state j given that we are in state i,
#' assuming:
#' - Any move to a neighbor is equally probable
#' - We can transition from i to i
#'
#' @param edges a vector with the edges
#' @param n.states the number of states
generate.transition.matrix <- function(edges,n.states) {
  # Initialise the transistion matrix as the adjacency matrix
  transition.matrix = generate.adjacency.mtrx(edges)

  # Normalize the '1':s in the transition matrix to real probabilities
  for (i in 1:n.states) {
    # Number of neighbors of i
    neighbors = length(which(transition.matrix[i,] == 1))
    for (j in 1:n.states) {
      transition.matrix[i,j] = transition.matrix[i,j] / neighbors
    }
  }
  
  return (transition.matrix)
}

#' Generates an emission matrix E, where E[i,j] is the probability that
#' we observe a value within interval no. j given state i.
#' The first interval (j=1) is [-Inf,min], and the last interval is [max,Inf].
#' The other intervals are [min + delta*(j-2), min + delta*(j-1)].
#'
#' Example: min = 0, max = 20, delta = 5, then we have the 6 intervals:
#' 1: [-Inf, 0]
#' 2: [0,5]
#' 3: [5,10]
#' 4: [10,15]
#' 5: [15,20]
#' 6: [20, Inf]
#'
#' @param data a matrix with n rows (one row per state) and 2 columns.
#' The first column is the mean value and the second column is the
#' standard deviation.
#' @param min the minimum value
#' @param max the maximum value
#' @param delta the length of each interval
generate.emission.matrix <- function(data,min,max,delta) {
  # The number of intervals (the "+ 2" is for the cases < min and > max)
  n.intervals = (max-min)/delta + 2
  # The number of states
  n.states = nrow(data)
  # Initialize the emission matrix
  emission.matrix = matrix(nrow=n.states,ncol=n.intervals)

  # Special case for first and last intervals
  for (i in 1:n.states) {
    #' First "interval": P(x <= min)
    p.min = pnorm(min,data[i,1],data[i,2])
    emission.matrix[i,1] = p.min

    #' Last "interval": P(x >= max) = 1 - P(x <= max)
    p.max = pnorm(max,data[i,1],data[i,2])
    emission.matrix[i,n.intervals] = 1 - p.max
  }
  
  # Fill in the emission matrix for the middle intervals
  for (j in 2:(n.intervals-1)) {
    #' Find start and end of interval
    start = min + delta*(j-2)
    end = min + delta*(j-1)
    
    stopifnot(start >= min)
    stopifnot(end <= max)

    for (i in 1:n.states) {
      #' P(start <= x <= end) = P(x <= end) - P(x <= start)
      p.start = pnorm(start,data[i,1],data[i,2])
      p.end = pnorm(end,data[i,1],data[i,2])
      emission.matrix[i,j] = p.end - p.start
    }
  }

  return (emission.matrix)
}

#' Returns the interval index of i, for the intervals used in
#' generate.emission.matrix
n.interval <- function(i,min,max,delta) {
  n.intervals = (max - min)/delta + 2
  if (i < min)
    return (1)
  else if (i > max)
    return (n.intervals)
  else
    return (1 + floor((i-min)/delta))
}

# A small test function
test.emission.matrix <- function() {
  data = cbind(runif(NSTATES,100,200),runif(NSTATES,5,30))
  #print(generate.emission.matrix(data,100,200,5))

  data = cbind(runif(10,5,25),runif(10,1,2))
  print("data")
  print(data)
  e.m = generate.emission.matrix(data,-25,25,5)
  print(e.m)
  print("This should be filled with 1")
  for (i in 1:nrow(e.m)) {
    print(sum(e.m[i,]))
  }

}
