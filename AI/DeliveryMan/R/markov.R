markovWC=function(moveInfo,readings,positions,edges,probs) {
  print(moveInfo)
  print("readings:")
  print(readings)
  print("positions:")
  print(positions)
  print("edges")
  print(edges)
  print("probs")
  print(probs)
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
  moveInfo$moves=c(mv1,mv2)
  return(moveInfo)
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

test.emission.matrix <- function() {
  data = cbind(runif(40,100,200),runif(40,5,30))
  #print(generate.emission.matrix(data,100,200,5))

  data = cbind(runif(10,5,25),runif(10,1,2))
  print("data")
  print(data)
  e.m = generate.emission.matrix(data,5,25,5)
  print(e.m)
  print("This should be filled with 1")
  for (i in 1:nrow(e.m)) {
    print(sum(e.m[i,]))
  }

}
