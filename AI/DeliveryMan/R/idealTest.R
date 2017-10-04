
testIdeal =function(n){
	stats = c()
	for(i in 1:n){
		step = runWheresCroc(makeMoves=idealWC,plot=F)
		stats = c(stats,step)
	}
	printStats(stats)
}

idealWC =function(moveInfo,readings,positions,edges,probs){
	CrocPos = positions[1]  #Where the Croc is.
	MyPos   = positions[4]  #Where I am.
	if(CrocPos == MyPos){ 	#we meet!
		moveInfo$moves = c(0,0)	
		return(moveInfo)
	}
	
	path = bfs(CrocPos,MyPos, edges) 

	if(length(path)==1){
		moveInfo$moves = c(path[1],0)
		return(moveInfo)
	}
	moveInfo$moves = c(path[1],path[2])
	return(moveInfo)
}
bfs = function(CrocPos, MyPos, edges){
	frontier = list(MyPos)
	visited  = list()
	previous = vector("list",40)
	path = list(CrocPos)
	
	if(CrocPos == MyPos){
		print("???")
		return (list())
	}
	while(TRUE){
		now = frontier[1]	
		visited = c(visited,now)
		frontier[1] = NULL											#remove now from the frontier
		
		nextFront = c(which(edges[,1]==now),which(edges[,2]==now))	# where to go for next step.
		nextFront = candidates(nextFront,edges,now)

		if(CrocPos %in% nextFront){
			previous[CrocPos] = now
			break
		}
		nextFront = setdiff(nextFront,visited)						# remove the visited nodes.
		if(length(nextFront)==0){									# nowhere to go.
			next
		}
		for(i in 1:length(nextFront)){
			tmp = nextFront[[i]]
			previous[tmp]=now
		}															# mark the previous node.
		frontier = c(frontier,nextFront)
		frontier = unique(frontier)									# remove repeated nodes.	
	}
	
	while(TRUE){
		if(path[1]==MyPos){
			path[1]=NULL
			return(path)
		}	
		path = c(previous[path[[1]]],path)
	}
}

candidates = function(cands,edges,now){
	answer = list()
	for(i in 1:length(cands)){
		if(edges[cands[i],1]==now){
			answer = c(answer,edges[cands[i],2])
		}
		else if(edges[cands[i],2]==now){
			answer = c(answer,edges[cands[i],1])
		}
	}
	return(answer)
}

