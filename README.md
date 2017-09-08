# 2017_fall_AI
Lab assignment

- A theoretical overview of the A* algorithm, including an explanation of optimality conditions.  
- A discussion of the A* search algorithm you implemented, including a discussion of your heuristic, and whether you used a tree or graph search. Explain why you made the choices you made.  
- A discussion of not A* strategies you made us of to improve your performance. Explain why you used these strategies.  
Explain Seungsu's BFS algorithm to decide the order of packages to pick up:

The A* algorithm is used to find a path from a starting point to the destination. The Delivery Man game has 5 packages to be delivered, which means there are 10 total "destinations" to be visited. We need to determine in which order the 10 points will be visited, and perform A* to find the paths between those points. 

One way to determine the order is to simply go the package closest to the current location. For example, in the following situation (insert picture below), the delivery man will first visit the package in (x,y) because it is closest to his starting point (1,1). He will then go to (x,y) to drop off the first package. Then, his next destination will be the closest package to him at that point, which is at (x,y). By always selecting the closest package to visit next, the delivery man may seem to travel in short distances at first. However, this method does not consider the distance between the pickup location of a package and its drop-off location. Thus, this method does not always guarantee the shortest total distance traveled by the delivery man.

What we did was...
