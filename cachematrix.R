## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
	m<-NULL
	##  Sets the new matrix
  set<-function(y){
  	x<<-y
  	m<<-NULL
	}
	get<-function() x ## This returns the matrix that was input
	setinverse<-function(solve) m<<- solve ## set the inversed matrix
	getinverse<-function() m  ## get the inversed matrix
	list(set=set, get=get,
  	   setinverse=setinverse,
   	   getinverse=getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix. 
## If the inverse has already been calculated (and the matrix has not changed)
## , then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
	m<-x$getinverse()
	## Determine if inverse of matrix is already cached
  if(!is.null(m)){
   	  message("getting cached data")
     	return(m)
 	}
 	## Since the inverse of matrix was not cached, below will get inverse for matrix and cache 
  matrix<-x$get()
  m<-solve(matrix, ...)  ## Get matrix inverse
  x$setinverse(m)
  m ## Return a matrix that is the inverse of 'x'
        
}
