## Functions cache the inverse of a matrix.


## Create a matrix object that will cache its own inverse.

makeCacheMatrix <- function(x = matrix()) {
	##Start inverse property
	i<-NULL
	
	## Matrix Setup
	set<-function(matrix){
		m<<-matrix
		i<<-NULL
	}
	## Retrieve the matrix
	get<-function(){
		## Return Matrix
		m
	}
	## Set inverse matrix
	setinverse<-function(inverse){
		i<<-inverse
	}
	## Retrieve inverse
	getinverse<-function(){
		## return inverse
		i
	}
	## list of methods
	list(set = set, get = get, 
	setInverse = SetInverse, 
	getInverse = getInverse)
}


## As noted in directions: Compute the inverse of the special matrix returned by "makeCacheMatrix"
## above. If the inverse has already been calculated (and the matrix has not
## changed), then the "cachesolve" should retrieve the inverse from the cache.


cacheSolve <- function(x, ...) {
        
        ## Return a matrix that is the inverse of 'x'
        m<-x$getinverse()
        
        ##return inverse if set
        if( !is.null(m)) {
        	message("cached data")
        	return(m)
        }
        ## Matrix from object
        data<-solve(data)%*%data
        
        ## Inverse of object
        x$setInverse(m)
        
        ## Return matrix
        m
}
