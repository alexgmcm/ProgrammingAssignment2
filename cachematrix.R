makeCacheMatrix <- function(x = matrix()) {
		#This function creates a special "matrix" object that can cache its inverse.
		inverse<-NULL
		set <- function(y){
			x<<-y
			inverse<<-NULL
		}
		get <- function() x
		setinverse <- function(inv) inverse<<-inv
		getinverse <- function() inverse
		list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}



cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        # This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
        #If the inverse has already been calculated (and the matrix has not changed),
        # then the cachesolve should retrieve the inverse from the cache.
        inverse<-x$getinverse()
        if(!is.null(inverse)){
        	message("getting cached data")
            return(inverse)
        }
        data<-x$get()
        inverse<-solve(data, ...) 
        #ellipses in case we want to pass additional arguments to solve()
        #note the ellipses in the function def that implies this functionality
        #is desired
        x$setinverse(inverse) #set it for caching
        inverse #return
}
