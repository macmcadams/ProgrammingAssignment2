## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#the matrix is passed in and m is set to NULL, get brings back the matrix
#set inverse should be called indirectly through the cacheSolve function below
#getinverse() gets set to what ever m is, m is outside of this global variable (NULL the first time the function is called directly with new input)

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL								
      set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x  						
        setinverse <- function(inverse) m <<- inverse 	 
        getinverse <- function() m  				 
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Write a short comment describing this function
#first check to see if inverse exists
#if m is null then calculate and set the inverse and assign to variable 'm'

cacheSolve <- function(x, ...) {
	m <- x$getinverse()				#assigns m as potential inverse
      if(!is.null(m)) {					#if m isn't null, it has been computed and send back data)
                message("getting cached data")
                return(m)
        }
        data <- x$get()					#if not exists run the get, then compute the inverse, set and return for availability above
        m <- solve(data, ...)				
        x$setinverse(m)
        m
        ## Return a matrix that is the inverse of 'x'
}
