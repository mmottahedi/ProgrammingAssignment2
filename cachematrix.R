## The functions will setup a new matrix, calculate the inverse once to
## avoid repeating computationally expensive calculations.


#this function makes the matrix, it can calculate the inverse and return 
#the inverse if it was calculated before
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y){
                x <<- y
                m <<- Null
        }
        get <- function () x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m 
        list(list =set, get = get, 
             setinverse = setinverse,
             getinverse = getinverse)
}


## this function returns the inverse from the cache if it was calculated 
## before. else calculates and returns the inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if (!is.null(m)){
                message("getting inverse matrix")
                return(m)
        }
        data <- x$get()
        m <- solve(data)
        x$setinverse(m)
        m
        
}
