## An R script with two functions to cache the inverse of a matrix
## Usage is as follows after sourcing the cachematrix.R file:
##      list <- makeCacheMatrix(matrix)
##      cacheSolve(list)
##      Repeat as necessary and now it should use the cached value


## The makeCacheMatrix function returns the list of functions 
## set, get, setinverse, getinverse that allow for caching of
## the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) i <<- solve
        getinverse <- function() i
        list(set = set, get = get, 
                setinverse = setinverse,
                getinverse = getinverse)
        
}


## Function that returns the inverse of a matrix
## Expects as input the list of functions returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)) {
                message("Getting cached data")
                return(i)
        }
        data <- x$get() 
        i <- solve(data, ...)
        x$setinverse(i)
        i 
}
