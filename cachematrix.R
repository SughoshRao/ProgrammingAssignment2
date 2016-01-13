## Program caches the inverse of a matrix to be reused
## so long as the original matrix is not changed.

## makeCacheMatrix: This function creates a special "matrix" 
## object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) 
{
    inv <- NULL
    set <- function(y) 
	{
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

## cacheSolve: This function computes the inverse of the special 
## "matrix" returned by makeCacheMatrix above. If the inverse has 
## already been calculated (and the matrix has not changed), then 
## the cachesolve retrieves the inverse from the cache.
cacheSolve <- function(x, ...) 
{
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data.")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    inv
}

## Sample run:
## > source("cachematrix.R")
## creating a new matrix (lets call it 'a')
## > x <- rbind(c(1, -1/4), c(-1/4, 1))
## > m <- makeCacheMatrix(x)
## > cacheSolve(m)
##           [,1]      [,2]
## [1,] 1.0666667 0.2666667
## [2,] 0.2666667 1.0666667
## Calling cacheSolve again with matrix 'a' unchanged
## > cacheSolve(m)
## getting cached data.
##           [,1]      [,2]
## [1,] 1.0666667 0.2666667
## [2,] 0.2666667 1.0666667

## creating a new matrix 'b'
## > x <- rbind(c(1, -1/2), c(-1/2, 1))
## > m <- makeCacheMatrix(x)
## > cacheSolve(m)
##           [,1]      [,2]
## [1,] 1.3333333 0.6666667
## [2,] 0.6666667 1.3333333
## Calling cacheSolve again with matrix 'b' unchanged 
## > cacheSolve(m)
## getting cached data.
##           [,1]      [,2]
## [1,] 1.3333333 0.6666667
## [2,] 0.6666667 1.3333333