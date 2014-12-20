## The overall description of what functions do is given below.

## 1. makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## 2. cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix function. 
##If the inverse has already been calculated (and the matrix has not changed), then the cachesolve will retrieve the inverse from the cache.


## The first function, makeCacheMatrix creates a special "Matrix", which is really a list containing a function to
##    1. set the value of the Matrix ie  set function.
##    2. get the value of the Matrix ie  get funtion.
##    3. set the value of the Inverse ie setInverse function.
##    4. get the value of the Inverse ie getInverse function.
makeCacheMatrix <- function(x = matrix()) {
   m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInverse <- function(Inverse) m <<- Inverse
        getInverse <- function() m
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}

##The following function calculates the inverse of the special "Matrix" created with the above function. 
##However, it first checks to see if the inverse has already been calculated. 
##If so, it gets the inverse from the cache and skips the computation. 
##Otherwise, it calculates the Inverse of the data and sets the value of the Inverse in the cache via the setmean function.

cacheSolve <- function(x, ...) {
        m <- x$getInverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data)
        x$setInverse(m)
        m
}
