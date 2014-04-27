## makeCacheMatrix: This function creates a special "matrix" 
## object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) { ##return a matrix argument "x"
        i <- NULL                           ##if the cache is NULL
        set <- function(y) {                ## replace argument x with  
                x <<- y                     ## argument y for a new function
                i <<- NULL
        }
        get <- function() x                 ## get the function with x
        setinverse <- function(inverse) i <<- inverse ##setinverse
        getinverse <- function() i                    ##getinverse
        list(set= set, get = get,                     ##it returns a list with 4 items
             setinverse = setinverse,
             getinverse = getinverse
             )
}
## cacheSolve: This function computes the inverse of the 
## special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix 
## has not changed), then the cachesolve should retrieve the 
## inverse from the cache.

cacheSolve <- function(x, ...) {  ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()       ##query the x vector's cache
        if(!is.null(i)){          ##if the cache exists 
                message("getting cached data")  
                return(i)         ## return cache
        }
        data <- x$get()           ##if no cache exists
        i <- solve(data, ...)     ##compute the in inverse of the matrix here
        x$setinverse(i)           ##save the result in the x's cache
        i                         ##and return the result
}
