## Caching the Inverse of a Matrix

## This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
		invertM = NULL
        
        set = function(y) {
                x <<- y
                invertM <<- NULL
        }
        get = function() x
        
        setInv = function(solve) invertM <<- solve
        getInv = function() invertM
        
        list(set = set, get = get,
             setInv = setInv,
             getInv = getInv)

}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
		## Return a matrix that is the inverse of 'x'
		invertM = x$getInv()
        
        if(!is.null(invertM)) {
                message("getting cached data")
                return(invertM)
        }
        
        data = x$get()
        invertM = solve(data, ...)
        
        x$setInv(invertM)
        invertM
}
