# cachematrix.R:
#   Methods for creating and using a CacheMatrix object,
#   a matrix which caches its inverse so as to avoid
#   repeated inverse calculations.
# 
# makeCacheMatrix:
#   Creates a CacheMatrix object
# methods:
#   get() - return matrix values
#   set(y) - Reinitialize matrix
#   getInvCache() - return cached matrix inverse, if it exists
#   setInvCache() - cache the inverse of the matrix, if it is
#                   not already cached
#
# cacheSolve:
#   Return cached inverse of a CacheMatrix, if it exists,
#   or calculate inverse, cache it, and return inverse values


# Create a CacheMatrix object
makeCacheMatrix <- function(x = matrix()) {
    invCache <- NULL
    set <- function(y) {
        # Reinitialize . . .
        x <<- y
        invCache <<- NULL
    }
    get <- function() x
    getInvCache <- function() invCache
    setInvCache <- function() {
        # Set the inverse cache if it is not already set
        if (is.null(getInvCache())) {
            message("Caching inverse . . .")
            invCache <<- solve(x)
        }
    }
    
    # Returns . . .
    list(set = set, get = get,
         getInvCache = getInvCache,
         setInvCache = setInvCache)
}


## Wrapper for returning the (possibly cached) inverse of a CacheMatrix

cacheSolve <- function(x, ...) {
    # Set the matrix cache (only if not already cached)
    x$setInvCache()
    
    # Returns . . .
    x$getInvCache()
}
