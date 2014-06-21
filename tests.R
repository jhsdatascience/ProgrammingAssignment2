# Tests for cachematrix.R VERY RAW CODE

source('cachematrix.R')

getInvertibleMatrix <- function(mat = matrix(), iter = 10L) {
    if (is.na(mat[1,1])) mat <- matrix(sample(1:9), 3,3)
    tryCatch(solve(mat), error=function(cond) {
            if (iter == 0) stop("Max iterations reached")
            mat <<- getInvertibleMatrix(mat, iter = iter - 1L)
    })
    # Returns . . .
    mat
}

# Get invertible matrix
m <- getInvertibleMatrix()
mInv <- solve(m)

# Does matrix intitialize?
cm <- makeCacheMatrix(m)

# Test CacheMatrix methods . . .
if (all(cm$get() == m)) {
    print("get() method works")
} else {
    stop()
}
mOther <- getInvertibleMatrix()
cm$set(mOther)
if (all(cm$get() == mOther)) {
    print("set() method works")
} else {
    stop()
}
cm$set(m)
if (is.null(cm$getInvCache())) {
    print("inv NULL to begin with")
} else {
    stop()
}
cm$setInvCache()
print("Should read 'Setting cache . . .' above but not below")
cm$setInvCache()
if (all(cm$getInvCache() == mInv)) {
    print("inverse set correctly")
}

# Testing cacheSolve:
inv <- cacheSolve(cm)
if (all(inv == cm$getInvCache())) {
    print("cachesolve works")
}

