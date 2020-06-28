# Caching the Inverse of a Matrix:
# Matrix inversion is usually a costly computation and 
# there may be some benefit to caching the inverse of a matrix rather than compute it repeatedly 
# The assignment is to write a pair of functions that cache the inverse of a matrix.

# The function creates a matrix object that caches the inverse.

makeCacheMatrix <- function(matriz = matrix()) {
        inv = NULL
        set <- function(y) {
                matriz <<- y
                inv <<- NULL
        }
        get <- function() { matriz }
        setInverse <- function(inverse) { inv <<- inverse }
        getInverse <- function() { inv }
        list(set = set, 
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


# The function computes the inverse of the generated matrix  
# If the inverse has already been calculated, and there was no change, then it should recover the inverse from the cache.

cacheSolve <- function(matriz, ...) {
        inv <- matriz$getInverse()
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        mat <- matriz$get()
        inv <- solve(mat, ...)
        matriz$setInverse(inv)
        inv
}