# Matrix inversion is usually a costly computation and there may
# be some benefit to caching the inverse of a matrix rather than
# computing it repeatedly. The following functions provide an
# effective way to calculate and cache the inverse of a matrix
# to avoid costly recomputation.
# Example usage:
# > mat = makeCacheMatrix(matrix(1:4, 2))
# > cacheSolve(mat)
# the inverse is calculated, stored, and returned
# > cacheSolve(mat)
# subsequent calls will return the cached value of the inverse

# This function takes an input matrix and creates a list
# which has get, set, getinverse, setinverse functions that get
# and set the matrix value, and get and set the inverse of the
# matrix, respectively. It is useful for caching the inverse
# value of a matrix.

makeCacheMatrix <- function(mat = matrix()) {
        inv <- NULL
        set <- function(y) {
                mat <<- y
                inv <<- NULL
        }
        get <- function() mat
        setinverse <- function(solve) inv <<- solve
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


# This function takes an input matrix, mat, and first checks for a
# cached value of its inverse; if one exists it is returned.
# If a cached value does not exist, it is first computed and stored
# before the inverse is returned.

cacheSolve <- function(mat, ...) {
    inv <- mat$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- mat$get()
    inv <- solve(data, ...)
    mat$setinverse(inv)
    inv
}
