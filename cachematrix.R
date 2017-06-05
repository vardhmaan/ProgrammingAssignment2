#### Here we made two functions makeCacheMatrix and #cacheSolve that help us in cache the inverse of matrix (which is # Invertible in Nature)
####

## This function creates a special "matrix" object that can cache ##its inverse.

makeCacheMatrix <- function(x = matrix()) {  m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        getMatrix <- function() x
        setinverseMatrix <- function(inverse) m <<- inverse
        getinverseMatrix <- function() m
        list(set = set, getMatrix = getMatrix,
             setinverseMatrix = setinverseMatrix,
             getinverseMatrix = getinverseMatrix)

}


#### This function computes the inverse of the special "matrix" #returned by makeCacheMatrix above. If the inverse has already #been calculated (and the matrix has not changed), then the #cachesolve should retrieve the inverse from the cache.
####
cacheSolve <- function(x, ...) {      m <- x$getinverseMatrix()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$getMatrix()
        m <- solve(data, ...)
        x$setinverseMatrix(m)
        m
        ## Return a matrix that is the inverse of 'x'
}
