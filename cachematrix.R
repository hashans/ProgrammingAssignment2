
## This function simply acts as a model for caching matrix.
## getters and setters for matrix and inversed matrix

makeCacheMatrix <- function(x = matrix()) {
	    cachedMatrix <- NULL
        set <- function(y) {
                x <<- y
                cachedMatrix <<- NULL
        }
        get <- function() x
        setInverseMatrix <- function(matrix) cachedMatrix <<- matrix
        getInverseMatrix <- function() cachedMatrix
        list(set = set, get = get,
             setInverseMatrix = setInverseMatrix,
             getInverseMatrix = getInverseMatrix)

}


## This function accepts makeCacheMatrix variable and returns
## inversed matrix with minimal compuation

cacheSolve <- function(x, ...) {
        inverseMatrix <- x$getInverseMatrix()
        if(!is.null(inverseMatrix)) {
                message("getting cached data")
                return(inverseMatrix)
        }
        data <- x$get()
        inverseMatrix <- solve(data)
        x$setInverseMatrix(inverseMatrix)
        inverseMatrix
}