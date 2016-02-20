## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

#####Creates a "matrix" object that caches its inverse#####

makeCacheMatrix <- function(x = matrix()) {
        m_inverse <- NULL
        set <- function(y) {
                x <<- y
                m_inverse <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) m_inverse <<- inverse
        getInverse <- function() m_inverse
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}



## Write a short comment describing this function

#####For the "matrix" returned by makeCacheMatrix, the function below calculates
#its inverse###

cacheSolve <- function(x, ...) {
        m_inverse <- x$getInverse()
        if (!is.null(m_inverse)) {
                message("getting cached data")
                return(m_inverse)
        }
        mat <- x$get()
        m_inverse <- solve(mat, ...)
        x$setInverse(m_inverse)
        m_inverse
}

