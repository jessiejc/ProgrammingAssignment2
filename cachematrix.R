## these two functions help us to find out if there is already a result(inverse matrix) we want 
## that has already existed in x
## if the answer is yes, we simply take the result we want and store it in makeCacheMatrix
## thus, we save much time
## however, if the answer is no, we compute it ourselves and store it in makeCacheMatrix


## makeCacheMatrix creates a matrix that can store the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <-NULL
        set <- function(y){
                X <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <-function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## cacheSolve returns an inverse matrix if there is an inverse matrix in x,
## if not, cacheSolve computes the inverse matrix and returns the result

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv1 <- x$getinv()
        if(!is.null(inv1)){
                message("getting cached inverse matrix")
                return(inv1)
        }
        matrix1 <- x$get()
        inv1 <- solve(matrix1,...)
        x$setinv(inv1)
        inv1
}
