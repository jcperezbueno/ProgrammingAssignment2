## Matrix inversion is usually a costly computation and their may be some benefit to caching the inverse of a matrix rather than ## compute it repeatedly (there are also alternatives to matrix inversion that we will not discuss here).

## Assignment is to write a pair of functions that cache the inverse of a matrix.


## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse
# 1. set the value of the matrix (set)
# 2. get the value of the matrix (get)
# 3. set the value of the inverse of the matrix (setinverse)
# 4. get the value of the inverse of the matrix (getinverse)
# 5. creates a list

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve: This function returns the inverse of the matrix (it assumes it is squared and invertible)
# 1. checks if the inverse has already been computed
# 2. if so, it gets the result directly
# 3. if not, it gets the value of the matrix
# 4. it computes the invers (solve function)
# 5. stores the value of the inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data)
        x$setinverse(m)
        m
}


## To check performance
#set.seed(1)

#ex_matrix <- matrix(rnorm(1000000),nrow=1000)
 
#list_matrix <- makeCacheMatrix(ex_matrix)

#print(system.time( for (i in 1:10) {cacheSolve(list_matrix)} ))

#print(system.time( for (i in 1:10) {solve(ex_matrix)} ))





