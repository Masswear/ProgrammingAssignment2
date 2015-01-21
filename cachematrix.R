## 1. Create an environment that stores a matrix and its inverse and allows other functions to access their contents
## --> makeCacheMatrix
## 2. Take the output of 'makeCacheMatrix and calculate, cache, and return the inverse of the matrix entered into
## 'makeCacheMatrix'. If the inverse has already been calculated, return the cached value
## --> cacheSolve

## create a list with 4 functions that write and retrieve the data of the input matrix and its inverse.
## Other functions can use these functions to access the objects that were defined in the environment of this function.

makeCacheMatrix <- function(x = matrix()) {
        ## initialize 'inv' as an empty vector that the 'cachesolve" function can use to store the inversion of 'x'
        inv <- NULL
        
        ## create a function that can be used to modify the matrix 'x' by assigning the new value 'y' to it. Because 'x'
        ## was defined in the parent environment of the 'set' function, we have to use the <<- operator, otherwise we
        ## create a new matrix 'x' in the environment of the 'set' function. When 'x' is changed, we have to set 'inv'
        ## back to 'NULL' to let the 'cachesolve' function know that the original matrix has changed. Again, we have to
        ## use the <<- operator to modify 'inv' in the parent environment.
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        
        ## create a function to retrieve the data of the matrix stored in 'x'. The use of a function returning 'x' instead
        ## of assigning 'x' directly ensures that 'get()' returns the current data of 'x', even when 'x' has changed using 
        ## the 'set' function
        get <- function() x
        
        ## create a function that 'cachesolve' uses to write the inversion of the matrix into 'inv'. Again, we have to 
        ## use the <<- operator to modify 'inv' in the parent environment. This function could be used to modify 'inv'
        ## without changing 'x', so be careful not to call it manually
        setinverse <- function(inverse) inv <<- inverse
        
        ## create a function to retrieve the current value of 'inv'. Similarly to the 'get' function, the use of a function
        ## returning 'inv' instead of assigning 'inv' directly ensures that 'getinverse()' returns the
        ## current data of 'inv', even when 'inv' has changed using the 'setinverse' function
        getinverse <- function() inv
        
        ## return a list where the functions defined above are the items of the list. When
        ## binding this output to a vector (eg, 'z'), we are able to call the functions by subsetting 'z' with the
        ## respective item of the list (eg, 'z$getinverse()')
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## calculate, cache and return the inverse of a matrix entered into 'makeCacheMatrix' after checking that it has not been
## calculated before. If it has already been calculated, return the cached data.

cacheSolve <- function(x, ...) {
        ## retrieve the content of 'inv' stored in the environment created by 'makeCacheMatrix' (using the 'getinverse'
        ## function defined in 'makeCacheMatrix') and store it in a new (local) vector also called 'inv'
        inv <- x$getinverse()
        
        ## check if the inverse matrix has already been calculated ('inv' is not 'NULL') and if yes, return a message and
        ## the data stored in 'inv'
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        
        ## This is the 'else' part if the inverse matrix has not yet been calculated ('inv' is 'NULL').
        
        ## retrieve the matrix stored in the environment of 'makeCacheMatrix' and store it in a new (local)
        ## vector 'data' using the 'get' function defined in 'makeCacheMatrix'.
        data <- x$get()
        
        ## inverse the matrix stored in 'data' using the 'solve' function. Additional arguments can be passed down from
        ## the 'cacheSolve' function call by using '...'. This is dangerous when only the additional arguments change, but
        ## the matrix to be inversed stays the same: In this case, 'cachesolve' returns the cached data rather than
        ## computing a new matrix with the changed arguments. The inverse is stored in the local copy of 'inv'.
        inv <- solve(data, ...)
        
        ## Pass the inversed matrix to the 'inv' vector stored in the environment of 'makeCacheMatrix' using the
        ## 'setinverse' function defined in 'makeCacheMatrix'.
        x$setinverse(inv)
        
        ## return the inversed matrix.
        inv
}
