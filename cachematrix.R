#makeCacheMatric would do the following:
#1. set the matrix
# 2. get the matrix
# 3. set the inverse of the matrix
# 4. get the inverse of the matrix



makeCacheMatrix <- function(x = matrix()) {
        inv<-NULL #set the value of variable
        set<-function(y){
                x<<-y #assign value
                inv<<-NULL #assign a value to an object in an environment that is different from the current environment.
        }
        get<-function()x
        setInverse<-function(inverse)inv<<-inverse
        getInverse<-function()inv
        list(set = set,
              get = get,
              setInverse = setInverse,
              getInverse = getInverse)
                
 }


## cacheSolve function would return matrix that is inverse of x, retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv<- x$getInverse()
        if(!is.null(inv)){
              message("getting cached data")
              return (inv)
        }
        data<-x$get()
        inv<-solve(data,...)
        x$setInverse(inv)
        inv
}
