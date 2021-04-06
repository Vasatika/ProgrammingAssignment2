# The makeCacheMatrix creates a special matrix that can cache it's inverse using the set, 
# get, setinv and getinv to set the value of the matrix, get the value of the matrix, set 
# the value of the inverse and get the value of the inverse of the matrix. 

#The cacheSolve function uses solve() to retrieve the inverse of the cache matrix. 


makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
    x <<- y
    m <<- NULL
  }
    get <- function() x
    setinv <- function(inverse) m <<- inverse
    getinv <- function() m
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


cacheSolve <- function(x, ...) {
       m <- x$getinv()
       if(!is.null(m)) {
       message("getting cached data")
       return(m)
  }
       data <- x$get()
       m <- solve(data, ...) ## Return a matrix that is the inverse of 'x' using solve()
       x$setinv(m)
       m
}

#Assigned matrix with numbers 6, 7, 8, 9 to MatA with 2 rows and 1 columns. 
#Assigned the inverse cache matrix value to InvMatA.
#Solved to find inverse of cached matrix using cacheSolve(InvMatA).
#Displayed chached matrix data.

MatA <- matrix(c(6:9), 2, 2) #sqaure matrix of value 6, 7, 8, 9.
InvMatA <- makeCacheMatrix(MatA)
cacheSolve(InvMatA) #Inverse after computation 

cacheSolve(InvMatA) #Inverse returned from cached data
