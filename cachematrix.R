makeCacheMatrix<-function(X = matrix()){
I<-NULL ## inatializes the inverse of a matrix to null matrix
set<-function(Y){
X<<-Y  ## sets the value of the matrix
I<<-NULL
}
get<-function()X ## gets the value of the matrix

setinverse <- function(inverse) I <<- inverse ## sets the inverse of the matrix
getinverse <- function() I ## gets the inverse of the matrix if it exits
list(set=set, get= get,setinverse = setinverse, getinverse = getinverse)
}

cachesolve <- function(X,...){ ## looks for inverse of the given matrix
I <- X$getinverse() ## gets inverse from the previous list if it exists
if(!is.null(I)){
message("getting cached data")
return(I) 
}
data <- X$get() ## if the inverse does not exists, sets the matrix to calculate its inverse
I <- solve(data,...) ## solves the inverse
X$setinverse(I) ## records on the list for future use
I ## returns the inverse after calculation
}
