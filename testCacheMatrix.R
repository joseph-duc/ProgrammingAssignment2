# Test cachematrix.R

source('cachematrix.R')

test.cachematrix <- function(m=matrix()) {
    if (is.matrix(m) && is.na(m)) {
        m <- rbind(c(1, -1/4), c(-1/4, 1))
    }
    
    cm <- makeCacheMatrix(m)
    inv_m <- cacheSolve(cm)
    print("Inverse matrix is:")
    print(inv_m)
    print("Input matrix * Inverse matrix is:")
    print(inv_m %*% m)
}

print("Use default matrix to test")
test.cachematrix()
print("test matrix 2x2")
test.cachematrix(rbind(c(1,0), c(0, 1)))
print("test matrix 3x3")
test.cachematrix(rbind(c(1,2,3), c(0,1,4), c(5,6,0)))
