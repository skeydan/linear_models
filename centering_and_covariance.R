data(mtcars)
head(mtcars)

#############################################################
###                 find best beta                        ###
#############################################################

x = cbind(1, mtcars$hp, mtcars$wt)
head(x)
y = mtcars$mpg

# solve a    %*% x = b for x
#       t(x) %*% ? = x      
solve(t(x) %*% x) %*% t(x) %*% y

# compare with result from lm
lm(mpg ~ hp + wt, data = mtcars)


#############################################################
###                 centering                             ###
#############################################################

n = nrow(x)
I = diag(rep(1, n))
H = matrix(1, n, n) / n

# I - H is the centering matrix
# I − 1(1_t 1)_inv 1_t y
# = I - 1/n I
# center the columns
xt = (I - H) %*% x

# check means of centered columns
apply(xt, 2, mean)

# alternatively, sweep out column means to center
xt2 = sweep(x, 2, apply(x, 2, mean))
apply(xt2, 2, mean)


#############################################################
###               covariance matrix                       ###
#############################################################

# covariance matrix: 1/(n-1) * <centered t(x)> * <centered x>
t(x) %*% (I - H) %*% x / (n - 1)

# check with var()
var(x)
