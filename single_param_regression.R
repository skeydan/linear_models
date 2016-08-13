library(ggplot2)
data(diamonds)

diamonds = head(diamonds, 10)

y = diamonds$price
x = diamonds$carat

# mean only regression
mean(y)
coef(lm(y ~ 1))

# center before doing regression through the origin
yc = y - mean(y)
xc = x - mean(x)

#same as yc %*% xc / xc %*% xc
# dot products formula for centered matrices
sum(yc * xc) / sum(xc * xc)

# check with lm
coef(lm(yc ~ xc - 1))

# correlation coefficient formula for centered matrices
cor(x, y) * sd(y) / sd(x)

# regression with intercept
beta1 = cor(x, y) * sd(y) / sd(x)
beta0 = mean(y) - beta1 * mean(x)
c(beta0, beta1)
# versus estimate with lm
coef(lm(y ~ x))
# beta1 = coefficient of centered regression through the origin
sum(yc * xc) / sum(xc^2)
