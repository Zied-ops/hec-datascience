mu = c()

for(i in 1:1000) {
    x = rnorm(30, -.3, 1.2)
    s = mean(x)
    mu=c(mu, s)
}

print(mu)