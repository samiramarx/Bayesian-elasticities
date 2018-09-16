#=========================================
# MSc Transporte Economics
# TRAN 5911 Dissertation
# Samira Marx
#=========================================

# 3D Plot

# lets first simulate a bivariate normal sample
library(MASS)
bivn <- mvrnorm(10000000, mu = c(0, 0), Sigma = matrix(c(1, .5, .5, 1), 2))

# now we do a kernel density estimate
bivn.kde <- kde2d(bivn[,1], bivn[,2], n = 50)

# now plot your results
contour(bivn.kde)
image(bivn.kde)
persp(bivn.kde, phi = 30, theta = 45)

# fancy contour with image
image(bivn.kde); contour(bivn.kde, add = T)

# fancy perspective
persp(bivn.kde, phi = 30, theta = 45, shade = .4, border = NA, ticktype = "detailed", box = TRUE)
