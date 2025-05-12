# load package plotly for interactive 3d plots
library(plotly)

# set seed for reproducibility
set.seed(42)

#-------------------------------------------------------------------------------
# HERE YOU CAN CHANGE VALUES TO ALTER PLOT DATA

# change for example the intercept and the slopes
# when finished, run all the code below...

n <- 100 # number of datapoints
b_0 = 10 # intercept
b_1 = 0.5 # slope x1
b_2 = -0.5 # slope x2

mean_error <- 0
sd_errors <- 2 # make bigger for larger residuals

min_x = 0 # lower range of x
max_x = 20 # upper range of x

x1 <- runif(n, min = min_x, max = max_x)
x2 <- runif(n, min = min_x, max = max_x)

y <- b_0 + b_1 * x1 + b_2 * x2 + rnorm(n, mean = mean_error, sd = sd_errors)

#-------------------------------------------------------------------------------
#linear regression model
model <- lm(y ~ x1 + x2)

# data
x1_seq <- seq(min_x, max_x, length.out = 30)
x2_seq <- seq(min_x, max_x, length.out = 30)
grid <- expand.grid(x1 = x1_seq, x2 = x2_seq)
grid$y <- predict(model, newdata = grid)


#-------------------------------------------------------------------------------
# HERE YOU CAN CHANGE PARAMETERS TO ALTER PLOT 

# plot
plot_ly() |>
  add_markers(x = x1, y = x2, z = y,
              marker = list(size = 3),
              name = "Data Points",
              showlegend = FALSE) |>
  # add_markers(x = x1, y = x2, z = 0,
  #             marker = list(color = "black", size = 3, opacity =  0.2),
  #             name = "Projected Data Points",
  #             showlegend = FALSE) |>
  add_surface(x = ~x1_seq, 
              y = ~x2_seq, 
              z = matrix(grid$y, nrow = length(x1_seq), byrow = TRUE),
              opacity = 0.2,
              surfacecolor =  y,
              # colorscale = list(c(0, 1), c("lightblue", "black")),
              colorscale = "Cool",
              showscale = FALSE,
              name = "Plane") |>
  layout(scene = list(
    xaxis = list(title = "x1"),
    yaxis = list(title = "x2"),
    zaxis = list(title = "y")),
    title = "Linear Regression Plane")
#-------------------------------------------------------------------------------


# calculate b0 and b1
cor(d$x1, d$x2)

cor(d$y, d$x2)
mx2 <- lm(scale(y) ~ scale(x2) , data = d)
summary(mx2)
