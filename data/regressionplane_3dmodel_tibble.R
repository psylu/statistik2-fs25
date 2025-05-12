library(tidyverse)
#-------------------------------------------------------------------------------
# enter data
d <- tibble(x1 = c(1, 3, 5, 7, 9),
            x2 = c(5, 2, 6, 8, 2),
            y = c(1, 5, 8, 6, 9))

## fit multiple regression model
m_x1_x2 <- lm(y ~ x1 + x2, d = d)
summary(m_x1_x2)

#-------------------------------------------------------------------------------
# load package plotly for interactive 3d plots
library(plotly)

# create data grid (estimated y values) 
min_x = 0 # lower range of x
max_x = 10 # upper range of x
x1_seq <- seq(min_x, max_x, length.out = 11)
x2_seq <- seq(min_x, max_x, length.out = 11)
grid_m_x1_x2 <- expand.grid(x1 = x1_seq, x2 = x2_seq)
grid_m_x1_x2$y <- predict(m_x1_x2, newdata = grid_m_x1_x2)

# plot
plot_ly() |>
  add_markers(x = d$x1, y = d$x2, z = d$y,
              marker = list(size = 5),
              name = "Data Points",
              showlegend = FALSE) |>
  add_markers(x = 0, y = 0, z = m_x1_x2$coefficients[1], # plot intercept
              marker = list(size = 5),
              name = "Data Points",
              showlegend = FALSE) |>
  # add_markers(x = x1, y = x2, z = 0, # projections of points on the x1x2 plane
  #             marker = list(color = "black", size = 3, opacity =  0.2),
  #             name = "Projected Data Points",
  #             showlegend = FALSE) |>
  add_surface(x = ~x1_seq, 
              y = ~x2_seq, 
              z = matrix(grid_m_x1_x2$y, nrow = length(x1_seq), byrow = TRUE),
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

