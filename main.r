# 1. Constructor for Circle
Circle <- function(radius) {
  if (!is.numeric(radius) || radius <= 0) {
    stop("radius must be a positive number")
  }
  obj <- list(radius = radius)
  class(obj) <- "Circle"
  obj
}

# 2. print method for Circle
print.Circle <- function(x, ...) {
  radius <- x$radius
  area   <- pi * radius^2
  cat("Circle object\n")
  cat(" • Radius:", radius, "\n")
  cat(" • Area:  ", round(area, 4), "\n")
  invisible(x)
}

# — Example usage —
c1 <- Circle(3)
print(c1)
# Or simply:
c1
