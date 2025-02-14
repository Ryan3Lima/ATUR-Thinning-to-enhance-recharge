
calculate_start_canopy <- function(end_canopy_cover) {
  # Ensure input is numeric and within valid range
  if (!is.numeric(end_canopy_cover) || any(end_canopy_cover < 0)) {
    stop("Input must be a non-negative numeric value.")
  }
  
  # Calculate starting canopy cover before 20% reduction
  start_canopy_cover <- end_canopy_cover / 0.8
  
  return(start_canopy_cover)
}

## Example usage
# Historic Min 10%
calculate_start_canopy(10)

# ideal min = 23 %
calculate_start_canopy(23)

calculate_start_basal_area <- function(end_basal_area) {
  # Ensure input is numeric and non-negative
  if (!is.numeric(end_basal_area) || any(end_basal_area < 0)) {
    stop("Input must be a non-negative numeric value (m²/ha).")
  }
  
  # Calculate starting basal area before 30% reduction
  start_basal_area <- end_basal_area / 0.7
  
  return(start_basal_area)
}

## Example usage

# Historic Min = 9.2 m2/ha
calculate_start_basal_area(9.2)

# historic max = 18 m2/ha
calculate_start_basal_area(18)



# Define suitability function for canopy cover
suitability_canopy <- function(x) {
  ifelse(x < 1, NA,  
         ifelse(x <= 12.5, 1,  
                ifelse(x <= 28.75, (9/16.25) * (x - 12.5) + 1,  
                       ifelse(x <= 100, 10, NA)  
                )
         )
  )
}

# Define suitability function for basal area (m²/ha)
suitability_basal_area <- function(x) {
  ifelse(x < 1, NA,  
         ifelse(x <= 13.14, 1,  
                ifelse(x <= 25.71, (9/12.57) * (x - 13.14) + 1,  
                       ifelse(x <= 100, 10, NA)  
                )
         )
  )
}

# Generate values for both plots
x_canopy <- seq(0, 105, by = 1)
y_canopy <- sapply(x_canopy, suitability_canopy)

x_basal <- seq(0, 105, by = 1)
y_basal <- sapply(x_basal, suitability_basal_area)

# Plot both graphs side by side
par(mfrow=c(1,2))  # Arrange plots side by side

# First plot: Suitability vs. Canopy Cover
plot(x_canopy, y_canopy, type="l", col="blue", lwd=2, 
     xlab="Canopy Cover", ylab="Suitability", 
     main="Suitability vs. Canopy Cover", 
     ylim=c(0, 11))
grid()

# Second plot: Suitability vs. Basal Area (m²/ha)
plot(x_basal, y_basal, type="l", col="red", lwd=2, 
     xlab="Basal Area (m²/ha)", ylab="Suitability", 
     main="Suitability vs. Basal Area", 
     ylim=c(0, 11))
grid()

# Reset plotting parameters
par(mfrow=c(1,1))
