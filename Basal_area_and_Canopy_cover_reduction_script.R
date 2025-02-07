calculate_start_canopy <- function(end_canopy_cover) {
  # Ensure input is numeric and within valid range
  if (!is.numeric(end_canopy_cover) || any(end_canopy_cover < 0)) {
    stop("Input must be a non-negative numeric value.")
  }
  
  # Calculate starting canopy cover before 20% reduction
  start_canopy_cover <- end_canopy_cover / 0.8
  
  return(start_canopy_cover)
}




# Example usage
calculate_start_canopy(35)# If end canopy cover is 40%, this returns the starting cover


calculate_start_basal_area <- function(end_basal_area) {
  # Ensure input is numeric and non-negative
  if (!is.numeric(end_basal_area) || any(end_basal_area < 0)) {
    stop("Input must be a non-negative numeric value (m²/ha).")
  }
  
  # Calculate starting basal area before 30% reduction
  start_basal_area <- end_basal_area / 0.7
  
  return(start_basal_area)
}

# Example usage
calculate_start_basal_area(18)  # If end basal area is 20 m²/ha, this returns the starting basal area



even_sequence_BA = seq(from = 13.14, to = 25.71, length.out = 10)
even_sequence_BA

even_sequence_CC = seq(from = 12.5, to = 28.75, length.out = 10)
even_sequence_CC
