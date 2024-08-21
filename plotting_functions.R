# Plot.gwr.coefs is a general function by Daniel Beene for plotting GWR and MGWWR results from the GWModel package,
# Department of Geography and Environmental Studies, University of New Mexico (dbeene@unm.edu).

plot.gwr.coefs = function(gwr.model.SDF, variable.name, tvalues) {
  # determine which observations are significant from via the t-values
  tval = tvalues
  insignif = tval >= -1.96 & tval <= 1.96
  
  # create the background and main layer with shaded polygons
  p = tm_shape(gwr.model.SDF) +
    tm_polygons(variable.name, palette = "RdYlBu", title = variable.name, alpha = 0.7) +
    tm_borders(lwd = 0.25) +  # Add borders for all polygons
    
    tm_layout(legend.outside = TRUE,  # Place legend outside the plot
              legend.position = c("right", "top"),  # Position the legend to the right
              legend.outside.size = 0.3,  # Adjust the legend width as needed
    )  
  
  # check if there are any non-empty units for the insignificant observations
  if (any(insignif)) {
    # now add the t-values layer
    p = p +
      tm_shape(gwr.model.SDF[insignif,]) +  # Use insignif for insignificant observations
      tm_polygons("black", alpha = 0.75)
  }
  
  return(p)
}


# EOF
