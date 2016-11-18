pitch_markings <- function(col_pitch = "#1a1a1a", col_lines = "#cccccc", 
                           x_scale = 1, y_scale = 1, pitch_type = "full") {
  require(ggplot2)
  require(grid)
  
  # Adds soccer pitch markings to a ggplot plot. 
  # For instance: ggplot(aes(x = x, y = y)) + 
  #                 pitch_markings() + 
  #                 geom_point() +
  #                 theme_pitch()
  #
  # Assumes 100x100 coordinates.
  #
  # Args:
  #   col_pitch: The colour of the pitch background. Default is green.
  #   col_pitch: The colour of the line markings. Default is white.
  #   x_scale: Amount to scale x-coordinates if not using 100x100. Default is 1.
  #   y_scale: Amount to scale y-coordinates if not using 100x100. Default is 1.
  #   juego: Whether to include Juego de Posicion markings. Default is FALSE.  
  #
  # Returns:
  #   ggplot layer
  
  markings <- list(
    
    #BASIC RECTANGLE
    
    # Add pitch markings
    geom_rect(xmin = 0*x_scale, xmax = 100*x_scale,
              ymin = 0*y_scale, ymax = 100*y_scale,
              colour  = col_lines,
              fill  = col_pitch,
              alpha = 1)
    )
    
    if(pitch_type=="full") {
      
      #Add Ds
      markings <- list(markings,
      
      annotation_custom(grob=circleGrob(r=unit(1,"npc"),
                                        gp = gpar(col=col_lines, fill = col_pitch, lwd = 2)),
                        xmin=(88.5-7)*x_scale, xmax=(88.5+7)*x_scale, 
                        ymin=(50-7)*y_scale, ymax=(50+7)*y_scale),
      
      annotation_custom(grob=circleGrob(r=unit(1,"npc"),
                                        gp = gpar(col=col_lines, fill = col_pitch, lwd = 2)),
                        xmin=(11.5-7)*x_scale, xmax=(11.5+7)*x_scale, 
                        ymin=(50-7)*y_scale, ymax=(50+7)*y_scale)
      )
    }
    
    markings <- list(markings,
                     
      #Rest of penalty area and halfway line
    
      geom_rect(xmin = 83*x_scale, xmax = 100*x_scale,
                ymin = 21.1*y_scale, ymax = 79.9*y_scale,
                colour  = col_lines,
                fill = col_pitch,
                alpha = 1),
      
      
      geom_rect(xmin = 0*x_scale, xmax = 17*x_scale,
                ymin = 21.1*y_scale, ymax = 79.9*y_scale,
                colour  = col_lines,
                fill = col_pitch,
                alpha = 1),
    
    # Add 6 yards
    geom_rect(xmin = 94.2*x_scale, xmax = 100*x_scale,
              ymin = 36.8*y_scale, ymax = 63.2*y_scale,
              colour  = col_lines, fill = col_pitch,
              alpha = 1),
    geom_rect(xmin = 0*x_scale, xmax = 5.8*x_scale,
              ymin = 36.8*y_scale, ymax = 63.2*y_scale,
              colour  = col_lines, fill = col_pitch,
              alpha = 1),
    
    # Add goals
    geom_rect(xmin = 100*x_scale, xmax = 102*x_scale,
              ymin = 44.2*y_scale, ymax = 55.8*y_scale,
              colour  = col_lines,
              fill = col_pitch,
              alpha = 1),
    
    geom_rect(xmin = 0*x_scale, xmax = -2*x_scale,
              ymin = 44.2*y_scale, ymax = 55.8*y_scale,
              colour  = col_lines,
              fill = col_pitch,
              alpha = 1)
    )
    
  if(pitch_type=="full") {
    
    #Penalty spots etc.
  
    markings <- list(markings,
                     # Centre circle
                     annotation_custom(grob=circleGrob(r=unit(1,"npc"),
                                                       gp = gpar(col=col_lines, fill = col_pitch, lwd = 2)),
                                       xmin=(50-7), xmax=(50+7), 
                                       ymin=(50-7)*y_scale, ymax=(50+7)*y_scale),
                     # Centre spot
                     geom_rect(xmin = 49.8*x_scale, xmax = 50.2*x_scale,
                               ymin = 49.8*y_scale, ymax = 50.2*y_scale,
                               colour  = col_lines,
                               fill  = col_lines),
                    
                     geom_rect(xmin = 88.4*x_scale, xmax = 88.6*x_scale, # Pen spot
                               ymin = 49.8*y_scale, ymax = 50.2*y_scale,
                               colour  = col_lines,
                               fill  = col_lines),
                     
                     geom_rect(xmin = 11.4*x_scale, xmax = 11.6*x_scale, # Pen spot
                               ymin = 49.8*y_scale, ymax = 50.2*y_scale,
                               colour  = col_lines,
                               fill  = col_lines)
    )
  }
    
    markings <- list(markings,
                     # Halfway line
                     annotate("segment",
                              x = 50*x_scale, xend = 50*x_scale,
                              y = 0*y_scale, yend = 100*y_scale,
                              colour = col_lines))
  
  if (pitch_type=="juego") {
    
    # Add juego de posicion markings
    
    markings <- list(markings,
                     geom_segment(aes(x = 17, xend = 17, y = 0, yend = 100),  colour  = col_lines,
                                   linetype = "dotted",
                                  alpha = 1),
                     geom_segment(aes(x = 83, xend = 83, y = 0, yend = 100),  colour  = col_lines,
                                   linetype = "dotted",
                                  alpha = 1),
                     geom_segment(aes(x = 31.2, xend = 31.2, y = 0, yend = 21.1),  colour  = col_lines,
                                   linetype = "dotted",
                                  alpha = 1),
                     geom_segment(aes(x = 31.2, xend = 31.2, y = 79.9, yend = 100),  colour  = col_lines,
                                   linetype = "dotted",
                                  alpha = 1),
                     geom_segment(aes(x = 68.8, xend = 68.8, y = 0, yend = 21.1),  colour  = col_lines,
                                   linetype = "dotted",
                                  alpha = 1),
                     geom_segment(aes(x = 68.8, xend = 68.8, y = 79.9, yend = 100),  colour  = col_lines,
                                   linetype = "dotted",
                                  alpha = 1),
                     geom_segment(aes(x = 0, xend = 100, y = 36.8, yend = 36.8),  colour  = col_lines,
                                   linetype = "dotted",
                                  alpha = 1),
                     geom_segment(aes(x = 0, xend = 100, y = 63.2, yend = 63.2),  colour  = col_lines,
                                   linetype = "dotted",
                                  alpha = 1),
                     geom_segment(aes(x = 0, xend = 100, y = 79.9, yend = 79.9),  colour  = col_lines,
                                   linetype = "dotted",
                                  alpha = 1),
                     geom_segment(aes(x = 0, xend = 100, y = 21.1, yend = 21.1),  colour  = col_lines,
                                   linetype = "dotted",
                                  alpha = 1)
    )
  }
  
  return(markings)
}