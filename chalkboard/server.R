library(shiny)
library(ggplot2)
library(dplyr)
library(tweenr)
library(gganimate)


source("pitch_markings.R")

squadlist <- read.csv("squad-list.csv", stringsAsFactors = FALSE)

squadlist$player <- iconv(squadlist$player, "latin1", "ASCII//TRANSLIT")

positions.xy <- data.frame(position = c("GK", "DF", "MF", "FW"), xo = c(15, 30, 50, 75))


shinyServer(function(input, output) {
  
  sessionid <- as.character(runif(1, 1, 10000))
  
  #-------------------------------------------------------------------------
  
  #Initialise the dataframe with one of each event, drawn off the pitch
  event.data <- data.frame(xo=c(-10,-10,-10,-10,-10,-10,-10),
                           yo=c(-10,-10,-10,-10,-10,-10,-10),
                           xd=c(-10,-10,-10,-10,-10,-10,-10),
                           yd=c(-20,-20,-20,-20,-20,-20,-20),
                           label=c("","","","","","",""),
                           event=c("pass","dribble","shot","run","oppo pass","position","oppo position"),
                           frame=c(1,1,1,1,1,1,1))
  
  
  pitchCoordsCur <- "Origin"
  pitchCoordsOrigin <- list(x=-10,y=-10)
  pitchCoordsDestination <- list(x=-20,y=-20)
  
  selectedItem <- NULL
  
  
  #Colours
  ball.colour = "#ffff66"
  player.colour = "white"
  opponent.colour = "#6699ff"
  
  #-------------------------------------------------------------------------
  
  drawPitch <- reactive({
    
    print("Started: drawPitch")
    
    #Refresh dependencies
    eventData()
    deleteEvent()
    clearPitch()
    updateColours()
    loadData()
    addSelectedPlayers()
    addFrame()

    arrow <- drawArrow()

    #Show selected frame (or frame 1 if selectFrame is NULL)
    if(is.null(input$uiSelectFrame)){
      event.data.frame <- event.data[event.data$frame==1,]
    } else {
      event.data.frame <- event.data[event.data$frame==input$uiSelectFrame,]
    }
    
    if(nrow(event.data.frame)==0){event.data.frame <- event.data}
    
    chalkboard <- 
      
      ggplot(data = event.data.frame, aes(x=xo, y=yo, label=label)) +  #No data needed here, but used for double click matching
      #Pitch background
      pitch_markings(pitch_type = input$uiPitchType)
    
    #Opponent position
    if(length(event.data.frame[event.data.frame$event=="oppo position",]$xo>0)){
      chalkboard <- chalkboard +
        geom_text(data = event.data.frame[event.data.frame$event=="oppo position",], aes(x=xo, y=yo, label=label), vjust=-1, colour=opponent.colour, size=6, na.rm=TRUE) +
        geom_point(data = event.data.frame[event.data.frame$event=="oppo position",], aes(x=xo, y=yo, label=label), size=6, colour=opponent.colour, na.rm=TRUE)
    }
    
    #Player position
    if(length(event.data.frame[event.data.frame$event=="position",]$xo>0)){
      chalkboard <- chalkboard +
        geom_text(data = event.data.frame[event.data.frame$event=="position",], aes(x=xo, y=yo, label=label), vjust=-1, colour=player.colour, size=6, na.rm=TRUE) +
        geom_point(data = event.data.frame[event.data.frame$event=="position",], aes(x=xo, y=yo, label=label), size=6, colour=player.colour, na.rm=TRUE)
    }
    
    #Off the ball run
    if(length(event.data.frame[event.data.frame$event=="run",]$xo>0)){
      chalkboard <- chalkboard +
        geom_text(data = event.data.frame[event.data.frame$event=="run",], aes(x=xo, y=yo, label=label), vjust=-1, colour=player.colour, size=6, na.rm=TRUE) +
        geom_segment(data = event.data.frame[event.data.frame$event=="run",], aes(x=xo, y=yo, xend=xd, yend=yd, label=label), size=0.5, colour=player.colour, arrow = grid::arrow(length = grid::unit(0.02, "npc")), na.rm=TRUE)
    }
    
    #Pass arrows
    if(length(event.data.frame[event.data.frame$event=="pass",]$xo>0)){
      chalkboard <- chalkboard +
        geom_text(data = event.data.frame[event.data.frame$event=="pass",], aes(x=xo, y=yo, label=label), vjust=-1, colour=ball.colour, size=6, na.rm=TRUE) +
        geom_curve(data = event.data.frame[event.data.frame$event=="pass",], aes(x=xo, y=yo, xend=xd, yend=yd, label=label), curvature=-0.1, size=1, linetype=1, colour=ball.colour, arrow = grid::arrow(length = grid::unit(0.02, "npc")), na.rm=TRUE)
    }
    
    #Dribble arrows
    if(length(event.data.frame[event.data.frame$event=="dribble",]$xo>0)){
      chalkboard <- chalkboard +
        geom_text(data = event.data.frame[event.data.frame$event=="dribble",], aes(x=xo, y=yo, label=label), vjust=-1, colour=ball.colour, size=6, na.rm=TRUE) +
        geom_curve(data = event.data.frame[event.data.frame$event=="dribble",], aes(x=xo, y=yo, xend=xd, yend=yd, label=label), curvature=-0.1, size=1, linetype=2, colour=ball.colour, arrow = grid::arrow(length = grid::unit(0.02, "npc")), na.rm=TRUE)
    }
    
    #Shot arrows
    if(length(event.data.frame[event.data.frame$event=="shot",]$xo>0)){
      chalkboard <- chalkboard +
        geom_text(data = event.data.frame[event.data.frame$event=="shot",], aes(x=xo, y=yo, label=label), vjust=-1, colour=ball.colour, size=6, na.rm=TRUE) +
        geom_segment(data = event.data.frame[event.data.frame$event=="shot",], aes(x=xo, y=yo, xend=xd, yend=yd, label=label), size=1, colour=ball.colour, arrow = grid::arrow(length = grid::unit(0.02, "npc")), na.rm=TRUE)
    }
    
    #Opposition pass arrows
    if(length(event.data.frame[event.data.frame$event=="oppo pass",]$xo>0)){
      chalkboard <- chalkboard +
        geom_text(data = event.data.frame[event.data.frame$event=="oppo pass",], aes(x=xo, y=yo, label=label), vjust=-1, colour=opponent.colour, size=6, na.rm=TRUE) +
        geom_curve(data = event.data.frame[event.data.frame$event=="oppo pass",], aes(x=xo, y=yo, xend=xd, yend=yd, label=label), curvature=-0.1, size=1, linetype=1, colour=opponent.colour, arrow = grid::arrow(length = grid::unit(0.02, "npc")), na.rm=TRUE)
    }
    
    chalkboard <- chalkboard +
      #Labels-----------
    #Header
    annotate("text", x = 1, y = 96, label = input$uiTitle, hjust = 0, size=10, colour="#cccccc") +
      #Sub Header
      annotate("text", x = 1, y = 91, label = input$uiSubtitle, hjust = 0, size=7, colour="#cccccc") +
      #Footer
      annotate("text", x = 1, y = 3, label = "apps.hilltop-analytics.com/chalkboard", hjust = 0, size=5, colour="#cccccc") +
      #Formatting
      xlim(-2, 102) +
      ylim(-2, 102) +
      theme(axis.ticks = element_blank(),
            axis.title = element_blank(),
            axis.text = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            panel.background = element_rect(fill = "#1a1a1a"))
    
    if(input$uiAddEdit=="edit" &
        !is.null(selectedItem)){
      
      #circle selected point 
      chalkboard <- chalkboard +
        geom_point(data = selectedItem, aes(x=xo, y=yo), size=10, colour="red", shape=1)

    } else {
      
      #Temp current coords arrow
      chalkboard <- chalkboard +
      geom_segment(data = arrow, aes(x=xo, y=yo, xend=xd, yend=yd, label=label), size=0.5, colour="dark grey", arrow = grid::arrow(length = grid::unit(0.01, "npc")))

    }
    
    print("Finished: drawPitch")
    
    return(chalkboard)
    
  })
  
  output$pitchPlot <- renderPlot({
    print("Started: pitchPlot")
    print("Finished: pitchPlot")
    drawPitch()
  })
  
#--------------------------------------------------------------------------------
# Choices for add event (server side to remove position options during animation)
#--------------------------------------------------------------------------------
 
  output$selectEventType <- renderUI({
    if(frame.total()==1){
      return(radioButtons("uiEventType", "Event Type", choices=list("Pass" = "pass", "Dribble" = "dribble", "Shot" = "shot", "Off the Ball Run" = "run", "Opponent Pass" = "oppo pass", "Player Position" = "position", "Opponent Position" = "oppo position")))
    } else {
      return(radioButtons("uiEventType", "Event Type", choices=list("Pass" = "pass", "Dribble" = "dribble", "Shot" = "shot", "Off the Ball Run" = "run", "Opponent Pass" = "oppo pass")))
    }
  })
  
#--------------------------------------------------------------------------------
# Single click on pitch (draw arrow or move point)
#--------------------------------------------------------------------------------
  
  pitchClick <- reactive({
    
    print("Started: pitchClick")
    
    click.coords <- input$pitch_click
    
    #Draw arrow
    if(input$uiAddEdit=="add" & !is.null(input$pitch_click$x)){
      if(pitchCoordsCur=="Origin"){
        pitchCoordsOrigin <<- list(x=click.coords$x,y=click.coords$y)
      }
      
      if(pitchCoordsCur=="Destination"){
        pitchCoordsDestination <<- list(x=click.coords$x,y=click.coords$y)
      }
    }    
    
    #Select points to move
    isolate(selectedPoint <- nearPoints(event.data[event.data$frame==frame.cur(),], click.coords, addDist = TRUE))
  
    #Take first near point only to avoid multi selecting
    selectedPoint <- selectedPoint[1,]
      
    if(input$uiAddEdit == "edit" & 
       !is.null(click.coords$x)){

      if(!is.null(selectedItem) & is.na(selectedPoint$xo)){  #A player is selected and a blank space was clicked.
        
        isolate(
          rowid <- which(event.data$xo == selectedItem$xo &
                          event.data$yo == selectedItem$yo &
                          event.data$label == selectedItem$label &
                          event.data$frame == frame.cur()) 
        )
        
        if(length(rowid)==1){   #Check that selected point is still present in event.data
          event.data[rowid,]$xo <<- click.coords$x
          event.data[rowid,]$yo <<- click.coords$y
        }
        
        #Reset the selected point
        selectedItem <<- NULL
        
      }
      
      isolate(selectedItem <<- event.data[event.data$xo == selectedPoint$xo &
                                          event.data$yo == selectedPoint$yo & 
                                          event.data$label == selectedPoint$label &
                                          event.data$frame == frame.cur(),][1,])
      
      #If no item is selected, set selectedItem to NULL again
      if(is.na(selectedItem$xo)){selectedItem <<- NULL}
      
    }
    
    print("Finished: pitchClick")
    
  })

  
#--------------------------------------------------------------------------------
# Draw temporary arrow
#--------------------------------------------------------------------------------

  drawArrow <- reactive({
    
    #Take dependencies so that the arrow is redrawn
    pitchClick()
    eventData()
    
    originxy <- pitchCoordsOrigin

    destinationxy <- pitchCoordsDestination
    
    #If destination is identical to origin, offset it slightly
    if(destinationxy$x == originxy$x){destinationxy$x <- destinationxy$x +1}
    if(destinationxy$y == originxy$y){destinationxy$y <- destinationxy$y +1}
    

    df <- data.frame(xo=originxy$x,
                     yo=originxy$y,
                     xd=destinationxy$x,
                     yd=destinationxy$y,
                     label="",
                     event="")

    if(!is.null(input$pitch_click)){    
      if(pitchCoordsCur=="Origin"){
        pitchCoordsCur <<- "Destination"
      } else {
        pitchCoordsCur <<- "Origin"
      }
    }    
    return(df)
  })
  
  
  
#--------------------------------------------------------------------------------
# Add/delete/clear rows in event data
#--------------------------------------------------------------------------------
  
  frame.cur <- reactive({
    
    if(is.null(input$uiSelectFrame)) {1} else {as.numeric(input$uiSelectFrame)}
    
  })
  
  frame.total <- reactive({
    
    loadData()
    addFrame()
    deleteFrame()
    clearPitch()
    
    return(max(event.data$frame))
    
  })
  
  
  
  eventData <- reactive({
    
    print("Started: eventData")
    
    #Take dependency on draw on pitch button
    if(input$uiDrawEvent==0){return(NULL)}
    
    isolate(
    
      new.event <- data.frame(xo = pitchCoordsOrigin$x,
                              yo = pitchCoordsOrigin$y,
                              xd = pitchCoordsDestination$x,
                              yd = pitchCoordsDestination$y,
                              label = input$uiPlayerName,
                              event = input$uiEventType,
                              frame = frame.cur())
    )
      
    event.data <<- rbind(event.data, new.event)
    
    #reset user drawn arrow
    pitchCoordsOrigin <<- list(x=-10,y=-10)
    pitchCoordsDestination <<- list(x=-20,y=-20)
    
    print("Finished: eventData")
    
  })
  
  deleteEvent <- reactive({
    
    selectedPoint <- nearPoints(event.data, input$pitch_dblclick, addDist = TRUE)

    #browser()
    
    if(length(selectedPoint$xo)==0){return(NULL)}
    
    rowid <- which(event.data$xo %in% selectedPoint$xo &
                     event.data$yo %in% selectedPoint$yo &
                     event.data$label %in% selectedPoint$label &
                     event.data$frame == frame.cur())

    event.data <<- event.data[-rowid,]
    
  })
  
  clearPitch <- reactive({
    
    print("Started: clearPitch")
    
    #take a dependency on the clear button
    input$uiClear
    
    #Initialise the dataframes again
    event.data <<- data.frame(xo=c(-10,-10,-10,-10,-10,-10,-10),
                             yo=c(-10,-10,-10,-10,-10,-10,-10),
                             xd=c(-10,-10,-10,-10,-10,-10,-10),
                             yd=c(-20,-20,-20,-20,-20,-20,-20),
                             label=c("","","","","","",""),
                             event=c("pass","dribble","shot","run","oppo pass","position","oppo position"),
                             frame=c(1,1,1,1,1,1,1))
    
    pitchCoordsCur <<- "Origin"
    pitchCoordsOrigin <<- list(x=-10,y=-10)
    pitchCoordsDestination <<- list(x=-20,y=-20)
    
    print("Finished: clearPitch")
    
  })
  
#--------------------------------------------------------------------------------
# Load and save
#--------------------------------------------------------------------------------
  
  output$downloadPitch <- downloadHandler(
    filename = function() { 'chalkboard.png' },
    content = function(file) {
      ggsave(file, plot = drawPitch(), device = "png", width=12, height=9, dpi=300)
    })
  
  output$downloadData <- downloadHandler(
    filename = function() { 'chalkboard.csv' },
    content = function(file) {
      write.csv(event.data[event.data$xo != -10,], file, row.names = FALSE)
    })

  output$downloadAnimation <- downloadHandler(
    filename = function() { 'chalkboard.gif' },
    content = function(file) {
      gg_animate(drawPitchAnimated(), filename=paste0(sessionid,"pitch.gif"), interval=0.1, title_frame = FALSE, ani.width=800, ani.height=600)
      file.rename(paste0(sessionid,"pitch.gif"), file)
    })
    
  
  loadData <- reactive({
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    
    loaded.data <- read.csv(inFile$datapath, header=TRUE, sep=',')
    
    #If file has been manually edited, Excel creates NA on unlabelled points. Replace with ""
    loaded.data$label[is.na(loaded.data$label)] <- ""
    
    event.data <<- rbind(event.data,loaded.data)

  })
  
  
  updateColours <- reactive({
    
    ball.colour <<- input$colOnBall
    player.colour <<- input$colPlayers
    opponent.colour <<- input$colOppoPlayers
    
  })
  
#--------------------------------------------------------------------------------
# Bulk add players
#--------------------------------------------------------------------------------
  
  output$selectTeams <- renderUI({
    selectInput("uiSelectTeams", "Select Team(s)", unique(squadlist$team), multiple = FALSE)
  })
  
  output$selectPlayers <- renderUI({
    selectInput("uiSelectPlayers", "Select Players", unique(squadlist[squadlist$team %in% input$uiSelectTeams,]$player), multiple = TRUE)
  })
  
  addSelectedPlayers <- reactive({
    
    print("Started: addSelectedPlayers")
    
    #Take dependency on add players button
    input$uiAddPlayers

    isolate(
      if(is.null(input$uiSelectPlayers)){return(NULL)}
    )
    
    isolate(
      selected.players <- squadlist[squadlist$player %in% input$uiSelectPlayers,]
    )
    
      #Merge to get horizontal (x) position
      selected.players <- merge(selected.players, positions.xy)
  
      #calculate y position
      selected.players <- selected.players %>%
        group_by(position) %>%
        mutate(count=n(),
               marginal=100/(count+1),
               yo = cumsum(marginal))
      
      selected.players$xd <- NA
      selected.players$yd <- NA
      selected.players$event <- "position"
      selected.players$frame <- 1
      
      selected.players <-rename(selected.players, label = player)
      
      selected.players <- selected.players[c("xo", "yo", "xd", "yd", "label", "event", "frame")]

      #add to event.data
      event.data <<- rbind(event.data, selected.players)
   
      print("Finished: addSelectedPlayers")
      
  })
  
#-------------------------------------------------------------------------
# Animation
#-------------------------------------------------------------------------
  
  output$selectFrame <- renderUI({
    
    loadData()
    addFrame()
    deleteFrame()
    clearPitch()
    
    selectInput("uiSelectFrame", "Select Frame", unique(event.data$frame), selected = max(event.data$frame),multiple=FALSE)
    
  })
  
  addFrame <- reactive({
    
    print("Started: addFrame")
    
    #Take a dependency on addFrame button and don't run when app initialises 
    if(input$uiAddFrame==0){return(NULL)}

    max.frame <- as.numeric(max(event.data$frame))

    #Copy last frame in dataset
    last.frame <- event.data[event.data$frame==max.frame,]

    #Increment frame number
    last.frame$frame <- max.frame + 1

    #Re-insert into event data
    event.data <<- rbind(event.data, last.frame)
    
    print("Finished: addFrame")
        
  })
  
  deleteFrame <- reactive({
    
    if(input$uiDeleteFrame==0){return(NULL)}
    
    if(max(event.data$frame) >1){
      event.data <<- event.data[event.data$frame != max(event.data$frame),]
    }
  })
  
  output$chalkboardCaption <- renderText({
    paste0('Frame: ',frame.cur())
  })
  
  
  drawPitchAnimated <- reactive({
    
    print("Started: drawPitchAnimated")
    
    #Refresh dependencies
    if(input$uiRenderAnimation==0){return(NULL)}
    
    # Interpolation parameters ---------------------------------------
    
    framesteps <- 9
    
    totalframes <- framesteps * max(event.data$frame)
    
    # Subset data ----------------------------------------------------

    positions <- event.data[event.data$event %in% c("position", "oppo position") & 
                             event.data$xo != -10,]
    
    events <- event.data[!(event.data$event %in% c("position", "oppo position")) &
                           event.data$xo != -10,]
    
    
    #Attempt to calc positions first and then hook in events. If no positions, just calc events
    if(length(positions$frame > 0)){
    
        # Interpolate positions ------------------------------------------
        
        positions.list <- split(positions, positions$frame)
        
        positions.interpolated <- tween_states(positions.list, tweenlength = framesteps, statelength = 1, 'linear', nframes = totalframes)
        
        # Add events -----------------------------------------------------
        
        if(length(events$frame >0)){
        
          events$event_id <- paste(events$xo, events$yo, events$event)
          
          # Filter to only first incidence of each event
          events <- events %>%
            group_by(event_id) %>%
            filter(frame==min(frame))
          
          # build lookup for frame numbers
          frame.lookup <- unique(positions.interpolated[c("frame", ".frame")])
          
          frame.lookup <- frame.lookup %>%
            group_by(frame) %>%
            filter(.frame==min(.frame))
          
          events <- merge(events, frame.lookup, all.x = TRUE)
          
          events$frame <- events$.frame - 10
          
          events <- events[names(events)!=".frame"]
          
          events <- tween_appear(events, "frame", timerange = c(1, max(positions.interpolated$.frame) + 10))
          
          events <- events[events$.age >= 0 & 
                             events$.frame <= max(positions.interpolated$.frame),]
          
          events <- events[!(names(events) %in% c(".age", "event_id"))]
        
        }
            
        event.data.animate <- rbind(positions.interpolated, events)
    
    } else {
      
      events$event_id <- paste(events$xo, events$yo, events$event)
      
      # Filter to only first incidence of each event
      events <- events %>%
        group_by(event_id) %>%
        filter(frame==min(frame))
      
      events <- tween_appear(events, "frame", timerange = c(1, max(events$frame) + 10), nframes = framesteps * max(events$frame))
      
      events <- events[events$.age >= 0,]
      
      events <- events[!(names(events) %in% c(".age", "event_id"))]
      
      event.data.animate <- events
      
    }
        
        
    #--- Add pause at end --------------------------
    
    lastframe.number <- max(event.data.animate$.frame)
    
    lastframe.df <- event.data.animate[event.data.animate$.frame == lastframe.number,]
    
    for(i in 0:10){
      
      lastframe.df$.frame <- lastframe.number + i
      
      event.data.animate <- rbind(event.data.animate, lastframe.df)
      
    }
    
    
    chalkboard <- 
      
      ggplot(data = event.data.animate, aes(x=xo, y=yo, label=label, frame=.frame)) +  #No data needed here, but used for double click matching
      #Pitch background
      pitch_markings(pitch_type = input$uiPitchType)
      
      #Opponent position
      if(length(event.data.animate[event.data.animate$event=="oppo position",]$xo>0)){
        chalkboard <- chalkboard +
        geom_text(data = event.data.animate[event.data.animate$event=="oppo position",], aes(x=xo, y=yo, label=label, frame=.frame), vjust=-1, colour=opponent.colour, size=6, na.rm=TRUE) +
        geom_point(data = event.data.animate[event.data.animate$event=="oppo position",], aes(x=xo, y=yo, label=label, frame=.frame), size=6, colour=opponent.colour, na.rm=TRUE)
      }
    
      #Player position
      if(length(event.data.animate[event.data.animate$event=="position",]$xo>0)){
        chalkboard <- chalkboard +
        geom_text(data = event.data.animate[event.data.animate$event=="position",], aes(x=xo, y=yo, label=label, frame=.frame), vjust=-1, colour=player.colour, size=6, na.rm=TRUE) +
        geom_point(data = event.data.animate[event.data.animate$event=="position",], aes(x=xo, y=yo, label=label, frame=.frame), size=6, colour=player.colour, na.rm=TRUE)
      }
    
      #Off the ball run
      if(length(event.data.animate[event.data.animate$event=="run",]$xo>0)){
        chalkboard <- chalkboard +
        geom_text(data = event.data.animate[event.data.animate$event=="run",], aes(x=xo, y=yo, label=label, frame=.frame), vjust=-1, colour=player.colour, size=6, na.rm=TRUE) +
        geom_segment(data = event.data.animate[event.data.animate$event=="run",], aes(x=xo, y=yo, xend=xd, yend=yd, label=label, frame=.frame), size=0.5, colour=player.colour, arrow = grid::arrow(length = grid::unit(0.02, "npc")), na.rm=TRUE)
      }
    
      #Pass arrows
      if(length(event.data.animate[event.data.animate$event=="pass",]$xo>0)){
        chalkboard <- chalkboard +
        geom_text(data = event.data.animate[event.data.animate$event=="pass",], aes(x=xo, y=yo, label=label, frame=.frame), vjust=-1, colour=ball.colour, size=6, na.rm=TRUE) +
        geom_curve(data = event.data.animate[event.data.animate$event=="pass",], aes(x=xo, y=yo, xend=xd, yend=yd, label=label, frame=.frame), curvature=-0.1, size=1, linetype=1, colour=ball.colour, arrow = grid::arrow(length = grid::unit(0.02, "npc")), na.rm=TRUE)
      }
    
      #Dribble arrows
      if(length(event.data.animate[event.data.animate$event=="dribble",]$xo>0)){
        chalkboard <- chalkboard +
        geom_text(data = event.data.animate[event.data.animate$event=="dribble",], aes(x=xo, y=yo, label=label, frame=.frame), vjust=-1, colour=ball.colour, size=6, na.rm=TRUE) +
        geom_curve(data = event.data.animate[event.data.animate$event=="dribble",], aes(x=xo, y=yo, xend=xd, yend=yd, label=label, frame=.frame), curvature=-0.1, size=1, linetype=2, colour=ball.colour, arrow = grid::arrow(length = grid::unit(0.02, "npc")), na.rm=TRUE)
      }
    
      #Shot arrows
      if(length(event.data.animate[event.data.animate$event=="shot",]$xo>0)){
        chalkboard <- chalkboard +
        geom_text(data = event.data.animate[event.data.animate$event=="shot",], aes(x=xo, y=yo, label=label, frame=.frame), vjust=-1, colour=ball.colour, size=6, na.rm=TRUE) +
        geom_segment(data = event.data.animate[event.data.animate$event=="shot",], aes(x=xo, y=yo, xend=xd, yend=yd, label=label, frame=.frame), size=1, colour=ball.colour, arrow = grid::arrow(length = grid::unit(0.02, "npc")), na.rm=TRUE)
      }
      
      #Opposition pass arrows
      if(length(event.data.animate[event.data.animate$event=="oppo pass",]$xo>0)){
        chalkboard <- chalkboard +
        geom_text(data = event.data.animate[event.data.animate$event=="oppo pass",], aes(x=xo, y=yo, label=label, frame=.frame), vjust=-1, colour=opponent.colour, size=6, na.rm=TRUE) +
        geom_curve(data = event.data.animate[event.data.animate$event=="oppo pass",], aes(x=xo, y=yo, xend=xd, yend=yd, label=label, frame=.frame), curvature=-0.1, size=1, linetype=1, colour=opponent.colour, arrow = grid::arrow(length = grid::unit(0.02, "npc")), na.rm=TRUE)
      }
      
    chalkboard <- chalkboard +
      #Labels-----------
    #Header
    annotate("text", x = 1, y = 96, label = input$uiTitle, hjust = 0, size=10, colour="#cccccc") +
      #Sub Header
      annotate("text", x = 1, y = 91, label = input$uiSubtitle, hjust = 0, size=7, colour="#cccccc") +
      #Footer
      annotate("text", x = 1, y = 3, label = "apps.hilltop-analytics.com/chalkboard", hjust = 0, size=5, colour="#cccccc") +
      #Formatting
      xlim(-2, 102) +
      ylim(-2, 102) +
      theme(axis.ticks = element_blank(),
            axis.title = element_blank(),
            axis.text = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            panel.background = element_rect(fill = "#1a1a1a"))
    

    print("Finished: drawPitchAnimated")
    
    return(chalkboard)
    
  })
  
  
  renderAnimation <- reactive({
    
    gg_animate(drawPitchAnimated(), filename=paste0(sessionid,"pitch.gif"), interval=0.1, title_frame = FALSE, ani.width=800, ani.height=600)

  })
  
  output$renderedPitch <- renderImage({
    
    #Redraw the pitch animation
    renderAnimation()
    
    list(src = paste0(sessionid,"pitch.gif"),
         contentType = 'image/gif',
         width = 800,
         height = 600,
         alt = "Animated Chalkboard")
  }, deleteFile = TRUE)
  
})
