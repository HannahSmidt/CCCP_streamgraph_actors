# Shiny App
library("dplyr")
library("shiny")
library("shinyjs")


source("process-ucdp-data.R", local = TRUE)

function(input, output, session) {
  
  # Process data for particulat stream graph
  dataStreamgraph2 <- dataStreamgraph %>%
    filter(deaths < 300000)  %>% # without rwandan genocide
    select(conflictSettings, year, SovereignStateActors, NonstateActors, ExternalActors) %>%
    rename(Actors.SovereignState = SovereignStateActors,
           Actors.Nonstate = NonstateActors,
           Actors.External = ExternalActors) %>%
    reshape(varying= c("Actors.SovereignState", "Actors.Nonstate", "Actors.External")
            , idvar=c("conflictSettings", "year")
            , times=c("SovereignState", "Nonstate", "External")
            , direction="long") %>%
    mutate(names = plyr::mapvalues(time,c("SovereignState","Nonstate","External"),
                                   c("State governments","Non-state actors","External governments") ) ) %>%
    mutate(conflictActors = paste(conflictSettings, names, sep=": ") )
  
  # Choose colors for streams
  sg_fill_manual_colors <- c("#C0392B", "#F1948A", "#E74C3C"
                             ,"#7FB3D5", "#2980B9", "#3498DB"
                             ,"#F7DC6F", "#B7950B", "#F5B041"
                             ,"#C39BD3", "#884EA0", "#8E44AD"
                             ,"#73C6B6", "#1E8449", "#2ECC71"
                             ,"#AAB7B8", "#797D7F", "#7F8C8D" )
  
  # Render streamgraph
  output$streamPlot <- renderStreamgraph({
    
    shinyjs::hide(id = "loading-content",
                  anim = TRUE,
                  animType = "fade")
    
    dataStreamgraph2 %>%
      group_by(conflictActors, year) %>%
      streamgraph(key = "conflictActors", value = "Actors", date = "year"
                  , offset="zero", interpolate="linear") %>%
      sg_axis_x(tick_interval = c(1990, 2014), tick_units = 5)  %>%
      sg_fill_manual(sg_fill_manual_colors) %>%
      sg_legend(show=TRUE, label="Setting & Actor: ") 
  })
}
