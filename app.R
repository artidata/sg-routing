library(shiny)
library(shinydashboard)
library(leaflet)
library(igraph)
library(sf)
library(DT)

options(digits=5,nsmall=2)
graph <- readRDS("sg graph 20200513.rds")
vertexGeom=graph %>% 
    vertex_attr("geometry") %>% 
    st_as_sfc(crs=4326)

initStart <- c(103.9897892,1.3620652)
initEnd <- c(103.7757289,1.2944849)
initNearest <- st_nearest_feature(
    st_as_sfc(list(st_point(initStart),st_point(initEnd)),crs=4326),
    vertexGeom)

initPath <- shortest_paths(graph,
                           initNearest[1],initNearest[2],
                           weights=E(graph)$time,
                           output="epath") 

ui <- dashboardPage(
    dashboardHeader(title="SG Routing"),
    dashboardSidebar(disable=T),
    dashboardBody(
        fluidRow(
            column(width=8,
                   box(width=NULL,
                       htmlOutput(outputId="instruction")),
                   box(width=NULL,
                       leafletOutput(outputId="map", 
                                     width="100%", 
                                     height=500))),
            column(width=4,
                   box(width=NULL,
                       textOutput(outputId="routeStart"),
                       tags$style(type="text/css", "#routeStart {white-space: pre-wrap;}"),
                       textOutput(outputId="routeEnd"),
                       tags$style(type="text/css", "#routeEnd {white-space: pre-wrap;}"),
                       htmlOutput(outputId="pinStart"),
                       tags$style(type="text/css", "#routeStart {white-space: pre-wrap;}"),
                       htmlOutput(outputId="pinEnd"),
                       tags$style(type="text/css", "#routeEnd {white-space: pre-wrap;}")),
                   box(width=NULL,
                       DTOutput(outputId="pathAttr"))
            ))
    )
)

server <- function(input, output, session) {
    
    xRouteStart<-reactiveVal(initStart[1])
    yRouteStart<-reactiveVal(initStart[2])
    output$routeStart<-renderText(sep="",{
        c("Route Start: ",
          format(yRouteStart(),nsmall=7),", ",
          format(xRouteStart(),nsmall=7))})
    
    xRouteEnd<-reactiveVal(initEnd[1])
    yRouteEnd<-reactiveVal(initEnd[2])
    output$routeEnd<-renderText(sep="",{
        c("Route End: ",
          format(yRouteEnd(),nsmall=7),", ",
          format(xRouteEnd(),nsmall=7))})
    
    xPinStart<-reactiveVal(initStart[1])
    yPinStart<-reactiveVal(initStart[2])
    output$pinStart<-renderText(sep="",{
        c("<span style='color:#00BA38'>Pin Start: ",
          format(yPinStart(),nsmall=7),", ",
          format(xPinStart(),nsmall=7),"</span>")})
    
    xPinEnd<-reactiveVal(initEnd[1])
    yPinEnd<-reactiveVal(initEnd[2])
    output$pinEnd<-renderText(sep="",{
        c("<span style='color:#F8766D'>Pin End: ",
          format(yPinEnd(),nsmall=7),", ",
          format(xPinEnd(),nsmall=7),"</span>")})
    
    instruction<-reactiveVal("Drag and drop pins to a new coordinate!")
    
    dfCoord <- reactive({
        data.frame(lng=c(xRouteStart(),xRouteEnd()),
                   lat=c(yRouteStart(),yRouteEnd()))})
    
    path <- reactive({
        nearest <- st_nearest_feature(st_as_sf(dfCoord(),coords=c("lng","lat"),crs=4326),vertexGeom)
        shortest_paths(graph,
                       nearest[1],nearest[2],
                       weights=E(graph)$time,
                       output="epath")})
    
    pathAttr <- reactive({
        data.frame(name=graph %>% edge_attr("name",path()$epath[[1]]),
                   distance=graph %>% edge_attr("distance",path()$epath[[1]]),
                   time=graph %>% edge_attr("time",path()$epath[[1]]))})
    
    output$map <- renderLeaflet({
        leaflet() %>%
            addProviderTiles(providers$CartoDB.Positron,
                             options = providerTileOptions(noWrap = TRUE))%>%
            addMeasure(
                primaryLengthUnit = "meters",
                primaryAreaUnit = "sqmeters",
                activeColor = "#3D535D",
                completedColor = "#7D4479") %>% 
            addAwesomeMarkers(
                lng=initStart[1],
                lat=initStart[2],
                #label="start",
                options=markerOptions(draggable=T),
                icon=awesomeIcons(
                    icon = 'ios-close',
                    iconColor = 'black',
                    library = 'ion',
                    markerColor = "green"),
                layerId="pinStart")%>%
            addAwesomeMarkers(
                lng=initEnd[1], 
                lat=initEnd[2], 
                #label="end",
                options=markerOptions(draggable=T),
                icon=awesomeIcons(
                    icon = 'ios-close',
                    iconColor = 'black',
                    library = 'ion',
                    markerColor = "red"),
                layerId="pinEnd")# %>% 
            # addPolylines(data=graph %>%
            #                  edge_attr("geometry",initPath$epath[[1]]) %>%
            #                  st_as_sfc(crs=4326))# %>%
            # addCircleMarkers(lng=c(initStart[1],initEnd[1]),
            #                  lat=c(initStart[2],initEnd[2]),layerId="test")
    })
    
    observeEvent(input$map_marker_mouseout,{
        #print(input$map_marker_mouseout)
        marker <- input$map_marker_mouseout
        if(marker$id=="pinStart"){
            xPinStart(marker$lng)
            yPinStart(marker$lat)
            if(xPinStart()!=xRouteStart()|
               yPinStart()!=yRouteStart()){
                instruction("Click the <span style='color:#00BA38'>green pin</span> to update the route's starting coordinate!")
            }
        }else if(marker$id=="pinEnd"){
            xPinEnd(marker$lng)
            yPinEnd(marker$lat)
            if(xPinEnd()!=xRouteEnd()|
               yPinEnd()!=yRouteEnd()){
                instruction("Click the <span style='color:#F8766D'>red pin</span> to update the route's ending coordinate!")}}})
    
    observeEvent(input$map_marker_click,{
        marker <- input$map_marker_click
        if(marker$id=="pinStart"){
            xRouteStart(marker$lng)
            yRouteStart(marker$lat)
        }else if(marker$id=="pinEnd"){
            xRouteEnd(marker$lng)
            yRouteEnd(marker$lat)
        }
        instruction("Drag and drop pins to a new coordinate!")})
    
    output$instruction<-renderText({instruction()})
    
    output$pathAttr<-renderDT({
        datatable(aggregate(.~name, pathAttr(), sum),
                  style="bootstrap4",
                  colnames = c("Road Name","Distance (m)","Time (s)"),
                  autoHideNavigation=F,
                  options=list(searching=F,pageLength=10,paging=F,info=T,ordering=F)) %>% 
            formatRound(columns=c("distance","time"),digits=1,mark="")})
    
    observe({
        leafletProxy("map") %>% 
            clearShapes()%>% 
            addPolylines(data=graph %>%
                             edge_attr("geometry",path()$epath[[1]]) %>%
                             st_as_sfc(crs=4326) %>% 
                             st_combine(),
                         label=HTML("Distance = ",graph %>% edge_attr("distance",path()$epath[[1]]) %>% sum() %>% format(nsmall=0)," m",
                                    "</br>Time = ",graph %>% edge_attr("time",path()$epath[[1]]) %>% sum() %>% format(nsmall=0)," s"),
                         weight=4,
                         labelOptions=labelOptions(permanent=T,opacity=0.8))})

}

shinyApp(ui, server)
