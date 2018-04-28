
### preparing the empty UI menu items

# preparing empty lists for input selection
cipNames<-c()
cipCodes<-c()
names(cipCodes)<-cipNames

# make an empty dropdown for the degree level
awardLevel <-c()

# Choices for race drop-downs
races <- c("American Indian or Alaska Native","Asian","Black or African American",
           "Hispanic or Latino","Native Hawaiian or Other Pacific Islanders",
           "White","Two or more races","Race/ethnicity unknown",
           "Nonresident alien")

## UI design
###################-----------------------------

# this is for the loading time
appCSS <- "
#loading-content {
  position: absolute;
  background: #000000;
  opacity: 0.5;
  z-index: 100;
  left: 0;
  right: 0;
  height: 100%;
  text-align: center;
  color: #FFFFFF;
}
"

ui <- tagList(
  useShinyjs(),
  inlineCSS(appCSS),
  
  div(
    id = "loading-content",
    h2(tags$br(),tags$br(),"Loading...")
  ),

  navbarPage(id="nav", "Graduation Statistics",

             tabPanel(id="1","Grads-Jobs Map",
                      div(class="outer",
                          
                          tags$head(
                            # Include our custom CSS
                            includeCSS("styles.css"),
                            includeScript("gomap.js")
                          ),
                         
                          leafletOutput("jgMap", width="100%", height="100%"),
                          
                          # this comment is from the template: Shiny versions prior to 0.11 should use class="modal" instead.
                          absolutePanel(id = "controls1",
                                        class = "panel panel-default",
                                        style="padding: 10px; background: white;",
                                        style = "opacity: 0.9",
                                        fixed = TRUE,
                                        draggable = TRUE, top = 80, left = "auto", right = 30, bottom = "auto",
                                        width = 330, height = "auto",
                                        
                                        h2("Control Panel"),
                                        
                                        selectizeInput("jobDegreeJG","Degree type:",choices=awardLevel
                                                       ,multiple = FALSE
                                                       ,selected = "Doctor's Degree"
                                        ),
                                        
                                        selectizeInput("cipNamesJG","Program(s):",choices=cipCodes
                                                       ,multiple = TRUE
                                                       ,selected = "11.07" #commented to make all tabs consistent
                                        ),
                                        checkboxGroupInput("jgChoice", "Groups to be shown:", 
                                                           choices = list("Graduates" = 1, "Jobs" = 2),
                                                           selected = c(1,2),inline = TRUE),
                                        htmlOutput("nJobs")
                          ),
                          
                          # handle the posirion of legend and the citaion
                          tagList(
                            tags$head(
                              tags$style(
                                ".leaflet .legend { bottom:45px; text-align: left;}",
                                ".leaflet .legend i{float: left;}",
                                ".leaflet .legend label{float:left; text-align: left;}"
                              )
                            )
                          ),
                          
                          tags$div(id="cite",#classclass="leaflet-control-attribution leaflet-control",
                                   
                                   "Data from ",
                                   tags$em('IPEDS and Bureau of Labor Statistics for the year of 2014'),
                                   tags$br(),
                                   'Developed by ',
                                   tags$a(href="mailto:sj214@duke.edu?Subject=DataPlus%202017%20Map%20App",
                                          style='color: black;'
                                          ,tags$strong('Amy Jiang,')),
                                   tags$a(href="mailto:k.zhang@duke.edu?Subject=DataPlus%202017%20Map%20App",
                                          style='color: black;'
                                          ,tags$strong('Kelly Zhang,')),
                                   tags$a(href="mailto:amirhossein.khoshroo@gmail.com?Subject=DataPlus%202017%20Map%20App",
                                          style='color: black;'
                                          ,tags$strong('Amir Khoshro'))
                                   
                          )
                      )
                      
             ),
             
             tabPanel(id="2","General Map",
                      
                      div(class="outer",
                          
                          tags$head(
                            # Include our custom CSS
                            includeCSS("styles.css"),
                            includeScript("gomap.js")
                          ),
                          
                          leafletOutput("generalLeaflet",width="100%", height="100%"),
                          
                          # this comment is from the template: Shiny versions prior to 0.11 should use class="modal" instead.
                          absolutePanel(id = "controls2", fixed = TRUE,
                                        class = "panel panel-default",
                                        style="padding: 10px; background: white;",
                                        style = "opacity: 0.9",
                                        draggable = TRUE, top = 80, left = "auto", right = 30, bottom = "auto",
                                        width = 330, height = "auto",
                                        
                                        h2("Control Panel"),
                                        
                                        selectizeInput("jobDegreeGeneral","Degree type:",choices=awardLevel
                                                       ,multiple = FALSE
                                                       ,selected = "Doctor's Degree"
                                        ),
                                        
                                        selectizeInput("cipNamesGeneral","Program(s):",choices=cipCodes
                                                       ,multiple = TRUE
                                                       #,selected = "11.07" commented to make all tabs consistent
                                        )
                          )
                      ),
                      
                      ## handling ledend psition and the citation
                      
                      tagList(
                        tags$head(
                          tags$style(
                            ".leaflet .legend { bottom:45px; text-align: left;}",
                            ".leaflet .legend i{float: left;}",
                            ".leaflet .legend label{float:left; text-align: left;}"
                          )
                        )
                      ),
                      
                      tags$div(id="cite",#classclass="leaflet-control-attribution leaflet-control",
                               
                               "Data from ",
                               tags$em('IPEDS for the year of 2014'),
                               tags$br(),
                               'Developed by ',
                               tags$a(href="mailto:sj214@duke.edu?Subject=DataPlus%202017%20Map%20App",
                                      style='color: black;'
                                      ,tags$strong('Amy Jiang,')),
                               tags$a(href="mailto:k.zhang@duke.edu?Subject=DataPlus%202017%20Map%20App",
                                      style='color: black;'
                                      ,tags$strong('Kelly Zhang,')),
                               tags$a(href="mailto:amirhossein.khoshroo@gmail.com?Subject=DataPlus%202017%20Map%20App",
                                      style='color: black;'
                                      ,tags$strong('Amir Khoshro'))
                               
                      )
             ),
             
             tabPanel(id="3","Gender Map",
                      div(class="outer",
                          
                          tags$head(
                            # Include our custom CSS
                            includeCSS("styles.css"),
                            includeScript("gomap.js")
                          ),
                          
                          leafletOutput("genderLeaflet", width="100%", height="100%"),
                          
                          # this comment is from the template: Shiny versions prior to 0.11 should use class="modal" instead.
                          absolutePanel(id = "controls3",
                                        class = "panel panel-default",
                                        style="padding: 10px; background: white;",
                                        style = "opacity: 0.9",
                                        fixed = TRUE,
                                        draggable = TRUE, top = 80, left = "auto", right = 30, bottom = "auto",
                                        width = 330, height = "auto",
                                        
                                        h2("Control Panel"),
                                        
                                        selectizeInput("jobDegreeGender","Degree type:",choices=awardLevel
                                                       ,multiple = FALSE
                                                       ,selected = "Doctor's Degree"
                                        ),
                                        
                                        selectizeInput("cipNamesGender","Program(s):",choices=cipCodes
                                                       ,multiple = TRUE
                                                       #,selected = "11.07" commented to make all tabs consistent
                                        )
                                        
                          )
                      ),
                      
                      ## handling the legend psition and the citation
                      
                      tagList(
                        tags$head(
                          tags$style(
                            ".leaflet .legend { bottom:45px; text-align: left;}",
                            ".leaflet .legend i{float: left;}",
                            ".leaflet .legend label{float:left; text-align: left;}"
                          )
                        )
                      ),
                      
                      
                      tags$div(id="cite",#classclass="leaflet-control-attribution leaflet-control",
                               
                               "Data from ",
                               tags$em('IPEDS for the year of 2014'),
                               tags$br(),
                               'Developed by ',
                               tags$a(href="mailto:sj214@duke.edu?Subject=DataPlus%202017%20Map%20App",
                                      style='color: black;'
                                      ,tags$strong('Amy Jiang,')),
                               tags$a(href="mailto:k.zhang@duke.edu?Subject=DataPlus%202017%20Map%20App",
                                      style='color: black;'
                                      ,tags$strong('Kelly Zhang,')),
                               tags$a(href="mailto:amirhossein.khoshroo@gmail.com?Subject=DataPlus%202017%20Map%20App",
                                      style='color: black;'
                                      ,tags$strong('Amir Khoshro'))
                               
                      )
             ),
             
             tabPanel("Race Map",
                      div(id="4",class="outer",
                          
                          tags$head(
                            # Include our custom CSS
                            includeCSS("styles.css"),
                            includeScript("gomap.js")
                          ),
                          
                          leafletOutput("raceLeaflet", width="100%", height="100%"),
                          
                          # this comment is from the template: Shiny versions prior to 0.11 should use class="modal" instead.
                          absolutePanel(id = "controls4", 
                                        class = "panel panel-default",
                                        style="padding: 10px; background: white;",
                                        style = "opacity: 0.9",
                                        fixed = TRUE,
                                        draggable = TRUE, top = 80, left = "auto", right = 30, bottom = "auto",
                                        width = 330, height = "auto",
                                        
                                        h2("Control Panel"),
                                        
                                        selectizeInput("jobDegreeRace","Degree type:",choices=awardLevel
                                                       ,multiple = FALSE
                                                       ,selected = "Doctor's Degree"
                                        ),
                                        
                                        selectizeInput("cipNamesRace","Program(s):",choices=cipCodes
                                                       ,multiple = TRUE
                                                       #,selected = "11.07" commented to make all tabs consistent
                                        ),
                                        
                                        selectInput("raceType", "Race to color:", 
                                                    choices = races)
                          )
                      ),
                      
                      ## handling legend psition and the citation
                      
                      tagList(
                        tags$head(
                          tags$style(
                            ".leaflet .legend { bottom:45px; text-align: left;}",
                            ".leaflet .legend i{float: left;}",
                            ".leaflet .legend label{float:left; text-align: left;}"
                          )
                        )
                      ),
                      
                      tags$div(id="cite",#classclass="leaflet-control-attribution leaflet-control",
                               
                               "Data from ",
                               tags$em('IPEDS for the year of 2014'),
                               tags$br(),
                               'Developed by ',
                               tags$a(href="mailto:sj214@duke.edu?Subject=DataPlus%202017%20Map%20App",
                                      style='color: black;'
                                      ,tags$strong('Amy Jiang,')),
                               tags$a(href="mailto:k.zhang@duke.edu?Subject=DataPlus%202017%20Map%20App",
                                      style='color: black;'
                                      ,tags$strong('Kelly Zhang,')),
                               tags$a(href="mailto:amirhossein.khoshroo@gmail.com?Subject=DataPlus%202017%20Map%20App",
                                      style='color: black;'
                                      ,tags$strong('Amir Khoshro'))
                               
                      )
             
             ),
             
             conditionalPanel("false", icon("crosshair"))
  )
  
)






