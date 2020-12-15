## app.R ##
library(shinydashboard)
library(shiny)
library(shinyWidgets)
library(plotly)
library(Rtsne)
library(DT)
library( plyr )
library(rsconnect)
source('kit.R')

ui <- dashboardPage(
    dashboardHeader(title = "Image visualization"),
    dashboardSidebar(
        prettyRadioButtons(
            inputId ='perp',
            label= tags$h3('Perplexity'),
            choices = c(15,30,50,100,200),
            selected = 50,
            status = "success",
            shape = "curve",
            outline = FALSE,
            fill = T,
            thick = T,
            animation = "jelly",
            icon = icon('bahai'),
            plain = FALSE,
            bigger = T,
            inline = TRUE,
            width = '200px',
            choiceNames = NULL,
            choiceValues = NULL
        ),
        prettyRadioButtons(
            inputId ='cluster',
            label= tags$h3('Cluster Number'),
            choices = c(6,9,10,13),
            selected = NULL,
            status = "primary",
            shape = "curve",
            outline = FALSE,
            fill = T,
            thick = T,
            animation = "jelly",
            icon = icon('bahai'),
            plain = FALSE,
            bigger = T,
            inline = TRUE,
            width = '200px',
            choiceNames = NULL,
            choiceValues = NULL
        ),
        actionBttn(
            inputId='lala',
            label = 'Generate',
            icon = NULL,
            style = "unite",
            color = "default",
            size = "l",
            block = FALSE,
            no_outline = TRUE
        ),
        actionBttn(
            inputId='display',
            label = 'Display Clusters',
            icon = NULL,
            style = "jelly",
            color = "default",
            size = "l",
            block = FALSE,
            no_outline = TRUE
        ),
        prettyRadioButtons(
            inputId ='clus',
            label= tags$h3('Cluster to Visualize'),
            choices = c(1,2,3,4,5,6),
            selected = NULL,
            status = "primary",
            shape = "curve",
            outline = FALSE,
            fill = T,
            thick = T,
            animation = "jelly",
            icon = icon('bahai'),
            plain = FALSE,
            bigger = T,
            inline = TRUE,
            width = '200px',
            choiceNames = NULL,
            choiceValues = NULL
        )
        
        
        
        
        
    ),
    dashboardBody(
        # Boxes need to be put in a row (or column)
        fluidRow(
            box(plotlyOutput("daplot",height = '800px'),height = 800),
            
            box(
               imageOutput("daimage"),
               imageOutput("othaimage")
            )
        ),
        fluidRow(width=12,
            box(width=12,
               
                uiOutput("plots")
            )
            
        )
    )
)

server <- function(input, output) {
    master<- list.files('./www')

    observeEvent(input$lala,{
        kitchen3d<<-Rtsne(kitchen, perplexity = as.numeric(input$perp),dims = 3,check_duplicates = FALSE)
        kit_3d_labels<<-kmeans(kitchen3d$Y,centers = input$cluster)
        output$daplot <- renderPlotly({
            
            plot_ly(as.data.frame( kitchen3d$Y), x = kitchen3d$Y[,1], y = kitchen3d$Y[,2], z = kitchen3d$Y[,3], 
                    color = kit_3d_labels$cluster) %>% add_markers(kitchen2$image) 
            
        })
        
    })
    output$ma<- renderText(paste0(event_data("plotly_hover")[3]))
   
    
    observeEvent(event_data('plotly_click'),{
        
      
        output$daimage <- renderImage({
            
            
            
            filename <- normalizePath(file.path('./www',
                                                paste(event_data('plotly_click')[3], sep='')))
            list(src = filename)
            
            
        }, deleteFile = FALSE)
        
        
    })
    
    observeEvent(event_data('plotly_hover'),{
        
        
        output$othaimage <- renderImage({
            
            
            
            filename <- normalizePath(file.path('./www',
                                                paste(event_data('plotly_hover')[3], sep='')))
            list(src = filename)
            
            
        }, deleteFile = FALSE)
        
        
    })
    
    
    
    observeEvent(input$display,{
        kitchen3<-cbind(master, kit_3d_labels$cluster)
        kitchen3<- as.data.frame(kitchen3)
        output$plots<- renderUI({
            b64 <- list()
            for (i in master) {
                if(as.character( i) %in% as.character( kitchen3$master[kitchen3$V2==isolate(input$clus)] )){
                name <- paste('image:', i, sep = '')
                filename <- normalizePath(file.path('./www',
                                                    paste(i, sep='')))
                tmp <- base64enc::dataURI(file =paste('www/',i,sep=''), mime = "image/jpg")
                b64[[name]] <- tmp }
            }
            
            a64 <- list()
            for (j in (1:length(b64))) {
                name_1 <- paste('www/', j, sep = '')
                tmp_1 <- img(src = b64[j],
                             width = 100,
                             height = 100)
                a64[[name_1]] <- tmp_1
            }
            
            a64
        })
    })
}

shinyApp(ui, server)