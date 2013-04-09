### Created by Justin Freels
### email: jfreels@gmail.com
### twitter: https://twitter.com/jfreels4
### github: https://github.com/jfreels

# Load libraries
libs<-c("lubridate","plyr","reshape2","ggplot2","xts","PerformanceAnalytics","shiny","devtools","scales","jfreels")
lapply(libs,require,character.only=TRUE)
#install_github(repo="r_jfreels",username="jfreels")
#library(jfreels)

# load example dataset
example<-read.csv("example.csv")
example$date<-ymd(example$date)
example<-dcast(example,date~fund,value.var="return")

##### SHINY SERVER
shinyServer(function(input, output) {
  
# reactive: upload_dataset
  upload_dataset <- reactive({
    if (is.null(input$csv)) { return(NULL) }
    d<-read.csv(input$csv$datapath,check.names=FALSE)
    d$date<-ymd(d$date)
    d
  })

# reactive: dataset_original
  dataset_original <- reactive({
    dat<-if (input$upload=="Yes") { 
      upload_dataset()
    }
    else { 
      example
    }
    dat
  })
    
# reactive: dataset
  dataset <- reactive({
    dat<-if (input$upload=="Yes") { 
        droplevels(upload_dataset()[,c("date",input$upload_choose_fund)])
      }
      else { 
        droplevels(example[,c("date",input$example_choose_fund)])
      }
  })
  
# reactive: choice
  choices <- reactive({
    if(input$upload=="No") { input$example_choose_fund }
    else { input$upload_choose_fund }
  })

# reactive: data_export
  data_export<-reactive({
    dat<-dataset()
    dat_subset<-if(input$data_subset=="Common") { na.omit(dat) }
                else { dat }
    dat_format<-if(input$data_format=="Wide") { dat_subset }
                else { na.omit(melt(dat_subset,id.vars="date")) }
    dat_format
  })

# reactive: dataset_final
  dataset_final<-reactive({
    subset(data_export(),date>=ymd(input$data_start_date_input)&date<=ymd(input$data_end_date_input))
    #data_export()[data_export()$date>=input$data_end_date_input,]
  })

# reactive: melted_dataset
  melted_dataset<-reactive({
    dat<-if(input$data_format=="Wide") { melt(dataset_final(),id="date") }
    else { dataset_final() }
    dat
  })
  
### sideBarPanel reactive UIs
  output$example_choose_fund<-renderUI({
    if (input$upload=="No") { return(NULL) }
    conditionalPanel(
      condition="input.upload=='Yes'",
      selectInput(inputId="upload_choose_fund",label="Choose Funds:",choices=names(upload_dataset()[-1]),multiple=TRUE)
    )
  })
  
  output$upload_choose_fund<-renderUI({
    if (input$upload=="Yes") { return(NULL) }
    conditionalPanel(
      condition="input.upload=='No'",
      selectInput(inputId="example_choose_fund",label="Choose Funds:",choices=names(example[-1]),multiple=TRUE)
    )    
  })
  
  output$data_start_date<-renderUI({
    selectInput(inputId="data_start_date_input",label="Start Date:",choices=unique(as.character(data_export()$date)))
  })
  
  output$data_end_date<-renderUI({
    selectInput(inputId="data_end_date_input",label="End Date:",choices=rev(unique(as.character(data_export()$date))))
  })


### Tab: "Rolling"
  output$rolling_plot12<-renderPlot({
    dat<-ddply(na.omit(melted_dataset()),.(variable),transform,roll=rollapply(value,12,FUN=function (x) {tail(cumprod(na.omit(x) + 1), 1) - 1},fill=NA,align="right"))
    dat<-dat[,-3]    
    print(horizon.panel.ggplot(dat,paste0("Horizon Plot: 12 Month Rolling Returns (",input$data_start_date_input," to ",input$data_end_date_input,")")))
  })
  
  output$rolling_plot36<-renderPlot({
    dat<-ddply(na.omit(melted_dataset()),.(variable),transform,roll=rollapply(value,36,FUN=function (x) {tail(cumprod(na.omit(x) + 1), 1) - 1},fill=NA,align="right"))
    dat<-dat[,-3]  
    print(horizon.panel.ggplot(dat,paste0("Horizon Plot: 36 Month Rolling Returns (",input$data_start_date_input," to ",input$data_end_date_input,")")))
  })
  
### Tab: "Drawdown"
  output$drawdown_plot<-renderPlot({
    dat<-melted_dataset()
    dat<-ddply(dat,.(variable),transform,dd=dd(value))
    dat<-dat[,-3]
    print(horizon.panel.ggplot(dat,paste0("Horizon Plot: Drawdown (",input$data_start_date_input," to ",input$data_end_date_input,")")))
  })

### Tab: "Drought"
  drought_max_summary<-reactive({
    dat<-ddply(melted_dataset(),.(variable),transform,drought=drought(value))
    dat$drought.max.start<-dat$date[dat$drought.index]
    dat$drought.max.end<-dat$date
    dat<-ddply(dat,.(variable),subset,drought.value==max(drought.value,na.rm=TRUE))
    colnames(dat)[6]<-"drought.max.value"
    dat[,c("date","variable","drought.max.value","drought.max.start","drought.max.end")]
    })

  output$drought_max_summary<-renderPrint({
    drought_max_summary()[,-1]
  })

  output$drought_plot<-renderPlot({
    dat<-melted_dataset()
    dat<-ddply(dat,.(variable),transform,vami=vami(value)) # add vami column
    dat.drought<-arrange(drought_max_summary()[,-1],variable,drought.max.value,desc(drought.max.end)) # format drought_max_summary() and drop the date column
    dat.drought<-dat.drought[!duplicated(dat.drought$variable),] # only keep the most recent max drought if there are duplicates
    dat<-join(dat,dat.drought,by="variable") # join the two datasets for ggplot
    #alpha=(nrow(dat)/10)/nrow(dat)
    p<-ggplot(dat)+
       geom_rect(aes(xmin=drought.max.start,xmax=drought.max.end,ymin=-Inf,ymax=Inf),fill="#FFEDA0")+
       geom_line(aes(x=date,y=vami,group=variable))+
       facet_wrap(~variable,scales="free_y")+
       theme_bw() +                  #this is optional, but I prefer to default
       theme(legend.position = "none",    #remove legend
             strip.text.y = element_text(angle=0, hjust=1),#rotate strip text to horizontal 
             axis.text.y = element_blank(),#remove y axis labels
             axis.ticks = element_blank(), #remove tick marks
             axis.title.y = element_blank(),#remove title for the y axis
             axis.title.x = element_blank(),#remove title for the x axis
             plot.title = element_text(size=16, face="bold", hjust=0))+
       labs(title=paste0("Growth of $1 with Max Drought Shaded Yellow (",input$data_start_date_input," to ",input$data_end_date_input,")"))
    print(p)
  })

  output$drought_plot2<-renderPlot({
    dat<-ddply(melted_dataset(),.(variable),transform,drought=drought(value))
    dat<-dat[,c(1:2,6)] 
    print(horizon.panel.ggplot(dat,paste0("Horizon Plot: Drought (",input$data_start_date_input," to ",input$data_end_date_input,")")))  
    })

  output$drought_test<-renderPrint({
    dat<-ddply(melted_dataset(),.(variable),transform,drought=drought(value))
    dat<-dat[,c(1:2,6)]
    dat
  })

  
### Tab: "Data Preview"
  output$data_choices<-renderPrint({
    choices()
  })
  
  output$data_export_str<-renderPrint({
    str(dataset_final())
  })
  
  output$data_export_summary<-renderPrint({
    head(dataset_final(),5)
  })
  
### Tab: "Example"
  output$example<-renderTable({
    example$date<-as.character(example$date)
    head(na.omit(example[,1:3]),10)
  },digits=4)

### Export Data
  output$exportData<-downloadHandler(
    filename=function() { paste0(input$exportName,".csv") },
    content = function(file) {
      write.csv(data_export(),file,row.names=FALSE)
    }
  )
  
})