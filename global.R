
## create pick list of UI modules

IslandNames<-unique(surveys$Island)
SpeciesNames<-species_tlu$CommonName

#### Island Module ####

IslChooserUI<-function(id){
  ns<-NS(id)
  selectizeInput(inputId=ns("IslIn"),label="Select Island:" , choices=NULL, selected ="All Islands")
}

IslChooser<-function(input,output,session,data, chosen){
  observe({updateSelectizeInput(session, "IslIn", selected=chosen(),
                                choices=c("Choose an Island"="", c("All Islands"), IslandNames )
  )})
  return(reactive(input$IslIn))
}


#### Species Module ####

SpeciesChooserUI<-function(id){
  ns<-NS(id)
  selectizeInput(inputId = ns("SpeciesIn"), label="Species:", choices=NULL)
}

SpeciesChooser<-function(input, output, session, data, island, chosen){
  observe({
    updateSelectizeInput(session, inputId = "SpeciesIn", selected=chosen(), 
                         choices=c("Choose a species"="",SpeciesNames), selected = "Common Eider")
  })
  return(reactive(input$SpeciesIn))
}

### Survey Module ####

SurveyChooserUI<-function(id){
  ns<-NS(id)
  selectizeInput(inputId=ns("SurveyIn"), label="Survey Class:", choices=NULL)
}


SurveyChooser<-function(input, output, session, data, island, species, chosen){
  observe(
    updateSelectizeInput(session, inputId="SurveyIn",selected=chosen(), choices=c("Choose a Survey Class"="", C("Boat",Ground))))
  
  return(reactive(input$SurveyIn))
}

#### Years Module ####
yearChooserUI<-function(id){
  ns<-NS(id)
  sliderInput(inputId=ns("YearsShow"), label= "Years to Display:", min=2000, max=2020, step=1, value=c(2000,2020),sep="",ticks=T)
}


yearChooser<-function(input,output,session,data,chosen)  {
  
  observe({
    req( data() )
    if(class(data()$Date)=="Date"){
      YrMax<-reactive(max(year(data()$Date), na.rm=T))
      YrMin<-reactive(min(year(data()$Date), na.rm=T))
      updateSliderInput(session, inputId="YearsShow", min=YrMin(),max=YrMax(),val=c(YrMin(),YrMax()) )
      #value=chosen())
    }
  })
  
  return(reactive(input$YearsShow))
}

