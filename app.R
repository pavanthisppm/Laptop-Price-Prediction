# import libraries
library(shiny)
library(shinythemes)
library(shinyWidgets)
library(data.table)
# Read in the RF model
model <-readRDS("model.rds")
# User interface
ui <- tagList(fluidPage( 
                theme = shinytheme("superhero"),
                setBackgroundImage(
                  src = "https://images.unsplash.com/photo-1510519138101-570d1dca3d66?ixlib=rb-1.2.1&ixid=MnwxMjA3fDB8MHxzZWFyY2h8ODF8fGxhcHRvcHxlbnwwfHwwfHw%3D&auto=format&fit=crop&w=500&q=60",
                  
                ),
                
                # Page header
                headerPanel('Laptop Price Prediction'),
                
                # Input values
                sidebarPanel(
                  HTML("<h3>Input parameters</h3>"),
                  style = "overflow-y:scroll; max-height: 600px; position:relative;",
                  selectInput("Company", label = "Company:",
                              choices = list("Acer" = "Acer", "Apple" = "Apple", "Asus" = "Asus",
                                             'Chuwi'='Chuwi','Dell'='Dell','Fujitsu'='Fujitsu',
                                             'Google'='Google','HP'='HP','Huawei'='Huawei',
                                             'Lenovo'='Lenovo','LG'='LG','Mediacom'='Mediacom',
                                             'Microsoft'='Microsoft','MSI'='MSI','Razer'='Razer',
                                             'Samsung'='Samsung','Toshiba'='Toshiba','Vero'='Vero',
                                             'Xiaomi'='Xiaomi'),
                              selected = "Asus"),
                  selectInput('TypeName',label='Type Name:',
                              choices = list('2 in 1 Convertible'='2 in 1 Convertible',
                                             'Gaming'='Gaming','Netbook'='Netbook','Notebook'='Notebook',
                                             'Ultrabook'='Ultrabook','Workstation'='Workstation'),
                              selected = 'Notebook'),
                  sliderInput("Ram", label="Ram(GB):",
                              min=2,max=64,
                              value = 8),
                  selectInput('OpSys',label = 'Operating System:',
                              choices = list('Linux'='Linux','Mac'='Mac','No OS'='No OS','Others'='Others','Windows'='Windows'),
                              selected = 'Windows'),
                  sliderInput("Weight", label="Weight(kg):",
                              min = 0.69, max = 4.70,
                              value = 3.0, step=0.01),
                  selectInput('IPS',label='IPS:',
                              choices = list('Yes'='Yes','No'='No'),
                              selected = 'Yes'),
                  selectInput('TouchScreen',label='Touch Screen:',
                              choices = list('Yes'='Yes','No'='No'),
                              selected = 'Yes'),
                  sliderInput("size", label="Screen Size(Inches):",
                              min = 10, max = 20,
                              value = 14, step=0.1),
                  selectInput("screen", label = "Screen Resolution:",
                              choices = list('1366x768'='1366x768','1440x900'='1440x900','1600x900'='1600x900',
                                             '1920x1080'='1920x1080','1920x1200'='1920x1200','2160x1440'='2160x1440',
                                             '2256x1504'='2256x1504','2304x1440'='2304x1440','2400x1600'='2400x1600',
                                             '2560x1440'='2560x1440','2560x1600'='2560x1600','2736x1824'='2736x1824',
                                             '2880x1800'='2880x1800','3200x1800'='3200x1800','3840x2160'),
                              selected = '1920x1080'),
                  selectInput('CpuBrand',label='Cpu Brand:',
                              choices = list('AMD Processor'='AMD Processor',
                                             'Intel Core i3'='Intel Core i3',
                                             'Intel Core i5'='Intel Core i5',
                                             'Intel Core i7'='Intel Core i7',
                                             'Other Intel Processor'='Other Intel Processor'),
                              selected = 'Intel Core i5'),
                  sliderInput("ClockSpeed", label = "Clock Speed(GHz):",
                              min = 0.9, max = 3.6,
                              value = 2.0, step=0.01),
                  sliderInput("SSD", label = "SSD(GB):",
                              min = 0, max = 1024,
                              value = 256,step = 4),
                  sliderInput("HDD", label = "HDD(GB):",
                              min = 0, max = 2000,step = 4,
                              value = 500),
                  sliderInput("FlashStorage", label = "Flash Storage(GB):",
                              min = 0, max = 512,step = 4,
                              value = 512),
                  sliderInput("Hybrid", label = "Hybrid(GB):",
                              min = 0, max = 1000,step = 4,
                              value = 1000),
                  selectInput('GpuBrand',label='GPU Brand:',
                              choices = list('AMD'='AMD',
                                             'ARM'='ARM',
                                             'Intel'='Intel',
                                             'Nividia'='Nividia'
                              ),
                              selected = 'Intel Core i5'),
                  
                  actionButton("submitbutton", "Submit", class = "btn btn-primary"),
                  HTML('<br><br><br><br><br>')
                ),
                
                mainPanel(
                  tags$style(
                    "body {overflow-y: hidden;}"
                  ),
                  tags$label(h3('Status/Output')), # Status/Output Text Box
                  verbatimTextOutput('contents'),
                  tableOutput('Result') ,# Prediction results table
                )
                ),
              tags$footer("Designed by: Group No:4/ST3082/2021-22", align = "center", style = "
              position:absolute;
              bottom:0;
              width:100%;
              height:30px;  
              color: white;
              padding: 5px;
              background-color: black;
              z-index: 1000;"))

# Server
server <- function(input, output, session) {
  
  # Input Data
  datasetInput <- reactive({
    
    df <- data.frame(
      Name = c("Company",
               "TypeName",
               "Ram",
               "OpSys",
               "Weight",
               "IPS","TouchScreen",'size',"screen","CpuBrand",
               "ClockSpeed","SSD","HDD","FlashStorage","Hybrid","GpuBrand"),
      Value = as.character(c(input$Company,
                             input$TypeName,
                             input$Ram,
                             input$OpSys,
                             input$Weight,
                             input$IPS,
                             input$TouchScreen,input$size,input$screen,input$CpuBrand,input$ClockSpeed,
                             input$SSD,input$HDD,input$FlashStorage,
                             input$Hybrid,input$GpuBrand)),
      stringsAsFactors = F)
    
    Price_euros <- 'Price_euros'
    df <- rbind(df, Price_euros)
    input <- transpose(df)
    write.table(input,"input.csv", sep=",", quote = FALSE, row.names = FALSE, col.names = FALSE)
    
    test1 <- read.csv(paste("input", ".csv", sep=""), header = TRUE)
    test1$Xres=as.integer(substr(test1$screen,1,4))
    test1$Yres=as.integer(substr(test1$screen,6,9))
    test1$PPI=sqrt((test1$Xres^2)+(test1$Yres^2))/test1$size
    test=subset(test1, select=-c(screen,size,Xres,Yres))
    
    
    
    test$Company <- factor(test$Company, levels = c('Acer','Apple','Asus','Chuwi','Dell','Fujitsu','Google','HP',
                                                    'Huawei','Lenovo','LG','Mediacom','Microsoft','MSI','Razer','Samsung',
                                                    'Toshiba','Vero','Xiaomi'))
    test$TypeName <- factor(test$TypeName, levels = c('2 in 1 Convertible','Gaming','Netbook','Notebook','Ultrabook' ,
                                                      'Workstation'))
    test$OpSys <- factor(test$OpSys, levels = c('Linux','Mac','No OS','Others','Windows'))
    test$CpuBrand <- factor(test$CpuBrand, levels = c('AMD Processor','Intel Core i3','Intel Core i5','Intel Core i7',
                                                      'Other Intel Processor'))
    test$GpuBrand <- factor(test$GpuBrand, levels = c('AMD','Intel','ARM','Nividia'
    ))
    
    
    Output <- data.frame(Prediction_in_Euros=predict(model,test))
    print(Output)
 
  })
  
  # Status/Output Text Box
  output$contents <- renderPrint({
    if (input$submitbutton>0) {
      isolate("Calculation complete.")
    } else {
      return("Server is ready for calculation.")
    }
  })
  
  # Prediction results
  output$Result <- renderTable({
    if (input$submitbutton>0) {
      isolate(datasetInput())
    }
  })
   
}

# Create the shiny app  
shinyApp(ui = ui, server = server)
