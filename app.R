#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
library(DT)
library(openxlsx)
library(writexl)
library(markdown)
source("rpkm.R", local = TRUE)
source("tpm.R", local = TRUE)
source("zScore.R", local = TRUE)
source("cpm.R", local = TRUE)

loadExampleData <- function() {
    data <- read.delim("data/exampleData.txt", sep = "\t", header = TRUE)
    return(data)
}

# Define UI for application
ui <- fluidPage(
    theme = shinytheme("cerulean"),
    titlePanel("Normalization of count matrix"),
    downloadButton("download_excel", "Download Data to Excel"),
    
    sidebarLayout(
        sidebarPanel(
            selectInput("dataset", "Dataset", c("upload my own", "example data")),
            conditionalPanel(
                "input.dataset === 'upload my own'",
                fileInput("datafile", ""),
                textInput("datafile_sep", "Field Separator", value = "\t")
            )
        ),
        mainPanel(
            tabsetPanel(
                tabPanel("About", includeMarkdown("README.md")),
                tabPanel("Data", DTOutput("dataTable")),
                # downloadButton("download_button", "Download all Data"),
                tabPanel("RPKM", DTOutput("rpkm")),
                tabPanel("TPM", DTOutput("tpm")),
                tabPanel("Z-score", DTOutput("zscore")),
                tabPanel("CPM", DTOutput("cpmTable"))
                
            )
        )
    )
)

# Define server logic
server <- function(input, output, session) {
    
    # Data Input and Pre-processing
    dataset <- reactive({
        if (input$dataset == "upload my own") {
            inFile <- input$datafile
            if (is.null(inFile))
                return(NULL)
            read.delim(inFile$datapath,
                       sep = input$datafile_sep,
                       header = TRUE)
        } else if (input$dataset == "example data") {
            loadExampleData()
        }
    })
    
    # Data Table
    # Render the data table
    output$dataTable <- renderDataTable({
        datatable(dataset())
    })
    
    # Rpkm =========================
    
    rpkm_data <- reactive({
        req(dataset())
        generate_rpkm(dataset())
    })
    
    output$rpkm <- renderDataTable({
        req(rpkm_data())
        datatable(rpkm_data())
    })
    
    # =========================
    
    # TPM  =========================
    tpm_data <- reactive({
        req(dataset())
        generate_tpm(dataset())
        
    })
    
    
    output$tpm <- renderDataTable({
        req(tpm_data())
        datatable(tpm_data())
    })
    
    # =========================
    
    # z-score  =========================
    
    z_data <- reactive({
        req(tpm_data())
        generate_zscore(tpm_data())
        
    })
    
    # Render the Z-score table
    output$zscore <- renderDataTable({
        req(z_data())
        datatable(z_data())
    })
    
    # =========================
    
    # CPM =========================
    cpm_data <- reactive({
        req(dataset())
        generate_cpm(dataset())
    })
    
    output$cpmTable <- renderDataTable({
        req(cpm_data())
        datatable(cpm_data())
    })
    
    # =========================
    
    
    # Download outputs as Excel files
    
    
    output$download_excel <- downloadHandler(
        filename = function() {
            "data.xlsx"
        },
        content = function(file) {
            my_workbook <- createWorkbook()
            
            addWorksheet(
                wb = my_workbook,
                sheetName = "raw Data"
            )
            
            writeData(
                my_workbook,
                sheet = 1,
                dataset(),
                startRow = 1,
                startCol = 1
            )
            
            #====
            addWorksheet(
                wb = my_workbook,
                sheetName = "rpkm Data"
            )
            
            writeData(
                my_workbook,
                sheet = 2,
                rpkm_data(),
                startRow = 1,
                startCol = 1
            )
            #====
            
            #====
            addWorksheet(
                wb = my_workbook,
                sheetName = "tpm Data"
            )
            
            writeData(
                my_workbook,
                sheet = 3,
                tpm_data(),
                startRow = 1,
                startCol = 1
            )
            #====
            #====
            addWorksheet(
                wb = my_workbook,
                sheetName = "z Data"
            )
            
            writeData(
                my_workbook,
                sheet = 4,
                z_data(),
                startRow = 1,
                startCol = 1
            )
            #====
            #====
            addWorksheet(
                wb = my_workbook,
                sheetName = "cpm Data"
            )
            
            writeData(
                my_workbook,
                sheet = 5,
                cpm_data(),
                startRow = 1,
                startCol = 1
            )
            #====
            
            
            saveWorkbook(my_workbook, file)
        }
    )
    
    # =========================
    
    
    
}
    

shinyApp(ui = ui, server = server)
