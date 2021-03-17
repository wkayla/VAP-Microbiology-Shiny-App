library(shinydashboard)
library(dplyr)
library(shiny)
library(tidyverse)
library(splitstackshape)
library(plotly)
library(ggplot2)


ui <- dashboardPage(
  dashboardHeader(title = "VAP Dashboard"),
  ## Sidebar content
  dashboardSidebar(sidebarMenu(
    menuItem(
      "Dashboard", tabName = "dashboard", icon = icon("dashboard")
    ),
    menuItem("Case Data", tabName = "widget", icon = icon("th")),
    menuItem("Control Data", tabName = "widget1", icon = icon("th")),
    
    menuItem("All Data", tabName = "widget2", icon = icon("th"))
  )),
  ## Body content
  dashboardBody(tabItems(
    # First tab content
    tabItem(tabName = "dashboard",
            
            fluidPage(box(
              title = "Subjects",
              selectInput("SubjectIDslide", "Subject:",as.character(names))),
              box(textOutput("diagnosis")),
              box(textOutput("age")),
              box(textOutput("day")),
              box(textOutput("prism")),
              
              box(textOutput("matched")),
              downloadButton("report", "Generate report")),
            fluidRow(
              box(plotlyOutput("bar", height = 500,width=700))),
              
  
            fluidRow(
              box(plotlyOutput("lq_all", height = 250, width = 500))),
              fluidRow(
              box(plotOutput("tile", height = 250, width = 700))
              
            ),
            fluidRow(
              box(plotlyOutput("sobs", height = 250, width = 500))),
            fluidRow(
              box(plotlyOutput("shannone", height = 250, width = 500))),
            fluidRow(
              box(plotlyOutput("shannon", height = 250, width = 500))),
    fluidRow(
      box(plotlyOutput("mh", height = 250, width = 500)))),
    
    # Second tab content
    tabItem(tabName = "widget",
            fluidRow(DT::dataTableOutput('case_dt'))),
    
    tabItem(tabName = "widget1",
            fluidRow(DT::dataTableOutput('control_dt'))),
    tabItem(tabName = "widget2",
            fluidRow(DT::dataTableOutput('all_dt')))
  ))
)
