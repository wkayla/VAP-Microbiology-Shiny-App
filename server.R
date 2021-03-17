server <- function(input, output) {
  
  
  #Bulid the bashboard
  ##Clinical parameters at the top
  selectData1<-reactive({
    data=taxa[taxa$SubjectID==input$SubjectIDslide,]
  })
  
  output$subject=renderText(paste("Subject:",input$SubjectIDslide,sep=" "))
  output$diagnosis=renderText(paste0("Primary Admitting Diagnosis: ",selectData1()%>%
                                      filter(SubjectID==input$SubjectIDslide)%>%
                                       ungroup()%>%
                                      select(AdmitPrimaryDx)%>%
                                      distinct(AdmitPrimaryDx)%>%
                                       mutate(AdmitPrimaryDx=as.character(AdmitPrimaryDx))))
  output$age=renderText(paste("Age at Intubation (Years): ",selectData1()%>%
                                filter(SubjectID==input$SubjectIDslide)%>%
                                ungroup()%>%
                                distinct(AgeIntubation),sep="\n"))
  output$day=renderText(paste("VAP Day of Diagnosis: ",selectData1()%>%
                                filter(SubjectID==input$SubjectIDslide)%>%
                                ungroup()%>%
                                select(VAPDay_full)%>%
                                distinct(VAPDay_full),sep="\n"))
  output$prism=renderText(paste("PRISM Score: ",selectData1()%>%
                                  filter(SubjectID==input$SubjectIDslide)%>%
                                  ungroup()%>%
                                  select(PRISMScore)%>%
                                  distinct(PRISMScore),sep="\n"))
  output$samples_collected=renderText(paste0("Total Number of Samples Collected: ",selectData1()%>%
                                  filter(SubjectID==input$SubjectIDslide)%>%
                                    ungroup()%>%
                                    select(samples_collected)%>%
                                  distinct(samples_collected)))
  
  output$matched=renderText(paste0("Matched?: ",selectData1()%>%
                                  filter(SubjectID==input$SubjectIDslide)%>%
                                    ungroup()%>%
                                  select(matched)%>%
                                  distinct(matched)%>%
                                  mutate(matched=as.character(matched))))
  

  
  selectData2<-reactive({
    data=mh_test[mh_test$SubjectID==input$SubjectIDslide,]
  })
  
  selectData3<-reactive({
    data=load[load$SubjectID==input$SubjectIDslide,]
  })
  
  selectData4<-reactive({
    data=expand_antibiotic[expand_antibiotic$SubjectID==input$SubjectIDslide,]
  })
  
  output$shannon <- renderPlotly({ggplot(selectData1(),aes(x=num,y=ShannonH.Median)) + 
      geom_point()+
      geom_line()+
      scale_x_continuous(breaks=c(0:14),limits = c(0,14))+
      theme_bw()+
      xlab("Study Day")+
      ylab("Shannon H Diversity")})
  
  output$sobs <- renderPlotly({ggplot(selectData1(),aes(x=num,y=Sobs.Median)) + 
      geom_point()+
      geom_line()+
      scale_x_continuous(breaks=c(0:14),limits = c(0,14))+
      theme_bw()+
      xlab("Study Day")+
      ylab("SOBS")})
  
  
  output$shannone <- renderPlotly({ggplot(selectData1(),aes(x=num,y=ShannonE.Median)) + 
      geom_point()+
      
      geom_line()+
      scale_x_continuous(breaks=c(0:14),limits = c(0,14))+
      theme_bw()+
      xlab("Study Day")+
      ylab("Shannon E Diversity")})
  
  output$mh <- renderPlotly({ggplot(selectData2(),aes(x=num,y=as.numeric(as.character(MH)))) + 
      geom_point()+
      geom_line()+
      scale_y_continuous(limits=c(0,1))+
      scale_x_continuous(breaks=c(0,14),limits = c(0,14))+
      theme_bw()+
      xlab("Study Day")+
      ylab("Moristia Horn Diversity")})
  
  output$lq_all <- renderPlotly({ggplot(selectData3(),aes(x=num,y=lq_all)) + 
      geom_point()+
      
      geom_line()+
      scale_x_continuous(breaks=c(0:14),limits = c(0,14))+
      theme_bw()+
      xlab("Study Day")+
      ylab("Total Bacterial Load")})
  
  
  
  
  output$bar<- renderPlotly({ ggplot(data=selectData1(), 
                                   aes(y=ra, 
                                       x=num, 
                                       fill=factor(taxa))) + 
      geom_bar(stat="identity",width=1,color="Black")+
      scale_x_continuous(breaks=c(0:14),limits = c(-1,15))+
      theme(legend.position = "bottom", 
            legend.box = "horizontal",
            axis.text.x = element_text(angle = 90, hjust = 1),
            legend.text = element_text(size=10))+
      labs(x="Collection", 
           y = "Percentage of Total")+
      labs(fill="")+
      theme_bw()})
  
  
  output$tile<- renderPlot({ ggplot(data=selectData4(), 
                                    aes(y=factor(Med.group), 
                                        x=as.numeric(as.character(num)), 
                                        fill=factor(anti)))+ 
      geom_tile(color="Black")+
      xlim(0,20)+
        scale_y_discrete(position="right")+
      scale_x_continuous(breaks=c(0:20),limits = c(-1,21))+
            guides(fill=F)+
      theme_bw()+
      theme(legend.position = "bottom", 
            legend.box = "horizontal",
            axis.text.x = element_text(angle = 90, hjust = 1,size=12.5),
            axis.text.y = element_text(size=20),
            legend.text = element_text(size=20),axis.title.x = element_text(size=15))+
      labs(x="Study Day", 
           y = "")+
      labs(fill="")})
  
  output$case_dt = DT::renderDataTable(
    Case,
    filter = 'bottom',
    options = list(scrollX = TRUE,page)
  )
  
  output$control_dt = DT::renderDataTable(
    Control,
    filter = 'bottom',
    options = list(scrollX = TRUE)
  )
  
  output$all_dt = DT::renderDataTable(
    all_med,
    filter = 'bottom',
    options = list(scrollX = TRUE)
  )
  output$report <- downloadHandler(
    # For PDF output, change this to "report.pdf"
    filename = "report.doc",
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path(tempdir(), "report.Rmd")
      file.copy("report.Rmd", tempReport, overwrite = TRUE)
      
      # Set up parameters to pass to Rmd document
      params <- list(n = input$SubjectIDslide)
      
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
    }
  )
}
