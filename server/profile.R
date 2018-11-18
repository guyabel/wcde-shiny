output$prof_dl <- downloadHandler(
  filename = function() { paste0(input$prof_geo,".pdf") },
  content = function(file) {
    file.copy(paste0("./pdf/",input$prof_geo,".pdf"), file)
  }
)
