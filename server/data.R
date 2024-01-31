allarea <- reactive({
  c(input$reg, input$nat)
})

output$tit_ind <- renderText({
  validate(need(
    expr = input$data_ind,
    message = "",
    label = ""
  ))
  ind  %>%
    filter(fullname %in% input$data_ind) %>%
    pull(fullname)
})

output$def_ind <- renderUI({
  validate(need(
    expr = input$data_ind,
    message = "",
    label = ""
  ))
  tt <- ind %>%
    select(definition, fullname) %>%
    filter(fullname %in% input$data_ind) %>%
    pull(definition)
  HTML(tt)
})

output$tit_scen <- renderText({
  validate(need(
    expr = input$scenario,
    message = "",
    label = ""
  ))
  tt <- dimen %>%
    filter(dim == "scenario") %>%
    filter(code == input$scenario) %>%
    pull(name)
  if (length(input$scenario) > 1)
    tt <- "Multiple Scenarios Selected"
  return(tt)
})

output$def_scen <- renderUI({
  validate(need(
    expr = input$scenario,
    message = "",
    label = ""
  ))
  tt <- dimen %>%
    filter(dim == "scenario") %>%
    filter(code == input$scenario) %>%
    pull(details)
  if (length(input$scenario) > 1)
    tt <- "See About tab for full scenario details."
  return(HTML(tt))
})

output$df_warn <- renderUI({
  tt <- ""
  w0 <- 0
  # geog %>%
  #   filter(name %in% input$nat) %>%
  #   pull(is171)
  if (sum(w0 == 0) > 0)
    tt <-
    "<FONT COLOR='gray'>Your selection includes at least one country with limited base year data on educational attainment. Please consult the FAQ in the About page for more information<br><br>"
  HTML(tt)
})

df_build <- reactive({
  validate(
    need(
      expr = input$data_ind,
      message = "",
      label = ""
    ),
    need(
      expr = input$scenario,
      message = "",
      label = ""
    ),
    need(
      expr = input$age,
      message = "",
      label = ""
    ),
    need(
      expr = input$sex,
      message = "",
      label = ""
    ),
    need(
      expr = input$year,
      message = "",
      label = ""
    )
  )
  d1 <- d2 <- d3 <- NULL
  
  d1 <- ind %>%
    filter(fullname == input$data_ind)
  
  withProgress(message = 'Creating Data',
               detail = 'May take a few seconds...',
               value = 0,
               {
                 for (i in input$scenario) {
                   #input<-NULL; input$age=0;  input$sex=2;  input$year=0;  input$year=2020; input$data_ind=ind2[[2]][3]; input$scenario=c(sn1[1],sn1[2]); input$isono=TRUE; i=2
                   # input$reg <- c("Oceania", "Asia")
                   d2 <- NULL
                   
                   # columns of data to load
                   v <-
                     c("period",
                       "year",
                       "age",
                       "ageno",
                       "sex",
                       "sexno",
                       "edu",
                       "eduno")
                   v <- geog %>%
                     filter(name %in% allarea()) %>%
                     # filter(name %in% c("France", "Asia")) %>%
                     pull(isono) %>%
                     c(v, .)
                   
                   sn <- dimen %>%
                     filter(dim == "scenario", code == i) %>%
                     pull(sname)
                   

                   d2 <- tibble(
                     v = v,
                     file = paste0("../wcde-data/wcde-v3-single/", i, "/", d1$name, "/", v, ".rds")
                   ) %>%
                     mutate(d = map(.x = file, .f = ~read_rds(.x))) %>%
                     select(-file) %>%
                     pivot_wider(names_from = v, values_from = d) %>%
                     unnest(col = names(.)) %>%
                     mutate(age = fct_inorder(age),
                            scenario = sn) %>%
                     relocate(scenario) %>%
                     filter(ageno %in% input$age,
                            year %in% input$year,
                            # eduno %in% edu0,
                            sexno %in% input$sex)
                   
                   # when get country or regions selected flip the data frame
                   if (length(v) > 8) {
                     d2 <- d2 %>%
                       gather(
                         key = "isono",
                         value = !!d1$cname,
                         -(1:9),
                         convert = TRUE
                       ) %>%
                       left_join(geog %>% select(isono, name), by = "isono") %>%
                       rename(area = name) %>%
                       arrange(eduno, sexno, ageno, area, year) %>%
                       select(scenario, area, everything())
                     
                     if (input$isono == FALSE)
                       d2 <- d2 %>% select(-isono)
                   }
                   
                   # drop columns
                   if (d1$period == 0)
                     d2 <- d2 %>% select(-period)
                   if (d1$period == 1)
                     d2 <- d2 %>% select(-year)
                   
                   if (sum(d1$age, d1$bage, d1$sage) == 0)
                     d2 <- d2 %>% select(-age)
                   if (sum(d1$age, d1$bage, d1$sage) > 0 &
                       sum(as.numeric(input$age)) == 0)
                     d2 <- d2 %>% select(-age)
                   
                   if (d1$sex == 0)
                     d2 <- d2 %>% select(-sex)
                   if (sum(as.numeric(input$sex)) == 0 & d1$sex == 1)
                     d2 <- d2 %>% select(-sex)
                   
                   if (d1$edu == 0)
                     d2 <- d2 %>% select(-edu)
                   if (d1$edu == 1)
                     d2 <- d2 %>% rename(education = edu)
                   
                   # names(d2)[ncol(d2)] <- ind %>% filter(fullname==input$data_ind) %>% .[["cname"]]
                   
                   if (length(input$scenario) == 1)
                     d2 <- d2 %>% select(-scenario)
                   
                   d2 <- d2 %>%
                     drop_na() %>%
                     select(-ageno,-sexno,-eduno) %>%
                     set_names(str_to_title(names(.)))
                   
                   if (input$isono == TRUE)
                     d2 <- d2 %>% rename(ISOCode = Isono)
                   
                   d3 <- d3 %>% bind_rows(d2)
                   incProgress(1 / length(input$scenario))
                 }
               })
  return(d3)
})

output$df <- renderDataTable({
  df_build()
},  options = list(
  searching = TRUE,
  paging = TRUE,
  aoColumnDefs = list(list(
    sClass = "alignRight", aTargets = list(-1)
  )),
  drawCallback = I(
    "function( settings ) {document.getElementById('df').style.width = '800px';}"
  )
))


output$data_dl <- downloadHandler(
  filename = function() {
    "wicdf.csv"
  },
  content = function(filename) {
    sn0 <-
      dimen %>% filter(dim == "scenario") %>% filter(code == input$scenario) %>% .[["name"]]
    d2 <- ind %>%
      filter(fullname == input$data_ind) %>%
      select(fullname, definition) %>%
      mutate(scen = ifelse(length(input$scenario) == 1, sn0, "Multiple Scenarios"))
    d2 <- rbind(t(d2), " ")
    colnames(d2) <- ""
    
    warn <- NULL
    if (length(allarea()) > 0) {
      if (sum(geog %>% filter(name %in% allarea()) %>% .[["is171"]], na.rm = TRUE) >
          0) {
        warn <-
          "Note: Your selection includes at least one country with limited base year data on educational attainment. Please consult the FAQ for more information"
      }
    }
    csvinfo0 <-
      paste0(substring(csvinfo, 1, nchar(csvinfo) - 1), warn, "\n")
    
    fh <- file(filename, "w")
    cat(csvinfo0, file = fh)
    write.csv(d2, file = fh, row.names = FALSE)
    write.csv(df_build(), fh, row.names = FALSE)
    close(fh)
  }
)

output$data_dl0 <- downloadHandler(
  filename = function() {
    "wcde_data.csv"
  },
  content = function(filename) {
    sn0 <- dimen %>%
      filter(dim == "scenario") %>%
      filter(code == input$scenario) %>%
      .[["name"]]
    d2 <- ind %>%
      filter(fullname == input$data_ind) %>%
      select(fullname, definition) %>%
      mutate(scen = ifelse(length(input$scenario) == 1, sn0, "Multiple Scenarios"))
    d2 <- rbind(t(d2), " ")
    colnames(d2) <- ""
    
    warn <- NULL
    if (length(allarea()) > 0) {
      if (sum(geog %>% filter(name %in% allarea()) %>% .[["is171"]], na.rm = TRUE) >
          0) {
        warn <-
          "Note: Your selection includes at least one country with limited base year data on educational attainment. Please consult the FAQ for more information"
      }
    }
    csvinfo0 <-
      paste0(substring(csvinfo, 1, nchar(csvinfo) - 1), warn, "\n")
    
    fh <- file(filename, "w")
    cat(csvinfo0, file = fh)
    write.csv(d2, file = fh, row.names = FALSE)
    write.csv(df_build(), fh, row.names = FALSE)
    close(fh)
  }
)
