#legend works for sac as well
leg_gvis <- function(edu = input$pyr_edu){
  if(edu==4){
    leg <- leg_data(d = edu4)
  }
  if(edu==6){
    leg <- leg_data(d = edu6)
  }
  if(edu==8){
    leg <- edu10 %>%
      drop_na() %>%
      leg_data()
  }
  if(edu != 8){
    g <- gvisBarChart(
      data = leg, 
      xvar = "Total", yvar = names(leg)[-1], 
      options = list(
        colors = get(paste0("iiasa", edu)),
        height = 30, width = 900, 
        legend = "{position:'top', textStyle: {fontSize: 12}, alignment:'center'}",
        chartArea = "{right:'0%',left:'0%',width:'100%',top:'100%',height:'0%'}"
      )
    )
  }
  if(edu == 8){
    g1 <- gvisBarChart(
      data = leg, 
      xvar = "Total", yvar = names(leg)[2:6], 
      options = list(
        colors = iiasa8a,
        height = 30, width = 900, 
        legend = "{position:'top', textStyle: {fontSize: 12}, alignment:'center'}",
        chartArea = "{right:'0%',left:'0%',width:'100%',top:'100%',height:'0%'}"
      )
    )
    g2 <- gvisBarChart(
      data = leg, 
      xvar = "Total", yvar = names(leg)[7:10], 
      options = list(
        colors = iiasa8b,
        height = 30, width = 900, 
        legend = "{position:'top', textStyle: {fontSize: 12}, alignment:'center'}",
        chartArea = "{right:'0%',left:'0%',width:'100%',top:'100%',height:'0%'}"
      )
    )
    g <- gvisMerge(x = g1, y = g2)
  }
  return(g)
}

dl_gvis <- function(g = gg, h = "head.html", file_type = input$pyr_dl){
  g$html$caption <- includeHTML(h)
  
  # print google vis
  print(g, file = "gg.html")

  if(file_type == "pdf"){
    system("wkhtmltopdf   --enable-javascript --javascript-delay 2000 gg.html gg.pdf") #; file.show("gg.pdf")
    file.copy("./gg.pdf", file)
    file.remove("gg.pdf")
  }
  if(file_type == "png"){
    system("wkhtmltoimage --enable-javascript --javascript-delay 2000 gg.html gg.png") #; file.show("gg.png")
    file.copy("./gg.png", file)
    file.remove("gg.png")
  }
  file.remove("gg.html")
  file.remove("head.html")
}


pyr_gvis <- function(d_pyr, 
                     pyr_year, 
                     pyr_col = iiasa6, 
                     w = 295, h = 500, 
                     legend = "top", 
                     pmax = NULL, no_edu = FALSE, 
                     prop = FALSE, 
                     edu = 6){
  #d_pyr<-d1;pyr_year=2015; pyr_col=iiasa4; w=400;pmax=NULL; h=550; prop=FALSE; no_edu=FALSE
  m_pyr <- d_pyr %>% 
    filter(year == pyr_year, 
           sexno == 1, 
           ageno != 0) %>% 
    select(ageno, age, edu, pop) %>% 
    mutate(pop = ifelse(pop == 0, NA, pop)) %>%
    spread(key = edu, value = pop) %>%
    arrange(desc(ageno)) %>% 
    select(-ageno)
  
  f_pyr <- d_pyr %>% 
    filter(year == pyr_year, 
           sexno == 2, 
           ageno != 0) %>% 
    select(ageno, age ,edu,pop) %>% 
    mutate(pop = ifelse(pop == 0, NA, pop)) %>%
    spread(key = edu, value = pop) %>%
    arrange(desc(ageno)) %>% 
    select(-ageno) 

  if(no_edu == TRUE){
    m_pyr <- m_pyr %>% select(age, Total)
    f_pyr <- f_pyr %>% select(age, Total)
  }
  if(no_edu == FALSE){
    m_pyr <- m_pyr %>% select(-Total)
    f_pyr <- f_pyr %>% select(-Total)
  }
  
  # pyramid sum
  s_pyr <- d_pyr %>% 
    filter(year == pyr_year, 
           sexno == 0, 
           ageno == 0, 
           eduno == 0) %>%
    mutate(pop = round(pop, 2)) %>%
    pull(pop)
  
  if(is.null(pmax)){
    pmax <- d_pyr %>% 
      filter(year == pyr_year, 
             ageno != 0, 
             sexno != 0, 
             eduno == 0) %>% 
      pull(pop) %>%
      max(., na.rm=TRUE)
  }
  
  if(prop==TRUE)
    pmax <- 1
  
  bar1 <- gvisBarChart(
    data = m_pyr, 
    xvar = "age", yvar = names(m_pyr)[-1],
    options = list(
      legend = "none", 
      bar = "{groupWidth:'90%'}", 
      isStacked = if(prop == TRUE) 'percent' else TRUE, 
      title = paste0("Total Population: ", s_pyr," m"), 
      titleTextStyle = "{fontName:'Arial',fontSize:16}",
      hAxis = paste0("{direction:-1, maxValue:",pmax,", title: 'Male'}"),
      colors = pyr_col, 
      height = h, width = w, 
      #tooltip = "{isHtml:'true'}",
      #focusTarget = "{'category'}",
      chartArea="{right:'0%',left:'15%',width:'85%',top:'5%',height:'85%'}"
    )
  )
  
  bar2 <- gvisBarChart(
    data = f_pyr, 
    xvar = "age", yvar = names(f_pyr)[-1],
    options = list(
      legend="none", 
      bar = "{groupWidth:'90%'}", 
      isStacked = if(prop == TRUE) 'percent' else TRUE, 
      # title = paste0("Total Population: ", s_pyr," m"), 
      titleTextStyle = "{fontName:'Arial',fontSize:16}",
      hAxis = paste0("{maxValue:",pmax,", title: 'Female'}"),
      colors = pyr_col, 
      height = h, width = w, 
      chartArea = "{right:'15%',left:'0%',width:'85%',top:'5%',height:'85%'}"
    )
  )
  
  # this will make both pyramids redraw
  # top1 <- leg_gvis(edu = edu)
  # if(no_edu == TRUE){
  #   top1 <- NULL
  # }

  gg0 <- gvisMerge(x = bar1, y = bar2, 
                   horizontal = TRUE, 
                   tableOptions = "cellspacing=0, cellpadding=0")
  
  # if(legend == "none")
  #   gg <- gg0
  # if(legend == "top"){
  #   gg <- gvisMerge(x = top1, 
  #                   y = gg0,
  #                   tableOptions = "cellspacing=0, cellpadding=0")
  # }
  gg <- gg0
  return(gg)
}
# pyr_gvis(d_pyr = d1, pyr_year = 2010, pyr_col = iiasa4, edu = 4) %>%
#   plot()

pyr_fill <- function(year = input$pyr_year1, edu = input$pyr_edu, geo = input$pyr_geo1){
  p <- FALSE
  if(year < 2015 & edu == "8"){
    p <- TRUE
  }
  g <- geog %>%
    filter(name %in% geo) %>%
    pull(edu8)
  if(g == 0 & edu == "8"){
    p <- TRUE
  }
  return(p)
}

pyr_warn <- function(f = NULL, year = input$pyr_year1){
  w <- ""
  if(f == TRUE)
    w0 <- "You have selected eight categories for the educational background. These data are only available from 2015 onwards for selected countries. Please consult the FAQ in the About page for more information."
  # if(f == TRUE & year < 2015)
  #   w0 <- "You have selected eight categories for the educational background. These data are only available from 2015 onwards for selected countries. Please consult the FAQ in the About page for more information."
  # if(f == TRUE & year >= 2015)
  #   w0 <- "You have selected eight categories for the educational background. These data are only available from 2015 onwards for selected countries. Please consult the FAQ in the About page for more information."
  if(f == TRUE)
    w <- paste0("<FONT COLOR='gray'>", w0, "<br><br>")
  return(w)
}

# input$pyr_geo1 = 250; input$pyr_sn1 = 2; input$pyr_edu = 8
# geo = input$pyr_geo1; sn = input$pyr_sn1; edu = input$pyr_edu
pyr_data <- function(geo = input$pyr_geo1, 
                     sn = input$pyr_sn1,
                     edu = input$pyr_edu){
  v <- c("year", "age", "ageno", "sex", "sexno", "edu", "eduno")
  v <- geog %>%
    filter(name %in% geo) %>%
    pull(isono) %>%
    c(v, .)
  # v <- c(v, 250)
  # v <- c(v, 44)
  
  # edu8_avail <- geog %>%
  #   replace_na(list(edu8 = 0)) %>%
  #   filter(name %in% geo) %>%
  #   pull(edu8) %>%
  #   sum()
  
  df1 <- loads(file = paste0("df", sn, "/epop"), 
               variables = v, ultra.fast = TRUE, to.data.frame=TRUE) %>%
    tbl_df() %>%
    mutate_if(is.factor, as.character) %>%
    mutate(edu = fct_inorder(edu)) %>%
    gather(key = isono, value = pop, -(1:7)) %>% 
    mutate(pop = pop/1e3) %>%
    mutate(scenario = sn) 
  # df1 %>% filter(year == 2015, sexno != 0, ageno == 14, sexno == 1)
  if(edu == "4"){
    df2 <- df1 %>%
      left_join(edu4, by = "eduno") %>%
      mutate(edu = fct_inorder(edu_name)) %>%
      select(-edu_name) %>%
      drop_na() %>%
      group_by(scenario, year, ageno, age, sex, sexno, edu) %>%
      summarise(pop=sum(pop)) %>%
      ungroup() %>%
      mutate(eduno = ifelse(edu == "Total", 0, 1))
  }
  if(edu == "6"){
    df2 <- df1 %>%
      left_join(edu6, by = "eduno") %>%
      mutate(edu = fct_inorder(edu_name)) %>%
      select(-edu_name) %>%
      drop_na() %>%
      group_by(scenario, year, ageno, age, sex, sexno, edu) %>%
      summarise(pop=sum(pop)) %>%
      ungroup() %>%
      mutate(eduno = ifelse(edu == "Total", 0, 1))
  }
  if(edu == "8"){
    df2 <- df1 %>%
      # filter(eduno != 7) %>%
      left_join(edu10, by = "eduno") %>%
      mutate(edu = fct_inorder(edu_name)) %>%
      select(-edu_name) %>%
      drop_na() %>%
      group_by(scenario, year, ageno, age, sex, sexno, edu) %>%
      summarise(pop=sum(pop)) %>%
      ungroup() %>%
      # all education splits to zero if less than 2015
      mutate(pop = ifelse(year < 2015 & edu != "Total", 0, pop)) %>%
      # fill in missing rows for masters etc pre 2015
      complete(scenario, year, age, sex, edu, fill = list(pop = 0)) %>%
      fill(ageno, sexno) %>%
      mutate(eduno = ifelse(edu == "Total", 0, 1))
  }
  return(df2)
}

leg_data <- function(d){
  d %>%
    select(edu_name) %>%
    distinct() %>%
    mutate(edu_name = fct_inorder(edu_name), 
           age = 0) %>%
    spread(key = edu_name, value = age)
}

max_age_sex <- function(d){
  d %>% 
    filter(ageno != 0, 
           sexno != 0, 
           eduno == 0) %>% 
    pull(pop) %>%
    max(., na.rm=TRUE)
}
