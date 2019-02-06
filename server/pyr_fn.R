#legend works for sac as well
leg_gvis <- function(edu = input$pyr_edu, dl = FALSE){
  if(edu==4){
    leg <- leg_data(d = edu4)
  }
  if(edu==6){
    leg <- leg_data(d = edu6)
  }
  if(edu==8){
    leg <- edu10 %>%
      filter(eduno != 7) %>%
      leg_data()
  }
  
  if(edu != 8){
    g <- gvisBarChart(
      data = leg, 
      xvar = "Total", yvar = names(leg)[-1], 
      options = list(
        colors = get(paste0("iiasa", edu)),
        height = 30, width = 900, 
        legend = ifelse(dl == FALSE, 
                        "{position:'top', textStyle: {fontSize: 12}, alignment:'center'}",
                        "{position:'top', textStyle: {fontSize: 12}}"),
        chartArea = "{right:'0%',left:'0%',width:'100%',top:'100%',height:'0%'}"
      )
    )
  }
  
  if(edu == 6 & dl == TRUE){
    g1 <- gvisBarChart(
      data = leg, 
      xvar = "Total", yvar = names(leg)[2:5], 
      options = list(
        colors = iiasa8a,
        height = 30, width = 900, 
        legend = ifelse(dl == FALSE, 
                        "{position:'top', textStyle: {fontSize: 12}, alignment:'center'}",
                        "{position:'top', textStyle: {fontSize: 12}}"),
        chartArea = "{right:'0%',left:'0%',width:'100%',top:'100%',height:'0%'}"
      )
    )
    g2 <- gvisBarChart(
      data = leg, 
      xvar = "Total", yvar = names(leg)[6:8], 
      options = list(
        colors = iiasa8b,
        height = 30, width = 900, 
        legend = ifelse(dl == FALSE, 
                        "{position:'top', textStyle: {fontSize: 12}, alignment:'center'}",
                        "{position:'top', textStyle: {fontSize: 12}}"),
        chartArea = "{right:'0%',left:'0%',width:'100%',top:'100%',height:'0%'}"
      )
    )
    g <- gvisMerge(x = g1, y = g2)
  }
  
  
  if(edu == 8){
    g1 <- gvisBarChart(
      data = leg, 
      xvar = "Total", yvar = names(leg)[2:6], 
      options = list(
        colors = iiasa8a,
        height = 30, width = 900, 
        legend = ifelse(dl == FALSE, 
                        "{position:'top', textStyle: {fontSize: 12}, alignment:'center'}",
                        "{position:'top', textStyle: {fontSize: 12}}"),
        chartArea = "{right:'0%',left:'0%',width:'100%',top:'100%',height:'0%'}"
      )
    )
    g2 <- gvisBarChart(
      data = leg, 
      xvar = "Total", yvar = names(leg)[7:10], 
      options = list(
        colors = iiasa8b,
        height = 30, width = 900, 
        legend = ifelse(dl == FALSE, 
                        "{position:'top', textStyle: {fontSize: 12}, alignment:'center'}",
                        "{position:'top', textStyle: {fontSize: 12}}"),
        chartArea = "{right:'0%',left:'0%',width:'100%',top:'100%',height:'0%'}"
      )
    )
    g <- gvisMerge(x = g1, y = g2)
  }
  return(g)
}

dl_head <- function(year = input$pyr_year1, scenario = input$pyr_sn1, 
                    geo = input$pyr_geo1, type = "pyr", 
                    ind = NULL, age = NULL, sex = NULL, edu = NULL){
  w <- NULL
  if(type != "map"){
    w <- pyr_warn(f = f, year = year)
  }
  sn <- dimen %>%
    filter(dim=="scenario", code==scenario) %>%
    pull(name)
  
  # head file
  fh <- file("head.html", "w")
  cat(pdfinfo, file = fh)
  if(type == "pyr")
    cat(paste0("Population Pyramid  (in millions)","<br>\n"), file = fh)
  if(type == "sac")
    cat(paste0("Population (in millions)","<br>\n"), file = fh)
  cat(paste0(geo, "<br>\n"), file = fh)
  cat(paste0(year, "<br>"), file = fh)
  cat(paste0(sn, "<br>\n<br>\n"), file = fh)
  
  if(type == "map"){
    cat(paste0(ind,  "<br>\n"), file = fh)
    if(age != 0){
      a <- dimen %>% 
        filter(dim =="age", 
               code== age) %>% 
        pull(name)
      cat(paste0("Age: ", a, "<br>\n"), file = fh)
    }
    if(sex != 0){
      s <- dimen %>% 
        filter(dim =="sex", 
               code== sex) %>% 
        pull(name)
      cat(paste0("Sex: ", s, "<br>\n"), file = fh)
    }
    if(edu != 0){
      e <- dimen %>% 
        filter(dim =="edu", 
               code== edu) %>% 
        pull(name)
      cat(paste0("Education: ", e, "<br>\n"), file = fh)
    }
  }
  cat(w, file = fh)
  close(fh)
}



pyr_gvis <- function(d_pyr, 
                     pyr_year, 
                     pyr_col = iiasa6, 
                     w = 295, h = 500, 
                     legend = FALSE, 
                     pmax = NULL, no_edu = FALSE, 
                     prop = FALSE, 
                     edu = 6, 
                     ...){
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
  
  if(legend){
    top1 <- leg_gvis(edu = edu, ...)
    if(no_edu == TRUE){
      top1 <- NULL
    }
  }
  
  gg0 <- gvisMerge(x = bar1, y = bar2, 
                   horizontal = TRUE, 
                   tableOptions = "cellspacing=0, cellpadding=0")
  
  if(legend == FALSE)
    gg <- gg0
  if(legend == TRUE){
    gg <- gvisMerge(x = top1,
                    y = gg0,
                    tableOptions = "cellspacing=0, cellpadding=0")
  }
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
      filter(!eduno %in% 8:10) %>%
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
      filter(!eduno %in% 8:10) %>%
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
      filter(eduno != 7) %>%
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
# leg_data(df2)
# 
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
