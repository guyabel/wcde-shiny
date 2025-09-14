sac_gvis <- function(d_sac, pcol=iiasa6, no_edu = FALSE,
                     w = 295, h = 500, legend= FALSE, pmax=NULL, prop=FALSE, ...){
  #d_sac<-d;pyear=2010; pcol=iiasa6; w=400;h=500
  df1 <- d_sac %>%
    # filter(eduno != 0) %>%
    select(year, edu, pop) %>% 
    spread(key = edu, value = pop) %>%
    mutate(year = as.character(year))
  
  if(no_edu == FALSE){
    df1 <- df1 %>% select(-Total)
  }
  if(no_edu == TRUE){
    df1 <- df1 %>% select(year, Total)
  }

  if(is.null(pmax))
    pmax <- d_sac %>% 
      filter(eduno==0) %>% 
      pull(pop) %>%
      max(., na.rm=TRUE)
  if(prop==TRUE)
    pmax <- 1

  
  gg0 <- gvisAreaChart(
    data = df1, 
    xvar = "year", yvar = names(df1[,-1]),
    options = list(
      isStacked = if(prop == TRUE) 'percent' else TRUE, 
      colors = pcol, 
      chartArea="{left:'12.5%',top:'5%',height:'90%',width:'80%'}",
      height = h, width = w*2, areaOpacity = 0.8,
      hAxis = "{showTextEvery:5}",
      legend = "{position:'none'}",
      vAxis=paste0("{maxValue:",pmax,", viewWindowMode:'explicit'}")
      )
    )
  
  if(legend == FALSE)
    gg <- gg0
  
  if(legend == TRUE){
    top1 <- leg_gvis(edu = edu, ...)
    if(no_edu == TRUE){
      top1 <- NULL
    }
    gg <- gvisMerge(x = top1,
                    y = gg0,
                    tableOptions = "cellspacing=0, cellpadding=0")
  }
  return(gg)
}

# plot(p1)

sac_data <- function(geo = input$sac_geo1, 
                     sn = input$sac_sn1,
                     edu = input$sac_edu, 
                     year_range = c(input$sac_year1[1], input$sac_year1[2])){
  v <- c("year", "age", "ageno", "sex", "sexno", "edu", "eduno")
  v <- geog %>%
    filter(name %in% geo) %>%
    pull(isono) %>%
    c(v, .)

  
  df1 <- tibble(
    v = v,
    file = paste0("../wcde-data/wcde-v3-single/", sn, "/epop/", v, ".rds")
  ) %>%
    mutate(d = map(.x = file, .f = ~read_rds(.x))) %>%
    select(-file) %>%
    pivot_wider(names_from = v, values_from = d) %>%
    unnest(col = names(.)) %>%  
  # df1 <- loads(file = paste0("df", sn, "/epop"), 
  #              variables = v, ultra.fast = TRUE, to.data.frame=TRUE) %>%
  #   as_tibble() %>%
    filter(ageno==0, 
           sexno==0, 
           year %in% seq(from = year_range[1], to = year_range[2], by = 5)) %>%
    mutate_if(is.factor, as.character) %>%
    mutate(edu = fct_inorder(edu)) %>%
    gather(key = isono, value = pop, -(1:7)) %>% 
    mutate(pop = pop/1e3) %>%
    mutate(scenario = sn)
  
  if(edu==4){
    df2 <- df1 %>%
      filter(!eduno %in% 8:10) %>%
      left_join(edu4, by = "eduno") %>%
      mutate(edu = fct_inorder(edu_name)) %>%
      select(-edu_name) %>%
      mutate(eduno = ifelse(eduno == 0, 0, 1)) %>%
      drop_na() %>%
      group_by(scenario, year, ageno, age, sex, sexno, edu, eduno) %>%
      summarise(pop=sum(pop)) %>%
      ungroup()
  }
  if(edu==6){
    df2 <- df1 %>%
      filter(!eduno %in% 8:10) %>%
      left_join(edu6, by = "eduno") %>%
      mutate(edu = fct_inorder(edu_name)) %>%
      select(-edu_name) %>%
      mutate(eduno = ifelse(eduno == 0, 0, 1)) %>%
      drop_na() %>%
      group_by(scenario, year, ageno, age, sex, sexno, edu, eduno) %>%
      summarise(pop=sum(pop)) %>%
      ungroup()
  }
  if(edu == "8"){
    df2 <- df1 %>%
      filter(eduno != 7) %>%
      left_join(edu10, by = "eduno") %>%
      mutate(edu = fct_inorder(edu_name)) %>%
      select(-edu_name) %>%
      drop_na() %>%
      group_by(scenario, year, ageno, age, sex, sexno, edu, eduno) %>%
      summarise(pop=sum(pop)) %>%
      ungroup() %>%
      # # all education splits to zero if less than 2015
      # mutate(pop = ifelse(year < 2015 & eduno != 0, 0, pop)) %>%
      # # fill in missing rows for masters etc pre 2015
      # complete(scenario, year, age, sex, edu, fill = list(pop = 0)) %>%
      # fill(ageno, sexno) %>%
      mutate(eduno = ifelse(edu == "Total", 0, 1))
  }
  return(df2)
}

max_total <- function(d){
  d %>% 
    filter(ageno == 0, 
           sexno == 0, 
           eduno == 0) %>% 
    pull(pop) %>%
    max(., na.rm=TRUE)
}
