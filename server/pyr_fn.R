gpyr <- function(df_pyr, pyear, pcol=iiasa6, w=400, h=500, legend="top", pmax=NULL, no.edu=FALSE, prop=FALSE){
  #df_pyr<-df_pyr1;pyear=2015; pcol=iiasa4; w=400;pmax=NULL; h=550; prop=FALSE; no.edu=FALSE
  dfm <- df_pyr %>% 
    ungroup() %>%
    filter(year == pyear, sexno == 1, ageno != 0) %>% 
    select(ageno, age, edu, pop) %>% 
    mutate(pop = ifelse(pop == 0, NA, pop)) %>%
    spread(key = edu, value = pop) %>%
    arrange(desc(ageno)) %>% 
    select(-ageno)
  dff <- df_pyr %>% 
    ungroup() %>%
    filter(year==pyear, sexno==2, ageno!=0) %>% 
    select(ageno, age ,edu,pop) %>% 
    mutate(pop = ifelse(pop == 0, NA, pop)) %>%
    spread(key = edu, value = pop) %>%
    arrange(desc(ageno)) %>% 
    select(-ageno) 
#   if(prop == TRUE){
#     dfm <- dfm %>% mutate_each(funs(./Total), -(1:2)) %>% mutate_each(funs(round(., 2)), -(1:2))
#     dff <- dff %>% mutate_each(funs(./Total), -(1:2)) %>% mutate_each(funs(round(., 2)), -(1:2))
#   }
  if(no.edu==TRUE){
    dfm <- dfm %>% select(1:2)
    dff <- dff %>% select(1:2)
  }
  if(no.edu==FALSE){
    dfm <- dfm %>% select(-2)
    dff <- dff %>% select(-2)
  }
  dft <- df_pyr %>% 
    filter(year==pyear, sexno==0, ageno==0, eduno==0)
  if(is.null(pmax))
    pmax <- df_pyr %>% 
    filter(year==pyear, ageno!=0, sexno!=0, eduno==0) %>% 
    pull(pop) %>%
    max(., na.rm=TRUE)
  if(prop==TRUE)
    pmax <- 1
  bar1 <- gvisBarChart(data = dfm, xvar = "age", yvar = names(dfm)[-1],
            options = list(bar = "{groupWidth:'90%'}", 
                           isStacked = if(prop == TRUE) 'percent' else TRUE, 
                           title = paste0("Total Population: ",round(dft$pop/1000,2)," m"), 
                           titleTextStyle = "{fontName:'Arial',fontSize:16}",
                           hAxis=paste0("{direction:-1, maxValue:",pmax,", title: 'Male'}"),
                           colors = pcol, height = h, width = w, 
                           #tooltip = "{isHtml:'true'}",
                           #focusTarget = "{'category'}",
                           chartArea="{right:'0%',left:'15%',width:'85%',top:'5%',height:'85%'}"))

  bar2 <- gvisBarChart(data = dff, xvar="age", yvar=names(dfm)[-1],
            options = list(bar = "{groupWidth:'90%'}", 
                           isStacked = if(prop == TRUE) 'percent' else TRUE, 
                           hAxis=paste0("{maxValue:",pmax,", title: 'Female'}"),
                           legend="{position:'none'}",
                           colors = pcol, height = h, width = w, 
                           chartArea = "{right:'15%',left:'0%',width:'85%',top:'5%',height:'85%'}"))
  top1 <- gvisBarChart(data = dfm, xvar="age", yvar=names(dfm)[-1],
             options=list(colors=pcol, height=30, width=2*w, 
                          legend="{position:'top', textStyle: {fontSize: 12}}",
                          chartArea="{right:'0%',left:'0%',width:'100%',top:'100%',height:'0%'}"))
  if(legend=="none")
    gg<-gvisMerge(bar1, bar2, horizontal = TRUE, tableOptions="cellspacing=0, cellpadding=0")
  if(legend=="top")
    gg<-gvisMerge(top1, tableOptions="cellspacing=0, cellpadding=0",
              gvisMerge(bar1, bar2, horizontal = TRUE, tableOptions="cellspacing=0, cellpadding=0"))
  return(gg)
}
# plot(gpyr(df_pyr = df1, pyear = 2010))
# plot(gpyr(df1, 2010, legend="only"))
# plot(gg)
# plot(bar2)


pyr_data <- function(geo = input$pyr_geo1, 
                     sn = input$pyr_sn1,
                     edu = input$pyr_edu){
  v <- c("year", "age", "ageno", "sex", "sexno", "edu", "eduno")
  v <- geog %>%
    filter(name %in% geo) %>%
    pull(isono) %>%
    c(v, .)
  
  df1 <- loads(file = paste0("df", sn, "/epop"), 
               variables = v, ultra.fast = TRUE, to.data.frame=TRUE) %>%
    tbl_df() %>%
    mutate_if(is.factor, as.character) %>%
    mutate(edu = fct_inorder(edu)) %>%
    gather(key = isono, value = pop, -(1:7)) %>% 
    mutate(scenario = sn)
  
  if(edu==4){
    df2 <- df1 %>%
      left_join(edu4, by = "eduno") %>%
      mutate(edu = fct_inorder(edu_name)) %>%
      select(-edu_name) %>%
      mutate(eduno = ifelse(eduno == 0, 0, 1)) %>%
      group_by(scenario, year, ageno, age, sex, sexno, edu, eduno) %>%
      summarise(pop=sum(pop)) %>%
      ungroup()
  }
  if(edu==6){
    df2 <- df1 %>%
      left_join(edu6, by = "eduno") %>%
      mutate(edu = fct_inorder(edu_name)) %>%
      select(-edu_name) %>%
      mutate(eduno = ifelse(eduno == 0, 0, 1)) %>%
      group_by(scenario, year, ageno, age, sex, sexno, edu, eduno) %>%
      summarise(pop=sum(pop)) %>%
      ungroup()
  }
  if(edu==10){
    df2 <- df1 %>%
      left_join(edu10, by = "eduno") %>%
      mutate(edu = fct_inorder(edu_name)) %>%
      select(-edu_name) %>%
      mutate(eduno = ifelse(eduno == 0, 0, 1)) %>%
      group_by(scenario, year, ageno, age, sex, sexno, edu, eduno) %>%
      summarise(pop=sum(pop)) %>%
      ungroup()
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
