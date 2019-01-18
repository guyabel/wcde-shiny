sac_gvis <- function(df_sac, pcol=iiasa6, w=400, h=500, legend="top", pmax=NULL, prop=FALSE){
  #df_sac<-df_sac1;pyear=2010; pcol=iiasa6; w=400;h=500
  df1 <- df_sac %>%
    filter(eduno != 0) %>%
    select(year, edu, pop) %>% 
    spread(key = edu, value = pop)

  if(is.null(pmax))
    pmax <- df_sac %>% 
      filter(eduno==0) %>% 
      pull(pop) %>%
      max(., na.rm=TRUE)
  if(prop==TRUE)
    pmax <- 1

  p1 <- gvisAreaChart(data = df1, xvar = "year", yvar = names(df1[,-1]),
          options = list(isStacked = if(prop == TRUE) 'percent' else TRUE, 
                         colors=pcol, 
                         chartArea="{left:'12.5%',top:'5%',height:'90%',width:'80%'}",
                         height=h, width=w*2, areaOpacity=0.8,
                         hAxis="{showTextEvery:3, format:'####'}",
                         legend="{position:'none'}",
                         vAxis=paste0("{maxValue:",pmax,", viewWindowMode:'explicit'}")))
  top2 <- gvisAreaChart(data = df1,  xvar="year", yvar=names(df1[,-1]),
            options=list(width=w*2, height=30, colors=pcol,
                         legend="{position:'top', textStyle: {fontSize: 12}}",
                         chartArea="{right:'0%',left:'0%',width:'100%',top:'100%',height:'0%'}"))
  
  if(legend=="none")
    gg<-p1
  if(legend=="top")
    gg<-gvisMerge(top2, p1, tableOptions="cellspacing=0, cellpadding=0")
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
  
  df1 <- loads(file = paste0("df", sn, "/epop"), 
               variables = v, ultra.fast = TRUE, to.data.frame=TRUE) %>%
    tbl_df() %>%
    filter(ageno==0, 
           sexno==0, 
           year %in% seq(from = year_range[1], to = year_range[2], by = 5)) %>%
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
      drop_na() %>%
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
      drop_na() %>%
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
      drop_na() %>%
      group_by(scenario, year, ageno, age, sex, sexno, edu, eduno) %>%
      summarise(pop=sum(pop)) %>%
      ungroup()
  }
  return(df2)
}