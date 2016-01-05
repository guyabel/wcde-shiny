gsac<-function(df_sac, pcol=iiasa6, w=400, h=500, legend="top", pmax=NULL, prop=FALSE){
  #df_sac<-df_sac1;pyear=2010; pcol=iiasa6; w=400;h=500
  df1 <- df_sac %>%
    select(year, edu, pop) %>% 
    dcast(year ~ edu, value.var="pop") %>% 
    select(-2)
  #df1 <- df0 %>% filter(eduno!=0) %>% select(year, edu, pop) %>% dcast(year ~ edu, value.var="pop")
  if(is.null(pmax))
    pmax <- max(df_sac %>% filter(eduno==0) %>% .[["pop"]], na.rm=TRUE)
  if(prop==TRUE)
    pmax <- 1
  
  p1 <- gvisAreaChart(data = df1, xvar="year", yvar=names(df1[,-1]),
          options = list(isStacked = if(prop == TRUE) 'percent' else TRUE, 
                         colors=pcol, 
                         chartArea="{left:'12.5%',top:'5%',height:'90%',width:'80%'}",
                         height=h, width=w*2, areaOpacity=0.8,
                         hAxis="{showTextEvery:3}",
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