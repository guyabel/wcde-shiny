output$about_faq1 <- renderDT({
  faq %>% 
    filter(category != "general") %>% 
    select(-category) %>% 
    rename(Question = question,
           Answer = answer)
}, escape = FALSE, options = list(searching = FALSE,paging = FALSE,info=FALSE))

output$about_faq2 <- renderDT({
  faq %>% 
    filter(category == "general") %>% 
    select(-category) %>% 
    rename(Question = question, 
           Answer = answer)
}, escape = FALSE, options = list(searching = FALSE,paging = FALSE,info=FALSE))

output$about_scenario <- renderDT({
  dimen %>% 
    filter(dim == "scenario") %>% 
    arrange(wcde_code) %>%
    select(name,details) %>% 
    rename(Scenario = name,
           Definition = details)
}, escape = FALSE, options = list(searching = FALSE,paging = FALSE,info=FALSE))

output$about_edu <- renderDT({
  dimen %>% 
    filter(dim=="edu") %>% 
    select(name, details) %>% 
    rename(Level = name, 
           Definition = details)
}, escape = FALSE, options = list(searching = FALSE, paging = FALSE,info=FALSE))
