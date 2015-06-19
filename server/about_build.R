output$about_faq1 <- renderDataTable({
  faq %>% filter(category!="general") %>% select(-category) %>% rename(Question=question,Answer=answer)
}, escape = FALSE, options = list(searching = FALSE,paging = FALSE,info=FALSE))

output$about_faq2 <- renderDataTable({
  faq %>% filter(category=="general") %>% select(-category) %>% rename(Question=question,Answer=answer)
}, escape = FALSE, options = list(searching = FALSE,paging = FALSE,info=FALSE))

output$about_scenario <- renderDataTable({
  dimen %>% filter(dim=="scenario") %>% select(name,details) %>% rename(Scenario=name,Definition=details)
}, escape = FALSE, options = list(searching = FALSE,paging = FALSE,info=FALSE))

output$about_edu <- renderDataTable({
  dimen %>% filter(dim=="edu") %>% select(name,details) %>% rename(Level=name,Definition=details)
}, escape = FALSE, options = list(searching = FALSE,paging = FALSE,info=FALSE))