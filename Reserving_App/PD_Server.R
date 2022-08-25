
output$incr_tri_pd1 <- DT::renderDataTable({
  #show the dataset in raw form as in simple R script.
  tri1 <- tri_plan1
  tri1 <- upperTri_NoNA(tri1)
  tri1 <- tri1[apply(tri1, 1L, function(x) any(!is.na(x))),,drop=FALSE]
  class(tri1) <- c("matrix")
  tri1 <- format(round(tri1, digits = 0), big.mark = ",", scientific = F)
  
}, extensions = c("Buttons"), options = list(scrollX = TRUE, scrollY = "500px", paging = FALSE, searching = F, info = F, ordering = F, dom = "BFrtip", buttons=list("copy")))

output$cum_tri_pd1 <- DT::renderDataTable({
  #show the dataset in raw form as in simple R script.
  tri1 <- tri_plan1_cum
  tri1 <- upperTri_NoNA(tri1)
  class(tri1) <- c("matrix")
  tri1 <- format(round(tri1, digits = 0), big.mark = ",", scientific = F)
}, extensions = c("Buttons"), options = list(scrollX = TRUE, scrollY = "500px", paging = FALSE, searching = F, info = F, ordering = F, dom = "BFrtip", buttons=list("copy")))

output$ata_tri_pd1 <- DT::renderDataTable({
  #show the dataset in raw form as in simple R script.
  tri1 <- summary(ata_f1, digits = 3)
  
  f1_12 <- as.data.frame(as.list(f1_12))
  colnames(f1_12) <- colnames(ata_f1)
  
  f1_24 <- as.data.frame(as.list(f1_24))
  colnames(f1_24) <- colnames(ata_f1)
  
  tri1 <- rbind(tri1, f1_12,f1_24)
  rownames(tri1) <- c(head(rownames(tri1),-2), "l_wa_12", "l_wa_24")
  tri1
  #tri1 <- ata(tri1)
  #class(tri1) <- c("matrix")
  #tri1 <- format(round(tri1, digits = 3), big.mark = ",", scientific = F)
}, extensions = c("Buttons"), options = list(scrollX = TRUE, scrollY = "500px", paging = FALSE, searching = F, info = F, ordering = F, dom = "BFrtip", buttons=list("copy")))

observeEvent(input$ldf_selector, {
output$selected_df_1 <- DT::renderDataTable({
  
  y <- as.data.frame(as.list(f1_s))
  colnames(y) <- rownames(as.data.frame(f1_s))

  y <- rbind(y, rev(cumprod(rev(c(as.numeric(y[1,]),1)))))
  row.names(y) <- c("Selected", "Cumulative")
  y <- round(y,3)
  y
}, options = list(scrollX = TRUE, scrollY = "500px", paging = FALSE, searching = F, info = F, ordering = F))
})

observeEvent(input$ldf_selector, {
  if(input$ldf_selector == 1){
    f1_s <<- attr(ata_f1, "smpl")
  }
  else if(input$ldf_selector == 2){
    f1_s <<- attr(ata_f1, "vwtd")
  }
  else if(input$ldf_selector == 3){
    f1_s <<- f1_12
  }
  else if(input$ldf_selector == 4){
    f1_s <<- f1_24
  }
  
  #set ielr for each acc_cohort
  acc_cohorts <- dimnames(tri_plan1_cum)[[1]]
  ielr_s <- rep(0.89, length(acc_cohorts))
  #choose appropriate method for each cohort
  selected_mthd <- c(rep("CL", length(acc_cohorts)-2), rep("BF",2))
  
  pd1_res <<- reserve_df(tri_plan1_cum, 0, Historical_EP[,2], ielr_s, f1_s, selected_mthd)
  res1_df <<- reserve_df(tri_plan1_cum, 0, Historical_EP[,2], ielr_s, f1_s, selected_mthd,T)
  
})

observeEvent(input$ldf_selector, {
  output$res_pd1 <- DT::renderDataTable({
    res1_df
  }, extensions = c("Buttons"), options = list(scrollX = TRUE, scrollY = "500px", paging = FALSE, searching = F, info = F, ordering = F, dom = "BFrtip", buttons=list("copy")))
})

observeEvent(input$ldf_selector, {
output$P1_IBNR <- renderValueBox({
  valueBox(format(sum(pd1_res$selected_ibnr), big.mark = ",", scientific = F), "IBNR", icon = icon("naira-sign"), color = "orange")
})
})

output$P1_UPR <- renderValueBox({
  valueBox(format(round(UPR$UPR[1],0), big.mark = ",", scientific = F), "UPR", icon = icon("naira-sign"), color = "orange")
})

observeEvent(input$ldf_selector, {
output$P1_Res <- renderValueBox({
  valueBox(format(round(sum(pd1_res$selected_ibnr, UPR$UPR[1]),0), big.mark = ",", scientific = F), "Total Reserve", icon = icon("naira-sign"), color = "orange")
})
})

