#make_permanova_tables <- function(file_path = file_path, mapper_file = mapper_file, flies_unifrac_table = flies_unifrac_table, pform = pform, beta_div_tests = c("unweighted_unifrac","weighted_unifrac","bray_curtis"), seed_to_set = 42, byval = "terms") {
#write.table("Permanova tables", file = paste0('core-metrics-results-',file_path,"/",file_path,"_permanova_table.txt"), append = F, quote = F, sep = "\t", row.names = F, col.names = F)
#i="unweighted_unifrac"
#  for(i in beta_div_tests) {
#  assign(x = paste0("flies_",i), value = read.table(paste('core-metrics-results-',file_path,'/',i,'_distance_matrix/distance-matrix.tsv',sep=""), header=T, sep="\t") %>% mutate(X=as.character(X)))
#  assign(x = paste0("flies_",i,"_dm"), value = as.dist(get(paste0("flies_",i))[,2:dim(get(paste0("flies_",i)))[2]]))
#  flies_unifrac_table <- get(paste0("flies_",i)) %>% dplyr::select(X) %>% left_join(read.table(mapper_file,comment.char = "", header=T, fill=T, sep="\t", quote = '"'), by=c("X"="X.SampleID")) %>% droplevels()
#  set.seed(seed_to_set)
#    if(byval == "margin") {
#  assign(x = paste0("f_",i,"_permanova"), adonis2(as.formula(paste0("flies_",i,"_dm ",pform)), flies_unifrac_table, permutations=1000, by = "margin"))
#      } else { 
#  assign(x = paste0("f_",i,"_permanova"), adonis2(as.formula(paste0("flies_",i,"_dm ",pform)), flies_unifrac_table, permutations=1000, by = "terms"))
#      }
#  assign(x = paste0("fig_",i), value = tableGrob(data.frame(Df = round(get(paste0("f_",i,"_permanova"))$Df, 2), SS = round(get(paste0("f_",i,"_permanova"))$SumOfSqs, 2), R2 = round(get(paste0("f_",i,"_permanova"))$R2, 2), Fval = round(get(paste0("f_",i,"_permanova"))$`F`, 2), p = round(get(paste0("f_",i,"_permanova"))$`Pr(>F)`, 2)),theme=ttheme_minimal()))
#  plot(get(paste0("fig_",i)))
#  write.table(i, file = paste0('core-metrics-results-',file_path,"/",file_path,"_permanova_table.txt"), append = T, quote = F, sep = "\t", row.names = F, col.names = F)
#  write.table(round(get(paste0("f_",i,"_permanova")),2), file = paste0('core-metrics-results-',file_path,"/",file_path,"_permanova_table.txt"), append = T, quote = F, sep = "\t", row.names = T, col.names = T)
#}
#}


make_permanova_tables <- function(file_path = file_path, mapper_file = mapper_file, flies_unifrac_table = flies_unifrac_table, pform = pform, beta_div_tests = c("unweighted_unifrac","weighted_unifrac","bray_curtis"), seed_to_set = 42, byval = "terms", stratavar=NULL, converttable =NULL) {
  
  write.table("Permanova tables", file = paste0('core-metrics-results-',file_path,"/",file_path,"_permanova_table.txt"), append = F, quote = F, sep = "\t", row.names = F, col.names = F)
  
  
  i="unweighted_unifrac"
  for(i in beta_div_tests) {
    assign(x = paste0("flies_",i), value = read.table(paste('core-metrics-results-',file_path,'/',i,'_distance_matrix/distance-matrix.tsv',sep=""), header=T, sep="\t") %>% mutate(X=as.character(X)))
    assign(x = paste0("flies_",i,"_dm"), value = as.dist(get(paste0("flies_",i))[,2:dim(get(paste0("flies_",i)))[2]]))
    flies_unifrac_table <- get(paste0("flies_",i)) %>% dplyr::select(X) %>% left_join(read.table(mapper_file,comment.char = "", header=T, fill=T, sep="\t", quote = '"'), by=c("X"="X.SampleID")) %>% droplevels() 
    if (byval == "margin" & is.null(stratavar)) {
      set.seed(seed_to_set)
      assign(x = paste0("f_",i,"_permanova"), adonis2(as.formula(paste0("flies_",i,"_dm ",pform)), flies_unifrac_table, permutations=1000, by = "margin"))
      print("margin")
    } else if (byval == "terms" & is.null(stratavar)) {
      set.seed(seed_to_set)
      assign(x = paste0("f_",i,"_permanova"), adonis2(as.formula(paste0("flies_",i,"_dm ",pform)), flies_unifrac_table, permutations=1000, by = "terms"))
      print("terms")
    } else if (byval == "margin" & !is.null(stratavar)) {
      set.seed(seed_to_set)
      assign(x = paste0("f_",i,"_permanova"), adonis2(as.formula(paste0("flies_",i,"_dm ",pform)), data = flies_unifrac_table, permutations=1000, by = "margin", strata = flies_unifrac_table[,paste0(stratavar)]))
      print("marginstrata")
    } else if (byval == "terms" & !is.null(stratavar)) {
      set.seed(seed_to_set)
      assign(x = paste0("f_",i,"_permanova"), adonis2(as.formula(paste0("flies_",i,"_dm ",pform)), data = flies_unifrac_table, permutations=1000, by = "terms", strata = flies_unifrac_table[,paste0(stratavar)]))
      print("termsstrata")
    }
    
    
    assign(x = paste0("fig_",i), value = tableGrob(data.frame(Df = round(get(paste0("f_",i,"_permanova"))$Df, 2), SS = round(get(paste0("f_",i,"_permanova"))$SumOfSqs, 2), R2 = round(get(paste0("f_",i,"_permanova"))$R2, 2), Fval = round(get(paste0("f_",i,"_permanova"))$`F`, 2), p = round(get(paste0("f_",i,"_permanova"))$`Pr(>F)`, 4)),theme=ttheme_minimal()))
    plot(get(paste0("fig_",i)))
    write.table(i, file = paste0('core-metrics-results-',file_path,"/",file_path,"_permanova_table.txt"), append = T, quote = F, sep = "\t", row.names = F, col.names = F)
    write.table(round(get(paste0("f_",i,"_permanova")),2), file = paste0('core-metrics-results-',file_path,"/",file_path,"_permanova_table.txt"), append = T, quote = F, sep = "\t", row.names = T, col.names = T)
  }
  if(!is.null(converttable)) {
    convert_permanova_to_word(file_path = file_path, table_row_names = converttable)
  }
}

make_permanova_tables2fac <- function(file_path = file_path, mapper_file = mapper_file, flies_unifrac_table = flies_unifrac_table, pform = pform, beta_div_tests = c("unweighted_unifrac","weighted_unifrac","bray_curtis"), seed_to_set = 42, byval = "terms", stratavar=NULL, converttable =NULL, fac_col = fac_col) {
  
  write.table("Permanova tables", file = paste0('core-metrics-results-',file_path,"/",file_path,"_permanova_table.txt"), append = F, quote = F, sep = "\t", row.names = F, col.names = F)
  
  
  i="unweighted_unifrac"
  for(i in beta_div_tests) {
    assign(x = paste0("flies_",i), value = read.table(paste('core-metrics-results-',file_path,'/',i,'_distance_matrix/distance-matrix.tsv',sep=""), header=T, sep="\t") %>% mutate(X=as.character(X)))
    assign(x = paste0("flies_",i,"_dm"), value = as.dist(get(paste0("flies_",i))[,2:dim(get(paste0("flies_",i)))[2]]))
    flies_unifrac_table <- get(paste0("flies_",i)) %>% dplyr::select(X) %>% left_join(read.table(mapper_file,comment.char = "", header=T, fill=T, sep="\t", quote = '"'), by=c("X"="X.SampleID")) %>% droplevels() 
    flies_unifrac_table$time_num <- flies_unifrac_table[,fac_col]
    flies_unifrac_table$time_fac = factor(flies_unifrac_table$time_num, ordered = T)
    if (byval == "margin" & is.null(stratavar)) {
      set.seed(seed_to_set)
      assign(x = paste0("f_",i,"_permanova"), adonis2(as.formula(paste0("flies_",i,"_dm ",pform)), flies_unifrac_table, permutations=1000, by = "margin"))
      print("margin")
    } else if (byval == "terms" & is.null(stratavar)) {
      set.seed(seed_to_set)
      assign(x = paste0("f_",i,"_permanova"), adonis2(as.formula(paste0("flies_",i,"_dm ",pform)), flies_unifrac_table, permutations=1000, by = "terms"))
      print("terms")
    } else if (byval == "margin" & !is.null(stratavar)) {
      set.seed(seed_to_set)
      assign(x = paste0("f_",i,"_permanova"), adonis2(as.formula(paste0("flies_",i,"_dm ",pform)), data = flies_unifrac_table, permutations=1000, by = "margin", strata = flies_unifrac_table[,paste0(stratavar)]))
      print("marginstrata")
    } else if (byval == "terms" & !is.null(stratavar)) {
      set.seed(seed_to_set)
      assign(x = paste0("f_",i,"_permanova"), adonis2(as.formula(paste0("flies_",i,"_dm ",pform)), data = flies_unifrac_table, permutations=1000, by = "terms", strata = flies_unifrac_table[,paste0(stratavar)]))
      print("termsstrata")
    }
    
    
    assign(x = paste0("fig_",i), value = tableGrob(data.frame(Df = round(get(paste0("f_",i,"_permanova"))$Df, 2), SS = round(get(paste0("f_",i,"_permanova"))$SumOfSqs, 2), R2 = round(get(paste0("f_",i,"_permanova"))$R2, 2), Fval = round(get(paste0("f_",i,"_permanova"))$`F`, 2), p = round(get(paste0("f_",i,"_permanova"))$`Pr(>F)`, 4)),theme=ttheme_minimal()))
    plot(get(paste0("fig_",i)))
    write.table(i, file = paste0('core-metrics-results-',file_path,"/",file_path,"_permanova_table.txt"), append = T, quote = F, sep = "\t", row.names = F, col.names = F)
    write.table(round(get(paste0("f_",i,"_permanova")),2), file = paste0('core-metrics-results-',file_path,"/",file_path,"_permanova_table.txt"), append = T, quote = F, sep = "\t", row.names = T, col.names = T)
  }
  if(!is.null(converttable)) {
    convert_permanova_to_word(file_path = file_path, table_row_names = converttable)
  }
}

make_permanova_tables2date <- function(file_path = file_path, mapper_file = mapper_file, flies_unifrac_table = flies_unifrac_table, pform = pform, beta_div_tests = c("unweighted_unifrac","weighted_unifrac","bray_curtis"), seed_to_set = 42, byval = "terms", stratavar=NULL, converttable =NULL, date_col = date_col) {
  
  write.table("Permanova tables", file = paste0('core-metrics-results-',file_path,"/",file_path,"_permanova_table.txt"), append = F, quote = F, sep = "\t", row.names = F, col.names = F)
  
  
  i="unweighted_unifrac"
  for(i in beta_div_tests) {
    assign(x = paste0("flies_",i), value = read.table(paste('core-metrics-results-',file_path,'/',i,'_distance_matrix/distance-matrix.tsv',sep=""), header=T, sep="\t") %>% mutate(X=as.character(X)))
    assign(x = paste0("flies_",i,"_dm"), value = as.dist(get(paste0("flies_",i))[,2:dim(get(paste0("flies_",i)))[2]]))
    flies_unifrac_table <- get(paste0("flies_",i)) %>% dplyr::select(X) %>% left_join(read.table(mapper_file,comment.char = "", header=T, fill=T, sep="\t", quote = '"'), by=c("X"="X.SampleID")) %>% droplevels() 
    flies_unifrac_table <- flies_unifrac_table %>% mutate(time_date = as.Date(as.character(date2), "%m/%d/%y"))
    if (byval == "margin" & is.null(stratavar)) {
      set.seed(seed_to_set)
      assign(x = paste0("f_",i,"_permanova"), adonis2(as.formula(paste0("flies_",i,"_dm ",pform)), flies_unifrac_table, permutations=1000, by = "margin"))
      print("margin")
    } else if (byval == "terms" & is.null(stratavar)) {
      set.seed(seed_to_set)
      assign(x = paste0("f_",i,"_permanova"), adonis2(as.formula(paste0("flies_",i,"_dm ",pform)), flies_unifrac_table, permutations=1000, by = "terms"))
      print("terms")
    } else if (byval == "margin" & !is.null(stratavar)) {
      set.seed(seed_to_set)
      assign(x = paste0("f_",i,"_permanova"), adonis2(as.formula(paste0("flies_",i,"_dm ",pform)), data = flies_unifrac_table, permutations=1000, by = "margin", strata = flies_unifrac_table[,paste0(stratavar)]))
      print("marginstrata")
    } else if (byval == "terms" & !is.null(stratavar)) {
      set.seed(seed_to_set)
      assign(x = paste0("f_",i,"_permanova"), adonis2(as.formula(paste0("flies_",i,"_dm ",pform)), data = flies_unifrac_table, permutations=1000, by = "terms", strata = flies_unifrac_table[,paste0(stratavar)]))
      print("termsstrata")
    }
    
    
    assign(x = paste0("fig_",i), value = tableGrob(data.frame(Df = round(get(paste0("f_",i,"_permanova"))$Df, 2), SS = round(get(paste0("f_",i,"_permanova"))$SumOfSqs, 2), R2 = round(get(paste0("f_",i,"_permanova"))$R2, 2), Fval = round(get(paste0("f_",i,"_permanova"))$`F`, 2), p = round(get(paste0("f_",i,"_permanova"))$`Pr(>F)`, 4)),theme=ttheme_minimal()))
    plot(get(paste0("fig_",i)))
    write.table(i, file = paste0('core-metrics-results-',file_path,"/",file_path,"_permanova_table.txt"), append = T, quote = F, sep = "\t", row.names = F, col.names = F)
    write.table(round(get(paste0("f_",i,"_permanova")),2), file = paste0('core-metrics-results-',file_path,"/",file_path,"_permanova_table.txt"), append = T, quote = F, sep = "\t", row.names = T, col.names = T)
  }
  if(!is.null(converttable)) {
    convert_permanova_to_word(file_path = file_path, table_row_names = converttable)
  }
}

format_sig_digits <- function(x) {
  ## written by chat
  # Calculate how many significant digits the number naturally has
  # by removing the decimal point and leading/trailing zeros
  raw_digits <- nchar(gsub("(^0\\.|\\.0+$|\\.)", "", as.character(abs(x))))
  
  # Use 2 significant digits as a minimum, otherwise use the natural count
  digits_to_use <- pmax(2, raw_digits)
  
  # Apply formatting individually to each number
  sapply(seq_along(x), function(i) {
    signif(x[i], digits = digits_to_use[i])
  })
}

convert_permanova_to_word <- function(file_path = file_path, table_row_names = table_row_names) {
  
  perm_table <- read.table(paste0("core-metrics-results-",file_path,"/",file_path,"_permanova_table.txt"), fill = T, sep = "\t", skip = 2, header = F, row.names = NULL)
  
  num_rows = (which(perm_table$V1 == "weighted_unifrac")-1)
  
  unweighted_unifrac = perm_table[1:num_rows,3:6]
  weighted_unifrac = perm_table[(num_rows+2):(num_rows+1+num_rows),3:6]
  bray_curtis = perm_table[(num_rows*2+3):(num_rows*3+2),1:6]
  
  new_df <- cbind(bray_curtis, ` ` = "", weighted_unifrac, ` ` = "", unweighted_unifrac)
  new_df2 <- rbind(c("","Bray-Curtis","Bray-Curtis","Bray-Curtis","Bray-Curtis","Bray-Curtis","","Weighted Unifrac","Weighted Unifrac","Weighted Unifrac","Weighted Unifrac","","Unweighted Unifrac","Unweighted Unifrac","Unweighted Unifrac","Unweighted Unifrac"),c("","Df","SS","R2","F","p","","SS","R2","F","p","","SS","R2","F","p"),new_df)[-3,]
  colnames(new_df2) <- paste0("V",1:16)
  new_df2$V1 <- c("","",table_row_names,"Residual","Total")
  new_df2$V3[3:(num_rows+1)] <- as.character(sprintf("%.1f", as.numeric(new_df2$V3[3:(num_rows+1)])))
  new_df2$V4[3:(num_rows+1)] <- as.character(sprintf("%.2f", as.numeric(new_df2$V4[3:(num_rows+1)])))
  new_df2$V5[3:(num_rows+1)] <- as.character(sprintf("%.1f", as.numeric(new_df2$V5[3:(num_rows+1)])))
  new_df2$V8[3:(num_rows+1)] <- as.character(sprintf("%.1f", as.numeric(new_df2$V8[3:(num_rows+1)])))
  new_df2$V9[3:(num_rows+1)] <- as.character(sprintf("%.2f", as.numeric(new_df2$V9[3:(num_rows+1)])))
  new_df2$V10[3:(num_rows+1)] <- as.character(sprintf("%.1f", as.numeric(new_df2$V10[3:(num_rows+1)])))
  new_df2$V13[3:(num_rows+1)] <- as.character(sprintf("%.1f", as.numeric(new_df2$V13[3:(num_rows+1)])))
  new_df2$V14[3:(num_rows+1)] <- as.character(sprintf("%.2f", as.numeric(new_df2$V14[3:(num_rows+1)])))
  new_df2$V15[3:(num_rows+1)] <- as.character(sprintf("%.1f", as.numeric(new_df2$V15[3:(num_rows+1)])))
  new_df2$V6[3:(num_rows+1)] <- as.character(sprintf("%.2f", as.numeric(new_df2$V6[3:(num_rows+1)])))
  new_df2$V11[3:(num_rows+1)] <- as.character(sprintf("%.2f", as.numeric(new_df2$V11[3:(num_rows+1)])))
  new_df2$V16[3:(num_rows+1)] <- as.character(sprintf("%.2f", as.numeric(new_df2$V16[3:(num_rows+1)])))
  new_df2$V5[(num_rows):(num_rows+1)] <- ""
  new_df2$V10[(num_rows):(num_rows+1)] <- ""
  new_df2$V15[(num_rows):(num_rows+1)] <- ""
  new_df2$V6[(num_rows):(num_rows+1)] <- ""
  new_df2$V11[(num_rows):(num_rows+1)] <- ""
  new_df2$V16[(num_rows):(num_rows+1)] <- ""
  new_df2$V6[3:(num_rows-1)] <- ifelse(new_df2$V6[3:(num_rows-1)] == "0.00", "0.001",new_df2$V6[3:(num_rows-1)])
  new_df2$V11[3:(num_rows-1)] <- ifelse(new_df2$V11[3:(num_rows-1)] == "0.00", "0.001",new_df2$V11[3:(num_rows-1)])
  new_df2$V16[3:(num_rows-1)] <- ifelse(new_df2$V16[3:(num_rows-1)] == "0.00", "0.001",new_df2$V16[3:(num_rows-1)])
  col6replace <- which(as.character(new_df2$V6) == "0.001")
  col11replace <- which(as.character(new_df2$V11) == "0.001")
  col16replace <- which(as.character(new_df2$V16) == "0.001")
  
  
  format_sig_digits(as.numeric(new_df2$V6[3:(num_rows-1)]))
  
  ## working with flextable
  new_df3 <- flextable(new_df2)
  new_df3 <- compose(
    x = new_df3, 
    i = 2,
    j = c("V4","V9","V14"),
    value = as_paragraph("R", as_sup("2"))
  )
  
  new_df3 <- compose(x = new_df3, i = col6replace, j = "V6", value = as_paragraph("<10", as_sup("-3")))
  new_df3 <- compose(x = new_df3, i = col11replace, j = "V11", value = as_paragraph("<10", as_sup("-3")))
  new_df3 <- compose(x = new_df3, i = col16replace, j = "V16", value = as_paragraph("<10", as_sup("-3")))
  
  new_df3 <- merge_h(new_df3, i = 1)    # Merge identical vertical cells in Department
  
  new_df3 <- align(new_df3, align = "center", part = "all") # Center-align all text
  new_df3 <- align(new_df3, align = "right", j = "V1") # Vertically center merged text
  new_df3 <- autofit(new_df3)                    # Adjust column widths automatically
  
  new_df3 <- font(new_df3, fontname = "Cambria", part = "all")
  new_df3 <- fontsize(new_df3, size = 6, part = "all")
  
  doc <- officer::read_docx()
  doc <- body_add_flextable(doc, value = new_df3)
  print(doc, target = paste0('core-metrics-results-',file_path,'/',file_path,'_permanova_table.docx'))
  print(new_df3)
  
}



insert_beta_disp <- function(file_path = file_path, row_in_table = row_in_table, row_from_qzvname = row_from_qzvname) {
  
  
  ## read in doc containing the table
  input_document <- docxtractr::read_docx(paste0('core-metrics-results-',file_path,'/',file_path,'_permanova_table.docx'))
  
  ## check if there is exactly one table in the input doc
  if (docx_tbl_count(input_document) == 1) {
  } else {
    stop("There isn't one table in the filepath ",paste0('core-metrics-results-',file_path,'/',file_path,'_permanova_table.docx'))
  }
  
  ## get the one table out
  starting_table <- docx_extract_tbl(input_document, tbl_number = 1, header = TRUE)
  
  if (!dim(starting_table)[2] %in% c(16,22)) {
    stop("I was expected 16 or 22 columns; this doesn't jive, check it out")
  }
  
  working_table <- starting_table
  
  working_table <- working_table %>% add_row(.before = 2) %>% mutate(across(everything(), ~ ifelse(row_number() == 2, "", .))) %>%
    mutate(V6A = "", V6B = "", V11A = "", V11B = "", V16A = "", V16B = "", ) %>%
    relocate(V16A, V16B, .after = V16) %>%
    relocate(V11A, V11B, .after = V11) %>%
    relocate(V6A, V6B, .after = V6) 
  
  working_table[1,2:8] <- "Bray-Curtis"
  working_table[1,10:15] <- "Weighted Unifrac"
  working_table[1,17:22] <- "Unweighted Unifrac"
  working_table[2,c(2:6,10:13,17:20)] <- "PERMANOVA"
  working_table[2,c(7:8,14:15,21:22)] <- "PERMDISP"
  working_table[3,c(7,14,21)] <- "F"
  working_table[3,c(8,15,22)] <- "p"
  
  
  file_to_work_through <- list.files(paste0("core-metrics-results-",file_path))[str_detect(list.files(paste0("core-metrics-results-",file_path)),"disp.qzv")] 
  file_to_work_through2 <- file_to_work_through[str_detect(file_to_work_through, str_c(row_from_qzvname, collapse = "|"))]
  
  i=file_to_work_through2[1]
  for(i in file_to_work_through2) {
    
    diversity_metric <- str_to_title(str_replace_all(string = str_split(i, pattern = "-")[[1]][1],pattern = "_", replacement = " "))
    if(diversity_metric == "Bray Curtis") {diversity_metric = "Bray-Curtis"}
    
    tablerow <- str_split(i, pattern = "-")[[1]][2]
    
    exported_name <- str_replace(i, pattern = ".qzv", replacement = "_exported")
    
    row_to_mod <- which(working_table$V1 == row_in_table[which(row_from_qzvname==tablerow)])
    
    working_value <- read.table(paste0("core-metrics-results-",file_path,"/",exported_name,"/index.html"), fill= T, sep = "\n") %>% unlist() %>% unname()
    betadisp_test_statistic <- as.numeric(str_split(working_value[which(working_value == "      <th>test statistic</th>")+1], ">|<")[[1]][3])
    betadisp_p_value <- as.numeric(str_split(working_value[which(working_value == "      <th>p-value</th>")+1], ">|<")[[1]][3])
    betadisp_test_statisticB <- as.character(sprintf("%.1f", as.numeric(betadisp_test_statistic)))
    if((betadisp_p_value) < 0.001) {
      betadisp_test_statisticB <- "0.001"
    } else if (betadisp_p_value < 0.01) {
      betadisp_p_valueB <- as.character(sprintf("%.3f", betadisp_p_value))
    } else {
      betadisp_p_valueB <- as.character(sprintf("%.2f", betadisp_p_value))
    }
    working_table[row_to_mod,which(working_table[1,]  == diversity_metric & working_table[3,] == "F" & working_table[2,] == "PERMDISP")] = betadisp_test_statisticB
    working_table[row_to_mod,which(working_table[1,]  == diversity_metric & working_table[3,] == "p" & working_table[2,] == "PERMDISP")] = betadisp_p_valueB
  }
  
  col6replace <- which(as.character(working_table$V6) == "<10-3")
  col11replace <- which(as.character(working_table$V11) == "<10-3")
  col16replace <- which(as.character(working_table$V16) == "<10-3")
  col6Breplace <- which(as.character(working_table$V6B) == "0.001")
  col11Breplace <- which(as.character(working_table$V11B) == "0.001")
  col16Breplace <- which(as.character(working_table$V16B) == "0.001")
  
  
  ## working with flextable
  working_table3 <- flextable(working_table)
  working_table3 <- compose(
    x = working_table3, 
    i = 3,
    j = c("V4","V9","V14"),
    value = as_paragraph("R", as_sup("2"))
  )
  
  working_table3 <- compose(x = working_table3, i = col6replace, j = "V6", value = as_paragraph("<10", as_sup("-3")))
  working_table3 <- compose(x = working_table3, i = col11replace, j = "V11", value = as_paragraph("<10", as_sup("-3")))
  working_table3 <- compose(x = working_table3, i = col16replace, j = "V16", value = as_paragraph("<10", as_sup("-3")))
  working_table3 <- compose(x = working_table3, i = col6Breplace, j = "V6B", value = as_paragraph("<10", as_sup("-3")))
  working_table3 <- compose(x = working_table3, i = col11Breplace, j = "V11B", value = as_paragraph("<10", as_sup("-3")))
  working_table3 <- compose(x = working_table3, i = col16Breplace, j = "V16B", value = as_paragraph("<10", as_sup("-3")))
  
  working_table3 <- merge_h(working_table3, i = 1:2)    # Merge identical vertical cells in Department
  
  working_table3 <- align(working_table3, align = "center", part = "all") # Center-align all text
  working_table3 <- align(working_table3, align = "right", j = "V1") # Vertically center merged text
  working_table3 <- autofit(working_table3)                    # Adjust column widths automatically
  
  working_table3
  
  working_table3 <- font(working_table3, fontname = "Cambria", part = "all")
  working_table3 <- fontsize(working_table3, size = 6, part = "all")
  
  doc <- officer::read_docx()
  doc <- body_add_flextable(doc, value = working_table3)
  print(doc, target = paste0('core-metrics-results-',file_path,'/',file_path,'_permanova_table_withbetadisp.docx'))
  
}



make_taxon_plot_condensed <- function (
    ptp = ptp,
    plot_color = plot_color,
    facet_formula = facet_formula,
    sort_x_axis = sort_x_axis, 
    x_cluster = x_cluster,
    plotvarnames = plotvarnames,
    legend_position = legend_position,
    repcol = "X.SampleID", plot_order = plot_order, read_depth=1, relevel_col = NULL, relevel_vec = NULL, relevel_row = NULL, relevelrow_vec = NULL, xorder = NULL, column_bar_width = 1, dropTheLevels = F, default_colors = F, predefined_taxa = predefined_taxa, predefined_colors = predefined_colors, predefined_shortname = predefined_shortname, default_order = T, order_numbers = F) {
  
  varnames = c(plotvarnames, x_cluster, sort_x_axis)
  varnames <- varnames[!duplicated(varnames)]
  
  var3 <- c("X.SampleID","sample_num")
  varnames2 <- varnames[!varnames%in%c(var3)]
  
  ptp[[1]]$groupid <- ptp[[1]][x_cluster]
  
  growing_df <- data.frame(body_site = character(), mucosa_or_lumen = character(), treatment = character(),phylum.abun = numeric(), shortname = character())
  
  for (i in names(table(ptp[[1]]$groupid))) {
    working_df <- ptp[[1]] %>% 
      filter(groupid == i) %>% droplevels()
    head(working_df)
    num_groups <- dim(working_df %>% group_by(get(repcol)) %>% dplyr::summarize(count = dplyr::n(), .groups = "keep"))[1]
    abun_taxa_df <- working_df %>%
      filter((cluster%in%ptp[[3]]$Var1)) %>% 
      group_by(across(all_of(varnames2)),cluster) %>% 
      dplyr::summarise(phylum.abun = (sum(Count))/(read_depth)/num_groups, .groups = "keep") %>%
      ungroup() %>% 
      inner_join(ptp[[3]], by = c("cluster"="Var1")) %>%
      dplyr::select(-Freq,-perc,-cluster)
    rare_taxa_df <- working_df %>% 
      filter(!cluster%in%(ptp[[3]])$Var1) %>% 
      group_by(across(all_of(varnames2))) %>% 
      dplyr::summarise(phylum.abun = (sum(Count))/(read_depth)/num_groups, .groups = "keep") %>%
      ungroup() %>% 
      mutate(shortname = "other")    
    growing_df <- bind_rows(growing_df,abun_taxa_df,rare_taxa_df)  
  }
  growing_df  
  
  ## set the order of the bars and colors
  if(default_colors == T) {
    remaining_shortname <- ptp[[3]]$shortname[!ptp[[3]]$shortname %in% predefined_shortname]
    if(default_order == T) {
      plot_order <- c(remaining_shortname, predefined_shortname)
      newplot_color <- c(plot_color, predefined_colors)
      plot_color <- newplot_color
    } else {
      plot_order <- ptp[[3]]$shortname[order_numbers] 
      plot_color <- c(plot_color)
    }
  } 
  
  rta <- growing_df
  rta$genus2 <- factor(rta$shortname)
  
  if(length(plot_order) == 1) {
    plot_order <- ptp[[3]]$shortname %>% as.character()
    plot_color <- c(rep("red", length(table(list(rta$genus2)))))
  }
  
  if(sum(is.na(plot_order))> 0) {
    plot_order[is.na(plot_order)] <- "other"
    rta$genus2[is.na(rta$genus2)] <- "other"
    rta$genus2 <- factor(rta$genus2,levels=c("other",plot_order[-which(plot_order == "other")]))
  } else {
    rta$genus2 <- factor(rta$genus2,levels=c("other",plot_order))
  }
  
  if("" %in% plot_order) {
    plot_order[plot_order == ""] <- "other"
    rta$genus2[rta$genus2 == ""] <- "other"
    #  levels(factor(rta$genus2))
    rta$genus2 <- factor(rta$genus2,levels=c("other",plot_order[-which(plot_order == "other")]))
  } else {
    rta$genus2 <- factor(rta$genus2,levels=c("other",plot_order))
  }
  
  if (length(which(!plot_order %in% droplevels(rta$genus2))) > 0) {
    po2 <- plot_order[!(plot_order == "")]
    po3 <- po2[which(!po2 %in% droplevels(rta$genus2))]
    plot_color <- plot_color[-(which(plot_order %in% po3)+1)]
    plot_order <- plot_order[!plot_order %in% po3]
  }
  
  rta$genus2 <- droplevels(rta$genus2)
  
  ## order replicates within a cluster - variable x_cluster
  if(is.null(xorder)) {
    rta$xorder <- factor(unlist(unname(rta[,x_cluster])),levels=c(as.character(ptp[[4]])))
  } else {
    rta$xorder = rta[[xorder]]
  }
  
  ## reorder x-axis
  if (!is.null(relevel_col)) {
    rta[,relevel_col] <- factor(unlist(unname(rta[,relevel_col])), levels = relevel_vec)
    rta$xorder <- rta[,relevel_col]
  }
  
  ## reorder y-axis
  if (!is.null(relevel_row)) {
    rta[,relevel_row] <- factor(unlist(unname(rta[,relevel_row])), levels = relevelrow_vec)
  }
  
  print(levels(rta$genus2))
  print(plot_color)
  p<- ggplot(rta %>% droplevels(), aes(x = xorder, y = phylum.abun, fill = genus2)) + 
    facet_grid(as.formula(facet_formula) , drop = T, space = "free", scales = "free") + 
    geom_bar(stat = "identity", width = column_bar_width) +
    scale_fill_manual(values=plot_color) +
    theme_cowplot() + 
    theme(legend.position = legend_position,
          axis.text.x = element_blank()) +
    ylab("fractional abundance") 
  
  
  p
  # plot(p)
  # jpeg(h=800, w=1600, paste("taxon_plot_",file_path,"_",taxonomic_level,".jpg",sep=""), units = "px", quality = 0.9)
  # plot(p)
  # dev.off()
  
  return(p)
}




prep_taxon_plot <- function(
    file_path, 
    map_path="", 
    plotvarnames = plotvarnames, 
    mapper_file = mapper_file, 
    taxonomic_level="ASV", 
    rpa_in_chart = 0.05, 
    read_depth = read_depth, 
    legend_position=legend_position,
    sort_x_axis = sort_x_axis,
    x_cluster = "X.SampleID",
    predefined_taxa = "", 
    predefined_shortname = predefined_shortname
) {
  
  varnames = c(plotvarnames, x_cluster, sort_x_axis)
  varnames <- varnames[!duplicated(varnames)]
  
  hierarchy <- list(kindgom = "kingdom",
                    phylum = c("kingdom","phylum"),
                    class = c("kingdom","phylum","class"),
                    order = c("kingdom","phylum","class","order"),
                    family = c("kingdom","phylum","class","order","family"),
                    genus = c("kingdom","phylum","class","order","family","genus"),
                    species = c("kingdom","phylum","class","order","family","genus","species"),
                    ASV = c("kingdom","phylum","class","order","family","genus","species","Feature.ID")
  )
  
  tax_level <- unlist(unname(hierarchy[taxonomic_level]))
  
  otu_table <- read.table(paste('core-metrics-results-',file_path,'/rarefied_table.txt',sep=""), comment.char="", header=T, sep="\t", fill=T, skip=1) %>%
    left_join(read.csv(paste0('taxonomy',map_path,'/taxonomy_forR.csv')) %>%
                tidyr::unite(cluster,all_of(tax_level),sep = "_", remove = F) %>%
                dplyr::select(Feature.ID, cluster),
              by=c("X.OTU.ID"="Feature.ID")
    )
  
  map2 <- read.table(mapper_file,comment.char = "", header=T, fill=T, sep="\t") %>%
    mutate(sample2 = gsub("_","", X.SampleID)) %>%
    mutate(sample2 = gsub("-","", sample2)) %>%
    mutate(sample2 = gsub("\\.","",sample2))
  
  
  ## melt
  melted_table <- otu_table %>%
    reshape2::melt(id.vars = c("X.OTU.ID","cluster")) %>%
    filter(value>0) %>%
    dplyr::select(OTU = X.OTU.ID, cluster, Count = value, Sample = variable) %>%
    mutate(Sample = as.character(Sample), OTU = as.character(OTU)) %>%
    filter(Count>-1) %>%
    mutate(sample2 = gsub("\\.","", Sample)) %>%
    mutate(sample2 = gsub("_","", sample2)) %>%
    inner_join(map2, by = "sample2") %>%
    arrange(cluster) %>%
    dplyr::select(X.SampleID, OTU = 1, cluster = 2, Count = 3,Sample = 4,  all_of(varnames)) %>%
    tidyr::unite(col = twovar,  all_of(plotvarnames), sep = "_", remove = F) %>%
    droplevels()
  
  rare_taxa <- melted_table %>%
    group_by(cluster) %>%
    dplyr::summarize(total = sum(Count)/length(table(melted_table$X.SampleID))/read_depth, .groups = "keep") %>%
    filter(total < rpa_in_chart) %>%
    filter(!cluster %in% predefined_taxa) %>%
    dplyr::select(cluster) %>%
    unlist() %>% unname() %>% as.character()
  
  ## specify the x-axis order by two values
  axis_order <- melted_table %>%
    dplyr::select(all_of(x_cluster),all_of(sort_x_axis)) %>%
    arrange(get(sort_x_axis)) %>%
    distinct(get(x_cluster)) %>%
    unlist() %>% unname() #%>% droplevels()
  
  abun_taxa <- melted_table %>%
    group_by(cluster) %>%
    dplyr::summarize(perc = sum(Count)/length(table(melted_table$X.SampleID))/read_depth, .groups = "keep") %>%
    #mutate(Freq = perc * length(table(melted_table[,x_cluster])) * read_depth) %>%
    mutate(Freq = perc * length(table(melted_table$X.SampleID)) * read_depth) %>%
    filter(perc >= rpa_in_chart | cluster %in% predefined_taxa) %>%
    dplyr::select(Var1 = cluster,Freq,perc) %>%
    mutate(shortname = as.character(""))
  
  for(i in 1:length(abun_taxa$Var1)) {
    try(space_split_vector <- strsplit(abun_taxa$Var1[i], split = " "))
    try(while (paste(tail(strsplit(tail(space_split_vector[[1]], 1),split = "")[[1]], 2), collapse="") == "__") {
      space_split_vector[[1]] <- head(space_split_vector[[1]], -1); space_split_vector[[1]]
    })
    try(abun_taxa$shortname[i] <- gsub(strsplit(tail(space_split_vector[[1]],1), "__")[[1]][2], pattern = "_",replacement = ""))
  }
  
  ## test if any shortnames occur more than onces
  countvar <- table(abun_taxa$shortname)[table(abun_taxa$shortname)>1]
  
  ## add a counter to any names that do occur more than once
  if(sum(countvar)>0) {
    for(i in names(countvar)) {
      abun_taxa <- rbind(abun_taxa %>%
                           filter(shortname != i) %>%
                           droplevels(),
                         abun_taxa %>%
                           filter(shortname == i) %>%
                           droplevels() %>%
                           tibble::rowid_to_column("index") %>%
                           mutate(shortname = paste0(shortname,index)) %>%
                           dplyr::select(-index)
      )
    }
  }
  
  ptp <- list(melted_table, rare_taxa, abun_taxa, axis_order)
  print(abun_taxa$shortname)
  if(stringr::str_c(predefined_taxa, collapse = "") != "") {
    print(abun_taxa$shortname[!abun_taxa$shortname %in% predefined_shortname])
    
  }
  print(abun_taxa$Var1)
  return(list(melted_table, rare_taxa, abun_taxa, axis_order))
}


pcoa_manyvar <- function(folder_name = folder_name, mapper_file = mapper_file, title_name_add = title_name_add, legend_position = legend_position,pco1=1,pco2=2,shape_var = shape_var,color_var = color_var,fill_var = fill_var,circle_var = circle_var, linetype_var = linetype_var, plot_shapes = plot_shapes, plot_fills = plot_fills, plot_colors = plot_colors, plot_ellipses = plot_ellipses, circle_color = "black", legend_name = NULL, legend_label = NULL, return_coordinates = F) {
  
  varnames = c(shape_var, color_var, fill_var, circle_var,linetype_var)
  varnames <- varnames[!duplicated(varnames)]
  wfpc <- head(read.table(paste('core-metrics-results-',folder_name,'_pcoa_results/ordination.txt',sep=""), sep="\t", fill=T, skip = 9,blank.lines.skip = T, header=F),-2)
  pc2 <- head(read.table(paste('core-metrics-results-',folder_name,'_pcoa_results/ordination.txt', sep = ""), sep="\t", fill=T, skip = 3, header=F, colClasses = "character"),2)
  pc_values <- as.numeric(as.character(pc2[2,]))*100
  
  wfpc <- wfpc[,c(1,pco1+1, pco2+1)] 
  
  fut2 <- read.table(paste('core-metrics-results-',folder_name,'_distance_matrix/distance-matrix.tsv',sep=""), header=T, sep="\t", stringsAsFactors = T) %>% 
    dplyr::select(X) %>% 
    mutate(X=as.character(X)) %>% 
    left_join(read.table(paste0(mapper_file),comment.char = "", header=T, sep="\t") %>% dplyr::select(all_of(varnames),X.SampleID), by=c("X"="X.SampleID"))
  
  uwmpc_all <- wfpc %>% 
    inner_join(fut2, by=c("V1"="X")) %>%
    mutate(shape_var = as.factor(as.character(get(shape_var))),
           color_var = as.factor(as.character(get(color_var))),
           circle_var = as.factor(as.character(get(circle_var))),
           fill_var = as.factor(as.character(get(fill_var))),
           linetype_var = as.factor(as.character(get(linetype_var)))
    ) %>% droplevels()
  
  shape_table <- table(list(uwmpc_all[,shape_var]))
  fill_table <- table(list(uwmpc_all[,fill_var]))
  color_table <- table(list(uwmpc_all[,color_var]))
  circle_table <- table(list(uwmpc_all[,circle_var]))
  linetype_table <- table(list(uwmpc_all[,linetype_var]))
  
  cat("Shapes:",length(shape_table),"values")
  print(shape_table)
  cat("Fill:",length(fill_table),"values")
  print(fill_table)
  cat("Color:",length(color_table),"values")
  print(color_table)
  cat("Ellipse:",length(circle_table),"values")
  print(circle_table)
  cat("Linetype:",length(circle_table),"values")
  print(linetype_table)
  # 
  # plot_shapes <- c("square","circle")
  # plot_fills <- c("green","green")
  # plot_colors <- c("red","black")
  # plot_ellipses <- c(1:38)
  # 
  # legend_position = "bottom"
  #str(uwmpc_all)
  
  uwmpc_all$axis1 <- round(uwmpc_all[,paste0("V",pco1+1)],10)
  uwmpc_all$axis2 <- round(uwmpc_all[,paste0("V",pco2+1)],10)
  
  if(is.null(legend_name)){
    legend_name = c(color_var, fill_var, shape_var, linetype_var)
  }
  
  if(is.null(legend_label)){
    legend_label = list(colors = names(color_table)[color_table!=0], fills = names(fill_table)[fill_table!=0], shapes = names(shape_table)[shape_table!=0], linetypes = names(linetype_table)[linetype_table!=0])
  }
  
  coordinates <- uwmpc_all %>% group_by(!!!syms(varnames)) %>% dplyr::summarize(mean1 = mean(axis1),mean2 = mean(axis2)) %>% mutate(!!sym(paste0(unique(c(shape_var,color_var,fill_var,circle_var,linetype_var)), collapse = "_")) := paste(!!!syms(varnames), sep = "_")) %>% ungroup() 
  
  print(legend_label[[2]])
  
  outplot <- ggplot(uwmpc_all, aes(x = axis1, y = axis2, colour=color_var, shape=shape_var, fill=fill_var, lty = linetype_var)) + 
    geom_point(alpha = 1, size=2.5) + #, shape=plot_shapes, size=3, col=plot_colors) + 
    scale_color_manual(name=legend_name[1], labels=legend_label[[1]], values=plot_colors) + 
    scale_fill_manual(name=legend_name[2], labels=legend_label[[2]], values=plot_fills, breaks = names(fill_table)) + 
    scale_shape_manual(name=legend_name[3],labels=legend_label[[3]], values=plot_shapes) + 
    scale_linetype_manual(name=legend_name[4],labels=legend_label[[4]], values=plot_ellipses, breaks = names(linetype_table)) +
    #		scale_fill_manual(name="Legend", values=c("red","black","white")) +
    theme(panel.background = element_blank(), 
          axis.line = element_line(), 
          axis.ticks=element_blank(), 
          axis.title=element_text(size=14),	
          #           title=element_text(size=16),
          legend.position=legend_position,
          plot.title = element_text(size=16, hjust=0),
          axis.text = element_blank())+
    labs(x = paste("PCo",pco1," ( ",round(as.numeric(pc_values[pco1]),1),"% )",sep=""), 
         y = paste("PCo",pco2," ( ",round(as.numeric(pc_values[pco2]),1),"% )",sep=""), 
         title = title_name_add) + 
    stat_ellipse(aes(x = axis1, y = axis2, group= circle_var), show.legend = T, type = "t", geom = "polygon", level = 0.95, alpha = 0, inherit.aes=T) 
  
  print(str(uwmpc_all))
  
  if(return_coordinates == F) {
    print(outplot)
    return(outplot)
  } else {
    print(outplot)
    print(coordinates)
    return(list(outplot,coordinates))
  }
  
  
}

pcoa_manyvar_circle_elsewhere <- function(folder_name = folder_name, mapper_file = mapper_file, title_name_add = title_name_add, legend_position = legend_position,pco1=1,pco2=2,shape_var = shape_var,color_var = color_var,fill_var = fill_var,circle_var = circle_var, linetype_var = linetype_var, plot_shapes = plot_shapes, plot_fills = plot_fills, plot_colors = plot_colors, plot_ellipses = plot_ellipses, circle_color = "black", legend_name = NULL, legend_label = NULL, return_coordinates = F) {
  
  varnames = c(shape_var, color_var, fill_var, circle_var,linetype_var)
  varnames <- varnames[!duplicated(varnames)]
  wfpc <- head(read.table(paste('core-metrics-results-',folder_name,'_pcoa_results/ordination.txt',sep=""), sep="\t", fill=T, skip = 9,blank.lines.skip = T, header=F),-2)
  pc2 <- head(read.table(paste('core-metrics-results-',folder_name,'_pcoa_results/ordination.txt', sep = ""), sep="\t", fill=T, skip = 3, header=F, colClasses = "character"),2)
  pc_values <- as.numeric(as.character(pc2[2,]))*100
  
  wfpc <- wfpc[,c(1,pco1+1, pco2+1)] 
  
  fut2 <- read.table(paste('core-metrics-results-',folder_name,'_distance_matrix/distance-matrix.tsv',sep=""), header=T, sep="\t", stringsAsFactors = T) %>% 
    dplyr::select(X) %>% 
    mutate(X=as.character(X)) %>% 
    left_join(read.table(paste0(mapper_file),comment.char = "", header=T, sep="\t") %>% dplyr::select(all_of(varnames),X.SampleID), by=c("X"="X.SampleID"))
  
  uwmpc_all <- wfpc %>% 
    inner_join(fut2, by=c("V1"="X")) %>%
    mutate(shape_var = as.factor(as.character(get(shape_var))),
           color_var = as.factor(as.character(get(color_var))),
           circle_var = as.factor(as.character(get(circle_var))),
           fill_var = as.factor(as.character(get(fill_var))),
           linetype_var = as.factor(as.character(get(linetype_var)))
    ) %>% droplevels()
  
  shape_table <- table(list(uwmpc_all[,shape_var]))
  fill_table <- table(list(uwmpc_all[,fill_var]))
  color_table <- table(list(uwmpc_all[,color_var]))
  circle_table <- table(list(uwmpc_all[,circle_var]))
  linetype_table <- table(list(uwmpc_all[,linetype_var]))
  
  cat("Shapes:",length(shape_table),"values")
  print(shape_table)
  cat("Fill:",length(fill_table),"values")
  print(fill_table)
  cat("Color:",length(color_table),"values")
  print(color_table)
  cat("Ellipse:",length(circle_table),"values")
  print(circle_table)
  cat("Linetype:",length(circle_table),"values")
  print(linetype_table)
  # 
  # plot_shapes <- c("square","circle")
  # plot_fills <- c("green","green")
  # plot_colors <- c("red","black")
  # plot_ellipses <- c(1:38)
  # 
  # legend_position = "bottom"
  #str(uwmpc_all)
  
  uwmpc_all$axis1 <- round(uwmpc_all[,paste0("V",pco1+1)],10)
  uwmpc_all$axis2 <- round(uwmpc_all[,paste0("V",pco2+1)],10)
  
  if(is.null(legend_name)){
    legend_name = c(color_var, fill_var, shape_var, linetype_var)
  }
  
  if(is.null(legend_label)){
    legend_label = list(colors = names(color_table)[color_table!=0], fills = names(fill_table)[fill_table!=0], shapes = names(shape_table)[shape_table!=0], linetypes = names(linetype_table)[linetype_table!=0], circle_color = names(circle_table)[circle_table!=0])
  }
  
  coordinates <- uwmpc_all %>% group_by(!!!syms(varnames)) %>% dplyr::summarize(mean1 = mean(axis1),mean2 = mean(axis2)) %>% mutate(!!sym(paste0(unique(c(shape_var,color_var,fill_var,circle_var,linetype_var)), collapse = "_")) := paste(!!!syms(varnames), sep = "_")) %>% ungroup() 
  
  
  outplot <- ggplot(uwmpc_all, aes(x = axis1, y = axis2, colour=color_var, shape=shape_var, fill=fill_var, lty = linetype_var)) + 
    geom_point(alpha = 1, size=2.5) + #, shape=plot_shapes, size=3, col=plot_colors) + 
    scale_color_manual(name=legend_name[1], labels=legend_label[[1]], values=plot_colors) + 
    scale_fill_manual(name=legend_name[2], labels=legend_label[[2]], values=plot_fills, breaks = names(fill_table)) + 
    scale_shape_manual(name=legend_name[3],labels=legend_label[[3]], values=plot_shapes) + 
    scale_linetype_manual(name=legend_name[4],labels=legend_label[[4]], values=plot_ellipses, breaks = names(linetype_table)) +
    #		scale_fill_manual(name="Legend", values=c("red","black","white")) +
    theme(panel.background = element_blank(), 
          axis.line = element_line(), 
          axis.ticks=element_blank(), 
          axis.title=element_text(size=14),	
          #           title=element_text(size=16),
          legend.position=legend_position,
          plot.title = element_text(size=16, hjust=0),
          axis.text = element_blank())+
    labs(x = paste("PCo",pco1," ( ",round(as.numeric(pc_values[pco1]),1),"% )",sep=""), 
         y = paste("PCo",pco2," ( ",round(as.numeric(pc_values[pco2]),1),"% )",sep=""), 
         title = title_name_add) +
    theme(legend.position = "bottom") + 
    stat_ellipse(aes(x = axis1, y = axis2, color = circle_var, linetype = linetype_var), show.legend = T, type = "t", geom = "polygon", level = 0.95, alpha = 0, inherit.aes=F)
    
  
  print(str(uwmpc_all))
  
  if(return_coordinates == F) {
    print(outplot)
    return(outplot)
  } else {
    print(outplot)
    print(coordinates)
    return(list(outplot,coordinates))
  }
  
  
}




make_distance_plot_2var <- function (folder_path, mapper_file = mapper_file, var1, var2, comparisons, title_name_add="", the_xid="XID",anglenum = anglenum) {
  flies_unifrac_table <- read.table(paste('core-metrics-results-',folder_path,'_distance_matrix/distance-matrix.tsv',sep=""), header=T, sep="\t") %>% dplyr::select(X) %>% left_join(read.table(mapper_file,comment.char = "", sep="\t", header=T), by=c("X"="X.SampleID")) %>% mutate(XID=gsub(x = X,pattern = "-",replacement = ".")) %>% dplyr::select((all_of(c(the_xid, var1, var2))))
  
  flies_dm <- read.table(paste('core-metrics-results-',folder_path,'_distance_matrix/distance-matrix.tsv',sep=""), header=T, sep="\t")
  fs2 <- as.dist(flies_dm[,2:dim(flies_dm)[2]])
  
  fs3 <- as.matrix(fs2); fs3[lower.tri(fs3)] <- NA
  
  print(flies_unifrac_table[1,])
  ## melt the table and get out just the right comparisons
  fs3[1,]
  melt_table <- data.frame(melt(fs3)) %>% 
    filter(is.na(value)==F, value!=0) %>% 
    inner_join(flies_unifrac_table, by=c("Var1"="XID")) %>% 
    inner_join(flies_unifrac_table, by=c("Var2"="XID")) %>%  
    mutate(gs.x = paste0(as.character(get(paste0(var1,".x"))),as.character(get(paste0(var2,".x")))), gs.y=paste0(as.character(get(paste0(var1,".y"))),as.character(get(paste0(var2,".y"))))) %>% 
    mutate(gs=ifelse(gs.x < gs.y, paste(gs.x,gs.y, sep="_"), paste(gs.y, gs.x, sep="_"))) %>% 
    mutate(gs2 = as.factor(as.character(gs))) 
  
  str(melt_table)
  print(table(list(melt_table$gs2)))
  
  melt_table <- melt_table %>% 
    filter(gs2%in%comparisons) %>% droplevels()
  
  melt_table$gs2 <- factor(melt_table$gs2,levels=comparisons)
  
  table(list(melt_table$gs2))
  ## run statistics
  def <- kruskal.test(value ~ gs2, melt_table);   #print(def$p.value)
  print(def)
  efg <- dunn.test(melt_table$value,melt_table$gs2, method = "bh", table = F, kw = F)
  ghi <- cldList(comparison = efg$comparisons, p.value = efg$P.adjusted, threshold = 0.05);   print(ghi)
  
  #ghi$Group2 <- factor(ghi$Group, levels=reorder(comparisons))
  #ghi <- ghi %>% dplyr::slice(match(comparisons %>% gsub(pattern = " ",replacement = "",x = comparisons), Group))
  ghi
  
  ## make the summary statistics
  melt_table_plot <- melt_table %>% 
    group_by(gs2) %>% 
    summarize(mean=mean(value), sem=sd(value)/sqrt(length(value)))
  
  ## make the plot
  ggplot(melt_table_plot, aes(x=gs2, y=mean)) + 
    geom_bar(position=position_dodge(), stat="identity") +
    geom_errorbar(aes(ymin=mean+sem, ymax=mean-sem),
                  width=.2,                    # Width of the error bars
                  position=position_dodge(.9)) +
    coord_cartesian(ylim=c((min(melt_table_plot$mean-melt_table_plot$sem)*.9),max(melt_table_plot$mean+melt_table_plot$sem)*1.1)) + #scale_y_continuous(limits=c(.25,.4)) + 
    #	scale_x_discrete(labels=c("Base","Time1","Time2","Time3")) +
    theme(axis.text=element_text(size=14),
          axis.text.x=element_text(angle=anglenum),
          panel.background = element_blank(),
          axis.line = element_line(), 
          axis.ticks=element_line(), 
          axis.title=element_text(size=16), plot.title=element_text(hjust=0)) +
    labs(x="Season",
         y="Index distance",
         title=title_name_add) +
    geom_text(aes(label=ghi$Letter, y=mean+sem, vjust=-1.5), size=6)
}


make_ancom2_plots <- function(file_path, map_path="", mapper_file=mapper_file, taxonomic_level=taxonomic_level,var1=var1,var2=var2,var3=var3, var4 = var4,var5 = NULL, newcol="newcol", the_id = "X.SampleID", main.var=main.var, adj.formula=adj.formula, repeat.var = repeat.var, multcorr=multcorr, sig=sig, prev.cut=prev.cut, Group = Group, random.formula=NULL, return_plots = FALSE, return_tables = FALSE) {
  
  rm(otu_table, ref_names, otu2, map2, phyl2, phyl3, col1drop, col2drop, phyl4, ancom.OTU, detected_taxa, otu3, otu4, otu5, phyl5, plist, row_list)
  
  otu_table <- read.table(paste('core-metrics-results-',file_path,'/rarefied_table.txt',sep=""), comment.char="", header=T, sep="\t", fill=T, skip=1) %>% 
    left_join(read.csv(paste0('taxonomy',map_path,'/taxonomy_forR.csv')), by=c("X.OTU.ID"="Feature.ID"))
  
  ref_names <- subset(colnames(otu_table), (colnames(otu_table)%in%c("X.OTU.ID","kingdom","phylum","class","order","family","genus","species")==F))
  
  #  otu_table$X.OTU.ID <- as.factor(otu_table$X.OTU.ID)
  
  #  taxonomic_level <- "X.OTU.ID"
  ## Cluster rows by taxonomy hierarchically 
  taxon_vector <- c("kingdom","phylum","class","order","family","genus","species","X.OTU.ID")
  taxon_number <- which(taxon_vector==taxonomic_level)
  taxon_vector2 <- c()
  for (i in 1:taxon_number) {
    taxon_vector2 <- c(taxon_vector2,taxon_vector[i])
  }
  
  if(taxonomic_level!="X.OTU.ID") {
    otu2 <- otu_table[,c("X.OTU.ID",taxon_vector2)] %>% 
      tidyr::unite(clustered_taxonomy, (taxon_vector[1:length(taxon_vector2)])) %>%
      mutate(clustered_taxonomy = as.character(clustered_taxonomy)) %>%
      mutate(clustered_taxonomy = gsub(pattern = " ",replacement = "",x = clustered_taxonomy)) %>%
      mutate(clustered_taxonomy = gsub(pattern = "^__",replacement = "__unassigned",x = clustered_taxonomy)) %>%
      mutate(clustered_taxonomy = gsub(pattern = "___",replacement = "__unassigned_",x = clustered_taxonomy)) %>%
      mutate(clustered_taxonomy = gsub(pattern = "__$",replacement = "__unassigned",x = clustered_taxonomy))
  } else if (taxonomic_level == "X.OTU.ID") {
    otu2 <- otu_table[,c("X.OTU.ID",taxon_vector2)] %>% 
      mutate(OTUID2 = X.OTU.ID) %>%
      tidyr::unite(clustered_taxonomy, c(taxon_vector[1:7],"OTUID2")) %>%
      mutate(clustered_taxonomy = as.character(clustered_taxonomy)) %>%
      mutate(clustered_taxonomy = gsub(pattern = " ",replacement = "",x = clustered_taxonomy)) %>%
      mutate(clustered_taxonomy = gsub(pattern = "^__",replacement = "__unassigned",x = clustered_taxonomy)) %>%
      mutate(clustered_taxonomy = gsub(pattern = "___",replacement = "__unassigned_",x = clustered_taxonomy)) %>%
      mutate(clustered_taxonomy = gsub(pattern = "__$",replacement = "__unassigned",x = clustered_taxonomy)) %>%
      dplyr::select(-X.OTU.ID.1)
    
  } 
  
  if(is.null(var5)) {
    map2 <- read.table(mapper_file,comment.char = "", header=T, fill=T,sep="\t") %>% 
      dplyr::select(all_of(c(the_id,var1,var2,var3,var4,var5,Group)))
  } else {
    map2 <- read.table(mapper_file,comment.char = "", header=T, fill=T,sep="\t") %>% 
      dplyr::select(all_of(c(the_id,var1,var2,var3,var4,var5,Group)))
  }
  #print(head(map2))
  
  phyl2 <- otu_table %>% 
    inner_join(otu2, by = "X.OTU.ID") %>%
    group_by(clustered_taxonomy) %>% 
    summarize_at(ref_names,sum, na.rm=T)
  rownames(phyl2) <- as.character(unlist(phyl2$clustered_taxonomy))
  
  phyl3 <- phyl2 %>% dplyr::select(-clustered_taxonomy) %>% t() %>% data.frame() 
  colnames(phyl3) <- rownames(phyl2)
  phyl3 <- mutate(phyl3, X.SampleID=rownames(phyl3))
  phyl3$Sample.ID <- gsub("\\.","", phyl3$X.SampleID)
  phyl3$Sample.ID <- gsub("_","", phyl3$Sample.ID)
  map2$Sample.ID <- gsub("_","",map2$X.SampleID)
  map2$Sample.ID <- gsub("-","",map2$Sample.ID)
  map2$Sample.ID <- gsub("\\.","",map2$Sample.ID) # added 2021-05-05
  
  phyl5 <- phyl3 %>% dplyr::select(Sample.ID, everything()) %>% dplyr::select(-X.SampleID)
  colnames(phyl5)[1] <- "Sample.ID"
  
  map3 <- map2 %>% dplyr::select(Sample.ID, everything()) %>% dplyr::select(-X.SampleID) %>% filter(Sample.ID %in% phyl5$Sample.ID)
  colnames(map3)[1] <- "Sample.ID"
  
  phyl4 <- phyl3 %>% inner_join(map2, by=c("Sample.ID")) %>% mutate(Group = get(Group)) %>% droplevels()#%>% dplyr::select(.dots = list(col1drop, col2drop, paste0("-",var1))) %>% dplyr::select(-sample2)
  rm(comparison_test)
  
  if (taxonomic_level == "X.OTU.ID") {
    colnames(phyl5) <- paste0("X",colnames(phyl5))
    colnames(phyl5)[1] <- "Sample.ID"
  }
  
  write.csv(phyl5,"phyl5b.csv")
  comparison_test = ANCOM(otu_data = phyl5, 
                          meta_data = map3, 
                          main_var = main.var,  
                          zero_cut = prev.cut, 
                          p_adjust_method = multcorr, 
                          alpha = sig, 
                          adj_formula = adj.formula, 
                          rand_formula = random.formula)
  
  #print(comparison_test)
  write.csv(comparison_test, paste("ancomexcel_",main.var,"_",file_path,"_",taxonomic_level,".csv",sep=""))
  dt1 <- data.frame(comparison_test) %>% filter(detected_0.9==T) %>% dplyr::select(otu_id)
  detected_taxa <- data.frame(OTU=unlist(sapply(X = dt1$otu_id, function(x) ifelse(substring(x,1,1)=="X",substring(x,2),substring(x,1)))))	%>% unlist() %>% unname() %>% as.character()
  
  otu3 <- otu_table[,c("X.OTU.ID","kingdom","phylum","class","order","family","genus","species")]
  otu3$OTU <- as.character(otu3$X.OTU.ID)
  
  if(taxonomic_level!="OTU") {
    otu3[,paste(taxonomic_level)] = gsub(" ","",otu3[,paste(taxonomic_level)])
  }
  
  otu5 <- otu3 %>% inner_join(otu2, by = "X.OTU.ID") %>% filter(clustered_taxonomy%in%detected_taxa) %>% distinct(get(taxonomic_level), .keep_all = T)
  
  extra_cols2 <- names(table(list(phyl4$Group)))
  extra_cols <- unname(c(extra_cols2,sapply(extra_cols2, function(x) paste0(x,"_sem"))))
  otu5[extra_cols] <- "NA"
  
  phyl3$sample2 <- gsub("\\.","", phyl3$X.SampleID)
  phyl3$sample2 <- gsub("_","", phyl3$sample2)
  
  i
  for (i in 1:length(detected_taxa)) {
    
    rm(phyl6, column_name)
    column_name <- detected_taxa[i]
    try(phyl6 <-phyl4 %>% mutate(rabun = get(column_name)/sum(otu_table[,2])) %>% dplyr::select(rabun, Group) %>% group_by(Group) %>% dplyr::summarize(mean=mean(rabun), sem=sd(rabun)/sqrt(length(rabun))),T)
    if(exists("phyl6")==F) {
      try(phyl6 <- phyl4 %>% mutate(rabun = get(paste("X",column_name, sep=""))/sum(otu_table[,2])) %>% dplyr::select(rabun, Group) %>% group_by(Group) %>% dplyr::summarize(mean=mean(rabun), sem=sd(rabun)/sqrt(length(rabun))))
    }
    
    phyl6
    rm(t)
    for (t in 1:length(extra_cols2)) {
      otu5[i,paste0(extra_cols2[t])] <- phyl6 %>% filter(Group==extra_cols2[t]) %>% dplyr::select(mean) %>% unlist()
      otu5[i,paste0(extra_cols2[t],"_sem")] <- phyl6 %>% filter(Group==extra_cols2[t]) %>% dplyr::select(sem) %>% unlist()
    }
    
    assign(paste("p",i,sep="_"),value = ggplot(phyl6, aes(x=Group, y=mean)) + 
             geom_bar(position=position_dodge(), stat="identity") +
             geom_errorbar(aes(ymin=mean+sem, ymax=mean-sem),
                           width=.2,                    # Width of the error bars
                           position=position_dodge(.9)) +
             coord_cartesian(ylim=c((min(phyl6$mean-phyl6$sem)*.9),max(phyl6$mean+phyl6$sem)*1.3)) + #scale_y_continuous(limits=c(.25,.4)) + 
             theme(axis.text=element_text(size=14), 
                   panel.background = element_blank(),
                   axis.line = element_line(), 
                   axis.ticks=element_line(), 
                   axis.title=element_text(size=16),
                   title=element_text(size=13)) +
             labs(y="relative abundance",x=var1,title=detected_taxa[i]))
  }
  
  ## make a list of the plots
  if(length(detected_taxa)>1) {
    plist <- list(p_1)
    for(q in 2:length(detected_taxa)) {
      plist[[q]] <- get(paste0("p_",q))
    }
  } else if (length(detected_taxa)>0) {
    plist <- list(p_1)
  }
  
  write.csv(otu5, paste("ancom_",var1,"_",file_path,"_",taxonomic_level,".csv",sep=""))
  
  jpeg(h=800*1.25, w=1600*1.25, paste("ancom_",var1,"_",file_path,"_",taxonomic_level,".jpg",sep=""), units = "px", quality = 0.9)
  do.call("grid.arrange", c(plist))
  dev.off()
  
  if(return_plots == T) {
    return(plist)
  }
  
  if(return_tables == T) {
    for (i in 1:length(detected_taxa)) {
      
      rm(phyl6, phyl7, column_name)
      column_name <- detected_taxa[i]
      try(phyl6 <-phyl4 %>% mutate(rabun = get(column_name)/sum(otu_table[,2])) %>% dplyr::select(rabun, Group) %>% group_by(Group) %>% dplyr::summarize(mean=mean(rabun), sem=sd(rabun)/sqrt(length(rabun))),T)
      try(phyl7 <-phyl4 %>% mutate(rabun = get(column_name)/sum(otu_table[,2])) %>% dplyr::select(rabun, Group),T)
      if(exists("phyl6")==F) {
        try(phyl6 <- phyl4 %>% mutate(rabun = get(paste("X",column_name, sep=""))/sum(otu_table[,2])) %>% dplyr::select(rabun, Group) %>% group_by(Group) %>% dplyr::summarize(mean=mean(rabun), sem=sd(rabun)/sqrt(length(rabun))))
      }
      if(exists("phyl7")==F) {
        try(phyl7 <- phyl4 %>% mutate(rabun = get(paste("X",column_name, sep=""))/sum(otu_table[,2])) %>% dplyr::select(rabun, Group),T)
      }
      phyl6
      phyl7
      rm(t)
      for (t in 1:length(extra_cols2)) {
        otu5[i,paste0(extra_cols2[t])] <- phyl6 %>% filter(Group==extra_cols2[t]) %>% dplyr::select(mean) %>% unlist()
        otu5[i,paste0(extra_cols2[t],"_sem")] <- phyl6 %>% filter(Group==extra_cols2[t]) %>% dplyr::select(sem) %>% unlist()
      }
      
      assign(paste("table",i,sep="_"),value = phyl6)
      assign(paste("data",i,sep="_"),value = phyl7)
    }
    
    ## make a list of the plots
    if(length(detected_taxa)>1) {
      tablelist <- list(table_1)
      datalist <- list(data_1)
      for(q in 2:length(detected_taxa)) {
        tablelist[[q]] <- get(paste0("table_",q))
        datalist[[q]] <- get(paste0("data_",q))
      }
    } else if (length(detected_taxa)>0) {
      tablelist <- list(table_1)
      datalist <- list(data_1)
    }
    return(list(tablelist, datalist))
  }
}

make_ancom2_plots_timefac <- function(file_path, map_path="", mapper_file=mapper_file, taxonomic_level=taxonomic_level,var1=var1,var2=var2,var3=var3, var4 = var4,var5 = NULL, newcol="newcol", the_id = "X.SampleID", main.var=main.var, adj.formula=adj.formula, repeat.var = repeat.var, multcorr=multcorr, sig=sig, prev.cut=prev.cut, Group = Group, random.formula=NULL, return_plots = FALSE, return_tables = FALSE) {
  
  rm(otu_table, ref_names, otu2, map2, phyl2, phyl3, col1drop, col2drop, phyl4, ancom.OTU, detected_taxa, otu3, otu4, otu5, phyl5, plist, row_list)
  
  otu_table <- read.table(paste('core-metrics-results-',file_path,'/rarefied_table.txt',sep=""), comment.char="", header=T, sep="\t", fill=T, skip=1) %>% 
    left_join(read.csv(paste0('taxonomy',map_path,'/taxonomy_forR.csv')), by=c("X.OTU.ID"="Feature.ID"))
  
  ref_names <- subset(colnames(otu_table), (colnames(otu_table)%in%c("X.OTU.ID","kingdom","phylum","class","order","family","genus","species")==F))
  
  #  otu_table$X.OTU.ID <- as.factor(otu_table$X.OTU.ID)
  
  #  taxonomic_level <- "X.OTU.ID"
  ## Cluster rows by taxonomy hierarchically 
  taxon_vector <- c("kingdom","phylum","class","order","family","genus","species","X.OTU.ID")
  taxon_number <- which(taxon_vector==taxonomic_level)
  taxon_vector2 <- c()
  for (i in 1:taxon_number) {
    taxon_vector2 <- c(taxon_vector2,taxon_vector[i])
  }
  
  if(taxonomic_level!="X.OTU.ID") {
    otu2 <- otu_table[,c("X.OTU.ID",taxon_vector2)] %>% 
      tidyr::unite(clustered_taxonomy, (taxon_vector[1:length(taxon_vector2)])) %>%
      mutate(clustered_taxonomy = as.character(clustered_taxonomy)) %>%
      mutate(clustered_taxonomy = gsub(pattern = " ",replacement = "",x = clustered_taxonomy)) %>%
      mutate(clustered_taxonomy = gsub(pattern = "^__",replacement = "__unassigned",x = clustered_taxonomy)) %>%
      mutate(clustered_taxonomy = gsub(pattern = "___",replacement = "__unassigned_",x = clustered_taxonomy)) %>%
      mutate(clustered_taxonomy = gsub(pattern = "__$",replacement = "__unassigned",x = clustered_taxonomy))
  } else if (taxonomic_level == "X.OTU.ID") {
    otu2 <- otu_table[,c("X.OTU.ID",taxon_vector2)] %>% 
      mutate(OTUID2 = X.OTU.ID) %>%
      tidyr::unite(clustered_taxonomy, c(taxon_vector[1:7],"OTUID2")) %>%
      mutate(clustered_taxonomy = as.character(clustered_taxonomy)) %>%
      mutate(clustered_taxonomy = gsub(pattern = " ",replacement = "",x = clustered_taxonomy)) %>%
      mutate(clustered_taxonomy = gsub(pattern = "^__",replacement = "__unassigned",x = clustered_taxonomy)) %>%
      mutate(clustered_taxonomy = gsub(pattern = "___",replacement = "__unassigned_",x = clustered_taxonomy)) %>%
      mutate(clustered_taxonomy = gsub(pattern = "__$",replacement = "__unassigned",x = clustered_taxonomy)) %>%
      dplyr::select(-X.OTU.ID.1)
    
  } 
  
  if(is.null(var5)) {
    map2 <- read.table(mapper_file,comment.char = "", header=T, fill=T,sep="\t") %>% 
      dplyr::select(all_of(c(the_id,var1,var2,var3,var4,var5,Group)))
  } else {
    map2 <- read.table(mapper_file,comment.char = "", header=T, fill=T,sep="\t") %>% 
      dplyr::select(all_of(c(the_id,var1,var2,var3,var4,var5,Group)))
  }
  #print(head(map2))
  
  phyl2 <- otu_table %>% 
    inner_join(otu2, by = "X.OTU.ID") %>%
    group_by(clustered_taxonomy) %>% 
    summarize_at(ref_names,sum, na.rm=T)
  rownames(phyl2) <- as.character(unlist(phyl2$clustered_taxonomy))
  
  phyl3 <- phyl2 %>% dplyr::select(-clustered_taxonomy) %>% t() %>% data.frame() 
  colnames(phyl3) <- rownames(phyl2)
  phyl3 <- mutate(phyl3, X.SampleID=rownames(phyl3))
  phyl3$Sample.ID <- gsub("\\.","", phyl3$X.SampleID)
  phyl3$Sample.ID <- gsub("_","", phyl3$Sample.ID)
  map2$Sample.ID <- gsub("_","",map2$X.SampleID)
  map2$Sample.ID <- gsub("-","",map2$Sample.ID)
  map2$Sample.ID <- gsub("\\.","",map2$Sample.ID) # added 2021-05-05
  
  phyl5 <- phyl3 %>% dplyr::select(Sample.ID, everything()) %>% dplyr::select(-X.SampleID) 
  colnames(phyl5)[1] <- "Sample.ID"
  
  map3 <- map2 %>% dplyr::select(Sample.ID, everything()) %>% dplyr::select(-X.SampleID) %>% filter(Sample.ID %in% phyl5$Sample.ID) %>% mutate(time_fac = factor(time_num, ordered =T))
  colnames(map3)[1] <- "Sample.ID"
  
  phyl4 <- phyl3 %>% inner_join(map2, by=c("Sample.ID")) %>% mutate(Group = get(Group)) %>% droplevels()#%>% dplyr::select(.dots = list(col1drop, col2drop, paste0("-",var1))) %>% dplyr::select(-sample2)
  rm(comparison_test)
  
  if (taxonomic_level == "X.OTU.ID") {
    colnames(phyl5) <- paste0("X",colnames(phyl5))
    colnames(phyl5)[1] <- "Sample.ID"
  }
  
  write.csv(phyl5,"phyl5b.csv")
  comparison_test = ANCOM(otu_data = phyl5, 
                          meta_data = map3, 
                          main_var = main.var,  
                          zero_cut = prev.cut, 
                          p_adjust_method = multcorr, 
                          alpha = sig, 
                          adj_formula = adj.formula, 
                          rand_formula = random.formula)
  
  #print(comparison_test)
  write.csv(comparison_test, paste("ancomexcel_",main.var,"_",file_path,"_",taxonomic_level,".csv",sep=""))
  dt1 <- data.frame(comparison_test) %>% filter(detected_0.9==T) %>% dplyr::select(otu_id)
  detected_taxa <- data.frame(OTU=unlist(sapply(X = dt1$otu_id, function(x) ifelse(substring(x,1,1)=="X",substring(x,2),substring(x,1)))))	%>% unlist() %>% unname() %>% as.character()
  
  otu3 <- otu_table[,c("X.OTU.ID","kingdom","phylum","class","order","family","genus","species")]
  otu3$OTU <- as.character(otu3$X.OTU.ID)
  
  if(taxonomic_level!="OTU") {
    otu3[,paste(taxonomic_level)] = gsub(" ","",otu3[,paste(taxonomic_level)])
  }
  
  otu5 <- otu3 %>% inner_join(otu2, by = "X.OTU.ID") %>% filter(clustered_taxonomy%in%detected_taxa) %>% distinct(get(taxonomic_level), .keep_all = T)
  
  extra_cols2 <- names(table(list(phyl4$Group)))
  extra_cols <- unname(c(extra_cols2,sapply(extra_cols2, function(x) paste0(x,"_sem"))))
  otu5[extra_cols] <- "NA"
  
  phyl3$sample2 <- gsub("\\.","", phyl3$X.SampleID)
  phyl3$sample2 <- gsub("_","", phyl3$sample2)
  
  i
  for (i in 1:length(detected_taxa)) {
    
    rm(phyl6, column_name)
    column_name <- detected_taxa[i]
    try(phyl6 <-phyl4 %>% mutate(rabun = get(column_name)/sum(otu_table[,2])) %>% dplyr::select(rabun, Group) %>% group_by(Group) %>% dplyr::summarize(mean=mean(rabun), sem=sd(rabun)/sqrt(length(rabun))),T)
    if(exists("phyl6")==F) {
      try(phyl6 <- phyl4 %>% mutate(rabun = get(paste("X",column_name, sep=""))/sum(otu_table[,2])) %>% dplyr::select(rabun, Group) %>% group_by(Group) %>% dplyr::summarize(mean=mean(rabun), sem=sd(rabun)/sqrt(length(rabun))))
    }
    
    phyl6
    rm(t)
    for (t in 1:length(extra_cols2)) {
      otu5[i,paste0(extra_cols2[t])] <- phyl6 %>% filter(Group==extra_cols2[t]) %>% dplyr::select(mean) %>% unlist()
      otu5[i,paste0(extra_cols2[t],"_sem")] <- phyl6 %>% filter(Group==extra_cols2[t]) %>% dplyr::select(sem) %>% unlist()
    }
    
    assign(paste("p",i,sep="_"),value = ggplot(phyl6, aes(x=Group, y=mean)) + 
             geom_bar(position=position_dodge(), stat="identity") +
             geom_errorbar(aes(ymin=mean+sem, ymax=mean-sem),
                           width=.2,                    # Width of the error bars
                           position=position_dodge(.9)) +
             coord_cartesian(ylim=c((min(phyl6$mean-phyl6$sem)*.9),max(phyl6$mean+phyl6$sem)*1.3)) + #scale_y_continuous(limits=c(.25,.4)) + 
             theme(axis.text=element_text(size=14), 
                   panel.background = element_blank(),
                   axis.line = element_line(), 
                   axis.ticks=element_line(), 
                   axis.title=element_text(size=16),
                   title=element_text(size=13)) +
             labs(y="relative abundance",x=var1,title=detected_taxa[i]))
  }
  
  ## make a list of the plots
  if(length(detected_taxa)>1) {
    plist <- list(p_1)
    for(q in 2:length(detected_taxa)) {
      plist[[q]] <- get(paste0("p_",q))
    }
  } else if (length(detected_taxa)>0) {
    plist <- list(p_1)
  }
  
  write.csv(otu5, paste("ancom_",var1,"_",file_path,"_",taxonomic_level,".csv",sep=""))
  
  jpeg(h=800*1.25, w=1600*1.25, paste("ancom_",var1,"_",file_path,"_",taxonomic_level,".jpg",sep=""), units = "px", quality = 0.9)
  do.call("grid.arrange", c(plist))
  dev.off()
  
  if(return_plots == T) {
    return(plist)
  }
  
  if(return_tables == T) {
    for (i in 1:length(detected_taxa)) {
      
      rm(phyl6, phyl7, column_name)
      column_name <- detected_taxa[i]
      try(phyl6 <-phyl4 %>% mutate(rabun = get(column_name)/sum(otu_table[,2])) %>% dplyr::select(rabun, Group) %>% group_by(Group) %>% dplyr::summarize(mean=mean(rabun), sem=sd(rabun)/sqrt(length(rabun))),T)
      try(phyl7 <-phyl4 %>% mutate(rabun = get(column_name)/sum(otu_table[,2])) %>% dplyr::select(rabun, Group),T)
      if(exists("phyl6")==F) {
        try(phyl6 <- phyl4 %>% mutate(rabun = get(paste("X",column_name, sep=""))/sum(otu_table[,2])) %>% dplyr::select(rabun, Group) %>% group_by(Group) %>% dplyr::summarize(mean=mean(rabun), sem=sd(rabun)/sqrt(length(rabun))))
      }
      if(exists("phyl7")==F) {
        try(phyl7 <- phyl4 %>% mutate(rabun = get(paste("X",column_name, sep=""))/sum(otu_table[,2])) %>% dplyr::select(rabun, Group),T)
      }
      phyl6
      phyl7
      rm(t)
      for (t in 1:length(extra_cols2)) {
        otu5[i,paste0(extra_cols2[t])] <- phyl6 %>% filter(Group==extra_cols2[t]) %>% dplyr::select(mean) %>% unlist()
        otu5[i,paste0(extra_cols2[t],"_sem")] <- phyl6 %>% filter(Group==extra_cols2[t]) %>% dplyr::select(sem) %>% unlist()
      }
      
      assign(paste("table",i,sep="_"),value = phyl6)
      assign(paste("data",i,sep="_"),value = phyl7)
    }
    
    ## make a list of the plots
    if(length(detected_taxa)>1) {
      tablelist <- list(table_1)
      datalist <- list(data_1)
      for(q in 2:length(detected_taxa)) {
        tablelist[[q]] <- get(paste0("table_",q))
        datalist[[q]] <- get(paste0("data_",q))
      }
    } else if (length(detected_taxa)>0) {
      tablelist <- list(table_1)
      datalist <- list(data_1)
    }
    return(list(tablelist, datalist))
  }
}



