# functions ----------------------------------------------------------------------
testit <- function(x) {
  p1 <- proc.time()
  Sys.sleep(x)
  proc.time() - p1 # The cpu usage should be negligible
}
outliers_remove <- function(data) {
  lo_q = quantile(data)[2]
  up_q = quantile(data)[4]
# identify outliers
  up_out = (IQR(data) * 3) + up_q
  lo_out = lo_q - (IQR(data) * 3)
# remove outliers
  out <- which(data < up_out | data > lo_out)
  #print(out)
}
outliers_keep <- function(data) {
  lo_q = quantile(data)[2]
  up_q = quantile(data)[4]
  # identify outliers
  up_out = (IQR(data) * 3) + up_q
  lo_out = lo_q - (IQR(data) * 3)
  # remove outliers
  out <- which(data > up_out | data < lo_out)
  #print(out)
}
#
# libraries ----------------------------------------------------------------------
  library(ggplot2)
  #library(gganimate)
  #library(plotly)
  library(plyr)
  #library(magrittr)
  library(dplyr)
  library(tidyr)
  library(shiny)
  library(hexbin)
  library(ggpubr)
  library(DT)
  library(Hmisc)
  library(RColorBrewer)
  library(rlist)
  #library(rmarkdown)
  #library(knitr)
  library(kableExtra)
  library(shinycustomloader)
  library(shinycssloaders)
  #library(fpc)
  library(factoextra)
  library(FactoMineR)
  library(ggsci)
  library(ggalt)
  library(Rtsne)
# colors ----------------------------------------------------------------------
  #library(colourpicker)
  jet.colors <- colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan", "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"))
  angle.colors <- colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan", "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000", 
                                  "red", "#FF7F00", "yellow","#7FFF7F", "cyan", "#007FFF", "blue", "#00007F"))
# variables ----------------------------------------------------------------------
  sum_groups <- c("hpf", "group", "id")
  sum_vars <- c("ACIMajor", "ACIMinor", "MajorAngle", "Feret..unit.", "height..unit.", 
                 "Vol..unit.", "Surf..unit.", "Spher..unit.", "Comp..unit.",
                 "DCMean..unit.", "sav", "phi", "detect", "w.detect", "roset")
  data_groups <- c("M_", "ac_", "feret_")
# theme ----------------------------------------------------------------------
  theme <- theme_test() +
    theme(
      axis.title = element_text(size=15, face = "bold"),
      axis.ticks = element_line(size = .5, colour="black"),
      axis.ticks.length = unit(.1, "cm"),
      axis.text = element_text(size = 14, colour="black"),
      plot.title = element_text(size = 16, face="bold", colour="black"),
      panel.background = element_blank(),
      plot.background = element_rect(fill = "transparent", colour = NA),
      strip.background = element_blank(),
      strip.text = element_text(size=16, face = "bold"),
      legend.title = element_blank(),
      legend.text=element_text(size=12),
      legend.box.background = element_blank(),
      legend.background = element_blank(),
      legend.position = "right",
      legend.text.align = 0
      )# +
    #theme(legend.key.width = unit(1, "cm"))
# labels ----------------------------------------------------------------------
  # facets
    facet_stage <- list(
      '32_hpf'="32 hpf",
      '36_hpf'="36 hpf",
      '40_hpf'="40 hpf")
    facet_group <- list(
      'DMSO'="DMSO",
      'SU5402'="SU5402",
      'shrm3++'="shrm3++",
      'shrm3-- strong'="shrm3--")
    facet_stage_labeller <- function(variable,value){
      return(facet_stage[value])
    }
    facet_group_labeller <- function(variable,value){
      return(facet_group[value])
    }
    my_comparisons <- list(c("DMSO","SU5402"), c("DMSO","shrm3++"), c("shrm3++","shrm3-- strong"))
    mygroups = c("#E41A1C", "#377EB8", "#4DAF4A", "#96729B", "#9000A6")
  # date
    today <- Sys.Date()
# load data ----------------------------------------------------------------------
  load("data/ac.RData")
# define server ----------------------------------------------------------------------
  shinyServer(function(input, output, session) {
  ## reactive elements --------------
    ### dataset --------------
    datasetInput <- reactive({
      switch(input$dataset,
             "sample data" = all,
             "my data" = {
               validate(
                 need(input$data_upload != "", "No data to process, please upload")
               )
               #  load data
                  data_input <- input$data_upload
               #  empty filelist
                  fl <- list()
                  fl.names <- list()
               #  loop over data_groups
                  for (i in 1:length(data_groups)) {
                    fl.names[[i]] <- list.subset(data_input$name, grepl(pattern=data_groups[i], data_input$name)) # subset names based on data-group
                    fl[[i]] <- list.subset(data_input$datapath, grepl(pattern=data_groups[i], data_input$name)) # subset filepaths 'data[4]' based on file names
                  # 3D data
                    if (data_groups[i]=="M_") {
                      print("processing 3D data")
                      for (j in 1:length(fl.names[[i]])) {
                        dat <- read.table(fl[[i]][j], sep="\t", header=TRUE)
                      # delete columns + split label
                        dat$Nb <- NULL
                        dat$Type <- NULL
                        dat$X <- NULL
                      # filename
                        filename <- tools::file_path_sans_ext(fl.names[[i]][j]) # remove extension
                        filename <- sub("*M_", "", filename) # remove data-group id
                        #print(filename)
                        dat$Obj <- filename
                        dat <- dat %>% 
                          separate(Obj, c(input$lab_1, input$lab_2, input$lab_3, input$lab_4), "_")
                        dat$Label <- sub("-.*", "", dat$Label)
                        dat$obj <- 0:(nrow(dat)-1)
                        dat$clearid <- paste0(dat$stage, dat$group, dat$id, dat$date)
                        dat$clearobj <- paste0(dat$stage, dat$group, dat$id, dat$date, dat$obj)
                      # get scaling
                        Zcal <- dat$CZ..unit./dat$CZ..pix.                # To transform pix to unit value in Z
                        XYcal <- dat$CX..unit./dat$CX..pix.               # To transform pix to unit value in X and Y
                      # calculate variables
                        # b*l*h
                          dat$height..unit. <- (dat$Zmax..pix.-dat$Zmin..pix.)*Zcal  # calculate Height and transform to unit value
                          dat$length..unit. <- (dat$Xmax..pix.-dat$Xmin..pix.)*XYcal # calculate Length and transform to unit value
                          dat$width..unit. <- (dat$Ymax..pix.-dat$Ymin..pix.)*XYcal  # calculate Width and transform to unit value
                        # ratios
                          dat$sav <- dat$Surf..unit./dat$Vol..unit.           # calculate Surface Area to Volume ratio
                          dat$hf <- dat$height/dat$Feret..unit.               # calculate Height/Feret ratio
                          dat$phi <- ((atan(dat$height/dat$Feret..unit.))*180)/pi  # calculate Phi
                      # labels
                        # label trailing and leading
                          dat$cxn <- (max(dat$CX..pix.)-dat$CX..pix.)*XYcal   # normalize CX against leading edge
                          trail <- quantile(dat$cxn, 0.2, na.rm=TRUE)
                          dat$lt_split <- cut(dat$cxn, breaks = c(-Inf, trail, Inf), labels = c("leading", "trailing"))
                        # label midline
                          dat$ml_split <- cut(dat$CY..unit, seq(from=0, to=45, by=15), labels = c("lateral", "midline", "lateral"))
                      # transform pix to microns
                        dat$Zmax..pix. <- dat$Zmax..pix.*Zcal
                        dat$Zmin..pix. <- dat$Zmin..pix.*Zcal
                        dat <- plyr::rename(dat, c("Zmax..pix."="Zmax..unit.", "Zmin..pix."="Zmin..unit."))
                      # row bind
                        if (j > 1) {
                          temp <- rbind(temp, dat)
                        } else {
                          temp <- dat
                        }
                      }
                    }
                  # ac data
                    if (data_groups[i]=="ac_") {
                      print("processing AC data")
                      for (j in 1:length(fl.names[[i]])) {
                        dat <- read.table(fl[[i]][j], sep="\t", header=TRUE)
                      # delete columns
                        dat$objlabelArray <- NULL
                        dat$X <- NULL
                      # filename
                        filename <- tools::file_path_sans_ext(fl.names[[i]][j]) # remove extension
                        filename <- sub("*ac_", "", filename) # remove data-group id
                        dat$Obj <- filename
                        dat <- dat %>% 
                          separate(Obj, c(input$lab_1, input$lab_2, input$lab_3, input$lab_4), "_")
                        dat$obj <- 0:(nrow(dat)-1)
                        dat$clearid <- paste0(dat$stage, dat$group, dat$id, dat$date)
                        dat$clearobj <- paste0(dat$stage, dat$group, dat$id, dat$date, dat$obj)
                      # row bind
                        if (j > 1) {
                          temp <- rbind(temp, dat)
                        } else {
                          temp <- dat
                        }
                      }
                    }
                  # feret data
                    if (data_groups[i]=="feret_") {
                      print("processing feret data")
                      for (j in 1:length(fl.names[[i]])) {
                        dat <- read.table(fl[[i]][j], sep="\t", header=TRUE)
                      # delete columns
                        dat$objlabelArray <- NULL
                        dat$X <- NULL
                      # filename
                        filename <- tools::file_path_sans_ext(fl.names[[i]][j]) # remove extension
                        #print(filename)
                        filename <- sub("*feret_", "", filename) # remove data-group id
                        dat$Obj <- filename
                        dat <- dat %>% 
                          separate(Obj, c(input$lab_1, input$lab_2, input$lab_3, input$lab_4), "_")
                        dat$obj <- 0:(nrow(dat)-1)
                        dat$clearid <- paste0(dat$stage, dat$group, dat$id, dat$date)
                        dat$clearobj <- paste0(dat$stage, dat$group, dat$id, dat$date, dat$obj)
                      # normalize
                        dat$fx1N  <- max(dat$fx1array)-dat$fx1array   # normalize fx1 against leading edge
                        dat$fx0N  <- max(dat$fx0array)-dat$fx0array   # normalize fx0 against leading edge
                      # row bind
                        if (j > 1) {
                          temp <- rbind(temp, dat)
                        } else {
                          temp <- dat
                        }
                      }
                    }
                    id <- paste0(data_groups[i], "data")
                    assign(id, temp)
                  }
                  rm(i, id)
                # merge data
                  print("joining data")
                  data <- cbind(M_data, select_if(ac_data, is.numeric), select_if(feret_data, is.numeric))
                  data <- join(M_data, ac_data, by="clearobj")
                  data <- join(data, feret_data, by="clearobj")
                  data <- data[!duplicated(as.list(data))]
                # create factors
                  data$stage <- factor(data$stage)
                  data$group <- factor(data$group)
                  data$clearid <- factor(data$clearid)
                  print(paste("stages: ", nlevels(data$stage)))
                  print(paste("groups: ", nlevels(data$group)))
                  print(paste("ids: ", nlevels(data$clearid)))
                # sort data
                  data <- data %>%
                    select(which(sapply(.,is.character)),
                           which(sapply(.,is.factor)), 
                           everything())
                # merge major and minor aci
                  data$aci <- sqrt(data$ACIMajor*data$ACIMinor)
                # normalize to lateral height
                  data$aci <- data$Feret..unit./data$aci
                  data$ACIMajor <- data$Feret..unit./data$ACIMajor 
                  data$ACIMinor <- data$Feret..unit./data$ACIMajor
                # clean quantiles
                  data <- data %>%
                    group_by(stage, group) %>%
                    mutate(vol_out = isnt_out_tukey(Vol..unit.)) %>%
                    mutate(aci_out = isnt_out_tukey(aci))
                  data <- data %>%
                    filter(vol_out == "TRUE")
                  return(data)
             })
    })
    ### dataset.name --------------
    datasetname <- reactive({
      input$dataset
    })
    ### colour scale --------------
    colorscaleInput <- reactive({
      switch(input$colorscale,
             "jet.colors" = jet.colors(40),
             "angle.colors" = angle.colors(100))
    })
    ### stats --------------
    sumstatInput <- reactive({
      switch(input$sumstat,
             "mean" = function (x) {mean(x, na.rm=T)},
             "median" = function (x) {median(x, na.rm=T)},
             "min" = function (x) {min(x, na.rm=T)},
             "max" = function (x) {max(x, na.rm=T)})
    })
    hexstatInput <- reactive({
      switch(input$hexstat,
             "mean" = mean,
             "median" = median)
    })
    fitInput <- reactive({
      switch(input$fit,
             "lm" = lm,
             "glm" = glm,
             "loess" = loess)
    })
    ### dataset textoutput ------------
    output$datasetname <- renderText({
      datasetname()
    })
    ### ref groups reactive ui --------------
    output$refgroup <- reactiveUI(function(){
      data <- datasetInput()
      levels <- levels(data$group)
      selectInput("ref_group", "ref.group", levels)
    })
    ### factor groups reactive ui --------------
    output$grouplevels <- reactiveUI(function(){
      data <- datasetInput()
      nlevels <- nlevels(data$group)
      nstage <- nlevels(data$stage)
      nid <- nlevels(data$clearid)
      nobj <- nrow(data)
      paste("The dataset consists of", nstage, "stages,", nlevels, "groups,", nid, "embryos and", nobj, "single cells.")
    })
    
  ## inputs --------------
    observe({
      data <- datasetInput()
      cols_all <- names(Filter(is.numeric, data))
      cols <- Filter(function(x) !any(grepl("*X|*Y|*Z|*array|*fx|*cx|*Moment|
                                            *Ell|*Ratio|*l..pix|*f..pix|*r..pix", x)), cols_all)
      updateSelectInput(session, 'hex_var',
                        choices = cols
                        )
      updateSelectInput(session, 'dist_var',
                        choices = cols,
                        selected = cols[2]
      )
      updateSelectInput(session, 'statvariable',
                        choices = cols,
                        selected = cols[2]
      )
      })
    
  ## hexbins ----------------------------
  output$hexbins <- renderPlot({
    ## data to use
    if (input$dataset == "sample data"){
      data <- datasetInput()
    } 
    if (input$dataset == "my data"){
      validate(
        need(input$data_upload != "", "No data to process, please upload")
      )
      data <- datasetInput()
    }
    ## colors
      ggplot(data, aes_string(x=input$pointx, y=input$pointy, z=input$hex_var)) +
        #stat_density_2d(aes(fill=..density..), geom="raster", contour=FALSE) +
        #geom_rect(data = filter(all, group=="shrm3++"), 
        #          aes(xmin=0, ymin=0, xmax = quantile(cxn, probs = c(0.15)), ymax=max(fy1array)), show.legend = T, fill="grey80") +
        #geom_rect(data = filter(all, group=="shrm3-- strong"), 
                  #aes(xmin=0, ymin=min(fy1array), xmax = quantile(CXN, probs = c(0.15)), ymax=max(fy1array)), show.legend = F, fill="grey80") +
        #geom_rect(data = filter(all, group=="DMSO"), 
                  #aes(xmin=0, ymin=min(fy1array), xmax = quantile(CXN, probs = c(0.15)), ymax=max(fy1array)), show.legend = F, fill="grey80") +
        #geom_rect(data = filter(all, group=="SU5402"), 
                  #aes(xmin=0, ymin=min(fy1array), xmax = quantile(CXN, probs = c(0.15)), ymax=max(fy1array)), show.legend = F, fill="grey80") +
        stat_summary_hex(fun=hexstatInput(), binwidth = c(input$bin_adjust, input$bin_adjust)) +
        geom_vline(aes(xintercept = min(cxn), colour="L.E."), linetype=1, size=.5, show.legend = TRUE) +
        coord_fixed(xlim = c(200, 0)) +
        scale_fill_gradientn(colours = colorscaleInput(), name=input$colorscale) +
        scale_color_manual(name = "", values=c("L.E." = "blue", "trailing" = "black")) +
        #scale_linetype_manual(name = "t", values=c("leading edge" = 1, "trailing" = 2)) +
        scale_x_reverse(breaks = seq(20, 180, 30)) +
        scale_y_continuous(breaks = seq(10, 45, 15)) +
        facet_grid(group~stage) +
        labs(title = input$dataset, subtitle = today, x = input$pointx, y = input$pointy, caption = paste("haxagon size:")) +
        theme +
          theme(strip.text.y = element_text(size=12),
                legend.title = element_text(size=12))
    }
  )
  ## distributions ----------------------------
  output$frequencies <- renderPlot({
      ## select data
      if (input$dataset == "sample data"){
        data <- datasetInput()
      } 
      if (input$dataset == "my data"){
        validate(
          need(input$data_upload != "", "No data to process, please upload")
        )
        data <- datasetInput()
      }
    # levels & colors
      levels_group <- levels(data$group)
      nlevels_group <- nlevels(data$group)
      colors <- get_palette(palette = "npg", nlevels_group)
      # ggplot
        ggplot(data, aes_string(input$dist_var, colour="group")) +
        stat_density(aes(y=..density..), size=1.1, geom="line", bw=input$bw_adjust, position = "identity") +
        scale_colour_manual(values = colors, labels = levels_group, name="") +
        #scale_alpha_manual(values = c(0.8, 0.8, 0.8, 0.8, 0.8), guide = 'none') +
        labs(title = input$dataset, subtitle = today, x = input$dist_var, y = "Frequency", 
             caption = paste("binwith:",input$bw_adjust)) +
        facet_grid(.~stage, scales = "free") +
        theme +
          theme(strip.text.y = element_blank())
    }, height=300
  )
  # violins ----------------------------------------------------------------------
  output$violins <- renderPlot(
    {
      ## select data
      if (input$dataset == "sample data"){
        data <- datasetInput()
      } 
      if (input$dataset == "my data"){
        validate(
          need(input$data_upload != "", "No data to process, please upload")
        )
        data <- datasetInput()
      }
    # levels & colors
      levels_group <- levels(data$group)
      nlevels_group <- nlevels(data$group)
      #colors <- brewer.pal(nlevels_group, "Set1")
      colors <- get_palette(palette = "npg", nlevels_group)

    # ggplot
      ggplot(data, aes_string(x = "group", y = input$viol_var, fill="group")) +
        stat_ydensity(aes(), geom = "violin", adjust = .5, kernel = "gaussian",
                      trim = T, scale = "area", draw_quantiles = c(0.25, 0.5, 0.75)) +
        scale_fill_manual(values = colors, labels=levels_group, name="") +
        stat_compare_means(aes_string(label = input$statlabel),
                           method = input$stattestpair, 
                           ref.group = input$ref_group,
                           hide.ns = F, size = input$statlabelsize) +
        labs(title = input$dataset, subtitle = today, y = input$statvariable,
             caption = paste("quantiles", " = 0.25, 0.5, 0.75", "\ntest:", input$stattestpair)) +
        facet_grid(.~stage) +
        theme + 
          theme(axis.title.x = element_blank()) +
          theme(axis.ticks.x = element_blank()) +
          theme(axis.text.x = element_blank()) 
    }#, width=1000, height=250
  )
  # counts ----------------------------------------------------------------------
    output$counts <- renderPlot({
      ## select data
      if (input$dataset == "sample data"){
        data <- datasetInput()
      } 
      if (input$dataset == "my data"){
        validate(
          need(input$data_upload != "", "No data to process, please upload")
        )
        data <- datasetInput()
      }
      dataset <- ddply(data, .(stage, group, clearid), plyr::summarize,
                       prim_count = length(unique(clearid)),
                       cell_count = length(clearid),
                       cell_count_avg = round((length(clearid)/length(unique(clearid))), digits = 1)
      )
      # levels & colors
        levels_group <- levels(data$group)
        nlevels_group <- nlevels(data$group)
        colors <- get_palette(palette = "npg", nlevels_group)
      # ggplot
      ggplot(dataset, aes_string(x = "group", y = "cell_count")) +
      # geoms
        stat_boxplot(aes_string(group="group"), geom ='errorbar', width=.2, position=position_dodge(width=.75), show.legend = F) +
        geom_boxplot(aes_string(group="group"), notch = T, outlier.shape = NULL, show.legend = F) +
        geom_point(aes(group=group), position=position_jitter(width = .1, height = 0), colour="grey50", 
                   size = 3, shape=16, width = .1, height=.1, show.legend = F) +
        geom_boxplot(aes_string(group="group"), notch = T, outlier.shape = 4, outlier.size = 3, fill="transparent", show.legend = F) +
      # stats
        stat_summary(aes_string(colour="group"), geom = "crossbar", position = position_dodge(width=.75),
                     width =.4, size=2, fatten=0, 
                     fun.data = function(x){c(y=median(x), ymin=median(x), ymax=median(x))}) +
        stat_compare_means(aes_string(label = input$statlabel),
                           ref.group = input$ref_group,
                           method = input$stattestpair,
                           hide.ns = F, size = input$statlabelsize) +
        # scales
        #scale_colour_manual(values = colors, labels = levels_group, name="") +
        #scale_y_continuous(expand = c(0) +
        labs(title = input$dataset, subtitle = today, x = "groups", y = "cell count", 
             caption = paste("test:", input$stattestpair)) +
        facet_grid(.~stage, scales = "free") +
        # theme
        theme + 
        theme(axis.title.x = element_blank(),
              axis.text.x = element_blank(),
              axis.ticks.x = element_blank())
      
    }#, width=1000, height=250
    )
  # sum_stats ----------------------------------------------------------------------
  output$sum_stats <- renderPlot({
    ## select data
    if (input$dataset == "sample data"){
      data <- datasetInput()
    } 
    if (input$dataset == "my data"){
      validate(
        need(input$data_upload != "", "No data to process, please upload")
      )
      data <- datasetInput()
    }
    # check aci
    if (c("roset") %in% names(data)) {
      sum_set <- c("stage", "group", "clearid", 
                   "aci_major", "aci_minor", "aci_angle", "aci", "phi",
                   "Vol..unit.", "Surf..unit.", "sav", "Spher..unit.",
                   "Spher..unit.","Comp..unit.", "DCMean..unit.", "DCSD..unit.",
                   "detect", "w.detect", "roset")
    } else {
      sum_set <- c("stage", "group", "clearid", 
                   "ACIMajor", "ACIMinor", "MajorAngle", "aci", "phi",
                   "Vol..unit.", "Surf..unit.", "sav", "Spher..unit.",
                   "Spher..unit.", "Comp..unit.", "DCMean..unit.", "DCSD..unit.")
    }
    # levels & colors
      levels_group <- levels(data$group)
      nlevels_group <- nlevels(data$group)
      #colors <- brewer.pal(nlevels_group, "Set1")
      colors <- get_palette(palette = "npg", nlevels_group)
    # subset
      data <- data[ , sum_set]
    # summarize
      data_sum <- ddply(data, .(stage, group, clearid), colwise(sumstatInput()))
    # ggplot
      ggplot(data_sum, aes_string(x = "group", y = input$statvariable)) +
      # geoms
        stat_boxplot(aes_string(group="group"), geom ='errorbar', width=.2, position=position_dodge(width=.75), show.legend = F) +
        geom_boxplot(aes_string(group="group"), notch = T, outlier.shape = NULL, show.legend = F) +
        geom_point(aes(group="group"), position=position_jitter(width = .1, height = 0), colour="grey50", 
                  size = 3, shape=16, width = .1, height=.1, show.legend = F) +
        geom_boxplot(aes_string(group="group"), notch = T, outlier.shape = 4, outlier.size = 3, fill="transparent", show.legend = F) +
      # stats
        stat_summary(aes_string(colour="group"), geom = "crossbar", position = position_dodge(width=.75),
                    width =.4, size=2, fatten=0, 
                    fun.data = function(x){c(y=median(x), ymin=median(x), ymax=median(x))}) +
        stat_compare_means(aes_string(label = input$statlabel),
                           ref.group = input$ref_group,
                           method = input$stattestpair,
                           hide.ns = F, size = input$statlabelsize) +
      # scales
        scale_colour_manual(values = colors, labels = levels_group, name="") +
        #scale_y_continuous(expand = c(0) +
        labs(title = input$dataset, subtitle = today, x = "groups", y = paste(input$statvariable, input$sumstat), 
             caption = paste("test:", input$stattestpair)) +
        facet_grid(.~stage, scales = "free") +
      # theme
        theme + 
          theme(axis.title.x = element_blank(),
                axis.text.x = element_blank(),
                axis.ticks.x = element_blank())
          
    }#, width=1000, height=250
  )
    # correlations ----------------------------------------------------------------------
    output$correlations <- renderPlot({
      ## select data
      if (input$dataset == "sample data"){
        data <- datasetInput()
      } 
      if (input$dataset == "my data"){
        validate(
          need(input$data_upload != "", "No data to process, please upload")
        )
        data <- datasetInput()
        data <- read.table(data$datapath, sep = "\t", header=T)
      }
      # check stages
      if (c("hpf") %in% names(data)) {
      } else {
        data$hpf <- "undefined stage"
      }
      # check aci
      if (c("roset") %in% names(data)) {
        sum_set <- c("stage", "group", "clearid", 
                     "aci_major", "aci_minor", "aci_angle", "aci", "phi",
                     "Vol..unit.", "Surf..unit.", "Spher..unit.", "Comp..unit.",
                     "Spher..unit.","Comp..unit.", "DCMean..unit.", "DCSD..unit.",
                     "detect", "w.detect", "roset")
      } else {
        sum_set <- c("stage", "group", "clearid", 
                     "ACIMajor", "ACIMinor", "MajorAngle", "aci", "phi",
                     "Feret..unit.", "height..unit.", "Vol..unit.", "Surf..unit.", "sav", 
                     "Spher..unit.", "Comp..unit.", "DCMean..unit.", "DCSD..unit.")
      }
    # levels & colors
      levels_group <- levels(data$group)
      nlevels_group <- nlevels(data$group)
      #colors <- brewer.pal(nlevels_group, "Set1")
      colors <- get_palette(palette = "npg", nlevels_group)
    # subset
      data <- data[ , sum_set]
    # summarize
      data_sum <- ddply(data, .(hpf, group, group, clearid), colwise(sumstatInput()))
      data_cor <- ddply(data, .(hpf, group), summarise, 
                        pearson = cor(ACIMajor, roset, use = "complete.obs", method = c("pearson")),
                        kendall = cor(ACIMajor, roset, use = "complete.obs", method = c("kendall")),
                        spearman = cor(ACIMajor, roset, use = "complete.obs", method = c("spearman"))
      )

    # ggplot
      ggplot(data_cor, aes_string(x = "hpf", y = input$corstat)) +
      # geoms
        geom_hline(aes(yintercept = 0), linetype = 2, size=1) +
        geom_col(aes_string(fill="group"), colour="black", position = position_dodge(), width=.5) +
      # scales
        scale_fill_manual(values = colors, labels = levels_group, name="") +
        labs(title = input$dataset, subtitle = today, x = "group", y = paste(input$corstat, "coefficient"), 
             caption = paste("function:", input$stat)) +
      # theme
        theme + 
          theme(axis.title.x = element_blank(),
                axis.ticks.x = element_blank())
      
    }#, width=1000, height=250
    )
    # scatterplot ----------------------------------------------------------------------
    output$scatter <- renderPlot({
    ## select data
      if (input$dataset == "sample data"){
        data <- datasetInput()
      } 
      if (input$dataset == "my data"){
        validate(
          need(input$data_upload != "", "No data to process, please upload")
        )
        data <- datasetInput()
      }
    # check aci
      if (c("roset") %in% names(data)) {
        sum_set <- c("stage", "group", "clearid", 
                     "aci_major", "aci_minor", "aci_angle", "aci", "phi",
                     "Vol..unit.", "Surf..unit.", "sav", "Spher..unit.",
                     "Spher..unit.","Comp..unit.", "DCMean..unit.", "DCSD..unit.",
                     "detect", "w.detect", "roset")
      } else {
        sum_set <- c("stage", "group", "clearid", 
                     "ACIMajor", "ACIMinor", "MajorAngle", "aci", "phi",
                     "Vol..unit.", "Surf..unit.", "sav", "Spher..unit.",
                     "Spher..unit.", "Comp..unit.", "DCMean..unit.", "DCSD..unit.")
      }
    # levels & colors
      levels_group <- levels(data$group)
      nlevels_group <- nlevels(data$group)
      #colors <- brewer.pal(nlevels_group, "npg")
      colors <- get_palette(palette = "npg", nlevels_group)
    # subset
      data <- data[ , sum_set]
    # summarize
      data_sum <- ddply(data, .(stage, group, clearid), colwise(sumstatInput()))
      data_sum <- subset(data_sum, !roset==0)
    # ggplot
      ggplot(data_sum, aes_string(x = input$scatvar1, y = input$scatvar2)) +
      # geoms
        stat_ellipse(aes_string(colour="group", fill="group"), size=.5, level = 0.9, geom = "polygon", alpha=.1) +
        geom_point(aes_string(colour="group"), size = 3, alpha = .5, shape = 16) +
        geom_smooth(aes_string(), method = input$fitstat, span = input$scat_span, se=F, fullrange=T) +
      # scales
        scale_colour_manual(values = colors, labels = levels_group, name="") +
        scale_fill_manual(values = colors, labels = levels_group, name="") +
        labs(title = input$dataset, subtitle = today, x = input$scatvar1, y = input$scatvar2, 
             caption = paste("fit function:", input$fitstat, "\nsum function:", input$sumstat)) +
        #facet_grid(.~hpf, scales = "free") +
        theme +
          theme(axis.ticks.x = element_blank())
    })# width=1000, height=250
    # pca ----------------------------------------------------------------------
    output$pca <- renderPlot({
    ## select data
      if (input$dataset == "sample data"){
        data <- datasetInput()
      } 
      if (input$dataset == "my data"){
        validate(
          need(input$data_upload != "", "No data to process, please upload")
        )
        data <- datasetInput()
      }
    # check aci
      if (c("roset") %in% names(data)) {
        sum_set <- c("stage", "group", "clearid", 
                     "aci_major", "aci_minor", "aci_angle", "aci", "phi",
                     "Vol..unit.", "Surf..unit.", "sav", "Spher..unit.",
                     "Spher..unit.","Comp..unit.", "DCMean..unit.", "DCSD..unit.",
                     "detect", "w.detect", "roset")
      } else {
        sum_set <- c("stage", "group", "clearid", 
                     "ACIMajor", "ACIMinor", "MajorAngle", "aci", "phi",
                     "Vol..unit.", "Surf..unit.", "sav", "Spher..unit.",
                     "Spher..unit.", "Comp..unit.", "DCMean..unit.", "DCSD..unit.")
      }
    # levels & colors
      levels_group <- levels(data$group)
      nlevels_group <- nlevels(data$group)
      #colors <- brewer.pal(nlevels_group, "Set1")
      colors <- get_palette(palette = "npg", nlevels_group)
    # subset
      data <- data[ , sum_set]
      # only complete cases
        data <- data[complete.cases(data), ]
        data <- data[, colSums(is.na(data)) != nrow(data)]
    # summarize
      #sumstat <- sumstatInput()
      data_sum <- ddply(data, .(stage, group, clearid), colwise(sumstatInput()))
    # select only variables
      data_vars <- dplyr::select_if(data_sum, is.numeric)
      data.pca <- PCA(data_vars, graph = FALSE)
    # ggplot
      pca_plot <- reactive({
        switch(input$complabel,
               "variables" = fviz_pca_biplot(data.pca,
                                          title = input$dataset, subtitle = today, caption = "ellipse level = 0.95",
                                          pointsize = 3, pointshape = 16, 
                                          addEllipses = TRUE, ellipse.level = 0.95,
                                          label="var", repel=T, col.var = "black", labelsize = 5,
                                          habillage = data_sum$group, palette = colors,
                                          ggtheme = theme),
               "show none" = fviz_pca_ind(data.pca,
                                     title = input$dataset, subtitle = today, caption = "ellipse level = 0.95",
                                     pointsize = 3, pointshape = 16, alpha.ind = .5,
                                     addEllipses = TRUE, ellipse.level = 0.95, label="none",
                                     habillage = data_sum$group, palette = colors,
                                     ggtheme = theme)
               )
      })
      pca_plot()
    }#, width=1000, height=250
    )
    
  # tSNE ----------------------------------------------------------------------
    output$tsne <- renderPlot({
    ## select data
      if (input$dataset == "sample data"){
        data <- datasetInput()
      } 
      if (input$dataset == "my data"){
        validate(
          need(input$data_upload != "", "No data to process, please upload")
        )
        data <- datasetInput()
      }
    # check aci
      if (c("roset") %in% names(data)) {
        sum_set <- c("stage", "group", "clearid", 
                     "aci_major", "aci_minor", "aci_angle", "aci", "phi",
                     "Vol..unit.", "Surf..unit.", "sav", "Spher..unit.",
                     "Spher..unit.","Comp..unit.", "DCMean..unit.", "DCSD..unit.",
                     "detect", "w.detect", "roset")
      } else {
        sum_set <- c("stage", "group", "clearid", 
                     "ACIMajor", "ACIMinor", "MajorAngle", "aci", "phi",
                     "Vol..unit.", "Surf..unit.", "sav", "Spher..unit.",
                     "Spher..unit.", "Comp..unit.", "DCMean..unit.", "DCSD..unit.")
      }
    # levels & colors
      levels_group <- levels(data$group)
      nlevels_group <- nlevels(data$group)
      data$clearid <- factor(data$clearid)
      nlevels_id <- nlevels(data$clearid)
      print(paste("id levels", nlevels_id))
      print(paste("id levels", (nlevels_id/nlevels_group)/2))
      #browser()
      perplexity <- (nlevels_id/nlevels_group)/3
      #colors <- brewer.pal(nlevels_group, "Set1")
      colors <- get_palette(palette = "npg", nlevels_group)
    # subset
      data <- data[complete.cases(data), ]
      data <- data[ , sum_set]
    # sumamrize
      data_sum <- ddply(data, .(stage, group, clearid), colwise(sumstatInput()))
    # prepare df
      #data <- data[complete.cases(data), ]
      data_vars <- dplyr::select_if(data_sum, is.numeric)
    # calculate tSNE
      data.tsne <- Rtsne(as.matrix(data_vars),
                         initial_dims = nlevels_group,
                         check_duplicates = FALSE,
                         normalize = T, pca = TRUE, 
                         perplexity = perplexity, verbose = T,
                         num_threads = 0, dims = 2)
    # get dataframe
      data.tsne.dims = as.data.frame(data.tsne$Y)
    # create labels
      data.tsne.dims$group <- data_sum$group
      data.tsne.dims$hpf <- data_sum$stage
    # ggplot
      ggplot(data.tsne.dims, aes(x=V1, y=V2)) +  
        #geom_density2d() +
        stat_ellipse(aes(colour=group, fill=group), size=.5, level = 0.95, geom = "polygon", alpha=.1) +
        geom_point(aes(colour=group), size=2, shape=16, alpha=.75) +
        scale_colour_manual(values = colors, name="") +
        scale_fill_manual(values = colors, name="") +
        #scale_colour_brewer(palette = "Set1") +
        labs(title=input$dataset, subtitle = today, caption = paste("ellipse level = 0.95\nperplexity =", perplexity)) +
        facet_grid(.~hpf) +
        theme
      
    }#, width=1000, height=250
    )
  # sum_table ----------------------------------------------------------------------
  output$data_sum <- function() {
    if (input$dataset == "sample data"){
      data <- datasetInput()
    } 
    if (input$dataset == "my data"){
      validate(
        need(input$data_upload != "", "No data to process, please upload")
      )
      data <- datasetInput()
      #data <- read.table(data$datapath, sep = "\t", header=T)
    }
    dataset <- ddply(data, .(stage, group), plyr::summarize,
                     prim_count = length(unique(clearid)),
                     cell_count = length(clearid),
                     cell_count_avg = round((length(clearid)/length(unique(clearid))), digits = 1)
                     )
    cells_ratio <- paste0("cells ratio", footnote_marker_symbol(1))
    knitr::kable(dataset, escape = F, col.names = c("stage", "groups", "prims", "cells", cells_ratio)) %>%
      kable_styling(bootstrap_options = c("hover", "condensed", "responsive"), full_width = T) %>%
      add_header_above(c("ID labels" = 2, "Counts" = 3)) %>%
      collapse_rows(columns = 1:2, valign = "top") %>%
      footnote(general = "",
               symbol = " = cells / prims")
    }
  # download data ----------------------------------------------------------------------
  output$downloadData <- downloadHandler(
    filename = function(){
      paste("all", "csv", sep=".")
    },
  # fill with content
    content = function(file) {
      data <- datasetInput()
      write.csv(data, file)
    }
  )
  # download plot ----------------------------------------------------------------------
  output$downloadPlot <- downloadHandler(
    filename = function(){
      paste("hexbins", "pdf", sep=".")
    },
    # fill with content
    content = function(file) {
      data <- datasetInput()
      p <- ggplot(data, aes_string(x=input$pointx, y=input$pointy, z=input$hex_var)) +
        stat_summary_hex(fun=hexstatInput(), binwidth = c(input$bin_adjust, input$bin_adjust)) +
        geom_vline(aes(xintercept = min(cxn), colour="L.E."), linetype=1, size=.5, show.legend = TRUE) +
        coord_fixed() +
        scale_fill_gradientn(colours = colorscaleInput(), name=input$colorscale) +
        scale_color_manual(name = "", values=c("L.E." = "blue", "trailing" = "black")) +
        scale_x_reverse(limits = c(200, 0), breaks = seq(20, 180, 30)) +
        scale_y_continuous(breaks = seq(10, 45, 15)) +
        facet_grid(group~stage) +
        labs(title = "", x = input$pointx, y = input$pointy) +
        theme +
          theme(strip.text.y = element_text(size=12),
                legend.title = element_text(size=12))
      ggsave(file, p, device="pdf", dpi="retina", width = 12, height = 5)
    }
  )
  # images ----------------------------------------------------------------------
  output$pllps <- renderImage({
    return(list(
      src = "pllp_3d.png",
      contentType = "image/png"
    ))
  }, deleteFile = FALSE)
  output$cell <- renderImage({
    return(list(
      src = "cell.png",
      contentType = "image/png"
    ))
  }, deleteFile = FALSE)
  output$scale <- renderImage({
    return(list(
      src = "scales.png",
      contentType = "image/png"
    ))
  }, deleteFile = FALSE)
  output$stack <- renderImage({
    return(list(
      src = "stack.gif",
      contentType = "image/gif"
    ))
  }, deleteFile = FALSE)
}
)
