### Main Shiny server function

shinyServer(function(input, output, session) {

    values <- reactiveValues(selected=TRUE,  ## Current data subset selection
                             anglemax=0)

    ## Things to do when changing the base dataset
    observe({
        input$select
    })

    ## Button handler: set random effects SE to zero
    observe({
        if (input$zeroresd > 0 || input$egger){
            updateNumericInput(session, "resd", value=0)
        }
    })

    ## Button handler: reset random effects SE to data value
    observe({
        if (input$resetresd > 0) {
            dat <- getdata_static()
            updateNumericInput(session, "resd", value=resd_default(dat))
        }
    })

    ## Return the full dataset currently chosen
    getdata_base <- reactive({
        globals$newdata <<- NULL # note global assignment <<-
        dat <- switch(input$select,
                      "1"=magnesium,
                      "2"=catheter,
                      "3"=aspirin,
                      "4"=symmetric,
                      "5"={ ## user-uploaded dataset
            output$filechoiceui <- renderUI({ list(
                                                  checkboxInput('header', 'Header', TRUE),
                                                  radioButtons('sep', 'Separator',
                                                               c(Comma=',', Semicolon=';', Tab='\t'),
                                                               selected=',', inline=TRUE
                                                               ),
                                                  radioButtons('quote', 'Quote',
                                                               c(None='', 'Double Quote'='"', 'Single Quote'="'"),
                                                               selected='"', inline=TRUE)
                                              ) })
            inFile <- input$data
            validate( need(inFile != "", "Please select a dataset") ) ## needed to avoid function returning before data chosen
            if (is.null(inFile)) userdata <- NULL
            else userdata <- read.csv(inFile$datapath, header = input$header,
                                      sep = input$sep, quote = input$quote)
            ## Silence if error occurs - but maybe that's OK - can switch to another data set.
            validate(need(ncol(userdata)>=3, "Need at least 3 columns in the data"))
            names(userdata)[1:3] <- c("name","est","se")
            validate(need(is.numeric(userdata$est), "Second column (estimate) should be numeric"))
            validate(need(is.numeric(userdata$se), "Second column (SE) should be numeric"))
            userdata
        },
        "6" = {
            if (exists("userdata", envir = MetaAnalyser:::.dat_env))
                get("userdata", envir = MetaAnalyser:::.dat_env) else NULL
        }
        )
        
        dat
    })

    ## Add user-added studies to base data, append meta-analysis summary statistics
    getdata_static <- reactive({
        dat <- getdata_base()
        get_newstudy <- reactive({
            if (input$submitnewdata == 0) return(NULL)
            isolate({
                res <- data.frame(name=input$newname, est=input$newest, se=input$newse)
                if (is.null(input$newest) || is.null(input$newse) ||
                    is.na(res$est) || is.na(res$se)) res <- NULL
                res
            })
        })
        globals$newdata <<- rbind(globals$newdata, get_newstudy())  # note global assignment <<-
        if (!is.null(globals$newdata)){
            dat <- rbind(dat, globals$newdata)
            updateNumericInput(session, "newname", value="")
            updateNumericInput(session, "newest", value="")
            updateNumericInput(session, "newse", value="")
        }
        n <- nrow(dat)
        dat$key <- 1:n
        dat$wtfe <- (1/dat$se^2)
        dat$pwtfe <- dat$wtfe / sum(1/dat$se^2)
        resd <- if (input$egger) 0 else resd_default(dat)
        updateNumericInput(session, "resd", value=resd)
        sm <- metasumm(dat, resd, input$egger)
        dat$est <- sm$est
        dat$pwtre <- sm$pwtre
        attr(dat, "pool") <- sm$pool
        globals$pool <<- sm$pool
        attr(dat, "poolse") <- sm$poolse
        attr(dat, "poolci") <- sm$poolci

        values$selected <- rep(TRUE, n)
#        proxy = dataTableProxy('datatables')
#        selectRows(proxy, numeric()) # this doesn't work at resetting selection when changing data

        attr(dat, "plotkeys") <- rep(c("points","strings","topbar","baseline_vertical","baseline_horiz","current_vertical","current_horiz","pivot","baseline_point","current_point"),
                                     c(n, n, 2, 2, 2, 2, 2, 3, 1, 1))
        dat
    })

    ## Update the study-selection indicator in the data after clicking,
    ## and recompute meta-analysis summary statistics
    ## TODO nicer dplyr like syntax for defining data

    getdata_dynamic <- reactive({
        dat <- getdata_static()
        n <- nrow(dat)

        dat$selected <- factor(rep("No", n), levels=c("Yes","No")) # ggvis prefers grouping variable to be factor
        dat$selected[values$selected] <- "Yes"
        summ <- metasumm(dat[values$selected,,drop=FALSE], input$resd, input$egger)
        dat$est[values$selected] <- summ$est
        dat$wtfe <- 1/(dat$se^2)
        dat$wtre <- 1/(dat$se^2 + input$resd^2)
        dat$holesize <- 100*(dat$wtfe - dat$wtre)/sum(dat$wtfe)

        dat$percwtfe <- 100*dat$wtfe/sum(dat$wtfe)
        dat$percwtre <- 100*dat$wtre/sum(dat$wtre)

        dat$yfe <- switch(input$ytype, perc=dat$percwtfe, var=dat$wtfe)
        dat$yre <- switch(input$ytype, perc=dat$percwtre, var=dat$wtre)

        aspect_ratio <- default_options()$width / default_options()$height
        if (is.null(input$yrange)) {top <- 100; bottom <- 0}
        else {
            top <- input$yrange[2]
            bottom <- input$yrange[1] - (top - input$yrange[1])/40
        }
        bottomdyn <- bottom # + 0.01*(top-bottom)

        xex <- 0.1
        dxr <- diff(range(dat$est))
        xrange <- range(dat$est) + c(-1, 1)*xex*dxr
        yrange <- c(0, top)
        ## TODO choose default point size more intelligently
        sqsize <- dxr / 100 * input$pointsize
        dat$dx <- sqsize*sqrt(dat$wtfe)
        dat$dy <- dat$dx/diff(xrange)*diff(yrange)*aspect_ratio
        dat$dxre <- sqsize*sqrt(dat$wtfe - dat$wtre)
        dat$dyre <- dat$dxre/diff(xrange)*diff(yrange)*aspect_ratio

        pool.static <- attr(dat, "pool")
        pool.dyn <- summ$pool
        is_biased <- if (!isTRUE(all.equal(input$resd, resd_default(dat))) ||
                         !all(values$selected)) "Yes" else "No"

        ### Tilt the top bar proportional to bias, up to certain maximum angle
        bias <- (globals$pool - pool.dyn) / (dxr/2)
        degrees_tilt <- sign(bias * values$anglemax) * min(values$anglemax, abs(bias * values$anglemax))
        globals$pool <<- pool.dyn
        angle <- degrees_tilt * pi/180
        hl <- pool.dyn - min(dat$est)
        hr <- max(dat$est) - pool.dyn
        xltilt <- pool.dyn - hl*cos(angle)
        xrtilt <- pool.dyn + hr*cos(angle)
        yr <- top; xr <- dxr # adjust for differently-scaled x/y axes
        ybtilt <- top - hl*sin(angle)*yr/xr
        yttilt <- top + hr*sin(angle)*yr/xr
        dat$stringtop <- top + (dat$est - pool.dyn)*sin(angle)*yr/xr
        dat$estplot <- dat$est + (pool.dyn - dat$est)*(1 - cos(angle)) # plotted x position, different from actual estimate if bar is tilted
        vtop <- top + (pool.static - pool.dyn)*sin(angle)*yr/xr

        pivot.dx <- dxr / 25
        pivot.dy <- 0.04*top
        pivot.top <- 0.99*top

        ## Append plot coordinates to pass to ggvis, based on selected data

        auxdata <- list(
            ## List of data frames to make into reactive data sources
            topbar = data.frame(x = c(xltilt, xrtilt), y = c(ybtilt, yttilt), key=2*n+1:2),
            baseline_vertical = data.frame(x = rep(pool.static, 2), y = c(bottom, vtop-pivot.dy), key=2*n+3:4, is_biased=is_biased),
            baseline_horiz = data.frame(x = attr(dat, "poolci"),
                                  y = rep(bottom, 2), key=2*n+5:6, is_biased=is_biased),
            current_vertical = data.frame(x = rep(pool.dyn, 2), y = c(bottomdyn, top-pivot.dy), key=2*n+7:8),
            current_horiz= data.frame(x = summ$poolci,
                              y = rep(bottomdyn, 2), key=2*n+9:10),
            current_pivot = data.frame(x=c(pool.dyn, pool.dyn-pivot.dx/2, pool.dyn+pivot.dx/2),
                               y=c(pivot.top, pivot.top-pivot.dy, pivot.top-pivot.dy), key=2*n+11:13),
            baseline_pivot = data.frame(x=c(pool.static, pool.static-pivot.dx/2, pool.static+pivot.dx/2),
                               y=c(pivot.top, pivot.top-pivot.dy, pivot.top-pivot.dy), key=2*n+11:13, is_biased=is_biased),
            baseline_point = data.frame(x=pool.static, y=bottom, key=2*n+14),
            current_point = data.frame(x=pool.dyn, y=bottom, key=2*n+15)
        )
        for (i in seq(along=auxdata))
            auxdata[[i]]$show_scales <- if (input$show_scales) "Yes" else "No"

        attr(dat, "summ") <- summ
        attr(dat, "aux") <- c(list(xrange=xrange), auxdata)

        dat
    })

    ## Reactive data sources to enable animation in ggvis
    getdata_string <- function(i){
        reactive({
            dat <- getdata_dynamic()
            n <- nrow(dat)
            dat <- dat[i,,drop=FALSE]
            ret <- data.frame(x=rep(dat$estplot, 2), y=c(dat$stringtop, dat$yre+dat$dy),
                              selected=rep(dat$selected, 2), key=n + 1:2)
            if (!input$show_scales) ret$selected <- "No"
            ret
        })
    }

    getdata_allstrings <- reactive({
        dat <- getdata_dynamic()
        res <- data.frame(x=rep(dat$estplot, each=2),
                   y=as.numeric(rbind(dat$stringtop, dat$yre+dat$dx)),
                   selected=rep(dat$selected, each=2),
                   key=nrow(dat) + 1:(2*nrow(dat)), # TODO rejig keys
                   group=rep(1:nrow(dat), each=2)) %>% group_by(group)
        res
    })

    getdata_topbar <- reactive({
        dat <- getdata_dynamic()
        ret <- attr(dat, "aux")$topbar
        ret
    })
    getdata_baseline_vertical <- reactive({
        dat <- getdata_dynamic()
        ret <- attr(dat, "aux")$baseline_vertical
        ret$show_scales <- if (input$show_scales) "Yes" else "No"
        if (!input$show_scales) ret$is_biased <- "Noscales"
        ret
    })
    getdata_baseline_horiz <- reactive({
        dat <- getdata_dynamic()
        ret <- attr(dat, "aux")$baseline_horiz
        ret
    })
    getdata_current_vertical <- reactive({
        dat <- getdata_dynamic()
        ret <- attr(dat, "aux")$current_vertical
        ret
    })
    getdata_current_horiz <- reactive({
        dat <- getdata_dynamic()
        ret <- attr(dat, "aux")$current_horiz
        ret
    })
    getdata_baseline_pivot <- reactive({
        dat <- getdata_dynamic()
        ret <- attr(dat, "aux")$baseline_pivot
        ret$show_scales <- if (input$show_scales) "Yes" else "No"
        if (!input$show_scales) ret$is_biased <- "Noscales"
        ret
    })
    getdata_current_pivot <- reactive({
        dat <- getdata_dynamic()
        ret <- attr(dat, "aux")$current_pivot
        ret
    })
    getdata_baseline_point <- reactive({
        dat <- getdata_dynamic()
        ret <- attr(dat, "aux")$baseline_point
        ret
    })
    getdata_current_point <- reactive({
        dat <- getdata_dynamic()
        ret <- attr(dat, "aux")$current_point
        ret
    })
    ## fix the axis ranges so they don't wiggle when dataset is perturbed
    getdata_xdomain <- reactive({
        dat <- getdata_dynamic()
        ret <- attr(dat, "aux")$xrange
        ret
    })
    getdata_ydomain <- reactive({
        input$yrange
    })
    getdata_holescale <- reactive({
        dat <- getdata_dynamic()
        c(0, max(dat$yfe))
    })

    ## Change the limits of the y-axis range selector according to
    ## whether percentage weights or absolute weights selected
    observe({
        if (input$ytype == "perc"){
            ymin <- 0; ymax <- 100
            lab <- "y-axis range (percentage weight)"
        } else {
            dat <- getdata_static()
            yex <- 0.1
            ymax <- max(dat$wtfe)*(1 + yex)
            ymin <- 0
            lab <- "y-axis range (inverse variance)"
        }
        output$yslider <- renderUI({
            sliderInput("yrange", label = h4(lab),
                        min = ymin, max = ymax, value = c(ymin, ymax))
        })
    })

    ## Print the data table, with excluded studies greyed out, and other summary info
    observe({
        dat <- getdata_dynamic()
        sel <- (dat$selected=="Yes")
        summ <- attr(dat, "summ")
        Isq <- summ$Isq
        digits <- 3 # let user choose?
        if (sum(sel) > 0) {
            dat <- dat[,c("name","est","se","wtre","wtfe","percwtre","percwtfe")]
            dat$name <- as.character(dat$name)
            for (i in c("est","se","wtre","wtfe","percwtre","percwtfe"))
                dat[,i] <- formatC(dat[,i], digits=digits, format="f")
            names(dat) <- c("Name", "Estimate", "SE",
                            "Inverse variance", "Inverse variance (fixed effects)",
                            "Weight (%)", "Weight (%, fixed effects)")
            ## Table responds to clicks on plot - but this requires
            ## devel version of DT >=0.1.16 on github not CRAN.

            ## Plot doesn't respond to clicks on table
            ## below line breaks both things
            ## todo MRE with mtcars

            ## FIXME if exclude in one dataset, should reset selection when changing data
            ## difficult to reproduce this
            
            ## isolate(values$selected <- !(1:nrow(dat) %in% input$datatables_rows_selected))

            output$datatables =  DT::renderDataTable(dat, server = (nrow(dat)>100))
            proxy = dataTableProxy('datatables')
            selectRows(proxy, which(!sel))

            output$Isq = renderUI({
                div(paste("I-squared =",round(100*Isq,1),"%"))
            })
            output$pooled <- renderUI({
                div(paste("Pooled estimate = ", round(summ$pool, digits=digits), " (",
                          round(summ$poolci[1], digits), ", ",
                          round(summ$poolci[2], digits), ")", sep=""))
            })
        }
        output$invisible <- renderUI({
            conditionalPanel(condition="false",
                             ## used for animated tilt/untilt
                             checkboxInput("tilt", label=NULL, value = FALSE)
                             )
        })
    })

    ## Combine reactive value and hidden input to achieve animated tilt/untilt - unclear why need both.
    observe({
        if (isTRUE(input$tilt)) {
            invalidateLater(1000, session)
        }
        updateCheckboxInput(session, "tilt", value=FALSE)
        values$anglemax <- 0
    })

    hover_fn <- function(df){
        alldata <- isolate(getdata_static())
        keys <- attr(alldata, "plotkeys")
        if (df$key %in% which(keys=="points")){
            dat <- alldata[alldata$key == df$key,]
            sprintf("%s, estimate %s<br>Click to include / exclude", dat$name, round(as.numeric(dat$est), 3))
        }
                                        #else if (df$key %in% which(keys=="strings"))
                                        #else if (df$key %in% which(keys=="topbar"))
        else if (df$key %in% which(keys=="baseline_vertical"))
            sprintf("\"Baseline\" pooled estimate")
        else if (df$key %in% which(keys=="baseline_horiz"))
            sprintf("95%% confidence interval for \"baseline\" pooled estimate")
        else if (df$key %in% which(keys=="current_vertical") && input$show_scales)
            sprintf("\"Current\" pooled estimate")
        else if (df$key %in% which(keys=="current_horiz"))
            sprintf("95%% confidence interval for \"current\" pooled estimate")
        else if (df$key %in% which(keys=="topbar") && input$show_scales)
            sprintf("Top bar spans the range of study results")
    }

    click_fn <- function(data, location, session) {
        alldata <- isolate(getdata_static())
        keys <- attr(alldata, "plotkeys")
        if (data$key %in% which(keys=="points")){
            isolate(values$selected[data$key] <- !values$selected[data$key])
            isolate(values$anglemax <- 20)
            updateNumericInput(session, "resd", value = resd_default(alldata[values$selected,]))
            updateCheckboxInput(session, "tilt", value=TRUE)
        }
    }

    ## Draw the actual plot.
    p <- ggvis(x=~estplot, y=~yre, key:=~key, data=getdata_dynamic) %>%
      scale_numeric("x", domain = getdata_xdomain)  %>%
      scale_numeric("y", domain = getdata_ydomain)

    bias_fill <- prop("fill", ~ is_biased, scale="scale_biaspoolcolor")
    bias_stroke <- prop("stroke", ~ is_biased, scale="scale_biaspoolcolor")
    show_scales_stroke <- prop("stroke", ~ show_scales, scale="scale_showscalescolor")
    show_scales_fill <- prop("fill", ~ show_scales, scale="scale_showscalescolor")
    show_scales_strokewidth <- prop("strokeWidth", ~ show_scales, scale="scale_pooledstroke")
    p <- p %>%
      ## add original estimate and CI in gray
      layer_paths(x = ~x, y = ~y, data=getdata_baseline_vertical, strokeWidth:=3, bias_stroke) %>%
      layer_paths(x = ~x, y = ~y, data=getdata_baseline_horiz, show_scales_strokewidth, bias_stroke) %>%
      ## current estimate in black
      layer_paths(x = ~x, y = ~y, data=getdata_current_vertical, strokeWidth:=3, show_scales_stroke) %>%
      layer_paths(x = ~x, y = ~y, data=getdata_current_horiz, show_scales_strokewidth) %>%
      ## top bar
      layer_paths(x = ~x, y = ~y, data=getdata_topbar, strokeWidth:=6, show_scales_stroke) %>%
      layer_paths(x = ~x, y = ~y, data=getdata_baseline_pivot, bias_fill, bias_stroke) %>%
      layer_paths(x = ~x, y = ~y, data=getdata_current_pivot, show_scales_fill, show_scales_stroke) %>%
      layer_points(x = ~x, y = ~y, data=getdata_baseline_point, fill:="grey") %>%
      layer_points(x = ~x, y = ~y, data=getdata_current_point, fill:="black") %>%
      scale_nominal("scale_pooledstroke", domain = c("Yes", "No"), range = c(10, 3)) %>%
      scale_nominal("scale_showscalescolor", domain = c("Yes", "No"), range = c("black", "white")) %>%
      scale_nominal("scale_stringcolor", domain = c("Yes", "No"), range = c("lightgray", "white")) %>%
      scale_nominal("scale_biaspoolcolor", domain = c("Yes", "No", "Noscales"), range = c("lightgray", "black", "white"))

    ## Draw strings
    ## Hard-code the maximum number of strings that can be drawn.
    ## unclear how else to let the upper limit of the loop react to a
    ## change in the number of rows in the base dataset, while retaining animations.
    ## geom_segment would make this cleaner, but not implemented yet.
    ## feed dplyr grouped data to layer_paths?   https://github.com/rstudio/ggvis/issues/375
    max_rows <- 50
    for (i in seq_len(max_rows)){
        ai <- getdata_string(i)
        p <- layer_paths(p, x = ~x, y = ~y, data=ai,
                         prop("stroke", ~ selected, scale="scale_stringcolor"), opacity := 0.5)
    }

    ## if put key in layer_points but not string layer, then hover doesn't know key
    ## if put key in both, then draws only one
    ## need key for hover and click

#    p <- layer_paths(p, x = ~x, y = ~y, key:=~key, data=getdata_allstrings, prop("stroke", ~ selected, scale="scale_stringcolor"), opacity := 0.5)
    a_props <- axis_props(title=list(fontSize=15), labels=list(fontSize=14))

    p <- p %>%
      add_tooltip(hover_fn) %>%
      handle_click(click_fn) %>%
      add_axis("x", title="Estimate", grid=FALSE, properties=a_props)  %>%
      add_axis("y", title="Study weight", grid=FALSE, properties=a_props) %>%
      ## Implement the plot symbol as four adjacent rectangles
      ## top rectangle
      layer_rects(x=~est-dx, y=~yre+dyre, x2=~est+dx, y2=~yre+dy, fill=~selected, strokeWidth:=0, opacity:=0.5) %>%
      ## left rectangle
      layer_rects(x=~est-dx, y=~yre-dyre, x2=~est-dxre, y2=~yre+dyre, fill=~selected, strokeWidth:=0, opacity:=0.5) %>%
      ## bottom rectangle
      layer_rects(x=~est-dx, y=~yre-dy, x2=~est+dx, y2=~yre-dyre, fill=~selected, strokeWidth:=0, opacity:=0.5) %>%
      ## right rectangle
      layer_rects(x=~est+dxre, y=~yre-dyre, x2=~est+dx, y2=~yre+dyre, fill=~selected, strokeWidth:=0, opacity:=0.5) %>%
      ## and an invisible one in the middle that can be hovered / clicked
      layer_rects(x=~est-dxre, y=~yre-dyre, x2=~est+dxre, y2=~yre+dyre, strokeWidth:=0, opacity:=0) %>%
      hide_legend("size") %>%
      scale_nominal("fill", range = c("blue", "lightgray")) %>%
      hide_legend("fill") %>%
      scale_nominal("stroke", range = c("black", "lightgray")) %>%
      hide_legend("stroke") %>%
      set_options(duration = 500) ## animation transition duration
    observe({ # don't draw plot until RE SD has been computed from data
            bind_shiny(p, "ggvis")
    })

})

## TODO

## Selection doesn't get reset when changing the base dataset
## This bug was there before moving to datatables

## datatables: make selection color darker

## Egger reincludes all points

## get strings group_by thing working.  some bug or misunderstanding with key
## after update of shiny, Ok for static plot but strings go away on reclick
## Posted to ggvis forum (but not github issue yet)
## could try to debug manually

## button horiz alignment is a fudge and breaks on some screens

## Y axis title can't be changed dynamically
## https://github.com/rstudio/ggvis/issues/203
