# shiny server
server <- function(input, output, session) {
  
  options(max.print = .Machine$integer.max)
  
  # reactive value to control showing main app
  showMainApp <- reactiveVal(FALSE)
  
  # observes if user presses button to enter app
  observeEvent(input$enterApp, {
    showMainApp(TRUE)
    
    #sends message if true
    session$sendCustomMessage("fadeGreeting", list())
  })
  
  #conditional panel output
  output$showMainApp <- reactive({
    showMainApp()
  })
  outputOptions(output, "showMainApp", suspendWhenHidden = FALSE)
  
  selectedProjects <- reactiveVal(integer(0))
  solverResult <- reactiveVal(NULL)
  initialTableau <- reactiveVal(NULL)
  
  # creates the checkboxes
  output$projectCheckboxes <- renderUI({
    checkboxes <- lapply(1:nrow(Projects), function(i) {
      div(
        checkboxInput(
          inputId = paste0("project_", i),
          label = paste0(i, ". ", Projects$name[i], " ($", Projects$cost[i], ")"),
          value = i %in% selectedProjects()
        )
      )
    })
    do.call(tagList, checkboxes)
  })
  
  # observes if select all button is pressed, if yes, selects all projects
  observeEvent(input$selectAll, {
    selectedProjects(1:30)
    for (i in 1:30) {
      updateCheckboxInput(session, paste0("project_", i), value = TRUE)
    }
  })
  
  # unchecks all projects
  observeEvent(input$reset, {
    selectedProjects(integer(0))
    solverResult(NULL)
    initialTableau(NULL)
    for (i in 1:30) {
      updateCheckboxInput(session, paste0("project_", i), value = FALSE)
    }
  })
  
  # function that observes if a project is selected or not and updates it
  observe({
    selected <- sapply(1:30, function(i) {
      if (!is.null(input[[paste0("project_", i)]])) {
        if (input[[paste0("project_", i)]]) return(i)
      }
      return(NA)
    })
    selected <- selected[!is.na(selected)]
    selectedProjects(selected)
  })
  
  # basically a summary on what the user has selected
  output$selectedProjects <- renderText({
    if (length(selectedProjects()) == 0) {
      return("No projects selected")
    } else if (length(selectedProjects()) == 30) {
      return("All the possible mitigation projects have been selected")
    } else {
      projectNames <- Projects$name[selectedProjects()]
      paste(paste(projectNames, collapse = "\n"))
    }
  })
  
  # button that solves with mini loading screen
  observeEvent(input$solve, {
    req(length(selectedProjects()) > 0)
    
    withProgress(message = 'Solving optimization problem...', value = 0, {
      
      tableauData <- buildTableau(selectedProjects(), Projects, targets)
      initialTableau(tableauData$tableau)
      
      setProgress(0.5, detail = "Running Simplex algorithm...")
      result <- Simplex(tableauData$tableau, isMax = FALSE)
      
      solverResult(list(
        result = result,
        tableauData = tableauData
      ))
      
      setProgress(1)
    })
  })
  
  # checker if the solution exists
  output$hasSolution <- reactive({
    !is.null(solverResult())
  })
  outputOptions(output, "hasSolution", suspendWhenHidden = FALSE)
  
  # if it has solution, check if optimal
  output$isOptimal <- reactive({
    if (!is.null(solverResult())) {
      return(solverResult()$result$status == "OPTIMAL")
    }
    return(FALSE)
  })
  outputOptions(output, "isOptimal", suspendWhenHidden = FALSE)
  
  # prints result
  output$resultHeader <- renderUI({
    req(solverResult())
    result <- solverResult()$result
    
    if (result$status == "OPTIMAL") {
      div(class = "result-section",
          h3(style = "-webkit-text-stroke-color: #28a745", "Your Plan is FEASIBLE :)", class = "result"),
          h6(style = "color: #28a745; text-align:center;", 
             paste0("The cost of this optimal mitigation project is $", 
                    format(abs(result$Z), nsmall = 2, big.mark = ",")))
      )
    } else {
      div(class = "infeasible-section",
          h3(style = "-webkit-text-stroke-color: #a72828", "Your Plan is INFEASIBLE :(", class = "result"),
          h6(style = "color: #a72828; text-align:center;",result$message)
      )
    }
  })
  
  # the solution table
  output$solutionTable <- renderDT({
    req(solverResult())
    req(solverResult()$result$status == "OPTIMAL")
    
    result <- solverResult()$result
    tableauData <- solverResult()$tableauData
    
    numOfPollutants <- 10
    solution <- result$finalTableau[nrow(result$finalTableau), 
                                    (numOfPollutants + tableauData$numOfProjects + 1):(numOfPollutants + 2*tableauData$numOfProjects)]
    
    solutionDF <- data.frame(
      `Mitigation Project` = character(),
      `Number of Project Units` = double(),
      `Cost ($)` = double(),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
    
    for (i in 1:length(solution)) {
      if (solution[i] > 0.001) {
        projectCost <- solution[i] * Projects$cost[tableauData$projectNumbers[i]]
        solutionDF <- rbind(solutionDF, data.frame(
          `Mitigation Project` = tableauData$projectNames[i],
          `Number of Project Units` = round(solution[i], 2),
          `Cost ($)` = round(projectCost, 2),
          stringsAsFactors = FALSE,
          check.names = FALSE
        ))
      }
    }
    
    datatable(solutionDF, 
              options = list(pageLength = 20, dom = 't'),
              rownames = FALSE)
  })
  
  # solution summary
  output$BasicSolution <- renderDT({
    req(solverResult())
    res <- solverResult()
    req(res$result$status == "OPTIMAL")
    
    finalRow <- res$result$finalTableau[nrow(res$result$finalTableau), 
                                        -ncol(res$result$finalTableau)]
    
    finalRow <- round(as.numeric(finalRow), 2)
    
    # colnames since shiny removes it
    numVars <- length(finalRow)
    colNames <- paste0("x", 1:numVars)
    
    df <- data.frame(matrix(finalRow, nrow = 1))
    colnames(df) <- colNames
    
    datatable(df, 
              options = list(
                dom = 't',
                scrollX = TRUE,
                paging = FALSE,
                ordering = FALSE
              ),
              rownames = FALSE)
  }, server = FALSE)
  
  # verifies constraints
  output$constraintTable <- renderDT({
    req(solverResult())
    req(solverResult()$result$status == "OPTIMAL")
    
    result <- solverResult()$result
    tableauData <- solverResult()$tableauData
    
    numOfPollutants <- 10
    solution <- result$finalTableau[nrow(result$finalTableau), 
                                    (numOfPollutants + tableauData$numOfProjects + 1):(numOfPollutants + 2*tableauData$numOfProjects + 1)]
    
    pollutantCols <- c("co2", "nox", "so2", "pm2.5", "ch4", 
                       "voc", "co", "nh3", "bc", "n2o")
    
    verificationDF <- data.frame(
      Pollutant = character(),
      Target = double(),
      Achieved = double(),
      Status = character(),
      stringsAsFactors = FALSE
    )
    
    for (i in 1:10) {
      projectPollutants <- Projects[[pollutantCols[i]]][tableauData$projectNumbers]
      achieved <- sum(solution * projectPollutants)
      
      isMet <- achieved >= (targets[i] - 1e-6)
      
      verificationDF <- rbind(verificationDF, data.frame(
        Pollutant = toupper(pollutantCols[i]),
        Target = targets[i],
        Achieved = round(achieved, 3),
        Status = ifelse(isMet, "✓ Met", "✗ Not Met"),
        stringsAsFactors = FALSE
      ))
    }
    
    datatable(verificationDF, 
              options = list(dom = 't', paging = FALSE),
              rownames = FALSE) %>%
      formatStyle('Status',
                  backgroundColor = styleEqual(c('✓ Met', '✗ Not Met'), 
                                               c('#a0e6a04d', '#ff8c6466')))
  })
  
  # initial tableau
  output$initialTableau <- renderDT({
    req(initialTableau())
    tableau <- round(initialTableau(), 4)
    
    datatable(tableau, 
              options = list(
                dom = 't',
                scrollX = TRUE,
                paging = FALSE,
                ordering = FALSE
              ),
              rownames = TRUE)
  })
  
  # final tableau
  output$finalTableau <- renderDT({
    req(solverResult())
    tableau <- round(solverResult()$result$finalTableau, 4)
    
    datatable(tableau, 
              options = list(
                dom = 't',
                scrollX = TRUE,
                paging = FALSE,
                ordering = FALSE
              ),
              rownames = TRUE)
  })
  
  # prints details of the iteration details, prints pivot row and pivot col
  output$iterationDetails <- renderUI({
    req(solverResult())
    result <- solverResult()$result
    
    iterations <- lapply(1:(length(result$iterTables)-1), function(i) {
      iter <- result$iterTables[[i]]
      
      div(class = "tableau-box",
          h5(paste("Iteration", i)),
          p(strong("Pivot Row:"), ifelse(is.na(iter$pivotRow), "N/A", iter$pivotRow)),
          p(strong("Pivot Column:"), ifelse(is.na(iter$pivotCol), "N/A", iter$pivotCol)),
          DTOutput(paste0("iter_", i))
      )
    })
    
    do.call(tagList, iterations)
  })
  
  # ends program 
  observeEvent(input$exitButton, {
    session$close()
    stopApp()
  })
  
  # user manual modal
  observeEvent(input$infoButton, {
    showModal(modalDialog(
      title = div(style = "text-align: center;",
                  h2("Hi There!", 
                     style = "color: #FFFFFF; margin: 5px 0px 5px 0px; font-size: 20px; font-family: 'Neuropol'; text-shadow: none;")),
      
      div(style = "padding: 10px;",
          h3("Welcome to the Greenvale!", 
             style = "text-align: center; color: #1948a6; font-family: 'Nulshock'; margin: 5px 0px 5px 0px;"),
          
          p("Currently, our city is suffering from mass pollution. With this site, you can help us choose which 
          reduction strategies are the best using the Simplex Method for linear programming."),
          
          hr(),
          
          h4("How to Use:", style = "color: #1948a6; font-family: 'Neuropol', sans-serif;"),
          tags$ol(
            tags$li(strong("Select Projects:"), " Choose from 30 available mitigation projects 
                  by checking the boxes in the sidebar. Each project shows its cost."),
            tags$li(strong("Check All/Reset:"), " Use the buttons to quickly select all projects 
                  or clear your selection."),
            tags$li(strong("Optimize:"), " Click the 'Let's Optimize!' button to run the Simplex 
                  algorithm and find the most cost-effective solution."),
            tags$li(strong("Review Results:"), " The app will show whether your plan is feasible, 
                  the total cost, and a breakdown of selected projects."),
            tags$li(strong("Verify Constraints:"), " Check if all pollution reduction targets 
                  (CO2, NOx, SO2, etc.) are met.")
          ),
          
          hr(),
          
          h4("What This App Does:", style = "color: #1948a6; font-family: 'Neuropol', sans-serif;"),
          p("The Simplex Method is a mathematical optimization technique that finds the minimum 
          cost combination of projects needed to meet all pollution reduction targets. 
          The algorithm considers:"),
          tags$ul(
            tags$li("10 different pollutant types (CO2, NOx, SO2, PM2.5, CH4, VOC, CO, NH3, BC, N2O)"),
            tags$li("Minimum reduction targets for each pollutant"),
            tags$li("Project costs and pollution reduction capabilities"),
            tags$li("Minimum project unit constraints (at least 20 units per project)")
          ),
          
          hr(),
          
          h4("Understanding Results:", style = "color: #1948a6; font-family: 'Neuropol', sans-serif;"),
          tags$ul(
            tags$li(strong("Feasible:"), " A solution exists that meets all constraints"),
            tags$li(strong("Infeasible:"), " No combination of selected projects can meet all targets"),
            tags$li(strong("Basic Solution:"), " The optimal values for all decision variables"),
            tags$li(strong("Tableau:"), " The mathematical representation of the optimization problem")
          )
      ),
      
      easyClose = TRUE,
      footer = tags$button(
        "Got it!",
        type = "button",
        class = "btn frutiger-aero-button medium",
        style = "--hue: 150; font-size: 14px;",
        `data-dismiss` = "modal"
      )
    ))
  })
  
  
  observe({
    req(solverResult())
    result <- solverResult()$result
    
    # create outputs for all iterations
    lapply(1:(length(result$iterTables)-1), function(i) {
      output[[paste0("iter_", i)]] <- renderDT({
        tableau <- round(result$iterTables[[i]]$tableau, 4)
        
        datatable(tableau, 
                  options = list(
                    dom = 't',
                    scrollX = TRUE,
                    paging = FALSE,
                    ordering = FALSE
                  ),
                  rownames = TRUE)
      })
    })
  })
}