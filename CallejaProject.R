library(shiny)
library(shinyWidgets)
library(dplyr)
library(DT)

# the one that solves the problem
Simplex <- function(tableau, isMax = TRUE){
  iterTables <- list()
  iterCount <- 1
  
  while (TRUE) {
    # stores initial tableau
    iterTables[[iterCount]] <- list( # because r doesnt do 0 indexing
      tableau = tableau, 
      basicSol = double(ncol(tableau)), 
      pivotRow = NA, 
      pivotCol = NA
    )
    
    # checks for negative values in the last row
    # we take the last row only, not the column since the last column is the solution
    # if true ang sagot, we loop until false sagot
    lastRow <- tableau[nrow(tableau), -ncol(tableau)]
    
    # this stores the largest negative number
    negativeNum <- 0 
    
    # this stores the pivots column
    pivotCol <- 0
    
    # this stores the pivot row
    pivotRow <- 0
    
    # this stores the smallestRatio to compare with the current Ratio
    smallestRatio <- Inf # starts with infinite so ma change agad (since malaki siya na number)
    
    # STEP 1: FIND PIVOT COLUMN
    # find most negative value and its position
    # loops through the all values in the last row and updates if its smaller
    for (i in 1:length(lastRow)) {
      if (lastRow[i] < negativeNum) {
        negativeNum <- lastRow[i]
        pivotCol <- i
      }
    }
    
    # if no negative number found we stop, meaning we found the optimal value
    if (negativeNum >= 0) {
      break
    }
    
    # STEP 2: FIND PIVOT ROW
    # using pivot column, we'll find pivot row
    # we loop through all values in pivotCol
    for (i in 1:(nrow(tableau) -1)) {
      if (tableau[i, pivotCol] > 0){ # divide only if positive
        currentRatio <- tableau[i, ncol(tableau)] / tableau[i, pivotCol]
        
        # compares current ratio to smallest, updates if their is a ratio much smaller
        if (currentRatio < smallestRatio){
          smallestRatio <- currentRatio
          pivotRow <- i
        }
      }
      
    }
    
    # check if the smallest ratio is Inf → infeasible
    if (is.infinite(smallestRatio)) {
      
      # save the final tableau BEFORE stopping
      iterTables[[length(iterTables) + 1]] <- list(
        tableau = tableau,
        pivotRow = NA,
        pivotCol = pivotCol  # or NA if you prefer
      )
      
      return(list(
        status = "INFEASIBLE",
        message = "Problem is unbounded.",
        iterTables = iterTables
      ))
    }
    
    
    # store pivot information
    iterTables[[iterCount]]$pivotRow <- pivotRow
    iterTables[[iterCount]]$pivotCol <- pivotCol
    
    # STEP 3: PIVOT OPERATION
    # we now make a new tableau!
    # PR/PE on the pivot element then Row - (NormRow * Element in PivotCol in Row u want to elim (basta ung C))
    pivotElement = tableau[pivotRow, pivotCol]
    tableau[pivotRow, ] = tableau[pivotRow, ] / pivotElement
    
    # loop through each row in the matrix EXCEPT THE PIVOT ROW and change values
    for (i in 1:nrow(tableau)) {
      if (i != pivotRow){
        c <- tableau[i, pivotCol]
        tableau[i, ] <- tableau[i, ] - (c * tableau[pivotRow, ])
      }
    }
    # then this will loop again and again until no more yey
    iterCount <- iterCount + 1
  }
  
  # this is for the Basic Solution
  basicSol <- double(ncol(tableau))  # make a vector to store results, no -1 since we save it for the solution
  
  for (j in 1:(ncol(tableau) - 1)) {
    col <- tableau[1:(nrow(tableau)-1), j]  # take the column without Z row
    
    # if column has one 1 and the rest 0s, it is basic and we get the solution part
    if (sum(col == 1) == 1 && sum(col == 0) == length(col) - 1) {
      rowIndex <- which(col == 1)            # find where the 1 is
      basicSol[j] <- tableau[rowIndex, ncol(tableau)]
    } else {
      basicSol[j] <- 0
    }
  }
  
  # adds Z at the reseverd space
  basicSol[ncol(tableau)] <- tableau[nrow(tableau), ncol(tableau)]
  
  iterCount <- iterCount + 1
  iterTables[[iterCount]] <- list(
    tableau = tableau, 
    basicSol = basicSol, 
    pivotRow = pivotRow, 
    pivotCol = pivotCol
  )
  
  return(list(status ="OPTIMAL", finalTableau = tableau, basicSolution = basicSol, Z = tableau[nrow(tableau), ncol(tableau)], iterTables = iterTables))
  
}

# project data
Projects <- data.frame(
  number = 1:30, 
  name = c("Large Solar Park", "Small Solar Installations", "Wind Farm", "Gas-to-renewables conversion", "Boiler Retrofit",
           "Catalytic Converters for Buses", "Diesel Bus Replacement", "Traffic Signal/Flow Upgrade", "Low-Emission Stove Program", "Residential Insulation/Efficiency",
           "Industrial Scrubbers", "Waste Methane Capture System", "Landfill Gas-to-energy", "Reforestation (acre-package)", "Urban Tree Canopy Program (street trees)", 
           "Industrial Energy Efficiency Retrofit", "Natural Gas Leak Repair", "Agricultural Methane Reduction", "Clean Cookstove & Fuel Switching (community scale)", "Rail Electrification",
           "EV Charging Infrastructure", "Biochar for soils (per project unit)", "Industrial VOC", "Heavy-Duty Truck Retrofit", "Port/Harbor Electrification",
           "Black Carbon reduction", "Wetlands restoration", "Household LPG conversion program", "Industrial process change", "Behavioral demand-reduction program"),
  cost = c(4000, 1200, 3800, 3200, 1400, 2600, 5000, 1000, 180, 900, 4200, 3600, 3400, 220, 300, 1600, 1800, 2800, 450, 6000, 2200, 1400, 2600, 4200, 4800, 600, 1800, 700, 5000, 400),
  co2 = c(60, 18, 55, 25, 20, 30, 48, 12, 2, 15, 6, 28, 24, 3.5, 4.2, 22, 10, 8, 3.2, 80, 20, 6, 2, 36, 28, 1.8, 10, 2.5, 3, 9),
  nox = c(0, 0, 0, 1, 0.9, 2.8, 3.2, 0.6, 0.02, 0.1, 0.4, 0.2, 0.15, 0.04, 0.06, 0.5, 0.05, 0.02, 0.04, 2, 0.3, 0.01, 0.01, 2.2, 1.9, 0.02, 0.03, 0.03, 0.02, 0.4),
  so2 = c(0, 0, 0, 0.2, 0.4, 0.6, 0.9, 0.1, 0.01, 0.05, 6, 0.1, 0.05, 0.02, 0.01, 0.3, 0.01, 0.01, 0.02, 0.4, 0.05, 0, 0, 0.6, 0.8, 0.01, 0.02, 0.01, 0.01, 0.05),
  pm2.5 = c(0, 0, 0, 0.1, 0.2, 0.8, 1, 0.4, 0.7, 0.05, 0.4, 0.05, 0.03, 0.01, 0.03, 0.15, 0.01, 0.02, 0.9, 1.2, 0.1, 0.01, 0, 0.6, 0.7, 0.6, 0.02, 0.4, 0, 0.05),
  ch4 = c(0, 0, 0, 1.5, 0.1, 0, 0, 0.05, 0, 0.02, 0, 8, 6.5, 0.8, 0.6, 0.2, 4, 7.2, 0.1, 0, 0, 2.5, 0, 0, 0, 0.05, 3.2, 0.05, 0, 0.01),
  voc = c(0, 0, 0, 0.5, 0.05, 0.5, 0.7, 0.2, 0.01, 0.02, 0.1, 0.2, 0.1, 0.03, 0.02, 0.1, 0.02, 0.05, 0.02, 0.6, 0.05, 0.01, 6.5, 0.3, 0.2, 0.01, 0.01, 0.02, 0, 0.3),
  co = c(0, 0, 0, 2, 1.2, 5, 6, 3, 1.5, 0.5, 0.6, 0.1, 0.05, 0.1, 0.15, 1, 0.02, 0.02, 2, 10, 0.5, 0.01, 0.1, 4.2, 3.6, 1, 0.05, 1.2, 0, 2.5),
  nh3 = c(0, 0, 0, 0.05, 0.02, 0.01, 0.02, 0.02, 0.03, 0, 0.01, 0, 0, 0.01, 0.005, 0.01, 0, 0.1, 0.05, 0.02, 0.01, 0.2, 0, 0.01, 0.01, 0.02, 0.15, 0.03, 0, 0.01),
  bc = c(0, 0, 0, 0.01, 0.01, 0.05, 0.08, 0.02, 0.2, 0, 0.01, 0, 0, 0.005, 0.02, 0.01, 0, 0, 0.25, 0.1, 0.01, 0, 0, 0.04, 0.03, 0.9, 0.02, 0.1, 0, 0.01),
  n2o = c(0, 0, 0, 0.3, 0.05, 0.02, 0.03, 0.01, 0, 0.01, 0, 0.05, 0.03, 0.005, 0.002, 0.03, 0.01, 0.05, 0, 0.05, 0.01, 0.02, 0, 0.02, 0.02, 0, 0.04, 0, 1.5, 0.01)
)

buildTableau <- function(selectedProjects, projectData, target){
  
  # filters the selected projects and store into a var 
  finalProjects = projectData[selectedProjects, ]
  numOfProjects <- nrow(finalProjects) # counts num of chosen projects
  numOfPollutants <- 10 # always set to 10 as per pdf
  
  # build primal constraint matrix
  numConstraints <- numOfPollutants + numOfProjects
  A <- matrix(0, nrow = numConstraints, ncol = numOfProjects)
  b <- double(numConstraints)
  
  pollutantCols <- c("co2", "nox", "so2", "pm2.5", "ch4", 
                     "voc", "co", "nh3", "bc", "n2o")
  
  # constraints
  for (i in 1:numOfPollutants) {
    A[i, ] <- finalProjects[[pollutantCols[i]]]
    b[i] <- targets[i]
  }
  for(i in 11:(numOfProjects+10)){
    A[i,(i-10)] = -1
    b[i] = -20
  }
  
  A=cbind(A,b)
  print(A)
  
  # costs
  c <- finalProjects$cost
  
  proj_costs = c()
  for(i in 1:length(c)){
    proj_costs = c(proj_costs, c[i])
  }
  proj_costs = c(proj_costs, 0)
  A = rbind(A, proj_costs)
  print(A)
  
  # transpose to dual
  A_t <- t(A)
  print(A_t)
  A_t[nrow(A_t),] = A_t[nrow(A_t),] * -1
  
  slack_data = c()
  for(i in 1:nrow(A_t)){
    for(j in 1:nrow(A_t)){
      if(i==j){
        slack_data = c(slack_data, 1)
      }
      else {
        slack_data = c(slack_data, 0)
      }
    }
  }
  
  slack = matrix(data=slack_data, byrow=TRUE, ncol=numOfProjects+1)
  print(slack)
  
  
  A_t = cbind(A_t[,1:(10+numOfProjects)], slack, A_t[,ncol(A_t)])
  print(A_t)
  
  return(list(tableau = A_t, projectNames = finalProjects$name, projectNumbers = finalProjects$number, numOfProjects = numOfProjects))
}

# constraint value
targets <- c(
  co2 = 1000,
  nox = 35,
  so2 = 25,
  pm2.5 = 20,
  ch4 = 60,
  voc = 45,
  co = 80,
  nh3 = 12,
  bc = 6,
  n2o = 10
)

ui <- fluidPage(
  tags$head(
    # Import Google Font
    tags$link(
      rel = "stylesheet",
      href = "https://fonts.googleapis.com/css2?family=Segoe+UI:wght@300;400;600;700&display=swap"
    ),
    
    tags$style(HTML("
      /* custom font*/
      @font-face {
        font-family: 'Frutiger';
        src: url('fonts/Frutiger Regular.ttf') format('truetype');
        font-weight: normal;
        font-style: normal;
      }
      
      @font-face {
        font-family: 'Neuropol';
        src: url('fonts/Neuropol.otf') format('opentype');
        font-weight: normal;
        font-style: normal;
      }
      
      @font-face {
        font-family: 'Nulshock';
        src: url('fonts/Nulshock Bd.otf') format('opentype');
        font-weight: normal;
        font-style: normal;
      }
      
      @font-face {
        font-family: 'Conthrax';
        src: url('fonts/Conthrax.otf') format('opentype');
        font-weight: normal;
        font-style: normal;
      }
      
      @font-face {
        font-family: 'Sofachrome';
        src: url('fonts/Sofachrome Rg It.otf') format('opentype');
        font-weight: normal;
        font-style: normal;
      }\
      
      /* Apply font globally */
      body, .form-control, .btn, h1, label {
        font-family: 'Frutiger' ,'Segoe UI', sans-serif;
      }
      
      h1{
        font-family: 'Sofachrome' ,'Segoe UI', sans-serif;
        color: #0066CC; 
        font-size: 42px; 
        font-weight: 700;
        text-shadow: 0 2px 4px rgba(255, 255, 255, 0.8), 0 0 20px rgba(104, 176, 230, 0.3);
      }
      
      h2{
        font-family: 'Nulshock' ,'Segoe UI', sans-serif;
        font-size: 18px; 
        font-weight: 700;
        text-shadow: 0 2px 4px rgba(255, 255, 255, 0.8), 0 0 20px rgba(104, 176, 230, 0.3);
        
        background: linear-gradient(
        90deg,
        #3979FA 0%, 
        #68B0E6 50%,   
        #3979FA 100%
        ); 
      
        -webkit-background-clip: text;
        background-clip: text;
      
        color: transparent; 
      }
      
      h4 {
        font-family: 'Neuropol' ,'Segoe UI', sans-serif;
        margin: 10px 0px 10px 0px;
        font-size: 20px;
        color: #1948a6;
      }
      
      h3{
        font-family: 'Neuropol' ,'Segoe UI', sans-serif;
        margin: 10px 0px 10px 0px;
        font-size: 30px;
        color: #1948a6;
      }
      
      h5, h6{
        font-family: 'Conthrax' ,'Segoe UI', sans-serif;
        margin: 10px 0px 10px 0px;
        font-size: 20px;
        color: #1948a6;
      }
      
      p{
        font-family: 'Frutiger' ,'Segoe UI', sans-serif;
        margin: 10px 0px 10px 0px;
        font-size: 12px;
        color
      }
      
      /* Authentic Frutiger Aero Button CSS */
      /* from https://makeaero.com/ */
      .frutiger-aero-button {
        /* OKLCH Color System for accurate colors */
        --hue: 140;
        --sat: 0.4;
        --glow-intensity: 0.75;
        
        /* Color Variables */
        --fg: oklch(15% calc(var(--sat) * 0.5) var(--hue));
        --bg: oklch(75% var(--sat) var(--hue) / 0.8);
        --bg-dark: oklch(45% var(--sat) var(--hue) / 0.75);
        --bottom-glow: radial-gradient(
          farthest-corner at bottom center,
          rgba(255, 255, 255, var(--glow-intensity)),
          transparent
        );
        
        /* Base Styling */
        background-color: var(--bg);
        background: 
          var(--bottom-glow),
          linear-gradient(to bottom, var(--bg-dark), var(--bg));
        
        border: 1px solid var(--bg);
        border-radius: 9999px;
        
        /* Shadows and Effects */
        box-shadow: 0 4px 4px rgba(0, 0, 0, 0.4);
        
        /* Typography */
        color: var(--fg);
        font-family: 'Lucida Grande', 'Lucida Sans Unicode', 'Segoe UI', system-ui, sans-serif;
        font-weight: 700;
        text-shadow: 0 2px 0.5em rgba(0, 0, 0, 0.2);
        
        /* Layout */
        cursor: pointer;
        position: relative;
        transition: all 300ms ease;
        
        /* Prevent text selection */
        user-select: none;
        -webkit-user-select: none;
      }
      
      /* Top Highlight Effect */
      .frutiger-aero-button::after {
        content: '';
        position: absolute;
        top: 4%;
        left: 0.75em;
        width: calc(100% - 1.5em);
        height: 40%;
        background: linear-gradient(
          to bottom,
          rgba(255, 255, 255, 0.8),
          rgba(255, 255, 255, 0.1)
        );
        border-radius: inherit;
        transition: background 400ms ease;
        pointer-events: none;
      }
      
      /* Hover State */
      .frutiger-aero-button:hover,
      .frutiger-aero-button:focus {
        box-shadow: 0 6px 8px rgba(0, 0, 0, 0.4);
        transform: translateY(-1px);
      }
      
      /* Active State */
      .frutiger-aero-button:active {
        box-shadow: 0 2px 4px rgba(0, 0, 0, 0.4);
        transform: translateY(1px);
      }
      
      /* Size Variations */
      .frutiger-aero-button.small {
        padding: 0.5em 1.5em;
        font-size: 0.875rem;
      }
      
      .frutiger-aero-button.medium {
        padding: 0.75em 2em;
        font-size: 1rem;
      }
      
      .frutiger-aero-button.large {
        padding: 1em 3em;
        font-size: 1.125rem;
      }
      
      /* background image */
      body {
        background-image: url('frutigerbg.png');
        background-size: cover;
        background-position: center;
        background-attachment: fixed;
        background-repeat: no-repeat;
      }

      .container-fluid {
        background-color: rgba(255, 255, 255, 0.9);
        padding: 20px;
        border-radius: 30px;
        margin: 50px auto;
        max-width: 90%;
        box-shadow: 0 8px 32px rgba(0, 0, 0, 0.2), 0 0 80px rgba(255, 255, 255, 0.3);
      }
      
      /* Animated background glow */
      body::before {
        content: '';
        position: fixed;
        top: -50%;
        left: -50%;
        width: 200%;
        height: 200%;
        background: 
          radial-gradient(circle at 20% 50%, rgba(255, 255, 255, 0.05) 0%, transparent 50%),
          radial-gradient(circle at 80% 80%, rgba(104, 176, 230, 0.08) 0%, transparent 50%),
          radial-gradient(circle at 40% 20%, rgba(255, 255, 255, 0.06) 0%, transparent 50%);
        animation: float 20s ease-in-out infinite;
        pointer-events: none;
        z-index: 0;
      }
      
      @keyframes float {
        0%, 100% { transform: translate(0, 0) rotate(0deg); }
        33% { transform: translate(30px, -30px) rotate(120deg); }
        66% { transform: translate(-20px, 20px) rotate(240deg); }
      }
      
      /* Enhanced container with Vista-style chrome */
      .container-fluid {
        position: relative;
        overflow: visible;
      }
      
      .container-fluid::before {
        content: '';
        position: absolute;
        top: 0;
        left: 0;
        right: 0;
        height: 50px;
        background: linear-gradient(180deg, 
          rgba(255, 255, 255, 0.4) 0%, 
          rgba(255, 255, 255, 0.1) 100%);
        border-radius: 30px 30px 0 0;
        border-bottom: 1px solid rgba(255, 255, 255, 0.5);
        pointer-events: none;
      }
      
      /* Glossy sidebar enhancement */
      .col-sm-3 {
        background: linear-gradient(135deg, 
          rgba(255, 255, 255, 0.4) 0%, 
          rgba(240, 248, 255, 0.3) 100%);
        backdrop-filter: blur(10px);
        border-radius: 15px;
        padding: 20px;
        margin: 10px;
        box-shadow: 
          0 4px 16px rgba(0, 0, 0, 0.1),
          0 1px 0 rgba(255, 255, 255, 0.6) inset;
        border: 1px solid rgba(255, 255, 255, 0.5);
      }
      
      /* Main panel glass effect */
      .col-sm-8 {
        background: rgba(255, 255, 255, 0.3);
        backdrop-filter: blur(5px);
        border-radius: 15px;
        padding: 25px;
        margin: 10px;
        box-shadow: 
          0 2px 12px rgba(0, 0, 0, 0.08),
          0 1px 0 rgba(255, 255, 255, 0.4) inset;
        border: 1px solid rgba(255, 255, 255, 0.4);
      }
      
      /* Enhanced checkboxes with hover glow */
      .checkbox {
        margin: 8px 0;
        padding: 6px 10px;
        background: rgba(255, 255, 255, 0.3);
        border-radius: 8px;
        transition: all 0.2s ease;
        border: 1px solid transparent;
      }
      
      .checkbox:hover {
        background: rgba(255, 255, 255, 0.5);
        box-shadow: 0 2px 8px rgba(104, 176, 230, 0.2);
        border: 1px solid rgba(104, 176, 230, 0.3);
      }
      
      .checkbox label {
        cursor: pointer;
        transition: color 0.2s ease;
      }
      
      .checkbox:hover label {
        color: #0066CC;
      }
      
      input[type=checkbox] {
          accent-color: #4b8ccc;
          width: 1.1em;
          height: 1.1em;
          background-color: rgba(255, 255, 255, 0.5); 
          border: 1px solid rgba(104, 176, 230, 0.5);
      }
      
      /* Scrollable area with custom Vista-style scrollbar */
      .project-scroll-box {
        max-height: 400px;
        overflow-y: auto;
        padding-right: 10px;
        margin-bottom: 15px;
      }
      
      .project-scroll-box::-webkit-scrollbar {
        width: 14px;
      }
      
      .project-scroll-box::-webkit-scrollbar-track {
        background: rgba(200, 220, 240, 0.2);
        border-radius: 10px;
        border: 1px solid rgba(255, 255, 255, 0.4);
        box-shadow: 0 1px 3px rgba(0, 0, 0, 0.1) inset;
      }
      
      .project-scroll-box::-webkit-scrollbar-thumb {
        background: linear-gradient(180deg, 
          rgba(104, 176, 230, 0.7) 0%, 
          rgba(68, 150, 210, 0.7) 100%);
        border-radius: 10px;
        border: 2px solid rgba(255, 255, 255, 0.3);
        box-shadow: 
          0 2px 4px rgba(0, 0, 0, 0.2) inset,
          0 1px 0 rgba(255, 255, 255, 0.5) inset;
      }
      
      .project-scroll-box::-webkit-scrollbar-thumb:hover {
        background: linear-gradient(180deg, 
          rgba(124, 196, 250, 0.8) 0%, 
          rgba(88, 170, 230, 0.8) 100%);
      }
      
      /* Enhanced result sections with glass morphism */
      .result-section {
        background: linear-gradient(135deg, 
          rgba(180, 240, 180, 0.4) 0%, 
          rgba(160, 230, 160, 0.3) 100%);
        backdrop-filter: blur(15px);
        border: 1px solid rgba(120, 200, 120, 0.4);
        border-radius: 15px;
        padding: 20px;
        margin: 20px 0;
        box-shadow: 
          0 4px 20px rgba(0, 0, 0, 0.1),
          0 1px 0 rgba(255, 255, 255, 0.6) inset;
      }
      
      .infeasible-section {
        background: linear-gradient(135deg, 
          rgba(255, 200, 180, 0.4) 0%, 
          rgba(255, 180, 160, 0.3) 100%);
        backdrop-filter: blur(15px);
        border: 1px solid rgba(255, 140, 100, 0.4);
        border-radius: 15px;
        padding: 20px;
        margin: 20px 0;
        box-shadow: 
          0 4px 20px rgba(0, 0, 0, 0.1),
          0 1px 0 rgba(255, 255, 255, 0.6) inset;
      }
      
      /* Enhanced verbatim/pre elements */
      pre, .shiny-text-output {
        background: rgba(255, 255, 255, 0.5);
        backdrop-filter: blur(8px);
        border: 1px solid rgba(200, 220, 240, 0.4);
        border-radius: 12px;
        padding: 15px;
        box-shadow: 
          0 2px 8px rgba(0, 0, 0, 0.08) inset,
          0 1px 0 rgba(255, 255, 255, 0.4);
      }
      
      .checkbox-grid {
        display: grid;
        grid-template-columns: repeat(2, 1fr);
        gap: 10px;
      }
      
      .tableau-box {
        overflow-x: auto;
        font-size: 11px;
        background: rgba(255, 255, 255, 0.6);
        backdrop-filter: blur(10px);
        padding: 15px;
        border: 1px solid rgba(200, 220, 240, 0.5);
        border-radius: 12px;
        margin: 10px 0;
        box-shadow: 
          0 4px 12px rgba(0, 0, 0, 0.08),
          0 1px 0 rgba(255, 255, 255, 0.5) inset;
      }
      
      /* DataTables enhancement */
      .dataTables_wrapper {
        background: rgba(255, 255, 255, 0.3);
        backdrop-filter: blur(5px);
        border-radius: 12px;
        padding: 15px;
        border: 1px solid rgba(200, 220, 240, 0.3);
      }
      
      table.dataTable thead th {
        background: linear-gradient(180deg, 
          rgba(104, 176, 230, 0.2) 0%, 
          rgba(80, 156, 210, 0.2) 100%);
        border-bottom: 2px solid rgba(104, 176, 230, 0.4);
      }
      
      table.dataTable tbody tr:hover {
        background: rgba(104, 176, 230, 0.1) !important;
      }
      
      /* Enhanced HR with glow */
      hr {
        border: none;
        height: 1px;
        background: linear-gradient(90deg, 
          transparent 0%, 
          rgba(104, 176, 230, 0.4) 50%, 
          transparent 100%);
        box-shadow: 0 1px 2px rgba(104, 176, 230, 0.2);
        margin: 25px 0;
      }
      
      /* About modal styling */
      .modal-content {
        border-radius: 15px;
        background-color: rgba(255, 255, 255, 0.98);
        backdrop-filter: blur(20px);
        border: 1px solid rgba(255, 255, 255, 0.6);
      }
      
      .modal-header {
        background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
        color: white;
        border-radius: 15px 15px 0 0;
      }
      
      /* Smooth transitions for all interactive elements */
      * {
        transition: background 0.2s ease, box-shadow 0.2s ease, transform 0.2s ease;
      }
    "))
  ),
  
  div(class = "banner-container",
      div(class = "banner",
          div(class = "banner-text-content",
              h1("City of Greenvale", style = "margin: 0px 0px 0px 30px; padding: 0; line-height: 1.2;"),
              h2("Pollution Reduction Planner using Simplex Method!", style = "margin: 0px 0px 0px 50px; padding: 0; line-height: 1.2;")
          )
      ),
  
      div(class = "right-aligned-button",
          actionButton("infoButton", 
                       "About Us", 
                       icon = icon("info-circle"),
                       class = "frutiger-aero-button medium",
                       style = "margin: 5px 0px 10px 30px; --hue: 240; font-size: 14px;")
      ),
  ),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      h4("Select Projects"),
      
      # Frutiger Aero styled buttons
      actionButton("selectAll", "Check All", 
                   class = "frutiger-aero-button medium btn-block", 
                   style = "margin-bottom: 10px; font-size: 14px;"),
      actionButton("reset", "Reset", 
                   class = "frutiger-aero-button medium btn-block", 
                   style = "margin-bottom: 20px; --hue: 190; font-size: 14px;"),
      hr(),
      # Changed to use the new scrollable class
      div(class = "project-scroll-box",
          uiOutput("projectCheckboxes")
      ),
      
      hr(),
      actionButton("solve", "Let's Optimize!", 
                   class = "frutiger-aero-button medium btn-block", 
                   style = "margin-bottom: 10px; --hue: 180; font-size: 14px;",
                   icon = icon("calculator"))
    ),
    
    mainPanel(
      width = 8,
      
      h3("Your Input"),
      verbatimTextOutput("selectedProjects"),
      
      
      conditionalPanel(
        condition = "output.hasSolution",
        hr(),
        uiOutput("resultHeader"),
        
        conditionalPanel(
          condition = "output.isOptimal",
          
          hr(),
          h3("Basic Solution"),
          div(class = "tableau-box", 
              verbatimTextOutput("BasicSolution") # This is the new output
          ),
          
          hr(),
          h3("Project Breakdown"),
          DTOutput("solutionTable"),
          
          hr(),
          h3("Constraint Verification"),
          DTOutput("constraintTable")
        ),
        
          hr(),
          h3("Tableau and Iteration Details"),
          
          div(
            checkboxInput("showInitial", "Show Initial Tableau", value = FALSE),
            checkboxInput("showIterations", "Show All Iterations", value = FALSE),
            checkboxInput("showFinal", "Show Final Tableau", value = TRUE)
          ),
          
          conditionalPanel(
            condition = "input.showInitial",
            h4("Initial Tableau"),
            div(class = "tableau-box",
                verbatimTextOutput("initialTableau")
            )
          ),
          
          conditionalPanel(
            condition = "input.showIterations",
            h4("Iteration Details"),
            uiOutput("iterationDetails")
          ),
          
          conditionalPanel(
            condition = "input.showFinal",
            h4("Final Tableau"),
            div(class = "tableau-box",
                verbatimTextOutput("finalTableau")
            )
          )
        )
      )
    )
  )

# shiny server
server <- function(input, output, session) {
  
  options(max.print = .Machine$integer.max)
  
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
  
  # the select all button
  observeEvent(input$selectAll, {
    selectedProjects(1:30)
    for (i in 1:30) {
      updateCheckboxInput(session, paste0("project_", i), value = TRUE)
    }
  })
  
  # the reset button
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
          h3(style = "font-family: 'Nulshock'; font-size: 36px; font-weight: 700;text-shadow: 0 2px 4px rgba(255, 255, 255, 0.8), 0 0 20px rgba(104, 176, 230, 0.3);", "Your Plan is FEASIBLE :)"),
          h6(style = "color: #28a745;", 
             paste0("The cost of this optimal mitigation project is $", 
                    format(abs(result$Z), nsmall = 2, big.mark = ",")))
      )
    } else {
      div(class = "infeasible-section",
          h3(style = "font-family: 'Nulshock'; font-size: 36px; font-weight: 700;text-shadow: 0 2px 4px rgba(255, 255, 255, 0.8), 0 0 20px rgba(104, 176, 230, 0.3);", "Your Plan is INFEASIBLE :("),
          h6(style = "color: #a72828",result$message)
      )
    }
  })
  
  # helper function that prints rownames and colnames since shiny removes them by default
  printMatrix <- function(M) {
    M <- as.matrix(M)
    colnames(M) <- NULL
    rownames(M) <- NULL
    cat(capture.output(print(M)), sep="\n")
  }
  
  
  # the solution table
  output$solutionTable <- renderDT({
    req(solverResult())
    req(solverResult()$result$status == "OPTIMAL")
    
    result <- solverResult()$result
    tableauData <- solverResult()$tableauData
    
    numOfPollutants <- 10
    solution <- result$finalTableau[nrow(result$finalTableau), 
                                    (numOfPollutants + tableauData$numOfProjects + 1):(numOfPollutants + 2*tableauData$numOfProjects + 1)]
    
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
  output$BasicSolution <- renderPrint({
    req(solverResult())
    res <- solverResult()
    req(res$result$status == "OPTIMAL")
    options(width = 10000)
    
    # Extract last row but remove last column
    finalRow <- res$result$finalTableau[nrow(res$result$finalTableau), 
                                        -ncol(res$result$finalTableau)]
    
    # Convert to numeric vector
    finalRow <- as.numeric(finalRow)
    
    # Round to 2 decimals
    finalRow <- round(finalRow, 2)
    
    # Convert to 1-row matrix
    finalRowMatrix <- matrix(finalRow, nrow = 1)
    
    printMatrix(finalRowMatrix)
  })
  
  

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
      achieved <- sum(solution * Projects[[pollutantCols[i]]][tableauData$projectNumbers])
      
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
                                               c('#d4edda', '#f8d7da')))
  })
  
  # prints initial tableau
  output$initialTableau <- renderPrint({
    req(initialTableau())
    options(width = 10000)
    printMatrix(initialTableau())
  })
  
  # final tableau
  output$finalTableau <- renderPrint({
    req(solverResult())
    options(width = 10000)
    printMatrix(round(solverResult()$result$finalTableau, 4))
  })
  
  # prints details of the iteration details, prints pivot row and pivot col
  output$iterationDetails <- renderUI({
    req(solverResult())
    options(width = 10000)
    result <- solverResult()$result
    
    iterations <- lapply(1:(length(result$iterTables)-1), function(i) {
      iter <- result$iterTables[[i]]
      
      div(class = "tableau-box",
          h5(paste("Iteration", i)),
          p(strong("Pivot Row:"), ifelse(is.na(iter$pivotRow), "N/A", iter$pivotRow)),
          p(strong("Pivot Column:"), ifelse(is.na(iter$pivotCol), "N/A", iter$pivotCol)),
          verbatimTextOutput(paste0("iter_", i))
      )
    })
    
    do.call(tagList, iterations)
  })
  
  # renders each tableau result
  observe({
    req(solverResult())
    result <- solverResult()$result
    
    for (i in 1:(length(result$iterTables)-1)) {
      local({
        my_i <- i
        output[[paste0("iter_", my_i)]] <- renderPrint({
          printMatrix(round(result$iterTables[[my_i]]$tableau, 4))
        })
      })
    }
  })
}

# Run the app
options(shiny.launch.browser = TRUE) # makes it open in the browser asap
shinyApp(ui = ui, server = server)