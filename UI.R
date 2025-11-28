ui <- fluidPage(
  includeCSS("www/Style.css"),
  
  tags$head(
    tags$link(
      rel = "stylesheet",
      href = "https://fonts.googleapis.com/css2?family=Segoe+UI:wght@300;400;600;700&display=swap"
    ),
    
    # greeting page
    conditionalPanel(
      condition = "!output.showMainApp",
      div(class = "greeting-page",
          id = "greetingPage",
          div(class = "greeting-content",
              h2("this is", style = "margin-bottom: 10px; margin-top: 0px; color: white; -webkit-text-stroke-width: 1px; -webkit-text-stroke-color: #50C878;"),
              actionButton(
                "enterApp",
                "AeroSolve",
                icon = icon("arrow-right"),
                class = "frutiger-aero-button large h1",
                style = "--hue: 140; font-size: 70px; padding: 25px 80px; margin-top: 0px; color: white; font-family: 'Frutiger';"
              )
          )
      )
    )
  ),
  
  # main header
  div(
    class = "main-content",
    id = "mainContent",
    
    div(
      class = "banner",
      style = "display: flex; justify-content: space-between; align-items: center;",
      
      div(
        style = "display: flex; align-items: center; gap: 10px;",
        
        tags$img(
          src = "logo.png",
          style = "width: 90px; height: 90px; border-radius: 50%; margin: 0px 0px 0px 10px;"
        ),
        
        div(
          style = "display: flex; flex-direction: column; margin-right: 20px;",
          
          h1("AeroSolve",
             style = "margin: 0; padding: 0; line-height: 1.2;"
          ),
          
          actionButton(
            "infoButton",
            "About Us",
            icon = icon("info-circle"),
            class = "frutiger-aero-button small",
            style = "margin-top: 5px; --hue: 180; font-size: 12px;"
          )
        )
      ),
      
      div(
        class = "right-aligned-button",
        actionButton(
          "exitButton", "",
          icon = icon("times"),
          class = "frutiger-aero-button medium circular",
          style = "margin-left: 20px; --hue: 190; font-size: 14px;"
        )
      )
    ),
    
    # segment of  selecting projects
    sidebarPanel(
      width = 3,
      
      h4("Select Projects"),
      actionButton(
        "selectAll",
        "Select All",
        class = "frutiger-aero-button medium btn-block",
        style = "margin-bottom: 10px; font-size: 14px;"
      ),
      
      actionButton(
        "reset",
        "Reset",
        class = "frutiger-aero-button medium btn-block",
        style = "margin-bottom: 20px; --hue: 190; font-size: 14px;"
      ),
      
      hr(),
      div(class = "project-scroll-box", uiOutput("projectCheckboxes")),
      hr(),
      
      actionButton(
        "solve",
        "Let's Optimize!",
        class = "frutiger-aero-button medium btn-block",
        style = "margin-bottom: 10px; --hue: 180; font-size: 14px;",
        icon = icon("calculator")
      ),
      
      hr(),
      div(
        class = "user-info-container",
        style = "
      backdrop-filter: blur(20px);
      border-radius: 15px;
      padding: 15px 20px;
      margin-top: 10px;
      box-shadow: 0 4px 12px rgba(0,0,0,0.1),
                  0 1px 0 rgba(255,255,255,0.6) inset;
      border: 1px solid rgba(255,255,255,0.6);
      text-align: center;
    ",
        
        # segment about me!
        h4( "Behind the Scenes"),
        hr(style = "margin: 10px 0 20px 0;"),
        
        # pabida  time with my little smiski
        div(
          style = "
        display: flex;
        flex-direction: column;
        align-items: center;
        gap: 8px;
        margin-bottom: 20px;
      ",
          
          tags$img(
            src = 'smiski.png',
            style = "
          width: 80px;
          height: 80px;
          border-radius: 50%;
        "
          ),
          
          span(
            HTML("Hi There!<br><b>I'm Jodi.</b>"),
            style = "
          font-family: 'Conthrax';
          font-size: 18px;
          color: #1948a6;
        "
          )
        ),
        
        p(
          "I am a Sophomore Computer Science Student at the University of the Philippines - Los Baños. 
       While the work is tedious, I enjoy bringing niche and quirky designs into real life!",
          style = "
        font-size: 14px;
        margin: 10px 10px 5px 10px;
        text-align: justify;
        line-height: 1.4;
      "
        ),
        
        div(
          style = "
        display: flex;
        justify-content: center;
        align-items: center;
        margin-top: 12px;
        gap: 6px;
        font-size: 14px;
        color: #1948a6;
      ",
          
          icon("envelope"),
          
          tags$a(
            href = "mailto:jbcalleja@up.edu.ph",
            "jbcalleja@up.edu.ph",
            style = "color: #1948a6; text-decoration: none;"
          )
        )
      )
    ),
    # panel that shows table iteration, selected inputs, etc
    mainPanel(
      width = 8,
      
      div(class = "header-box", style = "margin-top: 0px;", "Your Input"),
      
      div(class = "content-box",
          verbatimTextOutput("selectedProjects")
      ),
      
      conditionalPanel(
        condition = "output.hasSolution",
        
        hr(),
        div(uiOutput("resultHeader")),
        
        conditionalPanel(
          condition = "output.isOptimal",
          
          hr(),
          div(class = "header-box", "Basic Solution"),
          div(class = "content-box", DTOutput("BasicSolution")),
          
          hr(),
          div(class = "header-box", "Project Breakdown"),
          div(class = "content-box", DTOutput("solutionTable")),
          
          hr(),
          div(class = "header-box", "Constraint Verification"),
          div(class = "content-box", DTOutput("constraintTable"))
        ),
        
        hr(),
        div(class = "header-box", "Tableau and Iteration Details"),
        
        div(class = "content-box",
            checkboxInput("showInitial", "Show Initial Tableau", FALSE),
            checkboxInput("showIterations", "Show All Iterations", FALSE),
            checkboxInput("showFinal", "Show Final Tableau", TRUE)
        ),
        
        conditionalPanel(
          condition = "input.showIterations",
          div(class = "header-box", "Iteration Details"),
          div(class = "content-box", uiOutput("iterationDetails"))
        ),
        
        conditionalPanel(
          condition = "input.showInitial",
          div(class = "header-box", "Initial Tableau"),
          div(class = "content-box", DTOutput("initialTableau"))
        ),
        
        conditionalPanel(
          condition = "input.showFinal",
          div(class = "header-box", "Final Tableau"),
          div(class = "content-box", DTOutput("finalTableau"))
        )
      )
    )
  ),
  
  tags$script(HTML("
    Shiny.addCustomMessageHandler('fadeGreeting', function(message) {
      $('#enterApp').addClass('clicked');
      setTimeout(function() {
        $('.greeting-content').addClass('fade-out');
      }, 300);
      setTimeout(function() {
        $('#greetingPage').addClass('fade-out');
      }, 600);
      setTimeout(function() {
        $('#greetingPage').addClass('hidden');
        $('#mainContent').addClass('show');
      }, 0);
    });

    $(document).ready(function() {
      $('#mainContent').removeClass('show');
    });
  "))
)