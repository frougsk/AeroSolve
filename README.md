
# Pollution Reduction Simulator
An interactive Shiny application that uses **Simplex Method** to optimize pollution reduction strategies for the City of Greenvale. Built with a stunning Frutiger Aero aesthetic :)

---

## ✨ Features

### Core Functionality
- **30 Mitigation Projects**: Choose from various pollution reduction strategies including solar parks, wind farms, industrial retrofits, and more
- **10 Pollutant Types**: Optimize for CO2, NOx, SO2, PM2.5, CH4, VOC, CO, NH3, Black Carbon, and N2O
- **Simplex Algorithm**: Uses dual simplex method for linear programming optimization
- **Real-time Results**: Instant feasibility checking and cost calculations
- **Constraint Verification**: Automatically verifies if all pollution targets are met

### User Interface
- **Frutiger Aero Design**: Stunning glass morphism effects with Vista-era aesthetic
- **Interactive Tables**: Powered by DataTables for sorting and searching
- **Responsive Layout**: Works on various screen sizes
- **Progress Indicators**: Visual feedback during optimization
- **Modal Dialog**: Built-in user manual and about section

### Advanced Features
- **Iteration Viewer**: Step-by-step visualization of Simplex algorithm
- **Tableau Display**: View initial, final, and intermediate tableaus
- **Project Breakdown**: Detailed cost analysis per project
- **Export Ready**: All tables can be copied or exported

---

## 📦 Installation

### Prerequisites

```r
# R version 4.0.0 or higher recommended
R version 4.3.0 (2023-04-21) or later
```

### Required R Packages

```r
install.packages(c(
  "shiny",           # Core Shiny framework
  "shinyWidgets",    # Enhanced UI widgets
  "dplyr",           # Data manipulation
  "DT"               # Interactive tables
))
```

### Setup Instructions

1. **Download and Export the Project**

2. **Verify File Structure**
   ```
   Pollution-Reduction-Simulator/
   ├── Main.R              # Main application launcher
   ├── UI.R               # User interface definition
   ├── Server.R           # Server logic
   ├── Solve.R           # Data, functions, constants
   └── www/               # Static assets
       ├── Styles.css     # Custom CSS styling
       ├── frutigerbg.png # Background image
       ├── smiski.png     # Icon image
       └── fonts/         # Custom fonts
           ├── Frutiger Regular.ttf
           ├── Neuropol.otf
           ├── Nulshock Bd.otf
           ├── Conthrax.otf
           └── Sofachrome Rg It.otf
   ```

3. **Run the Application**
   ```r
   # Open app.R and click "Run App" in RStudio
   ```
---

## 🎯 How to Use

### Step-by-Step Guide

#### 1. **Select Projects**
- Browse the 30 available mitigation projects in the sidebar
- Each project shows its name and cost per unit
- Check the boxes for projects you want to include
- Use **"Check All"** to select all 30 projects
- Use **"Reset"** to clear your selection

#### 2. **Understand Project Types**
Projects are categorized by:
- **Energy**: Solar parks, wind farms, gas conversion
- **Transportation**: Bus replacement, EV infrastructure, rail electrification
- **Industrial**: Scrubbers, efficiency retrofits, process changes
- **Residential**: Insulation, stove programs, LPG conversion
- **Environmental**: Reforestation, wetlands restoration, tree canopy

#### 3. **Run Optimization**
- Click **"Let's Optimize!"** button
- Wait for the progress indicator (usually < 2 seconds)
- Algorithm will determine the optimal solution

#### 4. **Interpret Results**

##### Feasible Solution ✅
    You'll see:
    - Total Cost: Minimum budget needed
    - Basic Solution: Optimal variable values (x1, x2, ..., xn)
    - Project Breakdown: How many units of each project
    - Constraint Verification: Check if all pollution targets are met

##### Infeasible Solution ❌
    - No combination of selected projects meets all constraints
    - Try selecting more projects or different combinations
    - Check which pollutant targets cannot be met

#### 5. **Explore Tableau Details** (Optional)
Toggle these options:
- **Show Initial Tableau**: Starting point of the algorithm
- **Show All Iterations**: Step-by-step Simplex process
- **Show Final Tableau**: Optimal tableau with solution

---

## 🐛 Troubleshooting

### Common Issues

#### "Error: could not find function 'runApp'"
**Solution**: Install Shiny
```r
install.packages("shiny")
```

#### CSS Not Loading
**Problem**: Styles don't apply
**Solution**: 
- Verify `www/styles.css` exists
- Check file path in `ui.R`: `includeCSS("www/styles.css")`
- Restart R session

#### Images Not Displaying
**Problem**: Background or icons missing
**Solution**:
- Files must be in `www/` folder
- Use relative paths: `src = "image.png"` not `src = "www/image.png"`
- Check file extensions (case-sensitive on Linux/Mac)

#### "[object Object]" in Output
**Problem**: DataTables not rendering
**Solution**: Use `renderUI()` instead of `renderDT()`:
```r
output$myTable <- renderUI({
  DT::datatable(...)
})
```

#### Infeasible Solution Every Time
**Problem**: No solution found
**Possible Causes**:
- Too few projects selected
- Projects don't cover all pollutant types
- Targets too high for selected projects
**Solution**: Select more diverse projects, especially:
- CH4: Waste Methane Capture, Agricultural Methane Reduction
- VOC: Industrial VOC projects
- BC: Black Carbon reduction projects

#### Slow Performance
**Problem**: App takes too long
**Solutions**:
- Reduce number of selected projects
- Use fewer iteration displays
- Close other R processes
- Increase R memory limit: `memory.limit(size = 8000)`

#### For other problems not mentioned above
- Contact me @jbcalleja@up.edu.ph
  
---

### Design Inspiration
- **Frutiger Aero Style**: Windows Vista/7 design language
