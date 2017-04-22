library(shiny)
library(shinysky)
library(igraph)
library(RColorBrewer)

companyNames = read.csv("graph_edgelist.csv", stringsAsFactors = FALSE, header = FALSE)
industryNames = read.csv("Industry_Final.csv", stringsAsFactors = FALSE, header = FALSE)

g = graph(edges = companyNames$V1, directed = TRUE)
g = simplify(g, remove.multiple = TRUE, remove.loops = TRUE)
numSuppliers = degree(g, v = V(g), mode = "in")
numCustomers = degree(g, v = V(g), mode = "out")
e = eigen_centrality(g, directed = TRUE, scale = TRUE)
b = read.csv("betweenness.csv", stringsAsFactors = FALSE, header = FALSE)
layoutChoices = c("layout.auto", "layout.circle", "layout.drl", "layout.fruchterman.reingold", "layout.fruchterman.reingold.grid", "layout.gem", "layout.graphopt", "layout.grid", "layout.grid.3d", "layout.kamada.kawai", "layout.lgl", "layout.mds", "layout.random", "layout.reingold.tilford", "layout.sphere", "layout.spring", "layout.star", "layout.svd")

# Define UI for application that draws a histogram
ui <- fluidPage(
  titlePanel("Networks"),
  
  sidebarLayout(
    sidebarPanel(
      textInput.typeahead(id = "company", placeholder = "Type company name", local = data.frame(name=c(unique(companyNames$V1))), tokens = c(1:length(unique(companyNames$V1))), valueKey = "name", template = HTML("<p class='repo-language'>{{info}}</p> <p class='repo-name'>{{name}}</p>")),
      
      selectInput(inputId = "order", label = "Number of layers", choices = c(1:10)),
      
      selectInput(inputId = "layout", label = "Choose layout", choices = layoutChoices, multiple = FALSE),
      
      selectInput(inputId = "mode", label = "Choose mode", choices = c("Both", "Suppliers", "Customers"), multiple = FALSE)
    ),
    
    mainPanel(
      plotOutput(outputId = "network", width = "100%", height = "600px", click = "node_click"),
      fluidRow(
        splitLayout(cellWidths = c("60%","35%"), verbatimTextOutput(outputId = "companyDetails"), verbatimTextOutput(outputId = "graphParameters"))
      )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   output$network = renderPlot({
     mode = "all"
     if(input$mode == "Suppliers") {
       mode = "in"
     } else if(input$mode == "Customers") {
       mode = "out"
     } else {
       mode = "all"
     }
     
     g.sub = make_ego_graph(graph = g, order = as.integer(input$order), nodes = which(V(g)$name == input$company), mode = mode)
     
     set.seed(123)
     l = switch(input$layout,
                layout.auto = layout.auto(g.sub[[1]]),
                layout.circle = layout.circle(g.sub[[1]]),
                layout.drl = layout.drl(g.sub[[1]]),
                layout.fruchterman.reingold = layout.fruchterman.reingold(g.sub[[1]]),
                layout.fruchterman.reingold.grid = layout.fruchterman.reingold.grid(g.sub[[1]]),
                layout.gem = layout.gem(g.sub[[1]]),
                layout.graphopt = layout.graphopt(g.sub[[1]]),
                layout.grid = layout.grid(g.sub[[1]]),
                layout.grid.3d = layout.grid.3d(g.sub[[1]]),
                layout.kamada.kawai = layout.kamada.kawai(g.sub[[1]]),
                layout.lgl = layout.lgl(g.sub[[1]]),
                layout.mds = layout.mds(g.sub[[1]]),
                layout.random = layout.random(g.sub[[1]]),
                layout.reingold.tilford = layout.reingold.tilford(g.sub[[1]]),
                layout.sphere = layout.sphere(g.sub[[1]]),
                layout.spring = layout.spring(g.sub[[1]]),
                layout.star = layout.star(g.sub[[1]]),
                layout.svd = layout.svd(g.sub[[1]])
                )
     
     industries = unique(industryNames$V2[industryNames$V1 %in% V(g.sub[[1]])$name])
     colorPalette = colorRampPalette(c("blue","red"))(length(industries))
     color = unlist(sapply(V(g.sub[[1]])$name, function(x) return(colorPalette[which(industries == industryNames$V2[industryNames$V1 == x])])))
     
     vertexSize = sapply(V(g.sub[[1]])$name, function(x) if(x == input$company) return(30) else return(20))
     
     plot.igraph(g.sub[[1]], vertex.label = NA, vertex.size = vertexSize, layout = l, vertex.color = color, rescale = FALSE, xlim = c(min(l[,1]), max(l[,1])), ylim = c(min(l[,2]), max(l[,2])))
     
     
     
     output$companyDetails = renderText({
       if(is.null(input$node_click$x)) return("")
       else {
         x = l[,1]
         y = l[,2]
         dist = (x - input$node_click$x)^2 + (y - input$node_click$y)^2
         name = V(g.sub[[1]])$name[which.min(dist)]
         industry = industryNames$V2[industryNames$V1 == name]
         eigenCentrality = e$vector[name]
         betweennessCentrality = b$V2[b$V1 == name]
         clusteringCoefficient = transitivity(g, type = "undirected", vids = which(V(g)$name == name))
         return(paste(paste("Company:\t\t\t", name, sep = ""), paste("Industry:\t\t\t", industry, sep = ""), paste("Number of Suppliers:\t\t", numSuppliers[name], sep = ""), paste("Number of Customers:\t\t", numCustomers[name], sep = ""), paste("Eigen Centrality:\t\t", eigenCentrality, sep = ""), paste("Betweenness Centrality:\t\t", betweennessCentrality, sep = ""), paste("Clustering Coefficient:\t\t", clusteringCoefficient, sep = ""), sep = "\n"))
       }
     })
   })
   
   output$graphParameters = renderText({
     if(is.null(input$node_click$x)) return("")
     else {
       max_e = max(e$vector)
       max_b = max(b$V2)
       return(paste(paste("Max Eigen Centrality: ", max_e, sep = ""), paste("Max Betweenness: ", max_b, sep = ""), "Characteristic Path Length: 6.817057", sep = "\n"))
     }
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

