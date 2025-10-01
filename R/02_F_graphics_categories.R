#Grafico categorias de leucaena leucocephala
install.packages("networkD3")
install.packages("htmlwidgets")
library(networkD3)
library(htmlwidgets)
# Nodes — agora com grupo correto!
nodes <- data.frame(
  name = c(
    "Leucaena
    leucocephala",          # 0
    "Ecology", "Fitochemistry", "Restoration/revegetation","Genetics","Medicine","Industry","Agriculture","Others",  # 1-8
    "Ecological interactions", "Ecosystem", "Autoecology","Community", "Population", "Biogeography", "Invasion biology", # 9-15
    "Agriculture system/management", "Animal nutrition", "Production/yield","Pests","Soil/fertilization"  # 16-20
  ),
  group = c(
    "Leucaena 
    leucocephala",           # Leucaena
    "Ecology", "Fitochemistry", "Restoration/revegetation","Genetics","Medicine","Industry","Agriculture","Others",  # Categorias principais
    rep("Ecology", 7),   # Subcategorias Ecology
    rep("Agriculture", 5)     # Subcategorias Agriculture (corrigido)
  )
)

# Links com grupo já correto
links <- data.frame(
  source = c(
    0, 0, 0, 0, 0, 0, 0, 0,     # Leucaena -> Categorias principais
    1, 1, 1, 1, 1, 1, 1,        # Ecology -> Subcategorias
    7, 7, 7, 7, 7               # Agriculture -> Subcategorias (corrigido de 2 para 7)
  ),
  target = c(
    1, 2, 3, 4, 5, 6, 7, 8,
    9, 10, 11, 12, 13, 14, 15,
    16, 17, 18, 19, 20
  ),
  value = c(
    1057, 282, 81, 87, 123,454, 2273, 8,
    589, 37, 275, 68, 3, 26, 59,
    605, 1235, 183, 57, 193
  ),
  group = c(
    "Ecology", "Fitochemistry", "Restoration/revegetation","Genetics","Medicine","Industry","Agriculture","Others",  # Categorias principais
    rep("Ecology", 7),   # Subcategorias Ecology
    rep("Agriculture", 5)   # Subcategorias Agriculture
  )
)

# Sankey com LinkGroup e NodeGroup corrigido
p<-sankeyNetwork(
  Links = links,
  Nodes = nodes,
  Source = "source",
  Target = "target",
  Value = "value",
  NodeID = "name",
  LinkGroup = "group",
  NodeGroup = "group",
  fontSize = 12,
  nodeWidth = 20,
  nodePadding = 16,
  colourScale = JS(
    "d3.scaleOrdinal()
      .domain(['Leucaena leucocephala', 'Ecology', 'Fitochemistry', 'Restoration/revegetation', 'Genetics', 'Medicine', 'Industry', 'Agriculture', 'Others'])
      .range(['steelblue', '#1B9E77', '#D95F02', '#7570B3', '#E7298A', '#66A61E', '#E6AB02', '#A6761D', '#666666'])"),
    fontFamily = "Arial",
    margin = list(top = 50, right = 50, bottom = 50, left = 50),
    height = 600,
    width = 900)
onRender(p, "
  function(el) {
    // Torna o texto negrito
    d3.select(el).selectAll('.node text')
      .style('font-weight', 'bold');
  }
","
  function(el) {
    d3.select(el).selectAll('.node text')
      .attr('x', function(d) {
        return d.x + d.dx + 6;  // desloca para depois do retângulo
      })
      .attr('text-anchor', 'start') // alinha o texto à esquerda do novo ponto x
      .style('font-weight', 'bold'); // opcional: deixa o texto em negrito
  }
")
#Grafico arrumado
# Nodes — agora com grupo correto!
nodes <- data.frame(
  name = c(
    "",          # 0
    "Agriculture(2274)","Ecology(1061)","Industry(454)","Fitochemistry(282)","Medicine(123)","Genetics(87)","Restoration/revegetation(81)","General reviews(3)",  # 1-8
    "Animal nutrition(1236)","System/management(605)","Soil/fertilization(193)","Production/yield(183)","Pests(57)", # 9-13
    "Ecological interactions(590)","Autoecology(278)","Community(68)","Invasion biology(59)","Ecosystem(37)","Biogeography(26)","Population(3)"  # 14-20
  ),
  group = c(
    "",           # Leucaena
    "Agriculture(2274)","Ecology(1061)","Industry(454)","Fitochemistry(282)","Medicine(123)","Genetics(87)","Restoration/revegetation(81)","General reviews(3)",  # Categorias principais
    rep("Agriculture(2274)", 5),  # Subcategorias Agriculture (corrigido)
    rep("Ecology(1061)", 7)   # Subcategorias Ecology
  )
)

# Links com grupo já correto
links <- data.frame(
  source = c(
    0, 0, 0, 0, 0, 0, 0, 0,     # Leucaena -> Categorias principais
    1, 1, 1, 1, 1,         # Agriculture -> Subcategorias  
    2, 2, 2, 2, 2, 2, 2       # Ecology -> Subcategorias         
  ),
  target = c(
    1, 2, 3, 4, 5, 6, 7, 8,
    9, 10, 11, 12, 13,
    14, 15, 16, 17, 18, 19, 20
  ),
  value = c(
    2274, 1061, 454, 282, 123, 87, 81, 3,
    1236, 605, 193, 183, 57,
    590, 278, 68, 59, 37, 26, 3
    ),
  group = c(
    "Agriculture(2274)","Ecology(1061)","Industry(454)","Fitochemistry(282)","Medicine(123)","Genetics(87)","Restoration/revegetation(81)","General reviews(3)",  # Categorias principais
    rep("Agriculture(2274)", 5),    # Subcategorias Agriculture
    rep("Ecology(1061)", 7)   # Subcategorias Ecology
    
  )
)

# Sankey com LinkGroup e NodeGroup corrigido
# Comandos: importante, o sinkRight=FALSE faz com que o texto esteja a direita dos nos
# Nodewidth é a largura do nó, nodePadding a separação, quanto maior menor "tinta"
p<-sankeyNetwork(
  Links = links,
  Nodes = nodes,
  Source = "source",
  Target = "target",
  Value = "value",
  NodeID = "name",
  LinkGroup = "group",
  NodeGroup = "group",
  fontSize = 12,
  nodeWidth = 10,
  nodePadding = 32,
  colourScale = JS("
  d3.scaleOrdinal()
                   .domain(['','Agriculture(2274)','Ecology(1061)', 'Industry(454)', 'Fitochemistry(282)', 'Genetics(87)', 'Medicine(123)', 'Restoration/revegetation(81)', 'General reviews(3)', ])
                   .range(['#666666','#998650','#4D9D5A','#FF331F','#C97064','#F6AEC2','#ED90E1','#D8F793','#C38A6B'])"),
  fontFamily = "Arial",
  margin = list(top = 50, right = 50, bottom = 50, left = 50),
  height = 600,
  width = 900,
  sinksRight=FALSE)
onRender(p, "
  function(el) {
    // Torna o texto negrito
    d3.select(el).selectAll('.node text')
      .style('font-weight', 'bold');
  }
")
saveWidget(p, "sankey.html", selfcontained = TRUE)
#Passo a passo para o gráfico de pizza
#Grafico de pizza para as categorias de interações ecológicas
# load library
library(ggplot2)

# Create test data.
data <- data.frame(
  category=c("fungi-plant", "insect-plant", "mycorrhiza","plant-plant","rhizobia","vert-plant"),
  count=c(16, 91, 100, 65, 302, 16)
)

# Compute percentages
data$fraction <- data$count / sum(data$count)

# Compute the cumulative percentages (top of each rectangle)
data$ymax <- cumsum(data$fraction)


# Compute the bottom of each rectangle
data$ymin <- c(0, head(data$ymax, n=-1))

# Compute label position
data$labelPosition <- (data$ymax + data$ymin) / 2

# Compute a good label
data$label <- paste0(data$category, "\n  ", data$count)

#ajustado o texto
ggplot(data, aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 3, fill = category)) +
  geom_rect() +
  geom_text(
    x = 5,  # Aumenta para colocar o texto fora do gráfico
    aes(y = labelPosition, label = label),
    size = 4,
    color = "black",
    fontface = "bold",
    family = "Arial"
  ) +
  scale_fill_brewer(palette = 5) +
  coord_polar(theta = "y") +
  xlim(c(-1, 5)) +  # Aumenta o limite para acomodar os textos fora
  theme_void() +
  theme(legend.position = "none")
# Dados
data <- data.frame(
  category = c("fungi-plant", "insect-plant", "mycorrhiza", "plant-plant", "rhizobia", "vert-plant"),
  count = c(16, 91, 100, 65, 302, 16)
)

# Cálculos
data$fraction <- data$count / sum(data$count)
data$ymax <- cumsum(data$fraction)
data$ymin <- c(0, head(data$ymax, n = -1))
data$labelPosition <- (data$ymax + data$ymin) / 2
data$label <- paste0(data$category, "\n  ", data$count)

# Corrige sobreposição de "fungi-plant" e "vert-plant"
# movendo manualmente levemente os rótulos
data$labelPosition[data$category == "fungi-plant"] <- data$labelPosition[data$category == "fungi-plant"] - 0.01
data$labelPosition[data$category == "vert-plant"]  <- data$labelPosition[data$category == "vert-plant"] + 0.01

# Gráfico
plot<-ggplot(data, aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 3, fill = category)) +
  geom_rect() +
  geom_text(
    x = 5.9,  # Mais afastado para evitar corte
    aes(y = labelPosition, label = label),
    size = 4,
    color = "black",
    fontface = "bold",
    family = "Arial"
  ) +
  scale_fill_brewer(palette = 5) +
  coord_polar(theta = "y") +
  xlim(c(-1, 6)) +  # Limite estendido para dar espaço
  theme_void() +
  theme(legend.position = "none")
saveWidget(plot, "sankey1.html", selfcontained = TRUE)
