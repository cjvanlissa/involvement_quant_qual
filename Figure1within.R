library(motley)
library(ggplot2)
library(ggforce)

layout <- get_layout(read.csv("fig1_within_layout.csv", header = FALSE, stringsAsFactors = FALSE))

df_nodes <-  data.frame(node_id = 1:length(layout$param), param = layout$param, stringsAsFactors = FALSE)
df_nodes$shape <- "rect"
df_nodes$shape[grepl("^(ri|s)", df_nodes$param)] <- "oval"
df_nodes$label <- df_nodes$param

df_edges <- data.frame(matrix(c(
  1, 11, "last", "", 
  1, 12, "last", "", 
  #1, 13, "last", "", 
  
  2, 11, "last", "", 
  2, 12, "last", "", 
  2, 13, "last", "", 
  
  #3, 11, "last", "", 
  3, 12, "last", "", 
  3, 13, "last", "", 
  
  14, 11, "last", "",
  15, 12, "last", "",
  16, 13, "last", ""), ncol = 4, byrow = TRUE), stringsAsFactors = FALSE)
names(df_edges) <- c("from", "to", "arrow", "label")

df_edges$from <- df_nodes$param[as.numeric(df_edges$from)]
df_edges$to <- df_nodes$param[as.numeric(df_edges$to)]
df_edges$label[grepl("^ri", df_edges$from)] <- "1"
prep <- prepare_plot_sem(nodes = df_nodes, layout = layout, edges = df_edges)

# Plot --------------------------------------------------------------------
x <- prep
df_nodes <- x$nodes
df_edges <- x$edges
rect_width <- x$rect_width
rect_height <- x$rect_height
ellipses_a <- x$ellipses_a
ellipses_b <- x$ellipses_b
spacing_x <- x$spacing_x
spacing_y <- x$spacing_y
text_size <- x$text_size

df_nodes[grepl("^s(EM|ME|EF|FE)", df_nodes$param), c("x", "node_xmin", "node_xmax")] <- df_nodes[grepl("^s(EM|ME|EF|FE)", df_nodes$param), c("x", "node_xmin", "node_xmax")]+.25


connect_points <- setNames(data.frame(t(
  mapply(function(from, to, startpoint, endpoint){
    c(
      switch(startpoint,
             right = df_nodes$node_xmax[which(df_nodes$node_id == from)],
             left =  df_nodes$node_xmin[which(df_nodes$node_id == from)],
             df_nodes$x[which(df_nodes$node_id == from)]),
      switch(startpoint,
             top = df_nodes$node_ymax[which(df_nodes$node_id == from)],
             bottom =  df_nodes$node_ymin[which(df_nodes$node_id == from)],
             df_nodes$y[which(df_nodes$node_id == from)]),
      switch(endpoint,
             right = df_nodes$node_xmax[which(df_nodes$node_id == to)],
             left =  df_nodes$node_xmin[which(df_nodes$node_id == to)],
             df_nodes$x[which(df_nodes$node_id == to)]),
      switch(endpoint,
             top = df_nodes$node_ymax[which(df_nodes$node_id == to)],
             bottom =  df_nodes$node_ymin[which(df_nodes$node_id == to)],
             df_nodes$y[which(df_nodes$node_id == to)])
    )},
    from = df_edges$from,
    to = df_edges$to,
    startpoint = df_edges$connect_from,
    endpoint = df_edges$connect_to
  )
)), c("edge_xmin", "edge_ymin", "edge_xmax", "edge_ymax"))
df_edges <- cbind(df_edges, connect_points)

df_edges <- cbind(df_edges, setNames(data.frame(t(apply(connect_points, 1, function(x){(x[1:2]+x[3:4])/2}))), c("text_x", "text_y")))

df_edges$edge_xmin[df_edges$label == "1"] <- 7
# Make plot ---------------------------------------------------------------

p <- ggplot(NULL)
p <- p +
  geom_segment(data = df_edges[!df_edges$arrow %in% c("none", "curve"), ], aes(x = edge_xmin,
                                                                               xend = edge_xmax,
                                                                               y = edge_ymin,
                                                                               yend = edge_ymax
  ), arrow = arrow(angle = 25, length = unit(.1, "inches"), ends = df_edges[!df_edges$arrow == "none", ]$arrow, type = "closed"), arrow.fill = "black") +
  geom_segment(data = df_edges[df_edges$arrow == "none", ], aes(x = edge_xmin,
                                                                xend = edge_xmax,
                                                                y = edge_ymin,
                                                                yend = edge_ymax
  ))
  #
  # # Part segment, part curve ------------------------------------------------
#     browser()
#     geom_curve(data = df_edges[df_edges$arrow == "curve", ],
#                aes(x = edge_xmin,
#                    xend = edge_xmax,
#                    y = edge_ymin,
#                    yend = edge_ymax)) +
df_nodes$linet <- 1
df_nodes$linet[grepl("T-1$", df_nodes$param)] <- 2
df_nodes$linet[df_nodes$shape == "oval"] <- 3
if(any(df_nodes$shape == "rect")){
  p <- p + geom_rect(data = df_nodes[df_nodes$shape == "rect", ], aes(xmin = node_xmin, xmax = node_xmax, ymin = node_ymin, ymax = node_ymax), fill = "white", colour = "black", linetype = df_nodes[df_nodes$shape == "rect", ]$linet)
}
if(any(df_nodes$shape == "oval")){
  p <- p + geom_point(data = df_nodes[df_nodes$shape == "oval", ], aes(x = x, y = y), fill = "white", colour = "black", size = 14, shape = 21)
}
p <- p + geom_text(data = df_nodes, aes(x = x, y = y, label = label), size = text_size) 
p <- p + geom_label(data = df_edges[df_edges$label == "1", ],
           aes(x = text_x, y = text_y, label = label), size = text_size, fill = "white", label.size = NA)+ theme(axis.line = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(), legend.position = "none",
      panel.background = element_blank(), panel.border = element_blank(),
      panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
      plot.background = element_blank(),
      plot.title = element_text(hjust = 0.5)) + labs(title = "Within-family level")
p_within <- p

#ggsave("Figure1within.png", p, width = 5, height = 5)
