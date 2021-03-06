library(motley)
library(ggplot2)
library(ggforce)

layout <- get_layout(read.csv("fig1_between_layout.csv", header = FALSE, stringsAsFactors = FALSE))

df_nodes <-  data.frame(node_id = 1:length(layout$param), param = layout$param, stringsAsFactors = FALSE)
df_nodes$shape <- "rect"
df_nodes$shape[grepl("^(ri|s|Joint|Relative)", df_nodes$param)] <- "oval"
df_nodes$label <- df_nodes$param
#df_nodes$param <- df_nodes$node_id
df_edges <- data.frame(matrix(c(
  3, 1, "last", "1", 
  3, 2, "last", "1", 
  
  5, 1, "last", "+1", 
  5, 2, "last", "-1", 
  
  3, 6, "last", "", 
  3, 7, "last", "", 
  3, 8, "last", "", 
  3, 9, "last", "", 
  3, 10, "last", "", 
  3, 11, "last", "", 
  3, 12, "last", "",   
  
  4, 6, "last", "", 
  4, 7, "last", "", 
  4, 8, "last", "", 
  4, 9, "last", "", 
  4, 10, "last", "", 
  4, 11, "last", "", 
  4, 12, "last", "",   
  
  5, 6, "last", "", 
  5, 7, "last", "", 
  5, 8, "last", "", 
  5, 9, "last", "", 
  5, 10, "last", "", 
  5, 11, "last", "", 
  5, 12, "last", ""
  ), ncol = 4, byrow = TRUE), stringsAsFactors = FALSE)
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

#df_nodes[grepl("^s(EM|ME|EF|FE)", df_nodes$param), c("x", "node_xmin", "node_xmax")] <- df_nodes[grepl("^s(EM|ME|EF|FE)", df_nodes$param), c("x", "node_xmin", "node_xmax")]+.25


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

#df_edges$edge_xmin[df_edges$label == "1"] <- 7
# Make plot ---------------------------------------------------------------
df_edges$edge_xmax[df_edges$edge_xmax == 4.5] <- 5
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
  p <- p + geom_point(data = df_nodes[df_nodes$shape == "oval" & !df_nodes$param %in% c("Joint", "Relative"), ], aes(x = x, y = y), fill = "white", colour = "black", size = 14, shape = 21) + 
    geom_ellipse(data = df_nodes[df_nodes$param %in% c("Joint", "Relative"), ], aes(x0 = x, y0 = y,
                                                                           a = .5*ellipses_a,
                                                                           b = .5*ellipses_b, angle = 0), fill = "white", colour = "black")
}
p <- p + geom_text(data = df_nodes, aes(x = x, y = y, label = label), size = text_size) 
move_by = -.24
df_cors <- data.frame(t(mapply(function(from, to){
  c(unlist(df_nodes[df_nodes$param == from, c("y", "x")])+c(0,move_by),
    unlist(df_nodes[df_nodes$param == to, c("y", "x")])+c(0,move_by))
}, from = c("riM", "riM", "riE"), to = c("riE", "riF", "riF"), USE.NAMES = FALSE)))
names(df_cors) <- c("ystart", "xstart", "ymax", "xmax")
p <- p + geom_curve(data = df_cors, aes(x = xstart, y = ystart, xend = xmax, yend = ymax), curvature = .5, linetype = 2) 
p <- p + scale_x_continuous(expand = expand_scale(mult = c(0, .05)))
df_edges$text_x[c(2:3)] <- 1.5
df_edges$text_y[c(2:3)] <- c(6.2,7.8)
p <- p + geom_label(data = df_edges[!df_edges$label == "", ],
           aes(x = text_x, y = text_y, label = label), size = text_size, fill = "white", label.size = NA)+ theme(axis.line = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(), legend.position = "none",
      panel.background = element_blank(), panel.border = element_blank(),
      panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
      plot.background = element_blank(),
      plot.title = element_text(hjust = 0.5)) + labs(title = "Between-family level")
p_between<- p

#ggsave("Figure1between.png", p, width = 5, height = 5)
