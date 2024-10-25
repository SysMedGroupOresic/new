install.packages("remotes")
remotes::install_github("davidsjoberg/ggsankey")

library(ggsankey)

library(ggplot2)
library(dplyr)

#Create data which can be used for Sankey
set.seed(111)

# Simple

t1 <- sample(x = c("Hosp A", "Hosp B", "Hosp C","Hosp D") , size = 100, replace=TRUE)
t2 <- sample(x = c("Male", "Female")   , size = 100, replace=TRUE)
t3 <- sample(x = c("Survived", "Died") , size = 100, replace=TRUE)

d <- data.frame(cbind(t1,t2,t3))
names(d) <- c('Hospital', 'Gender',  'Outcome')

head(d)

# Step 1
df <- d %>%
  make_long(Hospital, Gender, Outcome)
df

# Chart 1
pl <- ggplot(df, aes(x = x
                     , next_x = next_x
                     , node = node
                     , next_node = next_node
                     , fill = factor(node)
                     , label = node)
)
pl <- pl +geom_sankey(flow.alpha = 0.5
                      , node.color = "black"
                      ,show.legend = FALSE)
pl <- pl +geom_sankey_label(size = 3, color = "black", fill= "white", hjust = -0.5)
pl <- pl +  theme_bw()
pl <- pl + theme(legend.position = "none")
pl <- pl +  theme(axis.title = element_blank()
                  , axis.text.y = element_blank()
                  , axis.ticks = element_blank()  
                  , panel.grid = element_blank())
pl <- pl + scale_fill_viridis_d(option = "inferno")
pl <- pl + labs(title = "Sankey diagram using ggplot")
pl <- pl + labs(subtitle = "using  David Sjoberg's ggsankey package")
pl <- pl + labs(caption = "@techanswers88")
pl <- pl + labs(fill = 'Nodes')
pl

# How to show the data labels with the values (count) of each node.
# We create the count of the data grouped by node (dataset dagg)and then merge the data back to our dataset which has been created by using the make_long command ( dataset df).

#Create data which can be used for Sankey
set.seed(111)

# Simple

t1 <- sample(x = c("Hosp A", "Hosp B", "Hosp C","Hosp D") , size = 100, replace=TRUE)
t2 <- sample(x = c("Male", "Female")   , size = 100, replace=TRUE)
t3 <- sample(x = c("Survived", "Died") , size = 100, replace=TRUE)

d <- data.frame(cbind(t1,t2,t3))
names(d) <- c('Hospital', 'Gender',  'Outcome')


# Step 1
df <- d %>%
  make_long(Hospital, Gender, Outcome)

# Step 2
dagg <- df%>%
  dplyr::group_by(node)%>%
  tally()


# Step 3
df2 <- merge(df, dagg, by.x = 'node', by.y = 'node', all.x = TRUE)

# Chart 2
pl <- ggplot(df2, aes(x = x
                      , next_x = next_x
                      , node = node
                      , next_node = next_node
                      , fill = factor(node)
                      
                      , label = paste0(node," n=", n)
)
) 
pl <- pl +geom_sankey(flow.alpha = 0.5,  color = "gray40", show.legend = TRUE)
pl <- pl +geom_sankey_label(size = 3, color = "white", fill= "gray40", hjust = -0.2)

pl <- pl +  theme_bw()
pl <- pl + theme(legend.position = "none")
pl <- pl +  theme(axis.title = element_blank()
                  , axis.text.y = element_blank()
                  , axis.ticks = element_blank()  
                  , panel.grid = element_blank())
pl <- pl + scale_fill_viridis_d(option = "inferno")
pl <- pl + labs(title = "Sankey diagram using ggplot")
pl <- pl + labs(subtitle = "using  David Sjoberg's ggsankey package")
pl <- pl + labs(caption = "@techanswers88")
pl <- pl + labs(fill = 'Nodes')


pl




1 <- sample(x = c("Hosp A", "Hosp B", "Hosp C","Hosp D") , size = 100, replace=TRUE)
t2 <- sample(x = c("Male", "Female")   , size = 100, replace=TRUE)
t3 <- sample(x = c("Survived", "Died") , size = 100, replace=TRUE)

d <- data.frame(cbind(t1,t2,t3))
names(d) <- c('Hospital', 'Gender',  'Outcome')

TotalCount = nrow(d)
# Step 1
df <- d %>%
  make_long(Hospital, Gender, Outcome)

# Step 2
dagg <- df%>%
  dplyr::group_by(node)%>%
  tally()

dagg <- dagg%>%
  dplyr::group_by(node)%>%
  dplyr::mutate(pct = n/TotalCount)


# Step 3
df2 <- merge(df, dagg, by.x = 'node', by.y = 'node', all.x = TRUE)

pl <- ggplot(df2, aes(x = x
                      , next_x = next_x
                      , node = node
                      , next_node = next_node
                      , fill = factor(node)
                      
                      , label = paste0(node," n=", n, '(',  round(pct* 100,1), '%)' ))
)

pl <- pl +geom_sankey(flow.alpha = 0.5,  color = "gray40", show.legend = TRUE)
pl <- pl +geom_sankey_label(size = 2, color = "black", fill= "white", hjust = -0.1)

pl <- pl +  theme_bw()
pl <- pl + theme(legend.position = "none")
pl <- pl +  theme(axis.title = element_blank()
                  , axis.text.y = element_blank()
                  , axis.ticks = element_blank()  
                  , panel.grid = element_blank())

pl <- pl + labs(title = "Sankey diagram using ggplot")
pl <- pl + labs(subtitle = "using  David Sjoberg's ggsankey package")
pl <- pl + labs(caption = "@techanswers88")
pl <- pl + labs(fill = 'Nodes')

pl <- pl + scale_fill_manual(values = c('Died'    = "red"
                                        ,'Female'  ="red"
                                        ,'Hosp A'  = "red"
                                        
) )

pl

# https://rpubs.com/techanswers88/sankey-with-own-data-in-ggplot



# https://cran.r-project.org/web/packages/ggsankeyfier/vignettes/decorating.html
install.packages('ggsankeyfier')
library(ggsankeyfier)
library(ggplot2)
theme_set(theme_light())
data("ecosystem_services")

## Let's subset the example data to create a less cluttered
## Sankey diagram
es_sub <-
  ecosystem_services |>
  subset(RCSES > 0.005) |>
  pivot_stages_longer(c("activity_realm", "biotic_realm", "service_section"),
                      "RCSES", "service_section")

ggplot(
  data    = es_sub,
  mapping = aes(x = stage, y = RCSES, group = node,
                edge_id = edge_id, connector = connector, colour = stage)) +
  ## apply fill and alpha aesthetic only to edges (not the nodes)
  geom_sankeyedge(aes(alpha = RCSES, fill = service_section)) +
  geom_sankeynode() +
  guides(fill   = guide_legend(ncol = 1),
         alpha  = guide_legend(ncol = 1),
         colour = guide_legend(ncol = 1)) +
  theme(legend.position = "top")

pos <- position_sankey(v_space = "auto", order = "ascending", align = "justify")

p <-
  ggplot(
    data    = es_sub,
    mapping = aes(x = stage, y = RCSES, group = node,
                  edge_id = edge_id, connector = connector))

# p +
#   geom_sankeynode(position = pos) +
#   geom_segment(aes(col = service_section),
#                position = pos, stat = "sankeyedge",
#                arrow = arrow(length = unit(0.2, "cm")))

# p +
#   geom_sankeyedge(slope = 1, position = pos, mapping = aes(fill = service_section)) +
#   geom_sankeynode(position = pos)


# p +
#   geom_sankeyedge(aes(waist = RCSES, fill = service_section), position = pos) +
#   geom_sankeynode(position = pos)
# 
# p +
#   geom_sankeyedge(aes(waist = RCSES, fill = RCSES), position = pos) +
#   geom_sankeynode(position = pos) +
#   scale_waist_binned(guide = "legend") +
#   scale_fill_binned(guide = "legend")
# 
# 
# p +
#   geom_sankeyedge(aes(fill = RCSES), position = pos) +
#   geom_sankeynode(position = pos) +
#   theme_void()

p +
  geom_sankeyedge(aes(fill = RCSES), position = pos) +
  geom_sankeynode(position = pos) +
  scale_fill_viridis_c(option = "turbo")






## Let's subset the example data to create a less cluttered
## Sankey diagram


########this works!!!!!###########:
es_sub <-
  ecosystem_services |>
  subset(RCSES > 0.005) |>
  pivot_stages_longer(c("activity_realm", "biotic_realm", "service_section"), "RCSES", "service_section")

pos <- position_sankey(v_space = "auto", order = "ascending", align = "justify")

p <-
  ggplot(
    data    = es_sub,
    mapping = aes(x = stage, y = RCSES, group = node,
                  edge_id = edge_id, connector = connector))
p +
  geom_sankeyedge(aes(fill = RCSES), position = pos) +
  geom_sankeynode(position = pos) +
  scale_fill_viridis_c(option = "turbo")
########!!!!!###########

#Trying to imitate:

# to do it for some names in a vector named 'col_names'
# col_names <- colnames(hoi[,1:3])
# hoi[col_names] <- lapply(hoi[col_names] , factor)
# hoie=tibble(hoi) #https://stackoverflow.com/questions/9251326/convert-data-frame-column-format-from-character-to-factor


df22 <-
  hoi |>
  subset(Weight > -0.49) |>
  pivot_stages_longer(c("Contaminants", "Steroids", "Bile Acids or Lipids"), "Weight", "Bile Acids or Lipids")

pos <- position_sankey(v_space = "auto", order = "ascending", align = "justify")


p <-
  ggplot(
    data    = df22,
    mapping = aes(x = stage, y = Weight, group = node,
                  edge_id = edge_id, connector = connector))

# p=
#   ggplot(df22, aes(x = stage,   y = Weight, group = node, edge_id = edge_id, connector = connector)) +
#   geom_sankey(flow.alpha = 1, node.color = 1) +
#   # geom_sankey_label(size = 4.0, color = 1, fill = "white") + #
#   geom_sankey_label(size = 8.0, color = 1, fill = "white")+
#   # scale_fill_viridis_d(option = "H", alpha = 0.75) +
#   scale_fill_viridis_c(option = "turbo")+
#   # theme_sankey(base_size = 23) + #
#   theme_sankey(base_size = 28) + theme(legend.position = "none") +
#   # scale_fill_grey(start = 0.5, end = 0.5)+
#   theme(axis.text.x = element_text(hjust = 0.5, vjust=7,colour = 'black') )+ #https://stackoverflow.com/questions/38862303/customize-ggplot2-axis-labels-with-different-colors
#   theme(axis.title.x = element_blank())




# p +
#   geom_sankeyedge(aes(fill = Weight), position = pos) +
#   geom_sankeynode(position = pos) +
#   scale_fill_viridis_c(option = "turbo")
# 
# #https://cran.r-project.org/web/packages/ggsankeyfier/vignettes/positioning.html
# pos_text <- position_sankey(v_space = "auto", order = "descending", nudge_x = 0.1)
# p + geom_sankeyedge(aes(fill = Weight), position = pos) +
#   geom_sankeynode(position = pos) +
#   geom_text(aes(label = node), stat = "sankeynode", position = pos_text, hjust = 0, cex = 2) +
#   scale_x_discrete(expand = expansion(add = c(0.2, .6)))+
#   scale_fill_viridis_c(option = "turbo")


pos <- position_sankey(v_space = "auto", order = "descending")
p + geom_sankeyedge(aes(fill = Weight), position = pos) +
  geom_sankeynode(position = pos) +
  geom_text(aes(label = node), stat = "sankeynode", position = pos, cex = 2)+scale_fill_viridis_c(option = "turbo")



# p +
#   geom_sankeyedge(aes(fill = Weight), position = pos) +
#   geom_sankeynode(position = pos) +
#   theme_void()
