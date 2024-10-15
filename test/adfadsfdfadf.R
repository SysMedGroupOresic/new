library(ggplot2)
library(cowplot)   # get_legend() & plot_grid() functions
library(patchwork) # blank plot: plot_spacer()
p1=p+scale_fill_manual(values=c("orange","blue",'white'),name=c(oute,'x'),labels=c(e1,e2,e3))
p2=p+scale_color_manual(name="Legend 1",values=e3)

leg1 <- get_legend(p1)
leg2 <- get_legend(p2)
blank_p <- plot_spacer() + theme_void()
# combine legend 1 & 2
leg12 <- plot_grid(leg1, leg2,
                   blank_p,
                   nrow = 3)

# combine all legends
leg123 <- plot_grid(leg12, blank_p,
                    ncol = 2)

final_p <- plot_grid(p,
                     leg123,
                     nrow = 1,
                     align = "h",
                     axis = "t",
                     rel_widths = c(1, 0.3)
)

print(final_p)