
require(dplyr)
require(ggplot2)
require(shadowtext)
require(nCov2019)

res <- query()
y <- res$historical
d <- y["global"]

time = as.Date("2020-03-19")
dd <- filter(d, date == time) %>% 
  arrange(desc(cases)) 

dd = dd[1:40, ]
dd$country = factor(dd$country, levels=dd$country)

dd$angle = 1:40 * 360/40
require(ggplot2)
p <- ggplot(dd, aes(country, cases, fill=cases)) + 
  geom_col(width=1, color='grey90') + 
  geom_col(aes(y=I(5)), width=1, fill='grey90', alpha = .2) +       
  geom_col(aes(y=I(3)), width=1, fill='grey90', alpha = .2) +    
  geom_col(aes(y=I(2)), width=1, fill = "white") +
  scale_y_log10() + 
  scale_fill_gradientn(colors=c("darkgreen", "green", "orange", "firebrick","red"), trans="log") + 
  geom_text(aes(label=paste(country, cases, sep="\n"), 
                y = cases *.8, angle=angle), 
            data=function(d) d[d$cases > 700,], 
            size=3, color = "white", fontface="bold", vjust=1)  + 
  geom_text(aes(label=paste0(cases, " cases ", country), 
                y = max(cases) * 2, angle=angle+90), 
            data=function(d) d[d$cases < 700,], 
            size=3, vjust=0) + 
  coord_polar(direction=-1) + 
  theme_void() + 
  theme(legend.position="none") +
  ggtitle("COVID19 global trend", time)

p1 = ggplotify::as.ggplot(p, scale=1.2)



y <- res$historical
d <- y["global"]



dd <- d %>% 
  as_tibble %>%
  filter(cases > 1000000) %>%
  group_by(country) %>%
  mutate(days_since_1m = as.numeric(date - min(date))) %>%
  ungroup 

breaks=c(1000, 10000, 20000, 50000, 500000,500000,5000000,20000000)


p <- ggplot(dd, aes(days_since_1m, cases, color = country)) +
  geom_smooth(method='lm', aes(group=1),
              data = dd, 
              color='grey10', linetype='dashed') +
  geom_line(size = 0.8) +
  geom_point(pch = 21, size = 1) +
  scale_y_log10(expand = expansion(add = c(0,0.1)), 
                breaks = breaks, labels = breaks) +
  scale_x_continuous(expand = expansion(add = c(0,1))) +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "none",
    plot.margin = margin(3,15,3,3,"mm")
  ) +
  coord_cartesian(clip = "off") +
  geom_shadowtext(aes(label = paste0(" ",country)), hjust=0, vjust = 0, 
                  data = . %>% group_by(country) %>% top_n(1,days_since_1m),
                  bg.color = "white") +
  labs(x = "Number of days since 1,000,000th case", y = "", 
       subtitle = "Total number of cases")


require(cowplot)
pp <- plot_grid(p2, p, ncol=1, labels=c("B", "C"), 
    rel_heights=c(.7, 1))  
ggsave(pp, filename = "nCov2019.jpg", width=16, height=11)

