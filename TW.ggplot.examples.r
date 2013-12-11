# ================================================================
# My ggplot2 examples
# ================================================================

# ================================================================
# Bar charts from quarterly reports
# ================================================================

# ========================= Since inception

gg.all = ggplot( data.all, aes( injury, pct, label=pretty.pct)) +
  scale_y_continuous( limits = c( 0 , max( global.ymax.with.expansion + .15 )), expand=c(0, .0)) +
  geom_rect( xmin = xmin, xmax=xmax, ymin=ymin, ymax=global.ymax.with.expansion, fill=panel.col) +
  geom_bar( stat='identity', fill=bar.col, width=.5) +
  coord_flip() + # annotate( 'text', x=data$injury, y = max(data$pct)+.1, label=data$pretty.pct) +
  
  geom_text( x=1:8, y = max( global.ymax.with.expansion + .07), label=data.all$pretty.t, hjust=1) +
  geom_text( x=1:8, y = max( global.ymax.with.expansion + .12 ), label=data.all$pretty.p, hjust=1) +
  labs(  title=paste( 'Since Trust inception --', my.currfmt( sum( data.all$sum.val)), 'total payments')) +
  
  theme( plot.title   = element_text(size = rel(1.8)),
         
         axis.title.x = element_blank(),
         axis.ticks.x = element_blank(),
         axis.text.x  = element_blank(), # element_text( color='black', size=rel(1.5)),
         
         axis.title.y = element_blank(),      
         axis.text.y  = element_text( color='black', size=rel(1.5)),
         axis.ticks.y = element_blank(),
         
         panel.background = element_rect(fill='white'), # element_rect( fill=bkgrnd.col)
         panel.grid       = element_blank()
  )
gg.all