

source ('./common_graph_settings.R')

s = 3

offset = 0.01
mtext.title = 1.8*s
mtext.subtitle = 1.5*s
mtext.sign = 1.2*s
mtext.sign.emo = 1.5*s

png ('./figures/F1_big_vertical_2.png', height=1280*s, width=905.5*s, res=96)

par(mfrow=c(1,1), # number and distribution of plots
    oma=c(1,0,4,0), # size of the outer margins in lines of text (can be specified in inches as well with `omi`)
    mar=c(4,1,1,8), # number of lines of margin to be specified on the four sides of the plot (can be specified in inches as well with `mai`) 
    bty='n', # no box
    cex = 1.25*s, # magnification of text and symbols
    xpd = FALSE, # clipping of plotting to the figure region
    ann = FALSE, # switch off titles,
    #yaxt = 'n', # switch off y axis, do not do this here, it cannot be overriden with axis() 
    #xaxt = 'n', # switch off x axis
    bg=background.color, # background color
    family='Quattrocento' # font family
    
)

plot(NULL, ylim=c(1, dim(pt)[1]), xlim=c(-x.min, x.max), yaxt = 'n', xaxt = 'n') 

axis (1, 
      line = 0, # position
      tck = -0.01,
      lwd = 1*s,
      col = dark.color, # the actual axis (line) 
      col.axis = dark.color, # colors of the actual labels
      cex.axis = 1, 
      font=2, # font type (bold)
      at=seq(-x.min, x.max, 10), # where to put labels  
      labels= paste0(c(rev(seq(0, x.min, 10)), seq(10, x.max,10)), "%"), # text of labels 
      las=1 # orientation of the labels
)

axis (4, 
      line = 0, # position
      tck = -0.01,
      lwd = 1*s,
      col = 'white', # the actual axis (line) 
      col.axis = dark.color, # colors of the actual labels
      cex.axis = 1, 
      font=2, # font type (bold)
      at=seq(1, dim(pt)[1]), # where to put labels  
      adj=0,
      labels=countrycode(rownames(pt), origin = 'iso2c', destination = 'country.name'), # text of labels 
      las=1 # orientation of the labels
)


for (i in 1:dim(pt)[1]){
  rect(ybottom = i - 0.25, ytop = i + 0.25, xleft = 0 - pt[i , 'Allow none'], xright =  0, col=red.1, border=red.1)
  rect(ybottom = i - 0.25, ytop = i + 0.25, xleft = 0, xright =  0 + pt$someplus[i], col=green.light, border=green.light)   
  rect(ybottom = i - 0.25, ytop = i + 0.25, xleft = 0, xright =  0 + pt[i , 'Allow a few'] + pt[i , 'Allow some'], col=green.1, border=green.1) 
  rect(ybottom = i - 0.25, ytop = i + 0.25, xleft = 0, xright =  0 + pt[i , 'Allow some'], col=green.dark, border=green.dark) 
}  


abline(v=seq(-50,100,10), col='white', lwd=1*s)
abline(v=0, col='white', lwd=3*s)
abline(h=seq(1:dim(pt)[1]), col='white', lwd=1*s)

# do that now so it is not crossed by the gridlines
for (i in 1:dim(pt)[1]){
  #text (rownames(pt)[i], y = i, x = 0 + pt$someplus[i] + 4, font=2)
  addImg(readPNG(paste0('./flags/197373-countrys-flags/png/', gsub(" ","-", tolower(countrycode(rownames(pt)[i], origin = 'iso2c', destination = 'country.name'))), '.png')), 
         y = i, x = 0 - pt[i , 'Allow none'] - 7, width = 6)
}  

#title
mtext(expression(bold('Allow many/few immigrants from poorer countries in Europe \nto come and live here')),
      side = 3, line = 2, adj = 0, padj = 1, outer = TRUE, at = offset,
      font=1, col=dark.color, cex = mtext.title)
mtext(expression(italic("Share of people who answer:            'None'             'Some'           'A few'           'Many'")),
      side = 3, line = 0, adj = 0, padj = 1, outer = TRUE, at = offset, 
      font=1, col=dark.color, cex = mtext.subtitle)

par(xpd = TRUE)
points(x = 24, y = 21.3, pch = 15, cex = 4, col=red.1)
points(x = 24 + 32, y = 21.3, pch = 15, cex = 4, col=green.dark)
points(x = 24 + 32*2, y = 21.3, pch = 15, cex = 4, col=green.1)
points(x = 24 + 32*3, y = 21.3, pch = 15, cex = 4, col=green.light)

#data statement
mtext(text = fontawesome('fa-table'), 
      side=1, line=-1, outer=T,
      col=red.1, cex=mtext.sign.emo, at = offset, 
      font=1, family='fontawesome-webfont', 
      adj=0, padj=0.8)

mtext(text=expression("Data: " * phantom("European Social Survey, Wave 7 [2014]")), 
      side=1, line=-1, outer=T, at = offset + 0.03,
      col=dark.color, cex=mtext.sign,
      font=1, family='Quattrocento Sans', 
      adj=0, padj=1)
mtext(text=expression(phantom("Data: ") * "European Social Survey, Wave 7 [2014]"), 
      side=1, line=-1, outer=T, at = offset + 0.03,
      col=red.1, cex=mtext.sign,
      font=1, family='Quattrocento Sans', 
      adj=0, padj=1)

#signature
mtext(text=expression(phantom("@DToshkov        ") * " http://dimiter" * phantom(".eu")), 
      side=1, line=-1, outer=T, at = 1 - offset - 0.03,
      col=dark.color, cex=mtext.sign,
      font=1, family='Quattrocento Sans', 
      adj=1, padj=1)

mtext(text=expression(phantom("@DToshkov         http://dimiter") * ".eu"),
      side=1, line=-1, outer=T, at = 1 - offset - 0.03,
      col=red.1, cex=mtext.sign,
      font=1, family='Quattrocento Sans', 
      adj=1, padj=1)

mtext(text=expression("@DToshkov        " * phantom(" http://dimiter.eu")), 
      side=1, line=-1, outer=T, at = 1 - offset - 0.03,
      col=blue.twitter, cex=mtext.sign,
      font=1, family='Quattrocento Sans', 
      adj=1, padj=1)

mtext(text= fontawesome('fa-creative-commons'), 
      side=1, line=-1, outer=T,
      col=dark.color, cex=mtext.sign.emo, at = 1 - 0.36, 
      font=1, family='fontawesome-webfont', 
      adj=1, padj=0.8)

mtext(text= fontawesome('fa-twitter'), 
      side=1, line=-1, outer=T,
      col=blue.twitter, cex=mtext.sign.emo, at = 1 - 0.21, 
      font=1, family='fontawesome-webfont', 
      adj=1, padj=0.8)

mtext(text= fontawesome('fa-rss'), 
      side=1, line=-1, outer=T,
      col=red.1, cex=mtext.sign.emo, at = 1 - offset, 
      font=1, family='fontawesome-webfont', 
      adj=1, padj=0.8)

dev.off()
