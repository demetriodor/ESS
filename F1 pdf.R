
source ('./common_graph_settings.R')

offset = 0.01
mtext.sign = 1.0
mtext.sign.emo = 1.5

pdf ('./figures/F1.pdf', width=11.69, height=8.27) # pdf, size A4

par(mfrow=c(1,1), # number and distribution of plots
    oma=c(1,0,3,0), # size of the outer margins in lines of text (can be specified in inches as well with `omi`)
    mar=c(1,4,1,1), # number of lines of margin to be specified on the four sides of the plot (can be specified in inches as well with `mai`) 
    bty='n', # no box
    cex = 1, # magnification of text and symbols
    xpd = FALSE, # clipping of plotting to the figure region
    ann = FALSE, # switch off titles,
    #yaxt = 'n', # switch off y axis, do not do this here, it cannot be overriden with axis() 
    #xaxt = 'n', # switch off x axis
    bg=background.color, # background color
    family='Quattrocento' # font family
    )

plot(NULL, xlim=c(1, dim(pt)[1]), ylim=c(-y.min, y.max), yaxt = 'n', xaxt = 'n') 

axis (2, 
      line = 0, # position
      tck = -0.01, 
      col = dark.color, # the actual axis (line) 
      col.axis = dark.color, # colors of the actual labels
      cex.axis = 1, 
      font=2, # font type (bold)
      at=seq(-y.min, y.max, 10), # where to put labels  
      labels= paste0(c(rev(seq(0, y.min, 10)), seq(10, y.max,10)), "%"), # text of labels 
      las=1 # orientation of the labels
      )

for (i in 1:dim(pt)[1]){
  rect(xleft = i - 0.25, xright = i + 0.25, ybottom = 0 - pt[i , 'Allow none'], ytop =  0, col=red.1, border=red.1)
  rect(xleft = i - 0.25, xright = i + 0.25, ybottom = 0, ytop =  0 + pt$someplus[i], col=green.light, border=green.light)   
  rect(xleft = i - 0.25, xright = i + 0.25, ybottom = 0, ytop =  0 + pt[i , 'Allow a few'] + pt[i , 'Allow some'], col=green.1, border=green.1) 
  rect(xleft = i - 0.25, xright = i + 0.25, ybottom = 0, ytop =  0 + pt[i , 'Allow some'], col=green.dark, border=green.dark) 
}  
abline(h=seq(-50,100,10), col='white', lwd=1)
abline(h=0, col='white', lwd=3)

# do that now so it is not crossed by the gridlines
for (i in 1:dim(pt)[1]){
  text (rownames(pt)[i], x = i, y = 0 + pt$someplus[i] + 4, font=2)
  
  addImg(readPNG(paste0('./flags/197373-countrys-flags/png/', gsub(" ","-", tolower(countrycode(rownames(pt)[i], origin = 'iso2c', destination = 'country.name'))), '.png')), 
         x = i, y =  0 - pt[i , 'Allow none'] - 7, width = 0.6)
}  

#title
mtext(expression(bold('Allow many/few immigrants from poorer countries in Europe to come and live here')),
      side = 3, line = 2, adj = 0, padj = 1, outer = TRUE, at = offset,
      font=1, col=dark.color, cex = 1.7)
mtext(expression(italic("Share of people who answer:                  'None'                 'Some'                 'A few'                 'Many'")),
      side = 3, line = 0, adj = 0, padj = 1, outer = TRUE, at = offset, 
      font=1, col=dark.color, cex = 1.2)

par(xpd = TRUE)
points(x = 4.9, y = 114.5, pch = 15, cex = 7, col=red.1)
points(x = 4.9 + 2.7, y = 114.5, pch = 15, cex = 7, col=green.dark)
points(x = 4.9 + 2*2.7, y = 114.5, pch = 15, cex = 7, col=green.1)
points(x = 4.9 + 3*2.7, y = 114.5, pch = 15, cex = 7, col=green.light)

#data statement
mtext(text = fontawesome('fa-table'), 
      side=1, line=-1, outer=T, col=red.1, cex=1.5, at = offset, 
      font=1, family='fontawesome-webfont', adj=0, padj=0.8)

mtext(text=expression("Data: " * phantom("European Social Survey, Wave 7 [2014]")), 
      side=1, line=-1, outer=T, at = offset + 0.03, col=dark.color, cex=1,
      font=1, family='Quattrocento Sans',  adj=0, padj=1)

mtext(text=expression(phantom("Data: ") * "European Social Survey, Wave 7 [2014]"), 
      side=1, line=-1, outer=T, at = offset + 0.03, col=red.1, cex=1,
      font=1, family='Quattrocento Sans', adj=0, padj=1)


#signature
mtext(text=expression(phantom("@DToshkov        ") * " http://dimiter" * phantom(".eu")), 
      side=1, line=-1, outer=T, at = 1 - offset - 0.02,
      col=dark.color, cex=mtext.sign, font=1, family='Quattrocento Sans', adj=1, padj=1)

mtext(text=expression(phantom("@DToshkov         http://dimiter") * ".eu"),
      side=1, line=-1, outer=T, at = 1 - offset - 0.02,
      col=red.1, cex=mtext.sign, font=1, family='Quattrocento Sans', adj=1, padj=1)

mtext(text=expression("@DToshkov        " * phantom(" http://dimiter.eu")), 
      side=1, line=-1, outer=T, at = 1 - offset - 0.02,
      col=blue.twitter, cex=mtext.sign, font=1, family='Quattrocento Sans', adj=1, padj=1)

mtext(text= fontawesome('fa-creative-commons'), 
      side=1, line=-1, outer=T,
      col=dark.color, cex=mtext.sign.emo, at = 1 - 0.24, font=1, family='fontawesome-webfont', adj=1, padj=0.8)

mtext(text= fontawesome('fa-twitter'), 
      side=1, line=-1, outer=T,
      col=blue.twitter, cex=mtext.sign.emo, at = 1 - 0.14, font=1, family='fontawesome-webfont', adj=1, padj=0.8)

mtext(text= fontawesome('fa-rss'), 
      side=1, line=-1, outer=T,
      col=red.1, cex=mtext.sign.emo, at = 1 - offset, font=1, family='fontawesome-webfont', adj=1, padj=0.8)

dev.off()