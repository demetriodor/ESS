### Common graphics settings

## Calculate the nearest higher number that is a multiple of 10 (useful for the range of the plot axes)
y.min <- plyr::round_any(max(pt[, 'Allow none']), 10, f = ceiling)
y.max <- plyr::round_any(max(pt$someplus), 10, f = ceiling)

x.min <- plyr::round_any(max(pt[, 'Allow none']), 10, f = ceiling)
x.max <- plyr::round_any(max(pt$someplus), 10, f = ceiling)

## add custom fonts
font_add_google('Quattrocento') #get the fonts 
font_add_google('Quattrocento Sans')
font_families() #check that the fonts are installed and available

showtext_auto() #this is to turn on the custom fonts availability
showtext_opts(dpi = 96) #set the resolution: 96 is default

## color settings
background.color = rgb(248, 244, 255, max=255) # color for the background: magnolia

dark.color = rgb(24, 24, 38, max=255) # dark color: almost black

red.1 = rgb(237, 41, 57, max=255) # default red

green.1 = rgb(0, 173, 67, max=255) # default green
green.dark = rgb(0, 66, 26, max=255) # dark green
green.light = rgb(15, 255, 108, max=255) # light green

blue.twitter = rgb (29, 161, 242, max=255) # twitter blue
