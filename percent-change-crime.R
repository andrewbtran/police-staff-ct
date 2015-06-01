library(plyr)
library(ggplot2)
library(gridExtra)
library(plotly)

leo <- read.csv("leo-nums.csv")

l2010 <- subset(leo, Year==2010)
l2012 <- subset(leo, Year==2012)

y2010 <- read.csv("2010crime.csv")
y2011 <- read.csv("2011crime.csv")
y2012 <- read.csv("2012crime.csv")

y2010$Department <- gsub(" Police Dept", "", y2010$Agency)
y2010$Department <- gsub(" Police", "", y2010$Department)

y2010$Department <- toupper(y2010$Department)

y2012$Department <- gsub(" Police Dept", "", y2012$Agency)
y2012$Department <- gsub(" Police", "", y2012$Department)

y2012$Department <- toupper(y2012$Department)


# Percent change calculations from Totals
l2012$pc <- ((l2012$Total.Personnel - l2010$Total.Personnel)/l2010$Total.Personnel)*100

y2012$pcViolent <- ((y2012$Violent.crime.total - y2010$Violent.crime.total)/y2010$Violent.crime.total)*100
y2012$pcMurder <- ((y2012$Murder.and.nonnegligent.Manslaughter - y2010$Murder.and.nonnegligent.Manslaughter)/y2010$Murder.and.nonnegligent.Manslaughter)*100
y2012$pcRape <- ((y2012$Forcible.rape - y2010$Forcible.rape)/y2010$Forcible.rape)*100
y2012$pcRobbery <- ((y2012$Robbery - y2010$Robbery)/y2010$Robbery)*100
y2012$pcAssault <- ((y2012$Aggravated.assault - y2010$Aggravated.assault)/y2010$Aggravated.assault)*100
y2012$pcProperty <- ((y2012$Property.crime.total - y2010$Property.crime.total)/y2010$Property.crime.total)*100
y2012$pcBurglary <- ((y2012$Burglary - y2010$Burglary)/y2010$Burglary)*100
y2012$pcLarceny <- ((y2012$Larceny.theft - y2010$Larceny.theft)/y2010$Larceny.theft)*100
y2012$pcMotor <- ((y2012$Motor.vehicle.theft - y2010$Motor.vehicle.theft)/y2010$Motor.vehicle.theft)*100

# Change calculations based on Rate differences

#another way
l2012$dif <- l2012$Employee.Rate.Per.1.000.Pop. - l2010$Employee.Rate.Per.1.000.Pop.

y2012$diffViolent <- y2012$Violent.Crime.rate - y2010$Violent.Crime.rate
y2012$diffMurder <- y2012$Murder.and.nonnegligent.manslaughter.rate - y2010$Murder.and.nonnegligent.manslaughter.rate
y2012$diffRape <- y2012$Forcible.rape.rate - y2010$Forcible.rape.rate
y2012$diffRobbery <- y2012$Robbery.rate - y2010$Robbery.rate
y2012$diffAssault <- y2012$Aggravated.assault.rate - y2010$Aggravated.assault.rate
y2012$diffProperty <- y2012$Property.crime.rate - y2010$Property.crime.rate
y2012$diffBurglary <- y2012$Burglary.rate - y2010$Burglary.rate
y2012$diffLarceny <- y2012$Larceny.theft.rate - y2010$Larceny.theft.rate
y2012$diffMotor <- y2012$Motor.vehicle.theft.rate - y2010$Motor.vehicle.theft.rate


# modifying a couple of names in the Department column before joining
l2012$Department <- gsub("CANTON", "CANTON TOWN", l2012$Department)
l2012$Department <- gsub("WINCHESTER", "WINCHESTER TOWN", l2012$Department)

joined_df <- join(y2012, l2012, by="Department")

joined_df <- subset(joined_df, Agency!="Wilton Police Dept")

# Hartford, Bridgeport, Stamford, New Haven, Norwich, New London, Waterbury, Danbury, 
# urban <- subset(joined_df)

joined_df_master <- joined_df

joined_df_urban <- subset(joined_df, Department=="BRIDGEPORT" |
                      Department=="HARTFORD" |
                      Department=="NEW HAVEN" |
                      Department=="NEW BRITAIN" |
                      Department=="WEST HAVEN" |
                      Department=="NEW LONDON" |
                      Department=="WATERBURY" |
                      Department=="NORWALK" |
                      Department=="ANSONIA" |
                      Department=="STAMFORD" |
                      Department=="STRATFORD" |
                      Department=="EAST HARTFORD" |
                      Department=="WEST HARTFORD" |
                      Department=="DERBY" |
                      Department=="MERIDEN")

joined_df_suburban <- subset(joined_df, Department!="BRIDGEPORT" &
                            Department!="HARTFORD" &
                            Department!="NEW HAVEN" &
                            Department!="NEW BRITAIN" &
                            Department!="WEST HAVEN" &
                            Department!="NEW LONDON" &
                            Department!="WATERBURY" &
                            Department!="NORWALK" &
                            Department!="ANSONIA" &
                            Department!="STAMFORD" &
                            Department!="STRATFORD" &
                            Department!="EAST HARTFORD" &
                            Department!="WEST HARTFORD" &
                            Department!="DERBY" &
                            Department!="MERIDEN")

# graphic diffs
par(mfrow=c(3,3), mar=c(2,5,2,1), las=1, bty="n")
plot(joined_df$dif, joined_df$diffViolent)
plot(joined_df$dif, joined_df$diffMurder)
plot(joined_df$dif, joined_df$diffRape)
plot(joined_df$dif, joined_df$diffRobbery)
plot(joined_df$dif, joined_df$diffAssault)
plot(joined_df$dif, joined_df$diffProperty)
plot(joined_df$dif, joined_df$diffBurglary)
plot(joined_df$dif, joined_df$diffLarceny)
plot(joined_df$dif, joined_df$diffMotor)


cor(joined_df$dif, joined_df$diffViolent)
cor(joined_df$dif, joined_df$diffMurder)
cor(joined_df$dif, joined_df$diffAssault)
cor(joined_df$dif, joined_df$diffProperty)
cor(joined_df$dif, joined_df$diffRobbery)
cor(joined_df$dif, joined_df$diffBurglary)
cor(joined_df$dif, joined_df$diffLarceny)
cor(joined_df$dif, joined_df$diffMotor)

#charting out the two most positive correlations

dg1 <- ggplot(joined_df, aes(x=dif, y=joined_df$diffProperty)) +
  geom_point(shape=1) +    # Use hollow circles
  geom_smooth(method=lm)
dg1 <- dg1 + ggtitle("Property crime rate change vs Police staff rate change - Correlation: .61")
dg1 <- dg1 + labs(x="Police staff rate change", y="Property crime rate change")

dg2 <- ggplot(joined_df, aes(x=dif, y=diffBurglary)) +
  geom_point(shape=1) +    # Use hollow circles
  geom_smooth(method=lm)
dg2 <- dg2 + ggtitle("Burglary rate vs Police staff rate change - Correlation: .44")
dg2 <- dg2 + labs(x="Police staff rate change", y="Burglary rate change")

dg3 <- ggplot(joined_df, aes(x=dif, y=diffLarceny)) +
  geom_point(shape=1) +    # Use hollow circles
  geom_smooth(method=lm)
dg3 <- dg3 + ggtitle("Larceny rate vs Police staff rate change - Correlation: .48")
dg3 <- dg3 + labs(x="Police staff rate change", y="Larceny rate change")

dg4 <- ggplot(joined_df, aes(x=dif, y=diffMotor)) +
  geom_point(shape=1) +    # Use hollow circles
  geom_smooth(method=lm)
dg4 <- dg4 + ggtitle("Vehicle theft rate vs Police staff rate change - Correlation: .53")
dg4 <- dg4 + labs(x="Police staff rate change", y="Motor vehicle theft rate change")


supg <- grid.arrange(dg1, dg2, dg3, dg4, ncol=2, 
                     main="Correlations between difference in police and crime rates between 2010 and 2012 in urban areas of Connecticut", sub="Data: UCR, TrendCT.org")

#fancier versions with ggplot

vg <- ggplot(joined_df, aes(x=dif, y=diffViolent)) +
  geom_point(shape=1) +    # Use hollow circles
  geom_smooth(method=lm) +
  xlim(c(-25,25))+ylim(c(-100,100))# Add linear regression line 
#  (by default includes 95% confidence region)

vg <- vg + ggtitle("Violent crime. Correlation: .024")
vg <- vg + labs(x="Staff diff: 2012, 2010", y="Violent crime diff")

pg <- ggplot(joined_df, aes(x=dif, y=diffProperty)) +
  geom_point(shape=1) +    # Use hollow circles
  geom_smooth(method=lm) +
  xlim(c(-25,25))+ylim(c(-100,100))# Add linear regression line 
#  (by default includes 95% confidence region)

pg <- pg + ggtitle("Property crime. Correlation: .128")
pg <- pg + labs(x="Staff diff: 2012, 2010", y="Property crime diff")

rg <- ggplot(joined_df, aes(x=dif, y=diffRobbery)) +
  geom_point(shape=1) +    
  geom_smooth(method=lm) +
  xlim(c(-25,25))+ylim(c(-100,100))

rg <- rg + ggtitle("Robbery crime. Correlation: -.105")
rg <- rg + labs(x="Staff diff: 2012, 2010", y="Robbery crime diff")


mg <- ggplot(joined_df, aes(x=dif, y=diffMotor)) +
  geom_point(shape=1) +    
  geom_smooth(method=lm)  +
  xlim(c(-25,20))+ylim(c(-100,100))

mg <- mg + ggtitle("Motor crime rate. Correlation: .373")
mg <- mg + labs(x="Staff diff: 2012, 2010", y="Motor crime diff")

supg <- grid.arrange(vg, pg, rg, mg, ncol=2, 
                     main="Police staffing versus crime numbers")


# old fashioned plotting but for percent changes
par(mfrow=c(3,3), mar=c(2,5,2,1), las=1, bty="n")
plot(joined_df$pc, joined_df$pcViolent)
plot(joined_df$pc, joined_df$pcMurder)
plot(joined_df$pc, joined_df$pcRape)
plot(joined_df$pc, joined_df$pcRobbery)
plot(joined_df$pc, joined_df$pcAssault)
plot(joined_df$pc, joined_df$pcProperty)
plot(joined_df$pc, joined_df$pcBurglary)
plot(joined_df$pc, joined_df$pcLarceny)
plot(joined_df$pc, joined_df$pcMotor)

cor(joined_df$pc, joined_df$pcViolent)
cor(joined_df$pc, joined_df$pcMurder)
cor(joined_df$pc, joined_df$pcAssault)
cor(joined_df$pc, joined_df$pcProperty)
cor(joined_df$pc, joined_df$pcRobbery)
cor(joined_df$pc, joined_df$pcBurglary)
cor(joined_df$pc, joined_df$pcLarceny)
cor(joined_df$pc, joined_df$pcMotor)

par(mfrow=c(3,3), mar=c(2,5,2,1), las=1, bty="n")
plot(joined_df$Total.Personnel, joined_df$Violent.crime.total)
plot(joined_df$Total.Personnel, joined_df$Murder.and.nonnegligent.Manslaughter)
plot(joined_df$Total.Personnel, joined_df$Forcible.rape)
plot(joined_df$Total.Personnel, joined_df$Robbery)
plot(joined_df$Total.Personnel, joined_df$Aggravated.assault)
plot(joined_df$Total.Personnel, joined_df$Property.crime.total)
plot(joined_df$Total.Personnel, joined_df$Burglary)
plot(joined_df$Total.Personnel, joined_df$Larceny.theft)
plot(joined_df$Total.Personnel, joined_df$Motor.vehicle.theft)

cor(joined_df$Total.Personnel, joined_df$Violent.crime.total)
cor(joined_df$Total.Personnel, joined_df$Murder.and.nonnegligent.Manslaughter)
cor(joined_df$Total.Personnel, joined_df$Robbery)
cor(joined_df$Total.Personnel, joined_df$Burglary)
cor(joined_df$Total.Personnel, joined_df$Larceny.theft)
cor(joined_df$Total.Personnel, joined_df$Motor.vehicle.theft)

#rates 


par(mfrow=c(3,3), mar=c(2,5,2,1), las=1, bty="n")
plot(joined_df$Employee.Rate.Per.1.000.Pop., joined_df$Violent.Crime.rate)
plot(joined_df$Employee.Rate.Per.1.000.Pop., joined_df$Murder.and.nonnegligent.manslaughter.rate)
plot(joined_df$Employee.Rate.Per.1.000.Pop., joined_df$Forcible.rape.rate)
plot(joined_df$Employee.Rate.Per.1.000.Pop., joined_df$Robbery.rate)
plot(joined_df$Employee.Rate.Per.1.000.Pop., joined_df$Aggravated.assault.rate)
plot(joined_df$Employee.Rate.Per.1.000.Pop., joined_df$Property.crime.rate)
plot(joined_df$Employee.Rate.Per.1.000.Pop., joined_df$Burglary.rate)
plot(joined_df$Employee.Rate.Per.1.000.Pop., joined_df$Larceny.theft.rate)
plot(joined_df$Employee.Rate.Per.1.000.Pop., joined_df$Motor.vehicle.theft.rate)

cor(joined_df$Employee.Rate.Per.1.000.Pop., joined_df$Violent.Crime.rate)
cor(joined_df$Employee.Rate.Per.1.000.Pop., joined_df$Murder.and.nonnegligent.manslaughter.rate)
cor(joined_df$Employee.Rate.Per.1.000.Pop., joined_df$Forcible.rape.rate)
cor(joined_df$Employee.Rate.Per.1.000.Pop., joined_df$Robbery.rate)
cor(joined_df$Employee.Rate.Per.1.000.Pop., joined_df$Aggravated.assault.rate)
cor(joined_df$Employee.Rate.Per.1.000.Pop., joined_df$Property.crime.rate)
cor(joined_df$Employee.Rate.Per.1.000.Pop., joined_df$Burglary.rate)
cor(joined_df$Employee.Rate.Per.1.000.Pop., joined_df$Larceny.theft.rate)
cor(joined_df$Employee.Rate.Per.1.000.Pop., joined_df$Motor.vehicle.theft.rate)


# ggplotting total personnel in URBAN areas

vg <- ggplot(joined_df, aes(x=Employee.Rate.Per.1.000.Pop., y=Violent.Crime.rate)) +
  geom_point(shape=1) +    # Use hollow circles
  geom_smooth(method=lm)
vg <- vg + ggtitle("Violent crime - Correlation: .748")
vg <- vg + labs(x="Staff per capita", y="Violent crime rate")

mg <- ggplot(joined_df, aes(x=Employee.Rate.Per.1.000.Pop., y=Murder.and.nonnegligent.manslaughter.rate)) +
  geom_point(shape=1) +    # Use hollow circles
  geom_smooth(method=lm)
mg <- mg + ggtitle("Murders - Correlation: .723")
mg <- mg + labs(x="Staff per capita", y="Murder rate")

rg <- ggplot(joined_df, aes(x=Employee.Rate.Per.1.000.Pop., y=Forcible.rape.rate)) +
  geom_point(shape=1) +    # Use hollow circles
  geom_smooth(method=lm)
rg <- rg + ggtitle("Forcible rape - Correlation: .219")
rg <- rg + labs(x="Staff per capita", y="Rape rate")

rog <- ggplot(joined_df, aes(x=Employee.Rate.Per.1.000.Pop., y=Robbery.rate)) +
  geom_point(shape=1) +    # Use hollow circles
  geom_smooth(method=lm)
rog <- rog + ggtitle("Robberies - Correlation: .673")
rog <- rog + labs(x="Staff per capita", y="Robbery rate")

ag <- ggplot(joined_df, aes(x=Employee.Rate.Per.1.000.Pop., y=Aggravated.assault.rate)) +
  geom_point(shape=1) +    # Use hollow circles
  geom_smooth(method=lm)
ag <- ag + ggtitle("Aggrevated assault - Correlation: .687")
ag <- ag + labs(x="Staff per capita", y="Assault rate")

pg <- ggplot(joined_df, aes(x=Employee.Rate.Per.1.000.Pop., y=Property.crime.rate)) +
  geom_point(shape=1) +    # Use hollow circles
  geom_smooth(method=lm)
pg <- pg + ggtitle("Property crime - Correlation: .463")
pg <- pg + labs(x="Staff per capita", y="Property crime rate")

bg <- ggplot(joined_df, aes(x=Employee.Rate.Per.1.000.Pop., y=Burglary.rate)) +
  geom_point(shape=1) +    # Use hollow circles
  geom_smooth(method=lm)
bg <- bg + ggtitle("Burglaries - Correlation: .379")
bg <- bg + labs(x="Staff per capita", y="Burglary rate")

lg <- ggplot(joined_df, aes(x=Employee.Rate.Per.1.000.Pop., y=Larceny.theft.rate)) +
  geom_point(shape=1) +    # Use hollow circles
  geom_smooth(method=lm)
lg <- lg + ggtitle("Larcenies - Correlation: .405")
lg <- lg + labs(x="Staff per capita", y="Larceny rate")

mvg <- ggplot(joined_df, aes(x=Employee.Rate.Per.1.000.Pop., y=Motor.vehicle.theft.rate)) +
  geom_point(shape=1) +    # Use hollow circles
  geom_smooth(method=lm)
mvg <- mvg + ggtitle("Motor vehicle thefts - Correlation: .539")
mvg <- mvg + labs(x="Staff per capita", y="Vehicle theft rate")

supergrid <- grid.arrange(vg, mg, rg, rog, ag, pg, bg, lg, mvg, ncol=3, 
                     main="Urban police staff rate versus crime rate")

joined_df$total <- joined_df$Violent.crime.total + joined_df$Murder.and.nonnegligent.Manslaughter + 
                        joined_df$Forcible.rape + joined_df$Robbery + joined_df$Aggravated.assault + 
                       joined_df$Property.crime.total + joined_df$Burglary + joined_df$Larceny.theft + 
                       joined_df$Motor.vehicle.theft

joined_df$crime.rate <- (joined_df$Pop.of.Jurisdaiction/joined_df$total)*1000

cor(joined_df$crime.rate, joined_df$Employee.Rate.Per.1.000.Pop.)

# this section creates a featured image for the blog post
fg <- ggplot(joined_df, aes(x=Employee.Rate.Per.1.000.Pop., y=Violent.Crime.rate, label=Department)) +
  geom_point(shape=1) +
  geom_text(aes(label=Department),hjust=--.5, vjust=-.5, size=4) +
  geom_smooth(method=lm)
fg <- fg + ggtitle("Violent crime in urban areas of Connecticut")
fg <- fg + labs(x="Police department staff per capita", y="Violent crime rate")

py <- plotly()

r <- py$ggplotly(fg) #Problem here. Getting an error

# The rest of this is experimenting with the plotly API to make ggplot chart interactive

toplot <- joined_df[,c("Employee.Rate.Per.1.000.Pop.","Violent.Crime.rate","Department")]
colnames(toplot) <- c("Police.staff.rate", "Violent.crime.rate", "Agency")

py <- plotly()


trace1 <-  list(
    x = toplot$Police.staff.rate, 
    y = toplot$Violent.crime.rate, 
    mode = "markers", 
    text = toplot$Agency, 
    type = "scatter",
    xaxis: "x1", 
    yaxis: "y1", 
    "showlegend": false, 
    marker = list(
      color: "rgb(0,0,0)", 
      size: 10, 
      symbol: "circle-open", 
      opacity: 1, 
      sizeref: 1, 
      sizemode: "area"
    )
  )

toplot$test<- lm(toplot$Police.staff, toplot$Violent.crime.rate)

trace2 <- list(
  x = toplot$Police.staff.rate, 
  y = toplot$Violent.crime.rate, 
  mode = "markers", 
  text = toplot$Agency, 
  type = "scatter",
  xaxis: "x1", 
  yaxis: "y1", 
  "showlegend": false, 
  marker = list(
    color: "rgb(0,0,0)", 
    size: 10, 
    symbol: "circle-open", 
    opacity: 1, 
    sizeref: 1, 
    sizemode: "area"
  )
)

data <- list(trace1,trace2)
layout <- list(
  title = "Violent crme in urban areas of Connecticut",
  hovermode="closest"
)
response <- py$plotly(data, kwargs=list(layout=layout, filename="violent-crime-in-urban-areas-of-connecticut", fileopt="overwrite"))
url <- response$url


py$ggplotly()
# ggplotting total personnel SUBURBAN


vg <- ggplot(joined_df, aes(x=Employee.Rate.Per.1.000.Pop., y=Violent.Crime.rate)) +
  geom_point(shape=1) +    # Use hollow circles
  geom_smooth(method=lm)
vg <- vg + ggtitle("Violent crime - Correlation: .164")
vg <- vg + labs(x="Staff per capita", y="Violent crime rate")

mg <- ggplot(joined_df, aes(x=Employee.Rate.Per.1.000.Pop., y=Murder.and.nonnegligent.manslaughter.rate)) +
  geom_point(shape=1) +    # Use hollow circles
  geom_smooth(method=lm)
mg <- mg + ggtitle("Murders - Correlation: -.23")
mg <- mg + labs(x="Staff per capita", y="Murder rate")

rg <- ggplot(joined_df, aes(x=Employee.Rate.Per.1.000.Pop., y=Forcible.rape.rate)) +
  geom_point(shape=1) +    # Use hollow circles
  geom_smooth(method=lm)
rg <- rg + ggtitle("Forcible rape - Correlation: .131")
rg <- rg + labs(x="Staff per capita", y="Rape rate")

rog <- ggplot(joined_df, aes(x=Employee.Rate.Per.1.000.Pop., y=Robbery.rate)) +
  geom_point(shape=1) +    # Use hollow circles
  geom_smooth(method=lm)
rog <- rog + ggtitle("Robberies - Correlation: .087")
rog <- rog + labs(x="Staff per capita", y="Robbery rate")

ag <- ggplot(joined_df, aes(x=Employee.Rate.Per.1.000.Pop., y=Aggravated.assault.rate)) +
  geom_point(shape=1) +    # Use hollow circles
  geom_smooth(method=lm)
ag <- ag + ggtitle("Aggrevated assault - Correlation: .144")
ag <- ag + labs(x="Staff per capita", y="Assault rate")

pg <- ggplot(joined_df, aes(x=Employee.Rate.Per.1.000.Pop., y=Property.crime.rate)) +
  geom_point(shape=1) +    # Use hollow circles
  geom_smooth(method=lm)
pg <- pg + ggtitle("Property crime - Correlation: .261")
pg <- pg + labs(x="Staff per capita", y="Property crime rate")

bg <- ggplot(joined_df, aes(x=Employee.Rate.Per.1.000.Pop., y=Burglary.rate)) +
  geom_point(shape=1) +    # Use hollow circles
  geom_smooth(method=lm)
bg <- bg + ggtitle("Burglaries - Correlation: .042")
bg <- bg + labs(x="Staff per capita", y="Burglary rate")

lg <- ggplot(joined_df, aes(x=Employee.Rate.Per.1.000.Pop., y=Larceny.theft.rate)) +
  geom_point(shape=1) +    # Use hollow circles
  geom_smooth(method=lm)
lg <- lg + ggtitle("Larcenies - Correlation: .33")
lg <- lg + labs(x="Staff per capita", y="Larceny rate")

mvg <- ggplot(joined_df, aes(x=Employee.Rate.Per.1.000.Pop., y=Motor.vehicle.theft.rate)) +
  geom_point(shape=1) +    # Use hollow circles
  geom_smooth(method=lm)
mvg <- mvg + ggtitle("Motor vehicle thefts - Correlation: .077")
mvg <- mvg + labs(x="Staff per capita", y="Vehicle theft rate")

supergrid <- grid.arrange(vg, mg, rg, rog, ag, pg, bg, lg, mvg, ncol=3, 
                          main="Suburban police staff rate versus crime rate")

superggrid <- multiplot(vg, mg, rg, rog, ag, pg, bg, lg, mvg, ncol=3)
# 

py <- plotly()

r <- py$ggplotly(mg) #OK, this works

data <- list(
  list(
    x = maz2$facthyp, 
    y = maz2$median, 
    mode = "markers", 
    text = maz2$zippity, 
    type = "scatter"
  )
)
layout <- list(
  hovermode="closest"
)
response <- py$ggplotly(mg, kwargs=list(layout=layout, filename="motor-crime-rate-correlation-373", fileopt="overwrite")) #this works, too

#multiplot code

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  require(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}