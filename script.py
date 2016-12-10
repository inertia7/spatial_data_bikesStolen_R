import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
plt.style.use('ggplot')

# RECALL TO HAVE THE SCRIPT WITHIN THE SAME DIRECTORY AS THE CSV FILE
bikesStolen = pd.read_csv('bikesStolen.csv', sep=',')
bikesStolen.head(7)

print("Let's view the Structure of our DataFrame:")
bikesStolen.dtypes

print("Make sure we converted the dates correctly:")
bikesStolen['fromDate'] = pd.to_datetime(bikesStolen['fromDate'], format='%m/%d/%Y')
print(bikesStolen['fromDate'].dtypes)
bikesStolen['toDate'] = pd.to_datetime(bikesStolen['toDate'], format='%m/%d/%Y')
print(bikesStolen['fromDate'].dtypes)
bikesStolen['Brand'] = bikesStolen['Brand'].astype("category")
bikesStolen['Model'] = bikesStolen['Model'].astype("category")
bikesStolen['Speed'] = bikesStolen['Speed'].astype("category")
bikesStolen['Color'].head(10)

# Because we are actually concerned with Colors we included a more indepth analysis as follows
bikesStolen['Color'].fillna(value='Not Given', inplace = True)
bikesStolen['Color'] = pd.Categorical(bikesStolen['Color'], categories = 
	["BLK","BLU","WHI","GRN","RED","GRY",
           "SIL","YEL","PLE","PNK","ONG",
           "BRO","LGR","LBL","MAR","TRQ","DGR",
           "TAN","COM","GLD","DBL","CRM","BGE", "Not Given"])
print("Let's see if we correctly converted to factor variables:")
print(bikesStolen['Color'].dtypes)


# Here we rename the categories for the color column
bikesStolen['Color'] = bikesStolen['Color'].cat.rename_categories(
	["Black","Blue","White",
	"Green","Red", "Grey",
	"Silver", "Yellow","Purple",
	"Pink","Orange","Brown",
	"Light Green","Light Blue",
	"Burgundy/Maroon","Turquoise",
	"Dark Green","Tan","Chrome",
	"Gold","Diamond Blue","Cream/Ivory",
	"Beige", "Not Given"]) 
print(bikesStolen['Color'].head(6))

print("Let's see the count for the colors:")
Colors = bikesStolen['Color'].value_counts()
print(Colors)

my_col = (
	"#000000", "#2d423f","#1f23af", 
	"#ffffff", "#03c11d","#dd1616", 
	"#808080","#C0C0C0","#ffff00",
	"#800080","#FFC0CB", "#FF8C00", 
	"#8B4513","#76EE00", "#ADD8E6", 
	"#800000", "#AFEEEE","#006400",
	"#D2B48C","#a8a8a8", "#D4AF37", 
	"#0EBFE9","#FCFBE3", "#f5f5dc"
	)
ax = plt.gca()
ax.set_axis_bgcolor('#fafafa')
plt.title("Color of Bikes Stolen within Isla Vista (2011-2016)")
plt.ylabel("Count")
plt.xlabel("Color of Bicycle")
Colors.plot(
	kind="bar",
	color=my_col,
	width=0.75,
	edgecolor=['#48D1CC'],
	linewidth=2.0
	)
plt.show()
