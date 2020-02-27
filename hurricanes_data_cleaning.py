import re

matrix = []
headers= []
data=[]

with open('AtlanticHurricaneDatabase(HURDAT2)1851-2018.txt') as file:
    ##array that will hold header and body
    ##reads in data from the file
    for line in file:
        lineSplit = re.split(',',line)
        #print(len(lineSplit[:-1]) )
        ##check the lenght of the line, if equal to 3, then its a header
        if(len(lineSplit) == 4):
            headers.append(lineSplit[:-1])
        else:
            data.append(lineSplit[:-1])

#strip off whitespace
headers = [[item.strip() for item in header] for header in headers]

#create a list of length numbers (amount of data to be considered per hurricane header)
amounts = [] 
for header in headers:
    amounts.append(int(header[2].strip()))


#append header to data rows
i = 0
k = 0
for i in range(len(headers)):
    for j in range (0, amounts[i]):
        data[k] = headers[i] + data[k]
        k += 1
    i += 1


#import pandas as pd
#dataframe = pd.DataFrame.from_records(data)

#dataframe.to_csv('cleaned_hurricane_data.csv')
