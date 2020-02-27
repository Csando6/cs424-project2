#This program takes in a text data file as input and converts it to 2D array
#Then it cleans it to a useable format by joining "header rows" with matching data rows
#Finally, it exports it as a csv file to the same directory.

import re

matrix = []
headers= []
data=[]

#input file:
filename = input("Enter file to be converted: ")  #ex: "AtlanticHurricaneDatabase(HURDAT2)1851-2018.txt"
print("processing " + filename + " . . .")

#create a 2D-array from the imported dataset.
with open(filename) as file:
    ##array that will hold header and body
    ##reads in data from the file
    for line in file:
        lineSplit = re.split(',',line)
        #print(len(lineSplit[:-1]) )
        ##check the lenght of the line, if equal to 4, then its a header
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


#append header to data rows (manipulation)
i = 0
k = 0
for i in range(len(headers)):
    for j in range (0, amounts[i]):
        data[k] = headers[i] + data[k]
        k += 1
    i += 1


#2D-array to dataframe:
import pandas as pd
custom_header = ['hur_code', 'hur_name', 'num_rows', 'date', 'time', 'record_id', 'status', 'lat', 'lon', 'max_speed',
                 'min_pressure', 'ne34', 'se34', 'sw34', 'nw34', 'ne50', 'se50', 'sw50', 'nw50', 'ne64', 'se64', 'sw64', 'nw64']
dataframe = pd.DataFrame.from_records(data, columns=custom_header)

print(dataframe.head(8))


#export dataframe to csv:
dataframe.to_csv('cleaned_hurricane_data.csv', index=False)
print("csv generated.")
