import re
##matrix that will be final product
matrix = []
##array that will hold header and body
header= []
data=[]

with open('NortheastAndNorthCentralPacificHurricaneDatabase(HURDAT2)1949-2018.txt') as file:
    ##reads in data from the file
    for line in file:
        lineSplit = re.split(',',line)
        #print(len(lineSplit[:-1]) )

        ##checkls the lenght of the line, if equal to 3, then its a header
        if(len(lineSplit) == 4):
            header.append(lineSplit[:-1])
        else:
            data.append(lineSplit[:-1])

for head in header:
    tempHead = head
    tempNum = int(head[2])
    #print(int(head[2]))
    #print(tempNum, tempHead)