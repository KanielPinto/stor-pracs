source = 3
destination = 4


supply = [100, 500, 700]
demand = [400, 400, 300, 200]




allocatedMatrix = [[0, 0, 0, 0], [0, 0, 0, 0], [0, 0, 0, 0]]


totalCost = 0


cost = [[5, 6, 7, 4], [8, 6, 9, 6], [9, 4, 8, 7]]


costNotFound = True


i = 0
j = 0


while costNotFound:
    if i < source and j < destination:
        if supply[i] == demand[j]:
            allocatedMatrix[i][j] = supply[i]
            i += 1
            j += 1
       
        elif supply[i] > demand[j]:
            supply[i] -= demand[j]
            allocatedMatrix[i][j] = demand[j]
            j += 1
           
        elif supply[i] < demand[j]:
            demand[j] -= supply[i]
            allocatedMatrix[i][j] = supply[i]
            i += 1
           
    else:
        costNotFound = False
       
print('Allocated Matrix = ')
print(allocatedMatrix[0])
print(allocatedMatrix[1])
print(allocatedMatrix[2])
print('')
       
for x in range(source):
    for y in range(destination):
        totalCost += cost[x][y] * allocatedMatrix[x][y]
       
       
print(f'Total Cost = {totalCost}')
