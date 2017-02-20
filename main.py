
def solve(filename):

    # parsing
    f = open("inputs/"+filename+".in")

    R, C, L, H = [int(x) for x in f.readline().split()]

    pizza = []
    for i in range(R):
        pizza.append(f.readline())

    def count(r1, c1, r2, c2):
        rows = [pizza[i][c1:c2+1] for i in range(r1, r2 + 1)]

        reduceFunc = lambda (t,m), c: (t+1,m) if c == 'T' else (t,m+1)
        perRow = [reduce(reduceFunc, row, (0,0)) for row in rows]

        return reduce(lambda (tt,tm),(t,m): (tt+t, tm+m), perRow, (0,0))

    def isFree(x,y):
        return firstFreePerCol[y] <= x

    def isSliceFree(r1, c1, r2, c2):
        for x in range(r1, r2 + 1):
            for y in range(c1, c2 + 1):
                if not isFree(x,y):
                    return False
        return True

    def isValid(r1, c1, r2, c2):
        if r2 < R and c2 < C and isSliceFree(r1, c1, r2, c2):
            t, m = count(r1, c1, r2, c2)

            return t >= L and m >= L and t+m <= H
        else:
            return False

    slices = []
    dimIndexes = []
    firstFreePerCol = [0 for i in range(C)]
    dimensions = []

    def calcDimensions():
        for T in range(H, 0, -1):
            for h in range(1, T + 1):
                if T % h == 0:
                    w = T/h
                    dimensions.append((w, h))
    def firstFree():
        firstI, firstJ = (R, C)
        for (j, i) in enumerate(firstFreePerCol):
            if(i < firstI):
                firstI, firstJ = (i, j)
        return (firstI, firstJ)

    def countFree():
        return reduce(lambda c, (j,i): c + R - i, enumerate(firstFreePerCol), 0)

    def updateFreeOnPush(r1, c1, r2, c2):
        height = r2 - r1 + 1
        for j in range(c1, c2 + 1):
            firstFreePerCol[j] += height

    def updateFreeOnPop(r1, c1, r2, c2):
        height = r2 - r1 + 1
        for j in range(c1, c2 + 1):
            firstFreePerCol[j] -= height

    def pushSlice(r1, c1, r2, c2):
        slice = (r1, c1, r2, c2)
        slices.append(slice)
        updateFreeOnPush(*slice)

    def popSlice():
        slice = slices.pop()
        updateFreeOnPop(*slice)
        return dimIndexes.pop()

    def createSlice(cell, dim):
        (i, j) = cell
        (w, h) = dim

        return (i, j, i + w - 1, j + h - 1)

    calcDimensions()

    # base algorithm

    def solveWithThreShold(threshold):
        dimIndex = 0
        while True:
            slice = createSlice(firstFree(), dimensions[dimIndex])
            if isValid(*slice):
                pushSlice(*slice)
                dimIndexes.append(dimIndex)
                dimIndex = 0
                if countFree() <= threshold:
                    break
            else:
                dimIndex += 1
                while dimIndex >= len(dimensions):
                    if len(slices) <= 0:
                        return False
                    dimIndex = popSlice() + 1
        return True

    for t in range(R*C):
        if(solveWithThreShold(t)):
            print(t)
            break
    #write output
    fout = open("submissions/"+filename+".out", "w")

    fout.write(str(len(slices))+"\n")
    for slice in slices:
        r1, c1, r2, c2 = slice
        fout.write(str(r1)+" "+str(c1)+" "+str(r2)+" "+str(c2)+"\n")
solve("small")
solve("example")
solve("medium")
solve("large")
