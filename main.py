
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

    def isValid(r1, c1, r2, c2):
        if r2 <= R and c2 <= C:
            t, m = count(r1, c1, r2, c2)

            t >= L and m >= L and t+m <= L
        else:
            False

    # base algorithm
    dimIndex = 0
    while True:
        slice = createSlice(firstFree(), dimensions[dimIndex])
        if isValid(slice*):
            pushSlice(slice*)
            if countFree <= threshold:
                break
        else:
            dimIndex += 1
            while dimIndex >= len(dimensions):
                dimIndex = popSlice()

    #write output
    fout = open("submissions/"+filename+".out", "w")

    fout.write(str(len(slices))+"\n")
    for slice in slices:
        r1, c1, r2, c2 = slice
        fout.write(str(r1)+" "+str(c1)+" "+str(r2)+" "+str(c2)+"\n")
solve("small")
