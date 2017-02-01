
def solve(filename):
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
solve("small")
