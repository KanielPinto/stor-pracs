
def sign(value):
    positive = True
    if (value < 0):
        positive = False
    return positive


def bisection(degree, coeff):
    ans = 0
    x1 = 0
    x2 = 0
    x0 = 0
    func_x0 = 0
    i = 0
    prev_ans = 0

    for x in range(degree+1):
        prev_ans += coeff[x] * pow(i, x)

    for x in range(degree+1):
        i += 1
        ans += coeff[x] * pow(i, x)

        if sign(ans) != sign(prev_ans):
            x1 = x-1
            x2 = x
        else:
            prev_ans = ans

    print(f'------------------------------------------')

    while True:
        x0 = (x1 + x2)/2

        for x in range(degree+1):
            func_x0 += coeff[x] * pow(x0, x)

        print(f'{"{:.5f}".format(x1)} |  {"{:.5f}".format(x2)} |  {"{:.5f}".format(x0)} |  {"{:.5f}".format(func_x0)}')
        print(f'------------------------------------------')

        if (x2 - x1) < 0.001:
            break

        if sign(func_x0):
            x2 = x0
        elif not sign(func_x0):
            x1 = x0

        func_x0 = 0

    return x2


if __name__ == "__main__":
    poly_degree = int(input("Degree of the polynomial = "))
    poly_coeff = []
    for x in range(poly_degree+1):
        next_coeff = int(input(f"Coefficient {x} = "))
        poly_coeff.append(next_coeff)

    poly_coeff.reverse()

    print(f'Root of the polynomial = {bisection(poly_degree, poly_coeff)}')
