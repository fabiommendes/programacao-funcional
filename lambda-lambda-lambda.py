(lambda fat: [print(f'fat({n}) =', fat(n)) for n in range(1, int(input('n: ')) + 1)])((lambda f2: (lambda n: f2(n, f2)))((lambda n, f: 1 if n == 0 else n * f(n -1, f))))
