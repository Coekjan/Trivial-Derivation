from sympy import *
from sys import argv

if __name__ == '__main__':
    x = Symbol('x')
    expr, inp = argv[1], argv[2]
    std = sympify(expr)
    print(std.equals(inp))
