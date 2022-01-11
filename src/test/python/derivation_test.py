from sympy import *
from sys import argv

if __name__ == '__main__':
    x = Symbol('x')
    expr, inp_diff_res = argv[1], argv[2]
    std_diff_res = diff(expr, x)
    print(std_diff_res.equals(inp_diff_res))
    print(std_diff_res)
