__author__ = 'quentin'
import random

nb_vars = 1000
nb_inequalities = 1000


def generate_linear_combination():
    l = []
    for i in range(0, nb_vars):
        cst = random.random()
        item = "%.2f x_%d" % (cst, i)
        l.append(item)
    return " + ".join(l)

def generate_inequality():
    l = generate_linear_combination()
    cst = 10*random.random()
    return "%s <= %.2f" % (l, cst)


print('MAXIMIZE')
print(generate_linear_combination())
print('SUBJECT TO')
for j in range(0, nb_inequalities):
    print(generate_inequality())
print('BOUNDS')
for i in range(0, nb_vars):
    print("x_%d >= 0" % i)
print('VARIABLES')
for i in range(0, nb_vars):
    print("x_%d" %i)

