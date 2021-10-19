# What does each of foo_x do?

def foo_1(x):
    return x ** 0.5
# foo_1 times x by the power 0.5

def foo_2(x, y):
    if x > y:
        return x 
    return y
# foo_2 returns the largest number

def foo_3(x, y, z):
    if x > y:
        temp = y
        y = x 
        x = temp
    if y > z:
        temp = z
        z = y
        y = temp
    return [x, y, z]
# foo_3 re orders lowest to highest

def foo_4(x):
    result = 1
    for i in range(1, x + 1):
        result = result * i
    return result
# foo_4 creates a list range 0 - x, + 1 and times them by eachother

def foo_5(x):
    if x == 1:
        return 1
    return x * foo5(x - 1) # double check original foo5(error not defined)
# foo_5 returns 1 if 1 inputted, else returns x times x - 1

def foo_6(x): #give factorial
    facto = 1
    while x >= 1:
        facto = facto * x 
        x = x -1
    return facto
#foo_6 while x ist greater the 1 times x by 1 and add to facto
#then reduce x by 1