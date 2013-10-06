################################################################
### Demo code for Unit 7 of Stat243, "Contrasting Python and R"
### Chris Paciorek, October 2013
################################################################

#############################
# 2: Introduction to Python
#############################

print("Hi there")
print(2 + 2)
2.1 * 5  # type casting/coercion
# indentation matters
  2.1 * 5
dist = 7
dist < 6
type(2)
type(2.0)
type(2*5.0)
isinstance(dist, int)


intro = "My name is "
name = "sam"
endLine = ".\n"
intro + name.capitalize() + endLine
intro.split(' ')
'abcdefghijkl'[0:10:2] # Python indexing starts with 0

# if-else
if name == "samuel":
        print(intro + name.capitalize() + endLine)
elif name == "sam":
        print("My nickname is " + name.capitalize() + endLine)
else:
        print("My name is something else.\n")


# for loop
x = 0
for i in range(10):
        x += i

# on the command line, we need a blank line above to end the loop
print("x is " + str(x))


# Getting help in Python
help(sum)
# in ipython, we can do tab completion and we'll see all the methods for a given object
str = "my text"
# now type "str." and hit tab - you'll see all the various string class methods. Very helpful!

# Packages
import os  # load the os package
print(os.environ['HOME']) # use 'environ' function in the package
import numpy as np  # import and assign a new name
vec = np.random.normal(size = 1000)
vec.mean()
import re #

### 3 Programming concepts in Python from an R user's perspective

# Lists
data = 7
data = 'blue' # no problem!
data = [1, True, "same", [5, 7, 'blue']]
data[0]
data[3][0]
origData = data
data[2] = [True, False, True]
origData.extend(data)
data.sort()

# Indexing
data = range(50)
data[0:3]
data[40:]
data[:5]

# Tuples
data = (12, True, "hi there", [5, 'sam'], (3, 11, 'Yang'))
data[0]
data[0] = 5
data[3][1] = 'sam I am'
data[4][1] = -13
data
type(data)
type(data[3])
type(data[3][0])
type(data[4])

# Dictionaries
zoo = {"lion": "Simba", "panda": None, "whale": "Moby", "numAnimals": 3}
zoo
zoo['lion']
zoo['elephants']
zoo['elephants'] = ['dumbo', 'mumbo']
zoo.keys()

### 3.2 Copying in Python

data = [1, 3, 5, 7]
myCopy = data
data[0] = 0
myCopy
id(data)
id(myCopy)
# how to copy?
myStaticCopy= list(data)
data[0] = -73
myStaticCopy[0]
id(myStaticCopy)

### 3.3 Functions in Python

def myFun(x, y, z):
	''' This doc string shows how to write help info for a function.
	This is my test function.
	It helps me understand how Python passes arguments. '''
	print id(x)
	print id(y)
	print id(z)
	x[0] = 7
	y = 5
	z = [3, 5]
	print id(x)
	print id(y)
	print id(z)
	return(y)

x = [0, 7, 5]
y = 3
z = [0, 1, 2]
id(x)
id(y)
id(z)
myFun(x, y, z)
x
y
z
help(myFun)


# examples to understand scoping
a = 3
def myFun(x):
	return(x*a)

myFun(7)

a = 3
def myInnerFun(x):
	return(x*a)
def myFun(x):
	a = 7
	return(myInnerFun(x))
myFun(7)

a = 3
def myFun(x):
	def myInnerFun(x):
		return(x*a)
	a = 7
	return(myInnerFun(x))

myFun(7)

# passing arguments by position vs. keyword

def myMult(x, y = 3, z = 5):
	return(x*y, z)

myMult(5)
myMult(5, 7)
myMult(5, 7, 11)
myMult(5, z = 4)
myMult(x = 7, z = 5, y = 3)
myMult(y = 5, x = 3, 7)

out1, out2 = myMult(5)
out3 = myMult(5)
out1
out2
out3
type(out3)

### 3.4 Calculations and efficiency

data = range(100)
notDiv2or3 = [num for num in data if (num % 2 != 0) and (num % 3 != 0)]
# stealing from Josh Bloom's python bootcamp (Josh is an astronomer)
particles = [{"name":" pi +" ,"mass": 139.57018}, 
             {"name":" pi 0" ,"mass": 134.9766}, 
             {"name":" eta 5" ,"mass": 47.853}, 
             {"name":" eta prime (958)","mass": 957.78}, 
             {"name":" eta c(1S)", "mass": 2980.5}, 
             {"name": " eta b(1S)","mass": 9388.9}, 
             {"name":"K+", "mass": 493.677}, 
             {"name":"K0" ,"mass": 497.614}, 
             {"name":"K0S" ,"mass": 497.614}, 
             {"name":"K0L" ,"mass": 497.614}, 
             {"name":"D+" ,"mass": 1869.62}, 
             {"name":"D0" ,"mass": 1864.84}, 
             {"name":"D+s" ,"mass": 1968.49}, 
             {"name":"B+" ,"mass": 5279.15}, 
             {"name":"B0" ,"mass": 5279.5}, 
             {"name":"B0s" ,"mass": 5366.3}, 
             {"name":"B+c" ,"mass": 6277}]
type(particles[0])
my_mesons = [ (x['name'],x['mass']) 
              for x in particles if x['mass'] <= 1000.0 
              and x['mass'] >= 100.0]
type(my_mesons)
type(my_mesons[0])


# this is not a vectorized calculation
data = range(10)
data + data
data * 3


# but this is
import pandas
import numpy as np
vec1 = np.array([1, 2, 3])
vec2 = np.random.normal(size = 3)
vec1 + vec2
vec1 * 8
mymat = np.array([[1, 2, 3], [4, 5, 6], [7, 8, 9]])
mymat.transpose()
mymat[0:2, :]


# exploring pre-allocation of memory
import timeit
expr = '''
n = 10000
x = [0]
for i in range(n):
	x = x + [i]
'''

timeit.timeit(expr, number = 10)

expr = '''
n = 10000
x2 = [0]*n
for i in range(n):
	x2[i] = i
'''

timeit.timeit(expr, number = 10)

# example of using pandas

import pandas as pd
df = pd.read_table('../data/hivSequ.csv', sep = ',')
df.head()
df[0:3]
df['CD4-t0'][0:20]


### 3.5 Object-oriented programming

# do in python not ipython if paste in
class indiv:
        '''The indiv class holds information about people.'''	
        print("indiv class is defined.")
        def __init__(self, name, age):
                self.name = name
                self.age = age               
        def getInfo(self):
                print("Object of class 'indiv'. " + self.name + " is " + str(self.age) + " years old.")	
        def makeOlder(self):
                self.age += 1

# import oop
pres = indiv("Obama", 50)
pres = indiv("Obama")
indiv
pres
pres.age
pres.makeOlder()
pres.getInfo()



import copy
newPres = copy.deepcopy(pres)


# do in python if paste in
class indiv:
        '''The indiv class holds information about people.'''	
        population = 0
        print("indiv class is defined.")
        def __init__(self, name, age):
                self.name = name
                self.age = age
                indiv.population += 1
        def getInfo(self):
                print("Object of class 'indiv'. " + self.name + " is " + str(self.age) + " years old. " + self.name + " is one of " + str(indiv.population) + " individuals.")

pres = indiv("Obama", 50)
friend = indiv("Fang", 40)
indiv
pres
pres.getInfo()
indiv.getInfo(pres)  # alternative


