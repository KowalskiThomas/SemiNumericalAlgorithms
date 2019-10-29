import sys
import numpy
import numpy.polynomial.polynomial as poly

times = list()
limbs = list()
with open("timing.txt") as f:
    for line in f:
        l, t = map(int, line.split(" "))
        limbs.append(l)
        times.append(t)

limbs = numpy.array(limbs)
times = numpy.array(times)

x = limbs
y = times

deg = int(sys.argv[1]) if len(sys.argv) else 2

result = numpy.polyfit(x, y, deg)[::-1]
ffit = poly.Polynomial(result)

print(result)

import matplotlib.pyplot as plt


plt.plot(x, y)
plt.plot(x, ffit(x))
plt.show()
