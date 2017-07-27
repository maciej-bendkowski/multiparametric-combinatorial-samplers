import numpy

data = numpy.load("statistics.npy")
print data/numpy.sum(data)
