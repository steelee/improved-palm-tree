from scipy import misc
import numpy as np

def printpen(state):
    if state == True:
        target.write("Pen Up, ")
        return False
    else:
        target.write("Pen Down, ")
        return True

def printmove(x, y):
    target.write("Move " + str(x) + " " + str(y) + ", ")

arr = misc.imread("sample.png")
np.transpose(arr, [1,0,2])
width = arr.shape[0]
height = arr.shape[1]
print "image dimension: ", arr.shape

target = open('output.txt', 'w')

for h in range(height-1, 0, -1):
    pen = printpen(False)
    printmove(0, h)
    pen = printpen(pen)
    lsrgb = arr[0][h]
    for w in range(0, width):
        rgb = arr[w][h]
        if (rgb != lsrgb).all():
            lsrgb = rgb
            pen = printpen(pen)
            printmove(w, h)

target.close()