from PIL import Image, ImageDraw
import sys
import numpy as np

import math, random


def generatePolygon(ctrX:int, ctrY:int, aveRadius:int, irregularity:float, spikeyness:float, numVerts:int):
    '''Start with the centre of the polygon at ctrX, ctrY,
        then creates the polygon by sampling points on a circle around the centre.
        Randon noise is added by varying the angular spacing between sequential points,
        and by varying the radial distance of each point from the centre.

        Params:
        ctrX, ctrY - coordinates of the "centre" of the polygon
        aveRadius - in px, the average radius of this polygon, this roughly controls how large the polygon is, really only useful for order of magnitude.
        irregularity - [0,1] indicating how much variance there is in the angular spacing of vertices. [0,1] will map to [0, 2pi/numberOfVerts]
        spikeyness - [0,1] indicating how much variance there is in each vertex from the circle of radius aveRadius. [0,1] will map to [0, aveRadius]
        numVerts - self-explanatory

        Returns a list of vertices, in CCW order.
        '''

    irregularity = clip(irregularity, 0, 1) * 2 * math.pi / numVerts
    spikeyness = clip(spikeyness, 0, 1) * aveRadius

    # generate n angle steps
    angleSteps = []
    lower = (2 * math.pi / numVerts) - irregularity
    upper = (2 * math.pi / numVerts) + irregularity
    sum = 0
    for i in range(numVerts):
        tmp = random.uniform(lower, upper)
        angleSteps.append(tmp)
        sum = sum + tmp

    # normalize the steps so that point 0 and point n+1 are the same
    k = sum / (2 * math.pi)
    for i in range(numVerts):
        angleSteps[i] = angleSteps[i] / k

    # now generate the points
    points = []
    angle = random.uniform(0, 2 * math.pi)
    for i in range(numVerts):
        r_i = clip(random.gauss(aveRadius, spikeyness), 0, 2 * aveRadius)
        x = ctrX + r_i * math.cos(angle)
        y = ctrY + r_i * math.sin(angle)
        points.append((int(x), int(y)))

        angle = angle + angleSteps[i]

    return points


def clip(x, min, max):
    if min > max:
        return x
    elif x < min:
        return min
    elif x > max:
        return max
    else:
        return x


polygon_gen = lambda: generatePolygon(random.randint(0, 512), random.randint(0, 512), 130, 0.5, 0.5, 3)
fill_gen = lambda: (random.randint(0, 255), random.randint(0, 255), random.randint(0, 255))

im = Image.new('RGB', [512, 512], (0, 0, 0))

draw = ImageDraw.Draw(im)

for i in range(50):
    draw.polygon(polygon_gen(), fill=fill_gen())

im2 = Image.open("/Users/Pavel/programs/AI/Assignment2/samples/van2.png")

out = Image.blend(im, im2, 0.5)


# out.show()


def mse(imageA, imageB):
    # the 'Mean Squared Error' between the two images is the
    # sum of the squared difference between the two images;
    # NOTE: the two images must have the same dimension
    err = np.sum((imageA.astype("float") - imageB.astype("float")) ** 2)
    err /= float(imageA.shape[0] * imageA.shape[1])

    # return the MSE, the lower the error, the more "similar"
    # the two images are
    return err

def sim(im1, im2):
    array_im1 = np.asarray(im1)
    array_im2 = np.asarray(im2)
    re = int(mse(array_im1, array_im2))
    print(re)
    return 1/(1+re)

print(sim(im, im2))