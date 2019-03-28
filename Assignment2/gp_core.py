from PIL import Image, ImageDraw
from typing import List
import numpy as np
import random
import math

HEIGHT = 200
WIDTH = 200
RADIUS = WIDTH // 5


class Polygon:
    def __init__(self, position, color):
        self.color = color
        self.position: List[List[int]] = position

    @staticmethod
    def random_polygon():
        position = generatePolygon(random.randint(10, WIDTH - 10), random.randint(10, HEIGHT - 10),
                                   random.randint(10, RADIUS),
                                   0.5,
                                   0.5, 3)
        color = random_normal_color()
        return Polygon(position, color)


class DNA:
    def __init__(self, genes: List[Polygon]):
        # array of polygons
        self.genes = genes

    @property
    def representation(self):
        img = Image.new('RGB', [WIDTH, HEIGHT], (0, 0, 0))
        # img = Image.alpha_composite(img, tmp)
        draw = ImageDraw.Draw(img, 'RGBA')
        for gene in self.genes:
            draw.polygon(gene.position, gene.color)
        return img

    @staticmethod
    def random_dna():
        return DNA([Polygon.random_polygon() for _ in range(150)])


class GeneticPrograming:
    def __init__(self, origin, epochs):
        self.number_of_epochs = epochs
        self.origin = origin
        self.size = origin.size
        self.population: List[DNA] = []

    def init_population(self):
        # generate list of random images
        self.population = [DNA.random_dna() for _ in range(1)]

    def fitness_evaluation(self, gene: DNA):
        # reversed Mean Squared Error of some item with original picture
        ev = sim(gene.representation, self.origin)
        return ev

    # def crossover(self):
    #     offspring = []
    #     for gen1 in self.population:
    #         for gen2 in self.population:
    #             if gen1 != gen2:
    #                 offspring.append(self.cross(gen1, gen2))
    #     return offspring
    #
    # @staticmethod
    # def cross(gen1: DNA, gen2: DNA):
    #     g1 = gen1.genes.copy()
    #     g2 = gen2.genes.copy()
    #     random.shuffle(g1)
    #     random.shuffle(g2)
    #     return DNA(g1[:len(g1) // 2] + g2[:len(g2) // 2])

    def mutate(self):
        for gene in self.population:
            if random.random() > 0.5:
                self.mutation(gene)

    @staticmethod
    def mutation(gene: DNA):
        gene.genes[random.randint(0, len(gene.genes)-1)] = Polygon.random_polygon()
        return
        if random.random() > 0.5:
            polygon.color = random_normal_color()
        else:
            point = polygon.position[random.randint(0, 2)]

            if random.randint(0, 1):
                point = (point[0] + random.randint(-RADIUS, RADIUS), point[1])
            else:
                point = (point[0], point[1] + random.randint(-RADIUS, RADIUS))

    def get_best(self, offset=1):
        score = [self.fitness_evaluation(item) for item in self.population]
        cross = zip(score, range(len(score)))

        return [self.population[i] for i in [x for _, x in sorted(cross, reverse=True)[:offset]]]

    def compute(self):
        self.init_population()

        i = 0
        for generation in range(self.number_of_epochs):
            genes = self.population[0].genes.copy()

            offspring = DNA(genes)
            self.mutation(offspring)

            self.population += [offspring]
            self.population = self.get_best()

            i += 1
            if i % 5000 == 0:
                self.population[0].representation.show()


def generatePolygon(ctrX: int, ctrY: int, aveRadius: int, irregularity: float, spikiness: float, numVerts: int):
    """Start with the centre of the polygon at ctrX, ctrY,
        then creates the polygon by sampling points on a circle around the centre.
        Random noise is added by varying the angular spacing between sequential points,
        and by varying the radial distance of each point from the centre.

        Params: ctrX, ctrY - coordinates of the "centre" of the polygon aveRadius - in px, the average radius of this
        polygon, this roughly controls how large the polygon is, really only useful for order of magnitude.
        irregularity - [0,1] indicating how much variance there is in the angular spacing of vertices. [0,1] will map
        to [0, 2pi/numberOfVerts] spikiness - [0,1] indicating how much variance there is in each vertex from the
        circle of radius aveRadius. [0,1] will map to [0, aveRadius] numVerts - self-explanatory

        Returns a list of vertices, in CCW order.
        """

    irregularity = clip(irregularity, 0, 1) * 2 * math.pi / numVerts
    spikiness = clip(spikiness, 0, 1) * aveRadius

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
        r_i = clip(random.gauss(aveRadius, spikiness), 0, 2 * aveRadius)
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
    return 1 / (1 + re)


def random_normal_color():
    return tuple(map(int, [random.gauss(127, 42), random.gauss(127, 42), random.gauss(127, 42), 100]))


if __name__ == '__main__':
    # todo generate initial picture more precise
    im = Image.open("/Users/Pavel/programs/AI/Assignment2/samples/judith.jpg").resize((WIDTH, HEIGHT))
    gp = GeneticPrograming(im, 30000)
    gp.compute()
    gp.get_best()[0].representation.show()
    # new1 = DNA.random_dna()
    # new1.representation().show()
    # new2 = DNA.random_dna()
    # new2.representation().show()
    # new3 = GeneticPrograming.cross(new1, new2)
    # new3.representation().show()
