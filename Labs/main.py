import random
# 1 create data set
MASTER_A = 9
MASTER_B = 5

X_vector = list(range(50))

Y_vector = list(map(lambda x: MASTER_A * x + MASTER_B, X_vector))

X_test, X_master = X_vector[:10], X_vector[10:]
Y_test, Y_master = Y_vector[:10], Y_vector[10:]


def cross(item1, item2):
    return [(item1[0]+item2[0])/2,(item1[1]+item2[1])/2]


def test(item):
    differences = []
    for i, x in enumerate(X_master):
        differences.append(abs(Y_master[i]-(item[0]*x+item[1])))

    return sum(differences)/len(differences)


def mutation(item):
    item[0] = item[0]*random.random() + random.randint(-5, 5)
    item[1] = item[1]*random.random() + random.randint(-5, 5)


def random_population():
    return [[random.randint(-10, 10), random.randint(-10, 10)]for i in range(10)]


def get_best(population, offset=6):
    score = []
    for item in population:
        score.append(test(item))
    return [x for _, x in sorted(zip(score, population))[:offset]]


def cross_over(population):
    offspring = []
    for item1 in population:
        for item2 in population:
            if item1 != item2:
                offspring.append(cross(item1, item2))
    return offspring


def mutate(population):
    for item in population:
        if random.random() > 0.9:
            mutation(item)


if __name__ == "__main__":
    population = random_population()

    for generation in range(5000):
        best = get_best(population)
        offspring = cross_over(best)

        # todo random mutation mutation(item1)
        population = best + offspring
        mutate(population)

    print(get_best(population))






