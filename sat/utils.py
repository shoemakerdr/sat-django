
def partition(xs, predicate):
    true_xs = []
    false_xs = []
    for x in xs:
        if predicate(x):
            true_xs.append(x)
        else:
            false_xs.append(x)

    return (true_xs, false_xs)
