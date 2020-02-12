from random import randint
from math import ceil

minDuration = 1
maxDuration = 4
minWorkers = 1
maxWorkers = 5
outputFile = "fff"
precMultiplier = 1


def writeWelcome():
    print("Welcome to the random project generator")
    print("This will create a n number of tasks in the following format")
    print("tarefa(id,[id1,id2,id3,...],duration,numberOfWorkers)")
    print("with id,id1,id2,...,duration,numberOfWorkers being all variables")


def writeTasks(file, numberOfTasks, allSuccessors=None, idt=None):
    if idt == 0:
        return
    if not idt:
        idt = numberOfTasks
    if not allSuccessors:
        allSuccessors = [None] * (numberOfTasks+1)
    outArray = []
    numSuccessors = randint(0, ceil(precMultiplier*(numberOfTasks-idt)))
    for j in range(0, numSuccessors):
        outArray.append(randint(idt+1, numberOfTasks))
    outArray = removeRedundantPrecs(idt, list(dict.fromkeys(outArray)), allSuccessors)
    outArray.sort()
    writeTasks(file, numberOfTasks, allSuccessors, idt-1)
    writeTask(file, idt, outArray)


def writeTask(file, idt, successors):
    file.write("tarefa(" + str(idt) + ",[")
    if successors:
        file.write(str(successors.pop(0)))
    for successor in successors:
        file.write("," + str(successor))
    file.write("],")
    file.write(str(randint(minDuration, maxDuration)) + ",")
    file.write(str(randint(minWorkers, maxWorkers)) + ").\n")


def removeRedundantPrecs(idt, successors, allSuccessors):
    clearSuccessors = successors.copy()
    idsForRemoval = getAllSuccessors(successors, allSuccessors)
    for sucid in idsForRemoval:
        if sucid in clearSuccessors:
            clearSuccessors.remove(sucid)
    setAllSuccessors(idt, successors, allSuccessors)
    return clearSuccessors


def getAllSuccessors(directSuccessors, allSuccessors):
    successors = []
    for directSuccessor in directSuccessors:
        successors += allSuccessors[directSuccessor]
    return list(dict.fromkeys(successors))


def setAllSuccessors(idt, directSuccessors, allSuccessors):
    successors = getAllSuccessors(directSuccessors, allSuccessors)
    for directSuccessor in directSuccessors:
        successors.append(directSuccessor)
    allSuccessors[idt] = successors


def run():
    writeWelcome()
    numberOfTasks = int(input("Choose the number of tasks"))
    file = open(outputFile, "w")
    writeTasks(file, numberOfTasks)
    file.close()


if __name__ == '__main__':
    run()
