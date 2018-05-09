import sys

class csvFile():
    __slots__ = ("contents", "variables")

    def __init__(self, filepath):
        self.contents = ""
        for line in open(filepath):
            self.contents += line
        self.parseCSV()
        self.checkRelations()

    def parseCSV(self):
        lineList = self.contents.split("\n")
        header = lineList[0].split(",")
        numVars = len(header)
        self.variables = []
        for h in header:
            self.variables.append(variableData(h))

        lineList.pop(0)
        for line in lineList:
            data = line.split(",")
            if len(data) == numVars:
                for i in range(0,numVars):
                    self.variables[i].addData(data[i])

    def toString(self):
        contents = ""
        for v in self.variables:
            contents += v.toString() + "\n"
        return contents

    def checkRelations(self):
        for var1 in self.variables:
            for var2 in self.variables:
                if var1.name != var2.name:
                    var1.checkEqualTo(var2)
                    var1.checkLessThan(var2)


class variableData():
    __slots__ = ("name", "data", "isPrimitive", "isPos", "isNeg", "isZero", "isNull", "high", "low", "varsLessThan", "varsEqualTo")

    def __init__(self, name):
        self.name = name
        nameContents = name.split(".")
        if len(nameContents) > 1:
            if isPrimitive(nameContents[1]):
                self.isPrimitive = True
            else:
                self.isPrimitive = False
        else:
            self.isPrimitive = False
        self.data = []
        self.high = 0
        self.low = 0
        self.isPos = False
        self.isZero = False
        self.isNeg = False
        self.isNull = False
        self.varsLessThan = []
        self.varsEqualTo = []

    def addData(self, point):
        if point.isdigit() or point.startswith("-"):
            point = int(point)
            if len(self.data) == 0:
                self.low = point
                self.high = point
            else:
                if point > self.high:
                    self.high = point
                if point < self.low:
                    self.low = point
            if point > 0:
                self.isPos = True
            if point < 0:
                self.isNeg = True
            if point == 0:
                self.isZero = True
        else:
            print(point)
            if point == "NA":
                self.isNull = True
        self.data.append(point)

    def checkLessThan(self, otherVar):
        isLessThan = True
        if len(self.data) == len(otherVar.data):
            for i in range(0,len(self.data)):
                if isinstance(self.data[i], int) and isinstance(otherVar.data[i], int):
                    if self.data[i] > otherVar.data[i]:
                        isLessThan = False
        else:
            isLessThan = False
        if isLessThan:
            self.varsLessThan.append(otherVar.name)


    def checkEqualTo(self, otherVar):
        isEqual = False
        if len(self.data) == len(otherVar.data):
            for i in range(0,len(self.data)):
                if isinstance(self.data[i], int) and isinstance(otherVar.data[i], int):
                    if self.data[i] == otherVar.data[i]:
                        isEqual = True
        if isEqual:
            self.varsEqualTo.append(otherVar.name)

    def toString(self):
        return self.name + ": " + str(self.data)

    def getResults(self):
        results = ""
        if self.isPos and not self.isNeg:
            if self.isZero:
                results += "0 <= " + self.name + "\n"
            else:
                results += "0 < " + self.name + "\n"
        elif self.isNeg and not self.isPos:
            if self.isZero:
                results += self.name + " <= 0\n"
            else:
                results += self.name + " < 0\n"

        for varName in self.varsLessThan:
            if varName in self.varsEqualTo:
                results += self.name + " <= " + varName + "\n"
            else:
                results += self.name + " < " + varName + "\n"
        if not self.isPrimitive:
            if not self.isNull:
                results += self.name + " != null\n"
        return results


def isPrimitive(varType):
    primitives = ["int", "double", "byte", "char", "float", "long", "short"]
    varType = varType.split("_")[0]
    if varType in primitives:
        return True
    else:
        return False


def main():
    if len(sys.argv) == 2:
        filepath = sys.argv[1]
        csv = csvFile(filepath)
        results = ""
        for var in csv.variables:
            results += var.getResults()
        print(results)

    else:
        print("Usage: python3 basicCompare.py path/to/file\nNote: File should be in csv format")



if __name__ == '__main__':
    main()
