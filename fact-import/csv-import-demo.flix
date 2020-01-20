

type alias Station = { name: Str, gridref: Str }

def getStation (row: CsvRow): Station = 
    { name = CsvImport.getString(row, 0), gridref = CsvImport.getString(row, 1) }

def printStation(station: Station): Str = 
    let name1 = station.name;
    let gridref1 = station.gridref;
    "name:${name1}, gridref:${gridref1}"

pub def loadTest01(): Unit =
    let path = "e:/coding/flix/flix-doodle/fact-import/data/stations.csv";
    let xs: List[Station] = CsvImport.collect(getStation, path, true);
    let str: Str = Util.mkStringBy(printStation, String.lineSeparator(), xs);
    Console.printLine(str) 

