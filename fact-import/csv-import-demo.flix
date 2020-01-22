

type alias Station = { name: Str, gridref: Str }

def getStation (row: CsvRow): Station = 
    { name = CsvImport.getStringByLabel(row, "Name"), gridref = CsvImport.getStringByLabel(row, "Grid_Ref") }

def printStation(station: Station): Str = 
    let name1 = station.name;
    let gridref1 = station.gridref;
    "name:${name1}, gridref:${gridref1}"

pub def loadTest01(): Unit =
    let path = "e:/coding/flix/flix-doodle/fact-import/data/stations.csv";
    let csvSource = CsvImport.newCsvSource(path, CsvFormat.Default, true);
    let xs: List[Station] = CsvImport.collect(csvSource, getStation);
    let str: Str = Util.mkStringBy(printStation, String.lineSeparator(), xs);
    Console.printLine(str) 

