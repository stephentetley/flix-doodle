

type alias Station = { name: Str, gridref: Str }

def getStation (row: CsvRow): Result[Station, Str] = 
    Util.pipeResult2(CsvImport.getStringByLabel(row, "Name"), 
                    CsvImport.getStringByLabel(row, "Grid_Ref"), 
                    (x,y) -> {name = x, gridref = y})
                

def printStation(station: Station): Str = 
    let name1 = station.name;
    let gridref1 = station.gridref;
    "name:${name1}, gridref:${gridref1}"

pub def loadTest01(): Unit =
    let path = "e:/coding/flix/flix-doodle/fact-import/data/stations.csv";
    let csvSource = CsvImport.newCsvSource(path, CsvFormat.Default, true);
    let xs: List[Station] = CsvImport.collectOks(csvSource, getStation);
    let str: Str = Util.mkStringBy(printStation, String.lineSeparator(), xs);
    Console.printLine(str) 

