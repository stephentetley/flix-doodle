

pub def test01(): Unit =
    let path = "e:/coding/flix/flix-doodle/fact-import/data/stations.csv";
    let csvSource = CsvImport.newCsvSource(path, CsvFormat.Default, true);
    let xs: List[Str] = CsvImport.collect(csvSource, row -> CsvImport.getStringByLabel(row, "Name"));
    let str: Str = Util.mkStringBy(x -> x, String.lineSeparator(), xs);
    Console.printLine(str) 


type alias Station = { name: Str, gridref: Str }


pub def getStation (row: CsvRow): CsvResult[Station] = 
    Validation.pipeV2(CsvImport.getStringByLabel(row, "Name"), 
                        CsvImport.getStringByLabel(row, "Grid_Ref"), 
                        (x,y) -> { name = x, gridref = y })


                
pub def printStation(station: Station): Str = 
    let name1 = station.name;
    let gridref1 = station.gridref;
    "name:${name1}, gridref:${gridref1}"

pub def test02(): Unit =
    let path = "e:/coding/flix/flix-doodle/fact-import/data/stations.csv";
    let csvSource = CsvImport.newCsvSource(path, CsvFormat.Default, true);
    let xs: List[Station] = CsvImport.collect(csvSource, getStation);
    let str: Str = Util.mkStringBy(printStation, String.lineSeparator(), xs);
    Console.printLine(str) 
