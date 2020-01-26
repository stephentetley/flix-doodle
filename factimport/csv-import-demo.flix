

pub def test01(): Unit =
    let path = "e:/coding/flix/flix-doodle/factimport/data/stations.csv";
    match CsvImport.newCsvSource(path, CsvFormat.Default, true) {
        case Err(msg) => Console.printLine(msg)
        case Ok(csvSource) => 
            match CsvImport.collect(row -> CsvImport.getStringByLabel(row, "Name"), csvSource) {
                case Err(msg) => Console.printLine(msg)
                case Ok(xs) => {
                    let str: Str = Util.mkStringBy(x -> x, String.lineSeparator(), xs);
                    Console.printLine(str) 
                }
            }
    }
    


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
    let path = "e:/coding/flix/flix-doodle/factimport/data/stations.csv";
    match CsvImport.newCsvSource(path, CsvFormat.Default, true) {
        case Err(msg) => Console.printLine(msg)
        case Ok(csvSource) => 
            match CsvImport.collect(getStation, csvSource) {
                case Err(msg) => Console.printLine(msg)
                case Ok(xs) => {
                    let str: Str = Util.mkStringBy(printStation, String.lineSeparator(), xs);
                    Console.printLine(str) 
                }
            }
    }


pub def temp01(x:Int32): (Int32, Int32) = (x, x)

