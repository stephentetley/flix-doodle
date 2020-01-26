
rel Name(name: Str)

def makeName(name: Str): #{ Name } = 
    Name(name).

def getName(row: CsvRow): CsvResult[#{Name}] = 
    Validation.fmap(makeName, CsvImport.getStringByLabel(row, "Name"))

def fail1(msg: Str): #{Name} = 
    Console.printLine(msg);
    #{ }

pub def test01(): #{Name} =
    let path = "e:/coding/flix/flix-doodle/factimport/data/stations.csv";
    match CsvImport.newCsvSource(path, CsvFormat.Default, true) {
        case Err(msg) => fail1(msg)
        case Ok(csvSource) => 
            match CsvImport.collect(getName, csvSource) {
                case Err(msg) => fail1(msg)
                case Ok(rel) => solve rel
            }
    }
    


// rel Station(name: Str, gridref: Str)


// def getStation(row: CsvRow): CsvResult[#{Station}] = 
//     Validation.pipeV2(CsvImport.getStringByLabel(row, "Name"), 
//                         CsvImport.getStringByLabel(row, "Grid_Ref"), 
//                         (x,y) -> Station(x, y).)


                
// pub def printStation(station: Station): Str = 
//     let name1 = station.name;
//     let gridref1 = station.gridref;
//     "name:${name1}, gridref:${gridref1}"

// pub def test02(): Unit =
//     let path = "e:/coding/flix/flix-doodle/factimport/data/stations.csv";
//     match CsvImport.newCsvSource(path, CsvFormat.Default, true) {
//         case Err(msg) => Console.printLine(msg)
//         case Ok(csvSource) => 
//             match CsvImport.collect(getStation, csvSource) {
//                 case Err(msg) => Console.printLine(msg)
//                 case Ok(rel) => solve rel            
//             }
//     }


