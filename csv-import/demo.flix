// :load d:/coding/flix/flix-doodle/csv-import/demo.flix

pub def hello(): Unit = 
    Console.printLine("Hello")

pub def loadTest01(): Unit =
    let path = "d:/coding/flix/flix-doodle/csv-import/data/stations.csv";
    let reader = CSVImport.new(path);
    if (CSVImport.hasNext(reader)) {
        let row = CSVImport.nextRow(reader);
        let str = CSVImport.getString(row,0) + " " + CSVImport.getString(row,1);
        Console.printLine(str) 
    } else {
        Console.printLine("no rows")
    }