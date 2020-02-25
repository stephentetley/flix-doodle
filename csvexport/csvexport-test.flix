
@test
def stations01(): Unit = 
    let dest = "d:/coding/flix/flix-doodle/csvexport/data/stations_export.csv";
    let writer = CsvExport.new(dest, ["Name", "Grid_Ref"]);
    CsvExport.writeRow(writer, ["Bradford Interchange", "SE1657832808"]);
    CsvExport.writeRow(writer, ["Halifax", "SE0973324980"]);
    CsvExport.writeRow(writer, ["Low Moor", "SE1633428224"]);
    CsvExport.writeRow(writer, ["Mytholmroyd", "SE0127125846"]);
    CsvExport.writeRow(writer, ["Sowerby Bridge", "SE0624023505"]);
    CsvExport.close(writer)


rel Dummy(name: String, intval: Int32)

pub def dummyRelations(): #{ Dummy } = solve #{
    Dummy("a", 0).
    Dummy("b", 0).
    Dummy("c", 1).
    Dummy("d", 1).
}

def dummyCells(src: (String,Int)): Array[String] = 
    let (name,i) = src;
    [name, Int32.toString(i)]


pub def exportDummy(filepath: String, d1: #{ Dummy }): Unit = 
    let writer = CsvExport.new(filepath, ["Name", "Grid_Ref"]);
    let writeRow = (x, _) -> { let cells = dummyCells(x); CsvExport.writeRow(writer, cells) };
    fold Dummy () writeRow d1;
    CsvExport.close(writer)

@test
def test01(): Unit = 
    let dest = "d:/coding/flix/flix-doodle/csvexport/data/dummy_export.csv";
    exportDummy(dest, dummyRelations())


