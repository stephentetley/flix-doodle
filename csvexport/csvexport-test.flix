



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



