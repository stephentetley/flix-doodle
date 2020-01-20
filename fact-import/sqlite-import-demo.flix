

type alias Lang = { name: Str, year: Int32 }

def getLang (row: SQLiteRow): Lang = 
    { name = SQLiteImport.getStringByLabel(row, "name"), year = SQLiteImport.getInt32ByLabel(row, "year") }

def printLang(lang: Lang): Str = 
    let name1 = lang.name;
    let year1 = Int32.toString(lang.year);
    "name:${name1}, year:${year1}"

pub def loadTest01(): Unit =
    let path = "e:/coding/flix/flix-doodle/fact-import/data/langs.sqlite";
    let query = "SELECT name, year FROM language";
    let xs: List[Lang] = SQLiteImport.collect(getLang, path, query);
    let str: Str = Util.mkStringBy(printLang, String.lineSeparator(), xs);
    Console.printLine(str) 

