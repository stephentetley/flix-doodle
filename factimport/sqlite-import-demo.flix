

type alias Lang = { name: Str, year: Int32 }

def getLang (row: SQLiteRow): SQLiteResult[Lang] = 
    Validation.pipeV2(
        SQLiteImport.getStringByLabel(row, "name"), 
        SQLiteImport.getInt32ByLabel(row, "year"), 
        (x,y) -> { name = x, year = y })

def printLang(lang: Lang): Str = 
    let name1 = lang.name;
    let year1 = Int32.toString(lang.year);
    "name:${name1}, year:${year1}"

pub def test01(): Unit =
    let path = "e:/coding/flix/flix-doodle/factimport/data/langs.sqlite";
    let query = "SELECT name, year FROM language";
    match SQLiteImport.newSQLiteSource(path, query) {
        case Err(msg) => Console.printLine(msg)
        case Ok(src) => {
            match SQLiteImport.collect(getLang, src) {
                case Err(msg) => Console.printLine(msg)
                case Ok(xs) => 
                    let str: Str = Util.mkStringBy(printLang, String.lineSeparator(), xs);
                    Console.printLine(str) 
            }
        }
    }
