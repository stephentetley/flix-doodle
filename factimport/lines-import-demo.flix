def fail1(msg: Str): reln = 
    Console.printLine(msg);
    #{ }


rel Colour(name: Str)

def makeColour(name: Str): #{ Colour } = 
    Colour(name).

def getColour(line: Str): LineResult[#{Colour}] = 
    Validation.fmap(makeColour, LinesImport.nonempty(line))



pub def test01(): #{Colour} =
    let path = "e:/coding/flix/flix-doodle/factimport/data/colours.txt";
    match LinesImport.newLinesSource(path) {
        case Err(msg) => fail1(msg)
        case Ok(linesSource) => 
            match LinesImport.collect(getColour, linesSource) {
                case Err(msg) => fail1(msg)
                case Ok(relns) => solve relns
            }
    }
    

