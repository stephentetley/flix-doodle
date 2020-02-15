

@test
def new01(): FilePath = 
    FilePath.new("e:/coding/flix")

@test
def new02(): FilePath = 
    FilePath.new("..")

@test
def toString01(): String = 
    FilePath.new("e:/coding/flix") |> FilePath.toString

@test
def toString02(): String = 
    FilePath.new("..") |> FilePath.toString


@test
def normalize01(): String = 
    FilePath.new("c:/root/child1/..") |> FilePath.normalize |> FilePath.toString

@test
def getCurrentDirectory01(): FilePath = 
    Directory.getCurrentDirectory()

@test
def readFile01(): String = 
    let path = FilePath.new("E:\coding\flix\flix-doodle\factimport\data\colours.txt");
    match File.readFile(path) {
        case Err(msg) => "***" + msg + "***"
        case Ok(s) => s
    }

@test
def writeFile01(): String = 
    let path = FilePath.new("E:\coding\flix\flix-doodle\factimport\data\colours-out.txt");
    match File.writeFile(path, "red, green, blue") {
        case Err(msg) => "***" + msg + "***"
        case Ok(_) => "Okay"
    }