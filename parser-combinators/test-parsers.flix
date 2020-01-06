


@test
def fail01() : Result[Unit, Str] = 
    StringParser.run(StringParser.fail(), "")

@test
def return01() : Result[Int32, Str] =
    StringParser.run(StringParser.return(1), "")

@test
def bind01() : Result[Int32, Str] =
    StringParser.run(StringParser.bind(StringParser.return(1), v -> StringParser.return(v)), "")

@test
def fmap01() : Result[Int32, Str] =
    StringParser.run(StringParser.fmap(x -> x + 1, StringParser.return(1)), "")


 @test
def throwError01() : Result[Unit, Str] = 
    StringParser.run(StringParser.throwError("** parser error**"), "")



@test
def anyChar01() : Result[Char, Str] = 
    StringParser.run(StringParser.anyChar(), "abc")

@test
def alt01() : Result[Char, Str] = 
    let parser = StringParser.alt(StringParser.char('A'), StringParser.char('a'));
    StringParser.run(parser, "abc")

@test
def count01() : Result[List[Char], Str] = 
    let parser = StringParser.count(3, StringParser.letter());
    StringParser.run(parser, "ABCdefg")


@test
def many01() : Result[List[Char], Str] = 
    let parser = StringParser.many(StringParser.letter());
    StringParser.run(parser, "ABC") 

@test
def many101() : Result[List[Char], Str] = 
    let parser = StringParser.many1(StringParser.letter());
    StringParser.run(parser, "ABC") 

@test
def many102() : Result[List[Char], Str] = 
    let parser = StringParser.many1(StringParser.letter());
    StringParser.run(parser, "123") 

    