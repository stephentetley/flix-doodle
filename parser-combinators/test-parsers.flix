


@test
def fail01() : Result[Unit, Str] = 
    StringParser.run(StringParser.fail(), "")

 @test
def throwError01() : Result[Unit, Str] = 
    StringParser.run(StringParser.throwError("** parser error**"), "")

 @test
def return01() : Result[Int32, Str] =
    StringParser.run(StringParser.return(1), "")

@test
def anyChar01() : Result[Char, Str] = 
    StringParser.run(StringParser.anyChar(), "abc")

@test
def alt01() : Result[Char, Str] = 
    let parser = StringParser.alt(StringParser.char('A'), StringParser.char('a'));
    StringParser.run(parser, "abc")

// @test
// def count01() : Result[List[Char], Str] = 
//     let parser = StringParser.count(3, StringParser.letter);
//     StringParser.run(parser, "AAA")

// StringParser.run(StringParser.count(3, StringParser.letter), "AAA")

// @test
// def many01() : Result[List[Char], Str] = 
//     let parser = StringParser.many(StringParser.letter);
//     StringParser.run(parser, "ABC") 

