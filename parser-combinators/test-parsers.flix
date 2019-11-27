


@test
def fail01() : Result[Unit, Str] = 
    ParserCombinators.run(ParserCombinators.fail(), "")

 @test
def throwError01() : Result[Unit, Str] = 
    ParserCombinators.run(ParserCombinators.throwError("** parser error**"), "")

 @test
def return01() : Result[Int32, Str] =
    ParserCombinators.run(ParserCombinators.return(1), "")

@test
def anyChar01() : Result[Char, Str] = 
    ParserCombinators.run(ParserCombinators.anyChar(), "abc")


