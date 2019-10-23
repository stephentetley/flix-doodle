


@test
def return01() : Result[Int, Str] = 
    Parser.run("", Parser.return(10))

@test
def merror01() : Result[Int, Str] = 
    Parser.run("", Parser.merror("Fail"))

@test
def bind01(): Result[Int, Str] = 
    Parser.run("", Parser.bind(Parser.return(1), x -> Parser.return(x+1)))
