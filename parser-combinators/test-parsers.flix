


@test
def mreturn01() : Result[Int, Str] = 
    Parser.run("", Parser.mreturn(10))

@test
def merror01() : Result[Int, Str] = 
    Parser.run("", Parser.merror("Fail"))

