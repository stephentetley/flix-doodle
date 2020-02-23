@test
def test01(): List[Str] = 
    Validation.singleton("xyz") 
        |> Validation.append(Validation.singleton("abc"))
        |> Validation.dlistToList

@test
def test02(): Result[Str, Int32] = 
    Ok("hello")

@test
def test03(): Result[Str, List[Str]] = 
    Validation.valid("hello") |> Validation.validate


@test
def test04(): Result[Str, List[Str]] = 
    Validation.alt( 
        Validation.error("hello"),
        Validation.valid("hello")
    ) |> Validation.validate

@test
def test05(): Result[Str, List[Str]] = 
    Validation.withDefault("world", Validation.error("ERR*** Hello")) |> Validation.validate

@test
def test06(): Result[Str, List[Str]] = 
    Validation.pipeV3(
        Validation.valid("Hello"),
        Validation.valid(" "),
        Validation.valid("World"),
        (x,y,z) -> x + y + z
        ) |> Validation.validate
// @test
// def test02(): Validation[List[String], String] = 
//     let xs = Validation.pure("hello") :: Validation.pure("world") :: Nil;
//     Validation.mapV(x -> x, xs)
    

