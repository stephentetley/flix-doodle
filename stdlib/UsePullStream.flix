// Use flix 0.10.0

@test
def test01(): Unit  = 
    let s = PullStream.range(1, 10);
    PullStream.foreach(x -> Console.printLine(Int32.toString(x)), s)

@test
def test02(): Unit  = 
    let isEven = x -> x % 2 == 0;
    let s = PullStream.range(1, 10) |> PullStream.filter(isEven);
    PullStream.foreach(x -> Console.printLine(Int32.toString(x)), s)

@test
def test03(): Unit  = 
    let isOdd = x -> x % 2 != 0;
    let s = PullStream.ofArray([1,2,3,4,5,6,7]) |> PullStream.filter(isOdd);
    PullStream.foreach(x -> Console.printLine(Int32.toString(x)), s)

@test
def test04(): Unit  = 
    let s = PullStream.ofArray([1,2,3,4,5,6,7]) |> PullStream.take(3);
    PullStream.foreach(x -> Console.printLine(Int32.toString(x)), s)

@test
def test05(): Unit = 
    let s = PullStream.ofArray([1,2,3,4,5,6,7,8,9,10]) |> PullStream.drop(3);
    PullStream.foreach(x -> Console.printLine(Int32.toString(x)), s)