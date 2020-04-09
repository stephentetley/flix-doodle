// Use flix 0.10.0

@test
def test01(): Unit  = 
    let s = Stream.range(1, 10);
    Stream.foreach(x -> Console.printLine(Int32.toString(x)), s)

@test
def test02(): Unit  = 
    let isEven = x -> x % 2 == 0;
    let s = Stream.range(1, 10) |> Stream.filter(isEven);
    Stream.foreach(x -> Console.printLine(Int32.toString(x)), s)

@test
def test03(): Unit  = 
    let isOdd = x -> x % 2 != 0;
    let s = Stream.ofArray([1,2,3,4,5,6,7]) |> Stream.filter(isOdd);
    Stream.foreach(x -> Console.printLine(Int32.toString(x)), s)

@test
def test04(): Unit  = 
    let s = Stream.ofArray([1,2,3,4,5,6,7]) |> Stream.take(3);
    Stream.foreach(x -> Console.printLine(Int32.toString(x)), s)

@test
def test05(): Unit = 
    let s = Stream.ofArray([1,2,3,4,5,6,7,8,9,10]) |> Stream.drop(3);
    Stream.foreach(x -> Console.printLine(Int32.toString(x)), s)