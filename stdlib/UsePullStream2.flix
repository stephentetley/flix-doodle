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

@test
def test06(): Unit = 
    let s = Stream.ofArray([1,2,3,4,5,6,7,8,9,10]) |> Stream.cons(-1000);
    Stream.foreach(x -> Console.printLine(Int32.toString(x)), s)

@test
def test07(): Unit = 
    let s1 = Stream.ofArray([1,2,3,4,5,6,7,8,9,10]);
    let s2 = Stream.ofArray(['a', 'b', 'c']);
    Stream.zip(s1, s2) |> Stream.foreach(match (x,y) -> Console.printLine(Int32.toString(x) + " " + Char.toString(y)))

@test
def test08(): Int32 = 
    let s1 = Stream.ofArray([1,2,3,4,5,6,7,8,9,10]);
    Stream.fold((x,y) -> x + y, 0, s1)

@test
def test09(): Unit = 
    let s1 = Stream.ofArray([1,2,3,4,5,6,7,8,9,10]);
    let s2 = Stream.ofList(100 :: 101 :: Nil);
    Stream.append(s1, s2) |> Stream.foreach(x -> Console.printLine(Int32.toString(x)))


@test
def test10(): Unit = 
    let s1 = Stream.unfold(x -> if (x > 6) None else Some(x, x+1), -3);
    Stream.foreach(x -> Console.printLine(Int32.toString(x)), s1)