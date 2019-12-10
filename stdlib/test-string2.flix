/////////////////////////////////////////////////////////////////////////////
// init                                                                    //
/////////////////////////////////////////////////////////////////////////////
@test
def init01(): Bool =
    String2.init(_ -> 'A', 0) `assertEq!` ""

@test
def init02(): Bool =
    String2.init(_ -> 'A', 1) `assertEq!` "AA"

@test
def init03(): Bool =
    String2.init(_ -> 'A', 2) `assertEq!` "AA"

@test
def init04(): Bool =
    String.length(String2.init(_ -> '*', 50)) `assertEq!` 50


/////////////////////////////////////////////////////////////////////////////
// toArray                                                                 //
/////////////////////////////////////////////////////////////////////////////
@test
def toArray01() : Bool = 
    Array.length(String2.toArray("abcde")) `assertEq!` 5

/////////////////////////////////////////////////////////////////////////////
// map                                                                     //
/////////////////////////////////////////////////////////////////////////////
@test
def map01() : Bool =
    String2.map(_ -> 'A', "") `assertEq!` ""

@test
def map02() : Bool =
    String2.map(_ -> 'A', "1") `assertEq!` "A"

@test
def map03() : Bool =
    String2.map(_ -> 'A', "12") `assertEq!` "AA"

/////////////////////////////////////////////////////////////////////////////
// take                                                                    //
/////////////////////////////////////////////////////////////////////////////
@test
def take01(): Bool = String2.take(-1, "") `assertEq!` ""

@test
def take02(): Bool = String2.take(0, "") `assertEq!` ""

@test
def take03(): Bool = String2.take(1, "") `assertEq!` ""

@test
def take04(): Bool = String2.take(-1, "1") `assertEq!` ""

@test
def take05(): Bool = String2.take(0, "1") `assertEq!` ""

@test
def take06(): Bool = String2.take(1, "1") `assertEq!` "1"

@test
def take07(): Bool = String2.take(2, "1") `assertEq!` "1"

@test
def take08(): Bool = String2.take(0, "12") `assertEq!` ""

@test
def take09(): Bool = String2.take(1, "12") `assertEq!` "1"

@test
def take10(): Bool = String2.take(2, "12") `assertEq!` "12"

@test
def take11(): Bool = String2.take(2, "123456") `assertEq!` "12"

@test
def take12(): Bool = String2.take(4, "123456") `assertEq!` "1234"

@test
def take13(): Bool = String2.take(10, "123456") `assertEq!` "123456"


/////////////////////////////////////////////////////////////////////////////
// drop                                                                    //
/////////////////////////////////////////////////////////////////////////////
@test
def drop01(): Bool = String2.drop(-1, "") `assertEq!` ""

@test
def drop02(): Bool = String2.drop(0, "") `assertEq!` ""

@test
def drop03(): Bool = String2.drop(1, "") `assertEq!` ""

@test
def drop04(): Bool = String2.drop(-1, "1") `assertEq!` "1"

@test
def drop05(): Bool = String2.drop(0, "1") `assertEq!` "1"

@test
def drop06(): Bool = String2.drop(1, "1") `assertEq!` ""

@test
def drop07(): Bool = String2.drop(2, "1") `assertEq!` ""

@test
def drop08(): Bool = String2.drop(0, "12") `assertEq!` "12"

@test
def drop09(): Bool = String2.drop(1, "12") `assertEq!` "2"

@test
def drop10(): Bool = String2.drop(2, "12") `assertEq!` ""

@test
def drop11(): Bool = String2.drop(2, "123456") `assertEq!` "3456"

@test
def drop12(): Bool = String2.drop(4, "123456") `assertEq!` "56"

@test
def drop13(): Bool = String2.drop(10, "123456") `assertEq!` ""


/////////////////////////////////////////////////////////////////////////////
// viewl                                                                   //
/////////////////////////////////////////////////////////////////////////////
@test
def viewl01() : Bool = 
    String2.viewl("") `assertEq!` None

@test
def viewl02() : Bool = 
    String2.viewl("hello") `assertEq!` Some('h', "ello")

/////////////////////////////////////////////////////////////////////////////
// viewr                                                                   //
/////////////////////////////////////////////////////////////////////////////
@test
def viewr01() : Bool = 
    String2.viewr("") `assertEq!` None

@test
def viewr02() : Bool = 
    String2.viewr("hello") `assertEq!` Some("hell", 'o')

/////////////////////////////////////////////////////////////////////////////
// foldLeft                                                                //
/////////////////////////////////////////////////////////////////////////////
@test 
def foldLeft01() : Bool = 
    String2.foldLeft((ac,b) -> ac ::: (b :: Nil), Nil, "hello") `assertEq!` 'h' :: 'e' :: 'l' :: 'l' :: 'o' :: Nil

/////////////////////////////////////////////////////////////////////////////
// foldRight                                                                //
/////////////////////////////////////////////////////////////////////////////
@test 
def foldRight01() : Bool = 
    String2.foldRight((b,ac) -> b :: ac, Nil, "hello") `assertEq!` 'h' :: 'e' :: 'l' :: 'l' :: 'o' :: Nil


/////////////////////////////////////////////////////////////////////////////
// foldLeft2                                                                //
/////////////////////////////////////////////////////////////////////////////
@test 
def foldLeft201() : Bool = 
    String2.foldLeft2((ac,b,c) -> b :: c :: ac, Nil, "eca", "fdb") 
        `assertEq!` 'a' :: 'b' :: 'c' :: 'd' :: 'e' :: 'f' :: Nil

@test 
def foldLeft202() : Bool = 
    String2.foldLeft2((ac,b,c) -> (b, c) :: ac, Nil, "a", "fdb") 
        `assertEq!` ('a', 'f') :: Nil

@test 
def foldLeft203() : Bool = 
    String2.foldLeft2((ac,b,c) -> (b, c) :: ac, Nil, "", "fdb") 
        `assertEq!` Nil

@test 
def foldLeft204() : Bool = 
    String2.foldLeft2((ac,b,c) -> (b, c) :: ac, Nil, "", "") 
        `assertEq!` Nil

@test 
def foldLeft205() : Bool = 
    String2.foldLeft2((ac,b,c) -> (b, c) :: ac, Nil, "", "fdb") 
        `assertEq!` Nil

/////////////////////////////////////////////////////////////////////////////
// takeWhileLeft                                                           //
/////////////////////////////////////////////////////////////////////////////
@test
def takeWhileLeft01() : Bool = 
    String2.takeWhileLeft(Char.isDigit, "") `assertEq!` ""

@test
def takeWhileLeft02() : Bool = 
    String2.takeWhileLeft(Char.isDigit, "1234~hello") `assertEq!` "1234"

@test
def takeWhileLeft03() : Bool = 
    String2.takeWhileLeft(Char.isDigit, "1234567890") `assertEq!` "1234567890"

@test
def takeWhileLeft04() : Bool = 
    String2.takeWhileLeft(x -> !(Char.isDigit(x)), "1234567890") `assertEq!` ""

/////////////////////////////////////////////////////////////////////////////
// dropWhileLeft                                                           //
/////////////////////////////////////////////////////////////////////////////
@test
def dropWhileLeft01() : Bool = 
    String2.dropWhileLeft(Char.isDigit, "") `assertEq!` ""

@test
def dropWhileLeft02() : Bool = 
    String2.dropWhileLeft(Char.isDigit, "1234~hello") `assertEq!` "~hello"

@test
def dropWhileLeft03() : Bool = 
    String2.dropWhileLeft(Char.isDigit, "1234567890") `assertEq!` ""

@test
def dropWhileLeft04() : Bool = 
    String2.dropWhileLeft(Char.isDigit, "A1234567890") `assertEq!` "A1234567890"

/////////////////////////////////////////////////////////////////////////////
// dropWhileRight                                                          //
/////////////////////////////////////////////////////////////////////////////
@test
def dropWhileRight01() : Bool = 
    String2.dropWhileRight(Char.isDigit, "") `assertEq!` ""

@test
def dropWhileRight02() : Bool = 
    String2.dropWhileRight(Char.isDigit, "hello~1234") `assertEq!` "hello~"

@test
def dropWhileRight03() : Bool = 
    String2.dropWhileRight(Char.isDigit, "1234567890") `assertEq!` ""

@test
def dropWhileRight04() : Bool = 
    String2.dropWhileRight(Char.isDigit, "1234567890A") `assertEq!` "1234567890A"


@test
def dropWhileRight05() : Bool = 
    String2.dropWhileRight(Char.isDigit, "A1234567890") `assertEq!` "A"

/////////////////////////////////////////////////////////////////////////////
// exists                                                                  //
/////////////////////////////////////////////////////////////////////////////
@test
def exists01() : Bool = 
    assert!(String2.exists(Char.isDigit, "number 5 middle"))

@test
def exists02() : Bool = 
    assertNot!(String2.exists(Char.isDigit, ""))

@test
def exists03() : Bool = 
    assertNot!(String2.exists(Char.isDigit, "number"))

/////////////////////////////////////////////////////////////////////////////
// forall                                                                  //
/////////////////////////////////////////////////////////////////////////////
@test
def forall01() : Bool = 
    assert!(String2.forall(Char.isDigit, ""))

@test
def forall02() : Bool = 
    assert!(String2.forall(Char.isDigit, "12345"))

@test
def forall03() : Bool = 
    assertNot!(String2.forall(Char.isDigit, "12b45"))

/////////////////////////////////////////////////////////////////////////////
// isAscii                                                                 //
/////////////////////////////////////////////////////////////////////////////
@test
def isAscii01() : Bool = 
    assert!(String2.isAscii(""))


@test
def isAscii02() : Bool = 
    assert!(String2.isAscii("The Flix Programming Language"))

@test
def isAscii03() : Bool = 
    assertNot!(String2.isAscii("∀(x: e, y: e, z: e). ((z ⊑ x) ∧ (z ⊑ y)) → (z ⊑ (x ⊓ y))"))

/////////////////////////////////////////////////////////////////////////////
// isWhiteSpace                                                            //
/////////////////////////////////////////////////////////////////////////////
@test
def isWhiteSpace01() : Bool = 
    assert!(String2.isWhiteSpace(""))

@test
def isWhiteSpace02() : Bool = 
    assert!(String2.isWhiteSpace("       "))

@test
def isWhiteSpace03() : Bool = 
    assertNot!(String2.isWhiteSpace("     o  "))


// @test
// def isWhiteSpace04() : Bool = 
//     assert!(String2.isWhiteSpace("\t"))



/////////////////////////////////////////////////////////////////////////////
// splitAt                                                                 //
/////////////////////////////////////////////////////////////////////////////
@test
def splitAt01() : Bool = 
    String2.splitAt(1,"hello") `assertEq!` ("h", "ello")


@test
def trimLeft01() : Bool = 
    String2.trimLeft("    body ") `assertEq!` "body "

@test
def trimRighr01() : Bool = 
    String2.trimRight(" body      ") `assertEq!` " body"

@test
def padLeft01() : Bool = 
    String2.padLeft(10, '.', "body") `assertEq!` "......body"

@test
def padRight01() : Bool = 
    String2.padRight(10, '.', "body") `assertEq!` "body......"

@test 
def replaceAll01() : Bool =     
    String2.replaceAll("[aeiou]", "*", "Hello") `assertEq!` "H*ll*"

@test 
def replaceFirst01() : Bool =     
    String2.replaceFirst("[L]", "l", "HeLLo") `assertEq!` "HelLo"

/////////////////////////////////////////////////////////////////////////////
// indent                                                                  //
/////////////////////////////////////////////////////////////////////////////
@test 
def indent01(): Bool =     
    String2.indent(2, "onetwo") `assertEq!` "  onetwo"

/////////////////////////////////////////////////////////////////////////////
// lines                                                                   //
/////////////////////////////////////////////////////////////////////////////
@test 
def lines01(): Bool =     
    let s = "one" + String2.lineSeparator() + "two";
    Array.toList(String2.lines(s)) `assertEq!` ("one" :: "two" :: Nil)

@test 
def unlines01(): Bool = 
    let a = Array.new("", 2);
    a[0] = "one";
    a[1] = "two";
    String2.unlines(a) `assertEq!` ("one" + String2.lineSeparator() + "two")


@test 
def words01(): Bool =     
    let s = "one two three  four";
    Array.length(String2.words(s)) `assertEq!` 4

@test 
def unwords01(): Bool =     
    let a = Array.new("", 2);
    a[0] = "one";
    a[1] = "two";
    String2.unwords(a) `assertEq!` "one two"

/////////////////////////////////////////////////////////////////////////////
// reverse                                                                 //
/////////////////////////////////////////////////////////////////////////////
@test
def reverse01(): Bool = 
    String2.reverse("hello") `assertEq!` "olleh"

@test
def reverse02(): Bool = 
    String2.reverse("hello world!→") `assertEq!` "→!dlrow olleh"
