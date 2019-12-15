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
// dropRight                                                               //
/////////////////////////////////////////////////////////////////////////////
@test
def dropRight01(): Bool = String2.dropRight(-1, "") `assertEq!` ""

@test
def dropRight02(): Bool = String2.dropRight(0, "") `assertEq!` ""

@test
def dropRight03(): Bool = String2.dropRight(1, "") `assertEq!` ""

@test
def dropRight04(): Bool = String2.dropRight(-1, "1") `assertEq!` "1"

@test
def dropRight05(): Bool = String2.dropRight(0, "1") `assertEq!` "1"

@test
def dropRight06(): Bool = String2.dropRight(1, "1") `assertEq!` ""

@test
def dropRight07(): Bool = String2.dropRight(2, "1") `assertEq!` ""

@test
def dropRight08(): Bool = String2.dropRight(0, "12") `assertEq!` "12"

@test
def dropRight09(): Bool = String2.dropRight(1, "12") `assertEq!` "1"

@test
def dropRight10(): Bool = String2.dropRight(2, "12") `assertEq!` ""

@test
def dropRight11(): Bool = String2.dropRight(2, "123456") `assertEq!` "1234"

@test
def dropRight12(): Bool = String2.dropRight(4, "123456") `assertEq!` "12"

@test
def dropRight13(): Bool = String2.dropRight(10, "123456") `assertEq!` ""

/////////////////////////////////////////////////////////////////////////////
// takeWhileLeft                                                           //
/////////////////////////////////////////////////////////////////////////////
@test
def takeWhileLeft01(): Bool = String2.takeWhileLeft(Char.isDigit, "") `assertEq!` ""

@test
def takeWhileLeft02(): Bool = String2.takeWhileLeft(Char.isDigit, "A") `assertEq!` ""

@test
def takeWhileLeft03(): Bool = String2.takeWhileLeft(Char.isDigit, "4") `assertEq!` "4"

@test
def takeWhileLeft04(): Bool = String2.takeWhileLeft(Char.isDigit, "AB") `assertEq!` ""

@test
def takeWhileLeft05(): Bool = String2.takeWhileLeft(Char.isDigit, "A1")  `assertEq!` ""

@test
def takeWhileLeft06(): Bool = String2.takeWhileLeft(Char.isDigit, "1A") `assertEq!` "1"

@test
def takeWhileLeft07(): Bool = String2.takeWhileLeft(Char.isDigit, "12") `assertEq!` "12"

@test
def takeWhileLeft08(): Bool = String2.takeWhileLeft(Char.isDigit, "46A8B128717") `assertEq!` "46"

/////////////////////////////////////////////////////////////////////////////
// takeWhileRight                                                          //
/////////////////////////////////////////////////////////////////////////////
@test
def takeWhileRight01(): Bool = String2.takeWhileRight(Char.isDigit, "") `assertEq!` ""

@test
def takeWhileRight02(): Bool = String2.takeWhileRight(Char.isDigit, "A") `assertEq!` ""

@test
def takeWhileRight03(): Bool = String2.takeWhileRight(Char.isDigit, "4") `assertEq!` "4"

@test
def takeWhileRight04(): Bool = String2.takeWhileRight(Char.isDigit, "AB") `assertEq!` ""

@test
def takeWhileRight05(): Bool = String2.takeWhileRight(Char.isDigit, "1A")  `assertEq!` ""

@test
def takeWhileRight06(): Bool = String2.takeWhileRight(Char.isDigit, "A1") `assertEq!` "1"

@test
def takeWhileRight07(): Bool = String2.takeWhileRight(Char.isDigit, "12") `assertEq!` "12"

@test
def takeWhileRight08(): Bool = String2.takeWhileRight(Char.isDigit, "46A8B128717") `assertEq!` "128717"

/////////////////////////////////////////////////////////////////////////////
// dropWhileLeft                                                           //
/////////////////////////////////////////////////////////////////////////////
@test
def dropWhileLeft01(): Bool = String2.dropWhileLeft(Char.isDigit, "") `assertEq!` ""

@test
def dropWhileLeft02(): Bool = String2.dropWhileLeft(Char.isDigit, "A") `assertEq!` "A"

@test
def dropWhileLeft03(): Bool = String2.dropWhileLeft(Char.isDigit, "4") `assertEq!` ""

@test
def dropWhileLeft04(): Bool = String2.dropWhileLeft(Char.isDigit, "AB") `assertEq!` "AB"

@test
def dropWhileLeft05(): Bool = String2.dropWhileLeft(Char.isDigit, "A1") `assertEq!` "A1"

@test
def dropWhileLeft06(): Bool = String2.dropWhileLeft(Char.isDigit, "1A") `assertEq!` "A"

@test
def dropWhileLeft07(): Bool = String2.dropWhileLeft(Char.isDigit, "12") `assertEq!` ""

@test
def dropWhileLeft08(): Bool = String2.dropWhileLeft(Char.isDigit, "46A8B128717") `assertEq!` "A8B128717"

/////////////////////////////////////////////////////////////////////////////
// dropWhileRight                                                          //
/////////////////////////////////////////////////////////////////////////////
@test
def dropWhileRight01(): Bool = String2.dropWhileRight(Char.isDigit, "") `assertEq!` ""

@test
def dropWhileRight02(): Bool = String2.dropWhileRight(Char.isDigit, "A") `assertEq!` "A"

@test
def dropWhileRight03(): Bool = String2.dropWhileRight(Char.isDigit, "4") `assertEq!` ""

@test
def dropWhileRight04(): Bool = String2.dropWhileRight(Char.isDigit, "AB") `assertEq!` "AB"

@test
def dropWhileRight05(): Bool = String2.dropWhileRight(Char.isDigit, "A1") `assertEq!` "A"

@test
def dropWhileRight06(): Bool = String2.dropWhileRight(Char.isDigit, "1A") `assertEq!` "1A"

@test
def dropWhileRight07(): Bool = String2.dropWhileRight(Char.isDigit, "12") `assertEq!` ""

@test
def dropWhileRight08(): Bool = String2.dropWhileRight(Char.isDigit, "46A8B128717") `assertEq!` "46A8B"

/////////////////////////////////////////////////////////////////////////////
// foldLeft                                                                //
/////////////////////////////////////////////////////////////////////////////
@test 
def foldLeft01() : Bool = 
    String2.foldLeft((ac,b) -> if (Char.isDigit(b)) ac+1 else ac, 0, "") `assertEq!` 0

@test 
def foldLeft02() : Bool = 
    String2.foldLeft((ac,b) -> if (Char.isDigit(b)) ac+1 else ac, 0, "1") `assertEq!` 1

@test 
def foldLeft03() : Bool = 
    String2.foldLeft((ac,b) -> if (Char.isDigit(b)) ac+1 else ac, 0, "A") `assertEq!` 0

@test 
def foldLeft04() : Bool = 
    String2.foldLeft((ac,b) -> if (Char.isDigit(b)) ac+1 else ac, 0, "12") `assertEq!` 2

@test 
def foldLeft05() : Bool = 
    String2.foldLeft((ac,b) -> if (Char.isDigit(b)) ac+1 else ac, 0, "1234567890") `assertEq!` 10

@test
def foldLeft06() : Bool =
    String2.foldLeft((ac,b) -> if (Char.isDigit(b)) ac+1 else ac, 0, "1!2!3!4!5!") `assertEq!` 5

@test
def foldLeft07() : Bool =
    String2.foldLeft((ac,b) -> Char.toString(b) + ac, "", "hello") `assertEq!` "olleh"

/////////////////////////////////////////////////////////////////////////////
// foldRight                                                               //
/////////////////////////////////////////////////////////////////////////////
@test 
def foldRight01() : Bool = 
    String2.foldRight((b,ac) -> if (Char.isDigit(b)) ac+1 else ac, 0, "") `assertEq!` 0

@test 
def foldRight02() : Bool = 
    String2.foldRight((b,ac) -> if (Char.isDigit(b)) ac+1 else ac, 0, "1") `assertEq!` 1

@test 
def foldRight03() : Bool = 
    String2.foldRight((b,ac) -> if (Char.isDigit(b)) ac+1 else ac, 0, "A") `assertEq!` 0

@test 
def foldRight04() : Bool = 
    String2.foldRight((b,ac) -> if (Char.isDigit(b)) ac+1 else ac, 0, "12") `assertEq!` 2

@test 
def foldRight05() : Bool = 
    String2.foldRight((b,ac) -> if (Char.isDigit(b)) ac+1 else ac, 0, "1234567890") `assertEq!` 10

@test
def foldRight06() : Bool =
    String2.foldRight((b,ac) -> if (Char.isDigit(b)) ac+1 else ac, 0, "1!2!3!4!5!") `assertEq!` 5


@test
def foldRight07() : Bool =
    String2.foldRight((b,ac) -> Char.toString(b) + ac, "", "hello") `assertEq!` "hello"

/////////////////////////////////////////////////////////////////////////////
// foldLeft2                                                               //
/////////////////////////////////////////////////////////////////////////////

@test
def foldLeft201(): Bool =
    String2.foldLeft2((c, a, b) -> c + Char.toString(b) + Char.toString(a), "3", "", "") `assertEq!` "3"

@test
def foldLeft202(): Bool =
    String2.foldLeft2((c, a, b) -> c + Char.toString(b) + Char.toString(a), "3", "1", "") `assertEq!` "3"

@test
def foldLeft203(): Bool =
    String2.foldLeft2((c, a, b) -> c + Char.toString(b) + Char.toString(a), "3", "", "1") `assertEq!` "3"

@test
def foldLeft204(): Bool =
    String2.foldLeft2((c, a, b) -> c + Char.toString(b) + Char.toString(a), "3", "1", "2") `assertEq!` "321"

@test
def foldLeft205(): Bool =
    String2.foldLeft2((c, a, b) -> c + Char.toString(b) + Char.toString(a), "3", "2", "1") `assertEq!` "312"

@test
def foldLeft206(): Bool =
    String2.foldLeft2((c, a, b) -> c + Char.toString(b) + Char.toString(a), "5", "31", "42") `assertEq!` "54321"

@test
def foldLeft207(): Bool =
    String2.foldLeft2((c, a, b) -> c + Char.toString(b) + Char.toString(a), "5", "13", "24") `assertEq!` "52143"
    
/////////////////////////////////////////////////////////////////////////////
// foldRight2                                                              //
/////////////////////////////////////////////////////////////////////////////

@test
def foldRight201(): Bool =
    String2.foldRight2((a, b, c) -> Char.toString(a) + Char.toString(b) + c, "3", "", "") `assertEq!` "3"

@test
def foldRight202(): Bool =
    String2.foldRight2((a, b, c) -> Char.toString(a) + Char.toString(b) + c, "3", "1", "") `assertEq!` "3"

@test
def foldRight203(): Bool =
    String2.foldRight2((a, b, c) -> Char.toString(a) + Char.toString(b) + c, "3", "", "1") `assertEq!` "3"

@test
def foldRight204(): Bool =
    String2.foldRight2((a, b, c) -> Char.toString(a) + Char.toString(b) + c, "3", "1", "2") `assertEq!` "123"

@test
def foldRight205(): Bool =
    String2.foldRight2((a, b, c) -> Char.toString(a) + Char.toString(b) + c, "3", "2", "1") `assertEq!` "213"

@test
def foldRight206(): Bool =
    String2.foldRight2((a, b, c) -> Char.toString(a) + Char.toString(b) + c, "5", "13", "24") `assertEq!` "12345"

@test
def foldRight207(): Bool =
    String2.foldRight2((a, b, c) -> Char.toString(a) + Char.toString(b) + c, "5", "24", "13") `assertEq!` "21435"

/////////////////////////////////////////////////////////////////////////////
// unfold                                                                  //
/////////////////////////////////////////////////////////////////////////////
@test
def unfold01() : Bool = 
    String2.unfold(_ -> None, 0) `assertEq!` ""

@test
def unfold02() : Bool =
    String2.unfold(x -> if (x > 0) None else Some('A', x+1), 0) `assertEq!` "A"

@test
def unfold03() : Bool =
    String2.unfold(x -> if (x > 1) None else Some('A', x+1), 0) `assertEq!` "AA"

@test
def unfold04() : Bool =
    String2.unfold(x -> if (x > 4) None else Some('A', x+1), 0) `assertEq!` "AAAAA"

/////////////////////////////////////////////////////////////////////////////
// exists                                                                  //
/////////////////////////////////////////////////////////////////////////////
@test
def exists01() : Bool = assertNot!(String2.exists(Char.isDigit, ""))
    
@test
def exists02() : Bool = assertNot!(String2.exists(Char.isDigit, "A"))

@test
def exists03() : Bool = assert!(String2.exists(Char.isDigit, "5"))

@test
def exists04() : Bool = assertNot!(String2.exists(Char.isDigit, "A!"))

@test
def exists05() : Bool = assert!(String2.exists(Char.isDigit, "A1"))

@test
def exists06(): Bool = assert!(String2.exists(Char.isDigit, "1A"))

@test
def exists07(): Bool = assert!(String2.exists(Char.isDigit, "*6"))

@test
def exists08(): Bool = assertNot!(String2.exists(Char.isDigit, "ZERO"))

@test
def exists09(): Bool = assert!(String2.exists(Char.isDigit, "A1B"))

/////////////////////////////////////////////////////////////////////////////
// forall                                                                  //
/////////////////////////////////////////////////////////////////////////////
@test
def forall01(): Bool = assert!(String2.forall(Char.isDigit, ""))

@test
def forall02(): Bool = assertNot!(String2.forall(Char.isDigit, "A"))

@test
def forall03(): Bool = assert!(String2.forall(Char.isDigit, "5"))

@test
def forall04(): Bool = assertNot!(String2.forall(Char.isDigit, "A!"))

@test
def forall05(): Bool = assertNot!(String2.forall(Char.isDigit, "A1"))

@test
def forall06(): Bool = assertNot!(String2.forall(Char.isDigit, "1A"))

@test
def forall07(): Bool = assert!(String2.forall(Char.isDigit, "19"))

@test
def forall08(): Bool = assertNot!(String2.forall(Char.isDigit, "A+z"))

@test
def forall09(): Bool = assertNot!(String2.forall(Char.isDigit, "A1B"))

@test
def forall10(): Bool = assert!(String2.forall(Char.isDigit, "2019"))


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

/////////////////////////////////////////////////////////////////////////////
// trimLeft                                                                //
/////////////////////////////////////////////////////////////////////////////
@test
def trimLeft01() : Bool = String2.trimLeft("") `assertEq!` ""

@test
def trimLeft02() : Bool = String2.trimLeft("A") `assertEq!` "A"

@test
def trimLeft03() : Bool = String2.trimLeft(" A") `assertEq!` "A"

@test
def trimLeft04() : Bool = String2.trimLeft("  A") `assertEq!` "A"

@test
def trimLeft05() : Bool = String2.trimLeft(" ABC") `assertEq!` "ABC"

@test
def trimLeft06() : Bool = String2.trimLeft("ABC ") `assertEq!` "ABC "

@test
def trimLeft07() : Bool = String2.trimLeft(" ABC ") `assertEq!` "ABC "

/////////////////////////////////////////////////////////////////////////////
// trimRight                                                               //
/////////////////////////////////////////////////////////////////////////////
@test
def trimRight01() : Bool = String2.trimRight("") `assertEq!` ""

@test
def trimRight02() : Bool = String2.trimRight("A") `assertEq!` "A"

@test
def trimRight03() : Bool = String2.trimRight("A ") `assertEq!` "A"

@test
def trimRight04() : Bool = String2.trimRight("A  ") `assertEq!` "A"

@test
def trimRight05() : Bool = String2.trimRight("ABC ") `assertEq!` "ABC"

@test
def trimRight06() : Bool = String2.trimRight(" ABC") `assertEq!` " ABC"

@test
def trimRight07() : Bool = String2.trimRight(" ABC ") `assertEq!` " ABC"


/////////////////////////////////////////////////////////////////////////////
// splitAt                                                                 //
/////////////////////////////////////////////////////////////////////////////
@test
def splitAt01() : Bool = 
    String2.splitAt(1,"hello") `assertEq!` ("h", "ello")


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
