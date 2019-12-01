
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
    String2.map(_ -> '*', "hello") `assertEq!` "*****"

/////////////////////////////////////////////////////////////////////////////
// take                                                                    //
/////////////////////////////////////////////////////////////////////////////
@test
def take01() : Bool = 
    String2.take(10, "hello") `assertEq!` "hello"

@test
def take02() : Bool = 
    String2.take(1, "hello") `assertEq!` "h"

/////////////////////////////////////////////////////////////////////////////
// drop                                                                    //
/////////////////////////////////////////////////////////////////////////////
@test
def drop01() : Bool = 
    String2.drop(10, "hello") `assertEq!` ""

@test
def drop02() : Bool = 
    String2.drop(1, "hello") `assertEq!` "ello"

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
// dropWhile                                                               //
/////////////////////////////////////////////////////////////////////////////
@test
def dropWhile01() : Bool = 
    String2.dropWhile(Char.isDigit, "12345 hello") `assertEq!` " hello"

/////////////////////////////////////////////////////////////////////////////
// takeWhile                                                               //
/////////////////////////////////////////////////////////////////////////////
@test
def takeWhile01() : Bool = 
    String2.takeWhile(Char.isDigit, "12345 hello") `assertEq!` "12345"

/////////////////////////////////////////////////////////////////////////////
// exists                                                                  //
/////////////////////////////////////////////////////////////////////////////
@test
def exists01() : Bool = 
    assert!(String2.exists(Char.isDigit, "number 5"))

// @test
// def isAsciiChar01() : Bool = 
//     assertNot!(Char.isAscii('∀'))


/////////////////////////////////////////////////////////////////////////////
// isAscii                                                                 //
/////////////////////////////////////////////////////////////////////////////
@test
def isAscii01() : Bool = 
    assert!(String2.isAscii("The Flix Programming Language"))

@test
def isAscii02() : Bool = 
    assertNot!(String2.isAscii("∀(x: e, y: e, z: e). ((z ⊑ x) ∧ (z ⊑ y)) → (z ⊑ (x ⊓ y))"))

/////////////////////////////////////////////////////////////////////////////
// splitAt                                                                 //
/////////////////////////////////////////////////////////////////////////////
@test
def splitAt01() : Bool = 
    String2.splitAt(1,"hello") `assertEq!` ("h", "ello")

@test
def isBlank01() : Bool = 
    assert!(String2.isBlank("       "))

@test
def isBlank02() : Bool = 
    assertNot!(String2.isBlank("     o  "))

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
