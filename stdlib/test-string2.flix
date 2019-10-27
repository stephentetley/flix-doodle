
@test
def take01() : Bool = 
    String2.take(10, "hello") `assertEq!` "hello"

@test
def take02() : Bool = 
    String2.take(1, "hello") `assertEq!` "h"


@test
def drop01() : Bool = 
    String2.drop(10, "hello") `assertEq!` ""

@test
def drop02() : Bool = 
    String2.drop(1, "hello") `assertEq!` "ello"

@test
def viewl01() : Bool = 
    String2.viewl("") `assertEq!` None

@test
def viewl02() : Bool = 
    String2.viewl("hello") `assertEq!` Some('h', "ello")

@test
def viewr01() : Bool = 
    String2.viewr("") `assertEq!` None

@test
def viewr02() : Bool = 
    String2.viewr("hello") `assertEq!` Some("hell", 'o')


@test
def isAscii01() : Bool = 
    assert!(String2.isAscii("The Flix Programming Language"))

@test
def isAsciiChar01() : Bool = 
    assertNot!(Char.isAscii('∀'))


@test
def isAscii02() : Bool = 
    assertNot!(String2.isAscii("∀(x: e, y: e, z: e). ((z ⊑ x) ∧ (z ⊑ y)) → (z ⊑ (x ⊓ y))"))


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
def trimStart01() : Bool = 
    String2.trimStart("    body ") `assertEq!` "body "

@test
def trimEnd01() : Bool = 
    String2.trimEnd(" body      ") `assertEq!` " body"

@test
def padStart01() : Bool = 
    String2.padStart(3, "body") `assertEq!` "   body"

@test
def padEnd01() : Bool = 
    String2.padEnd(3, "body") `assertEq!` "body   "

@test 
def replaceAll01() : Bool =     
    String2.replaceAll("[aeiou]", "*", "Hello") `assertEq!` "H*ll*"

@test 
def replaceFirst01() : Bool =     
    String2.replaceFirst("[L]", "l", "HeLLo") `assertEq!` "HelLo"

@test 
def indent01() : Bool =     
    String2.indent(2, "onetwo") `assertEq!` "  onetwo"

@test 
def lines01() : Bool =     
    let s = "one" + String2.lineSeparator() + "two";
    Array.toList(String2.lines(s)) `assertEq!` ("one" :: "two" :: Nil)

