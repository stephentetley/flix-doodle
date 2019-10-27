
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



