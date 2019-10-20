



@test
def isAscii01() : Bool = 
    assert!(String2.isAscii("The Flix Programming Language"))

@test
def isAscii02() : Bool = 
    assertNot!(String2.isAscii("∀(x: e, y: e, z: e). ((z ⊑ x) ∧ (z ⊑ y)) → (z ⊑ (x ⊓ y))"))



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



