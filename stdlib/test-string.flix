/// Additional tests for Str.flix


///
/// Asserts that the given thunk `f` succeeds without raising an exception when it is evaluated.
///
/// NB: For internal use only.
///
pub def assertSuccess!(f: Unit -> a): Bool =
    try {
        let _ = f(); 
        true
    } catch {
        case _: ##java.lang.Exception => false
    }

///
/// Asserts that the given thunk `f` fails and raises an exception when it is evaluated.
///
/// NB: For internal use only.
///
pub def assertFailure!(f: Unit -> a): Bool =
    try {
        let _ = f(); 
        false
    } catch {
        case _: ##java.lang.Exception => true
    }


/////////////////////////////////////////////////////////////////////////////
// charAt                                                                  //
/////////////////////////////////////////////////////////////////////////////
@test
def charAt01(): Bool = assertEq!(String.charAt(0, "Hello World!"), 'H')

@test
def charAt02(): Bool = assertEq!(String.charAt(5, "Hello World!"), ' ')

@test
def charAt03(): Bool = 
    assertFailure!(_ -> String.charAt(-1, "Hello World!"))

@test
def charAt04(): Bool = 
    assertFailure!(_ -> String.charAt(0, ""))

/////////////////////////////////////////////////////////////////////////////
// endsWith                                                                //
/////////////////////////////////////////////////////////////////////////////
@test
def endsWith01(): Bool = assert!(String.endsWith("Hello World!", ""))

@test
def endsWith02(): Bool = assert!(String.endsWith("Hello World!", "World!"))

@test
def endsWith03(): Bool = assertNot!(String.endsWith("Hello World!", "WORLD!"))

/////////////////////////////////////////////////////////////////////////////
// startsWith                                                              //
/////////////////////////////////////////////////////////////////////////////
@test
def startsWith01(): Bool = assert!(String.startsWith("Hello World!", ""))

@test
def startsWith02(): Bool = assert!(String.startsWith("Hello World!", "Hello"))

@test
def startsWith03(): Bool = assertNot!(String.startsWith("Hello World!", "HELLO"))

/////////////////////////////////////////////////////////////////////////////
// isEmpty                                                                 //
/////////////////////////////////////////////////////////////////////////////
@test
def isEmpty01(): Bool = assert!(String.isEmpty(""))

@test
def isEmpty02(): Bool = assertNot!(String.isEmpty("Hello"))


/////////////////////////////////////////////////////////////////////////////
// length                                                                  //
/////////////////////////////////////////////////////////////////////////////
@test
def length01(): Bool = assertEq!(String.length(""), 0)

@test
def length02(): Bool = assertEq!(String.length("0123456789"), 10)

/////////////////////////////////////////////////////////////////////////////
// split                                                                   //
/////////////////////////////////////////////////////////////////////////////
@test
def split01(): Bool = assertEq!(Array.toList(String.split("", "\s+")), "" :: Nil)

@test
def split02(): Bool = assertEq!(Array.length(String.split("1 3", "\s+")), 2)

/////////////////////////////////////////////////////////////////////////////
// toLowerCase                                                             //
/////////////////////////////////////////////////////////////////////////////
@test
def toLowerCase01(): Bool = assertEq!(String.toLowerCase(""), "")

@test
def toLowerCase02(): Bool = assertEq!(String.toLowerCase("Hello World!"), "hello world!")

@test
def toLowerCase03(): Bool = assertEq!(String.toLowerCase("→Hello World∀!"), "→hello world∀!")

/////////////////////////////////////////////////////////////////////////////
// toUpperCase                                                             //
/////////////////////////////////////////////////////////////////////////////
@test
def toUpperCase01(): Bool = assertEq!(String.toUpperCase(""), "")

@test
def toUpperCase02(): Bool = assertEq!(String.toUpperCase("Hello World!"), "HELLO WORLD!")

@test
def toUpperCase03(): Bool = assertEq!(String.toUpperCase("→Hello World∀!"), "→HELLO WORLD∀!")

/////////////////////////////////////////////////////////////////////////////
// toList                                                                  //
/////////////////////////////////////////////////////////////////////////////
@test
def toList01(): Bool = assertEq!(String.toList(""), Nil)

@test
def toList02(): Bool = assertEq!(String.toList("ABC"), 'A' :: 'B' :: 'C' :: Nil)

@test
def toList03(): Bool = assertEq!(String.toList("1 2 3"), '1' :: ' ' :: '2' :: ' ' :: '3' :: Nil)

