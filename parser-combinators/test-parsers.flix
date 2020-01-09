


@test
def fail01() : Result[Unit, Str] = 
    StringParser.run(StringParser.fail(), "")

@test
def return01() : Result[Int32, Str] =
    StringParser.run(StringParser.return(1), "")

@test
def bind01() : Result[Int32, Str] =
    StringParser.run(StringParser.bind(StringParser.return(1), v -> StringParser.return(v)), "")

@test
def fmap01() : Result[Int32, Str] =
    StringParser.run(StringParser.fmap(x -> x + 1, StringParser.return(1)), "")


 @test
def throwError01() : Result[Unit, Str] = 
    StringParser.run(StringParser.throwError("** parser error**"), "")



@test
def anyChar01() : Result[Char, Str] = 
    StringParser.run(StringParser.anyChar(), "abc")

@test
def alt01() : Result[Char, Str] = 
    let parser = StringParser.alt(StringParser.char('A'), StringParser.char('a'));
    StringParser.run(parser, "abc")

@test
def count01() : Result[List[Char], Str] = 
    let parser = StringParser.count(3, StringParser.letter());
    StringParser.run(parser, "ABCdefg")


@test
def many01() : Result[List[Char], Str] = 
    let parser = StringParser.many(StringParser.letter());
    StringParser.run(parser, "ABC") 

@test
def many101() : Result[List[Char], Str] = 
    let parser = StringParser.many1(StringParser.letter());
    StringParser.run(parser, "ABC") 

@test
def many102() : Result[List[Char], Str] = 
    let parser = StringParser.many1(StringParser.letter());
    StringParser.run(parser, "123") 


@test
def skipMany01() : Result[Char, Str] = 
    let parser = 
        StringParser.seqRight(StringParser.skipMany(StringParser.digit()), StringParser.letter());
    StringParser.run(parser, "123A") 

@test
def skipMany101() : Result[Char, Str] = 
    let parser = 
        StringParser.seqRight(StringParser.skipMany1(StringParser.digit()), StringParser.letter());
    StringParser.run(parser, "123A") 

@test
def sepBy01() : Result[List[Char], Str] = 
    let parser = 
        StringParser.sepBy(StringParser.digit(), StringParser.char('#'));
    StringParser.run(parser, "1#2#3") 

@test
def sepBy101() : Result[List[Char], Str] = 
    let parser = 
        StringParser.sepBy1(StringParser.digit(), StringParser.char('#'));
    StringParser.run(parser, "1#2#3") 

@test
def endBy01() : Result[List[Char], Str] = 
    let parser = 
        StringParser.endBy(StringParser.digit(), StringParser.char('#'));
    StringParser.run(parser, "1#2#3#") 

@test
def endBy101() : Result[List[Char], Str] = 
    let parser = 
        StringParser.endBy1(StringParser.digit(), StringParser.char('#'));
    StringParser.run(parser, "1#2#3#") 

@test
def sepEndBy01() : Result[List[Char], Str] = 
    let parser = 
        StringParser.sepEndBy(StringParser.digit(), StringParser.char('#'));
    StringParser.run(parser, "1#2#3#") 

@test
def sepEndBy02() : Result[List[Char], Str] = 
    let parser = 
        StringParser.sepEndBy(StringParser.digit(), StringParser.char('#'));
    StringParser.run(parser, "1#2#3") 

@test
def sepEndBy101() : Result[List[Char], Str] = 
    let parser = 
        StringParser.sepEndBy1(StringParser.digit(), StringParser.char('#'));
    StringParser.run(parser, "1#2#3#") 

@test
def sepEndBy102() : Result[List[Char], Str] = 
    let parser = 
        StringParser.sepEndBy1(StringParser.digit(), StringParser.char('#'));
    StringParser.run(parser, "1#2#3") 

// fails
@test
def sepEndBy103(): Result[List[Char], Str] = 
    let parser = 
        StringParser.sepEndBy1(StringParser.digit(), StringParser.char('#'));
    StringParser.run(parser, "This fails...") 

@test
def choice01(): Result[Char, Str] = 
    let parser = 
        StringParser.choice(StringParser.digit() :: StringParser.char('#') :: Nil);
    StringParser.run(parser, "#") 

@test
def manyTill01(): Result[List[Char], Str] = 
    let parser = 
        StringParser.manyTill(StringParser.digit(), StringParser.char('#'));
    StringParser.run(parser, "12345#") 

@test
def eof01(): Result[Unit, Str] = 
    let parser = StringParser.eof();
    StringParser.run(parser, "")

// fails
@test
def notFollowedBy01(): Result[Char, Str] = 
    let parser = StringParser.seqLeft(StringParser.digit(), StringParser.notFollowedBy(StringParser.char('#')));
    StringParser.run(parser, "1#")

@test
def oneOf01(): Result[Char, Str] = 
    let parser = StringParser.oneOf(['a', 'b', 'c']);
    StringParser.run(parser, "c")

@test
def noneOf01(): Result[Char, Str] = 
    let parser = StringParser.noneOf(['a', 'b', 'c']);
    StringParser.run(parser, "d")


@test
def upper01(): Result[Char, Str] = 
    let parser = StringParser.upper();
    StringParser.run(parser, "A")

@test
def alphaNum01(): Result[Char, Str] = 
    let parser = StringParser.alphaNum();
    StringParser.run(parser, "A")

@test
def alphaNum02(): Result[Char, Str] = 
    let parser = StringParser.alphaNum();
    StringParser.run(parser, "0")


@test
def octDigit01(): Result[List[Char], Str] = 
    let parser = StringParser.many1(StringParser.octDigit());
    StringParser.run(parser, "012345678")

@test
def hexDigit01(): Result[List[Char], Str] = 
    let parser = StringParser.many1(StringParser.hexDigit());
    StringParser.run(parser, "0123ABCDEFGH")

@test
def spaces01(): Result[Char, Str] = 
    let parser = StringParser.seqRight(StringParser.spaces(), StringParser.alphaNum());
    StringParser.run(parser, "        0")

@test
def int3201(): Result[Int32, Str] = 
    let parser = StringParser.int32();
    StringParser.run(parser, "-1")

@test
def tempReadInt3201(): Result[Int32, Str] = Util.readInt32("-1")

@test
def int3202(): Result[List[Int32], Str] = 
    let parser = StringParser.sepBy(StringParser.int32(), StringParser.comma());
    StringParser.run(parser, "-1,0,1000,999,3,-10,99,6")



