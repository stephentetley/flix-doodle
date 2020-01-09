/*
 * Copyright 2019 Stephen Tetley
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */




namespace StringParser {
    
    type alias SrcPos = Int32

    type alias ErrMsg = Str


    pub enum Parser[ans] {
        case Parser((Str, SrcPos) -> Result[(ans, SrcPos), ErrMsg])
    }

    pub def fail() : Parser[ans] = Parser((_,_) -> Err("fail"))

    pub def return(x: ans): Parser[ans] = Parser((_, pos) -> Ok(x, pos))

    def apply1(parser: Parser[ans], inp: Str, pos: SrcPos): Result[(ans, SrcPos), ErrMsg] = 
        let Parser(f) = parser;
        f(inp, pos)
        

    pub def bind(ma: Parser[a], k: a -> Parser[b]) : Parser[b] = 
        Parser((inp, pos) -> match apply1(ma, inp, pos) { 
                case Err(msg) => Err(msg)
                case Ok(ans, pos1) => apply1(k(ans), inp, pos1)
            })

    pub def fmap(f: a -> b, ma: Parser[a]) : Parser[b] = 
        Parser((inp, pos) -> match apply1(ma, inp, pos) { 
                case Err(msg) => Err(msg)
                case Ok(ans, pos1) => Ok(f(ans), pos1)
            })


    pub def liftM2(f: (a,b) -> ans, p1: Parser[a], p2: Parser[b]): Parser[ans] = 
        bind(p1, v1 -> 
            bind(p2, v2 -> return(f(v1,v2))))


    pub def run(parser: Parser[a], input: Str): Result[a, ErrMsg] = 
        let Parser(f) = parser;
        match f(input, 0) {
            case Err(msg) => Err(msg)
            case Ok((a, _)) => Ok(a)
        }

    pub def throwError(msg: Str): Parser[ans] = 
        Parser((_,_) -> Err(msg))

    pub def swapError(msg: Str, p: Parser[ans]): Parser[ans] = 
        Parser((inp, pos) -> match apply1(p, inp, pos) { 
                case Err(_) => Err(msg)
                case Ok(ans, pos1) => Ok(ans, pos1)
            })

    pub def option(one: ans, p: Parser[ans]): Parser[ans] = 
        Parser((inp, pos) -> match apply1(p, inp, pos) { 
                case Err(_) => Ok(one, pos)
                case Ok(ans, pos1) => Ok(ans, pos1)
            })

    pub def optional(p: Parser[ans]): Parser[Option[ans]] = 
        alt(fmap(x -> Some(x), p), return(None))

    pub def alt(p: Parser[ans], q: Parser[ans]): Parser[ans] =
        Parser((inp, pos) -> match (apply1(p, inp, pos), apply1(q, inp, pos)) {
            case (Err(_), Err(_)) => Err("alt")
            case (Err(_), b) => b
            case (a, Err(_)) => a
            case (Ok(a, pos1a), Ok(b, pos1b)) => 
                if (pos1b > pos1a) 
                    Ok(b, pos1b)
                else 
                    Ok(a, pos1a)
        })

    pub def ap(pf: Parser[a -> b], pa: Parser[a]): Parser[b] =
        bind(pf, f -> bind(pa, a -> return(f(a))))

    pub def seqLeft(pa: Parser[a], pb: Parser[b]): Parser[a] =
        bind(pa, a -> bind(pb, _ -> return(a)))

    pub def seqRight(pa: Parser[a], pb: Parser[b]): Parser[b] =
        bind(pa, _ -> bind(pb, b -> return(b)))


    pub def satisfies(p: Parser[ans], test: ans -> Bool): Parser[ans] = 
        bind(p, ans -> if (test(ans)) return(ans) else fail())

    pub def count(i: Int32, p: Parser[ans]): Parser[List[ans]] = 
        Parser((inp, pos) -> countHelper(i, p, inp, pos, () -> Err("count"), (xs, pos1)-> Ok(xs, pos1)))

    type alias ListAnswer[ans] = Result[(List[ans], SrcPos), ErrMsg]

    def countHelper(i: Int32, p: Parser[ans], inp: Str, pos: SrcPos, fk: Unit -> ListAnswer[ans], sk: (List[ans], SrcPos) -> ListAnswer[ans]): ListAnswer[ans] = 
        if (i <= 0)
            sk(Nil, pos)
        else 
            match apply1(p, inp, pos) {
                case Err(_) => fk()
                case Ok(a, pos1) => countHelper(i-1, p, inp, pos1, fk, (xs,posk) -> sk(a :: xs, posk))
            }
    
    pub def between(open: Parser[z1], close: Parser[z2], p: Parser[ans]): Parser[ans] = 
        seqRight(open, seqLeft(p, close))


    pub def many(p: Parser[ans]): Parser[List[ans]] = 
        Parser((inp, pos) -> manyHelper(p, inp, pos, (xs, pos1)-> Ok(xs, pos1)))


    pub def many1(p: Parser[ans]): Parser[List[ans]] = 
        bind(p, v1 -> 
            bind(many(p), vs -> return(v1 :: vs))) |> swapError("many1")


    def manyHelper(p: Parser[ans], inp: Str, pos: SrcPos, sk: (List[ans], SrcPos) -> ListAnswer[ans]): ListAnswer[ans] = 
        match apply1(p, inp, pos) {
            case Err(_) => sk(Nil, pos)
            case Ok(a, pos1) => manyHelper(p, inp, pos1, (xs,posk) -> sk(a :: xs, posk))
        }

    type alias SkipAnswer = Result[(Unit, SrcPos), ErrMsg]

    pub def skipMany(p: Parser[ans]): Parser[Unit] = 
        Parser((inp, pos) -> skipManyHelper(p, inp, pos, pos1-> Ok((), pos1)))

    pub def skipMany1(p: Parser[ans]): Parser[Unit] = 
        seqRight(p, skipMany(p))


    def skipManyHelper(p: Parser[ans], inp: Str, pos: SrcPos, sk: SrcPos -> SkipAnswer): SkipAnswer = 
        match apply1(p, inp, pos) {
            case Err(_) => sk(pos)
            case Ok(_, pos1) => skipManyHelper(p, inp, pos1, posk -> sk(posk))
        }

    pub def sepBy(p: Parser[ans], sep: Parser[sep]): Parser[List[ans]] = 
        Parser((inp, pos) -> sepByHelper(p, sep, inp, pos, (xs,pos1)-> Ok(xs, pos1)))


    def sepByHelper(p: Parser[ans], sep: Parser[sep], inp: Str, pos: SrcPos, sk: (List[ans], SrcPos) -> ListAnswer[ans]): ListAnswer[ans] = 
        match apply1(p, inp, pos) {
            case Err(_) => sk(Nil, pos)
            case Ok(a, pos1) => manyHelper(seqRight(sep, p), inp, pos1, (xs,posk) -> sk(a :: xs, posk))
        }

    pub def sepBy1(p: Parser[ans], sep: Parser[sep]): Parser[List[ans]] = 
        bind(p, v1 -> 
            bind(many(seqRight(sep,p)), vs -> return(v1 :: vs))) |> swapError("sepBy1")

    pub def endBy(p: Parser[ans], sep: Parser[sep]): Parser[List[ans]] = 
        many(seqLeft(p, sep))

    pub def endBy1(p: Parser[ans], sep: Parser[sep]): Parser[List[ans]] = 
        many1(seqLeft(p, sep)) |> swapError("endBy1")

    pub def sepEndBy(p: Parser[ans], sep: Parser[sep]): Parser[List[ans]] = 
        Parser((inp, pos) -> sepEndByHelper(p, sep, inp, pos, (xs,pos1)-> Ok(xs, pos1)))

    def sepEndByHelper(p: Parser[ans], sep: Parser[sep], inp: Str, pos: SrcPos, sk: (List[ans], SrcPos) -> ListAnswer[ans]): ListAnswer[ans] = 
        match apply1(p, inp, pos) {
            case Err(_) => sk(Nil, pos)
            case Ok(a, pos1) => match apply1(sep, inp, pos1) {
                case Err(_) => sk(a :: Nil, pos)
                case Ok(_, pos2) => sepEndByHelper(p, sep, inp, pos2, (xs,posk) -> sk(a :: xs, posk))
            }
        }

    pub def sepEndBy1(p: Parser[ans], sep: Parser[sep]): Parser[List[ans]] = 
        Parser((inp, pos) -> match apply1(p, inp, pos) {
            case Err(_) => Err("sepEndBy1")
            case Ok(a, pos1) => match apply1(sep, inp, pos1) {
                case Err(_) => Ok(a :: Nil, pos1)
                case Ok(_, pos2) => sepEndByHelper(p, sep, inp, pos2, (xs,posk) -> Ok(a :: xs, posk))
            }
        })

    pub def choice(ps: List[Parser[ans]]): Parser[ans] = 
        Parser((inp, pos) -> match ps {
                case Nil => Err("choice")
                case p :: rs =>  match apply1(p, inp, pos) {
                    case Ok(a, pos1) => Ok(a, pos1)
                    case Err(_) => apply1(choice(rs), inp, pos)
                }
            })

    pub def anyChar(): Parser[Char] = 
        Parser((inp, pos) -> match Util.charAtSafe(pos, inp) {
                case None => Err("anyChar")
                case Some(ch) => Ok(ch, pos+1)
            })

    pub def satisfy(test: Char -> Bool): Parser[Char] = 
        Parser((inp, pos) -> match Util.charAtSafe(pos, inp) {
                case None => Err("satisfy")
                case Some(ch) => 
                    if (test(ch)) Ok(ch, pos+1) else Err("satisfy")
            }) 

    pub def manyTill(p: Parser[ans], end: Parser[sep]): Parser[List[ans]] = 
        Parser((inp, pos) -> manyTillHelper(p, end, inp, pos, (xs, posk) -> Ok(xs, posk), () -> Err("manyTill")))

    def manyTillHelper(p: Parser[ans], end: Parser[sep], inp: Str, pos: SrcPos, sk: (List[ans], SrcPos) -> ListAnswer[ans], fk: Unit -> ListAnswer[ans]): ListAnswer[ans] = 
        match apply1(end, inp, pos) {
            case Ok(_, pos1) => sk(Nil, pos1)
            case Err(_) => match apply1(p, inp, pos) {
                case Err(_) => fk()
                case Ok(a, pos1) => manyTillHelper(p, end, inp, pos1, (xs,posk) -> sk(a :: xs, posk), fk)
            }
        }

    // TODO chain combinators

    pub def eof() : Parser[Unit] = 
        Parser((inp, pos) -> if (pos >= String.length(inp)) Ok((), pos) else Err("eof"))

    pub def notFollowedBy(p: Parser[ans]): Parser[Unit] = 
        Parser((inp, pos) -> match apply1(p, inp, pos) {
            case Err(_) => Ok((), pos)
            case Ok(_,_) => Err("notFollowedBy")
        })


    pub def oneOf(cs: Array[Char]): Parser[Char] = 
        satisfy(ch -> Array.memberOf(ch, cs)) |> swapError("oneOf")

    pub def noneOf(cs: Array[Char]): Parser[Char] = 
        satisfy(ch -> !Array.memberOf(ch, cs)) |> swapError("noneOf")

    pub def char(c: Char): Parser[Char] = 
        satisfy(ch -> ch == c) |> swapError("char")
    
    pub def horizon(i: Int): Parser[Str] = 
        Parser((inp, pos) -> match Util.trySlice(pos, pos+i, inp) {
                case None => Err("horizon")
                case Some(s) => Ok(s, pos+i)
            })

    pub def string(needle: Str): Parser[Str] = 
        Parser((inp, pos) -> match Util.trySlice(pos, pos + String.length(needle), inp) {
                case None => Err("string")
                case Some(s) => 
                    if (s == needle) Ok(needle, pos + String.length(needle)) else Err("string")
            })

    pub def upper(): Parser[Char] = satisfy(Char.isUpperCase)

    pub def lower(): Parser[Char] = satisfy(Char.isLowerCase)

    pub def letter(): Parser[Char] = satisfy(Char.isLetter)

    pub def alphaNum(): Parser[Char] = alt(letter(), digit())

    pub def digit(): Parser[Char] = satisfy(Char.isDigit)

    pub def octDigit(): Parser[Char] = 
        oneOf(['0', '1', '2', '3', '4', '5', '6', '7'])

    pub def hexDigit(): Parser[Char] = 
        oneOf(['0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 'B', 'C', 'D', 'E', 'F', 'a', 'b', 'c', 'd', 'e', 'f'])

    // TODO newline() 

    pub def tab(): Parser[Char] = char('\t')

    pub def space(): Parser[Unit] = satisfy(Char.isWhiteSpace) |> fmap(_ -> ())

    pub def spaces(): Parser[Unit] = skipMany(satisfy(Char.isWhiteSpace))

    pub def lexeme(p: Parser[ans]): Parser[ans] = seqLeft(p, spaces())

    pub def symbol(s: Str): Parser[ans] = lexeme(string(s))

    pub def parens(p: Parser[ans]): Parser[ans] = between(symbol("("), symbol(")"), p)

    pub def braces(p: Parser[ans]): Parser[ans] = between(symbol("{"), symbol("}"), p)

    pub def brackets(p: Parser[ans]): Parser[ans] = between(symbol("<"), symbol(">"), p)

    pub def squares(p: Parser[ans]): Parser[ans] = between(symbol("["), symbol("]"), p)

    pub def semi(): Parser[Str] = symbol(";")

    pub def comma(): Parser[Str] = symbol(",")

    pub def colon(): Parser[Str] = symbol(":")

    pub def dot(): Parser[Str] = symbol(".")

    pub def semiSep(p: Parser[ans]): Parser[List[ans]] = sepBy(p, semi())

    pub def semiSep1(p: Parser[ans]): Parser[List[ans]] = sepBy1(p, semi())

    pub def commaSep(p: Parser[ans]): Parser[List[ans]] = sepBy(p, comma())

    pub def commaSep1(p: Parser[ans]): Parser[List[ans]] = sepBy1(p, comma())

    // TODO - charLiteral and stringLiteral


    def sign(): Parser[Str] = 
        option("", alt(string("+"), string("-")))

    def digits(): Parser[Str] = 
        many1(digit()) |> fmap(List.toString)

    def intString(): Parser[Str] = 
        liftM2((x,y) -> x + y, sign(), digits())

    def primInt8(): Parser[Int8] =
        bind(intString(), v1 -> match Util.readInt8(v1) {
            case Err(_) => throwError("int8")
            case Ok(i) => return(i)
        })

    pub def int8(): Parser[Int8] = lexeme(primInt8())

    def primInt16(): Parser[Int16] =
        bind(intString(), v1 -> match Util.readInt16(v1) {
            case Err(_) => throwError("int16")
            case Ok(i) => return(i)
        })

    pub def int16(): Parser[Int16] = lexeme(primInt16())

    def primInt32(): Parser[Int32] =
        bind(intString(), v1 -> match Util.readInt32(v1) {
            case Err(_) => throwError("int32")
            case Ok(i) => return(i)
        })

    pub def int32(): Parser[Int32] = lexeme(primInt32())

    def primInt64(): Parser[Int64] =
        bind(intString(), v1 -> match Util.readInt64(v1) {
            case Err(_) => throwError("int64")
            case Ok(i) => return(i)
        })

    pub def int64(): Parser[Int64] = lexeme(primInt64())
    
}