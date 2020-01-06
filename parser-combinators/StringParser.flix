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

    pub def digit(): Parser[Char] = 
        satisfy(c -> Char.isDigit(c))

    pub def letter(): Parser[Char] = 
        satisfy(c -> Char.isLetter(c))

    pub def whiteSpace(): Parser[Char] = 
        satisfy(c -> Char.isWhiteSpace(c))

}