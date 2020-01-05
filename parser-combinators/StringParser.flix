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

    type alias FailK[t] = SrcPos -> Result[t, ErrMsg]

    type alias SuccessK[ans, t] = (ans, FailK[t], Str, SrcPos) -> Result[t, ErrMsg]

    pub enum Parser[ans, t] {
        case Parser((SuccessK[ans, t], FailK[t], Str, SrcPos) -> Result[t, ErrMsg])
    }

    pub def fail() : Parser[ans, t] = 
        Parser((_, fk, _, pos) -> fk(pos))

    pub def return(x: ans): Parser[ans, t] =
        Parser((sk, fk, inp, pos) -> sk(x, fk, inp, pos))

    def apply1(parser: Parser[ans, t], sk: SuccessK[ans, t], fk: FailK[t], inp: Str, pos: SrcPos): Result[t, ErrMsg] = 
        match parser {
            case Parser(f) => f(sk, fk, inp, pos)
        }

    pub def bind(ma: Parser[a, t], k: a -> Parser[b, t]) : Parser[b, t] = 
        Parser((sk, fk, inp, pos) -> match apply1(ma, sk, fk, inp, pos) { 
                case Err(msg) => Err(msg)
                case Ok(ans) => apply1(k(ans), sk, fk, inp, pos)
            })

    pub def fmap(f: a -> b, ma: Parser[a, t]) : Parser[b, t] = 
        Parser((sk, fk, inp, pos) -> match apply1(ma, sk, fk, inp, pos) { 
                case Err(msg) => Err(msg)
                case Ok(ans) => Ok(f(ans))
            })

    pub def run(parser: Parser[t, t], input: Str): Result[t, ErrMsg] = 
        let failk = _ -> Err("error");
        let successk = (x, _, _, _) -> Ok(x);
        match parser {
            case Parser(f) => f(successk, failk, input, 0)
        }

    pub def throwError(msg: Str): Parser[ans, t] = 
        Parser((_,_,_,_) -> Err(msg))

    pub def swapError(msg: Str, p: Parser[ans,t]): Parser[ans, t] = 
        Parser((sk, fk, inp, pos) -> match apply1(p, sk, fk, inp, pos) { 
                case Err(_) => Err(msg)
                case Ok(ans) => Ok(ans)
            })

    pub def option(one: ans, p: Parser[ans,t]): Parser[ans, t] = 
        Parser((sk, fk, inp, pos) -> match apply1(p, sk, fk, inp, pos) { 
                case Err(_) => Ok(one)
                case Ok(ans) => Ok(ans)
            })

    pub def optional(p: Parser[ans, t]): Parser[Option[ans], t] = 
        alt(fmap(x -> Some(x), p), return(None))

    def furthest (x: SrcPos, y: SrcPos): SrcPos = if (x >= y) x else y

    pub def alt(p: Parser[ans,t], q: Parser[ans,t]): Parser[ans, t] =
        Parser((sk, fk, inp, pos) -> 
            let fkp = { pos1 ->
                let fkq = pos2 -> fk(furthest(pos1,pos2));
                apply1(q, sk, fkq, inp, pos) };
            apply1(p, sk, fkp, inp, pos))

    pub def ap(pf: Parser[a -> b, t], pa: Parser[a, t]): Parser[b, t] =
        bind(pf, f -> bind(pa, a -> return(f(a))))

    pub def seqLeft(pa: Parser[a, t], pb: Parser[b, t]): Parser[a, t] =
        bind(pa, a -> bind(pb, _ -> return(a)))

    pub def seqRight(pa: Parser[a, t], pb: Parser[b, t]): Parser[b, t] =
        bind(pa, _ -> bind(pb, b -> return(b)))


    pub def satisfies(p: Parser[ans, t], test: ans -> Bool): Parser[ans, t] = 
        bind(p, ans -> if (test(ans)) return(ans) else fail())

    pub def count(i: Int32, p: Parser[ans, t]): Parser[List[ans], t] = countHelper(i, p, Nil)

    def countHelper(i: Int32, p: Parser[ans, t], acc: List[ans]): Parser[List[ans], t] = 
        if (i < 0) 
            return(List.reverse(acc)) 
        else { 
            bind(p, v1 -> countHelper(i-1, p, v1 :: acc)) 
        }

    pub def many(p: Parser[ans, t]): Parser[List[ans], t] = 
        alt(many1(p), return(Nil))

    pub def many1(p: Parser[ans, t]): Parser[List[ans], t] = 
        bind(p, v1 -> 
            bind(many(p), vs -> return(v1 :: vs)))

    pub def anyChar(): Parser[Char, t] = 
        Parser((sk, fk, inp, pos) -> match Util.charAtSafe(pos, inp) {
                case None => fk(pos)
                case Some(ch) => sk(ch, fk, inp, (pos+1))
            }) |> swapError("anyChar")

    pub def satisfy(test: Char -> Bool): Parser[Char, t] = 
        Parser((sk, fk, inp, pos) -> match Util.charAtSafe(pos, inp) {
                case None => fk(pos)
                case Some(ch) => 
                    if (test(ch)) sk(ch, fk, inp, (pos+1)) else fk(pos)
            }) |> swapError("satisfy")


    pub def char(c: Char): Parser[Char, t] = 
        satisfy(ch -> ch == c) |> swapError("char")
    
    pub def horizon(i: Int): Parser[Str, t] = 
        Parser((sk, fk, inp, pos) -> match Util.trySlice(pos, pos+i, inp) {
                case None => fk(pos)
                case Some(s) => sk(s, fk, inp, (pos+1))
            }) |> swapError("horizon")

    pub def string(needle: Str): Parser[Str, t] = 
        Parser((sk, fk, inp, pos) -> match Util.trySlice(pos, pos + String.length(needle), inp) {
                case None => fk(pos)
                case Some(s) => 
                    if (s == needle) sk(s, fk, inp, (pos+ String.length(needle))) else fk(pos)
            })

    pub def digit(): Parser[Char, t] = 
        satisfy(c -> Char.isDigit(c))

    pub def letter(): Parser[Char, t] = 
        satisfy(c -> Char.isLetter(c))

    pub def whiteSpace(): Parser[Char, t] = 
        satisfy(c -> Char.isWhiteSpace(c))

}