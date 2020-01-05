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



    pub def run(parser: Parser[t, t], input: Str): Result[t, ErrMsg] = 
        let failk = _ -> Err("error");
        let successk = (x, _, _, _) -> Ok(x);
        match parser {
            case Parser(f) => f(successk, failk, input, 0)
        }

    pub def throwError(msg: Str) : Parser[ans, t] = 
        Parser((_,_,_,_) -> Err(msg))


    def furthest (x: SrcPos, y: SrcPos): SrcPos = if (x >= y) x else y

    pub def alt(p: Parser[ans,t], q: Parser[ans,t]): Parser[ans, t] =
        Parser((sk, fk, inp, pos) -> 
            let fkp = { pos1 ->
                let fkq = pos2 -> fk(furthest(pos1,pos2));
                apply1(q, sk, fkq, inp, pos) };
            apply1(p, sk, fkp, inp, pos))

    pub def anyChar() : Parser[r, Char] = 
        Parser((sk, fk, inp, pos) -> match Util.charAtSafe(pos, inp) {
                case None => fk(pos)
                case Some(ch) => sk(ch, fk, inp, (pos+1))
            })

    pub def char(c: Char) : Parser[r, Char] = 
        Parser((sk, fk, inp, pos) -> match Util.charAtSafe(pos, inp) {
                case None => fk(pos)
                case Some(ch) => 
                    if (ch == c) sk(ch, fk, inp, (pos+1)) else fk(pos)
            })
    
}