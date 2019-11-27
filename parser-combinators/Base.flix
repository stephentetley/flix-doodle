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




namespace ParserCombinators {
    
    type alias SrcPos = Int32

    type alias ErrMsg = Str

    type alias Answer[ans] = Result[ans, ErrMsg]


    type alias FailK[ans] = SrcPos -> Answer[ans]

    type alias SuccessK[r, ans] = (r, FailK[ans], Str, SrcPos) -> Answer[ans]

    pub enum Parser[r, ans] {
        case Parser((SuccessK[r, ans], FailK[ans], Str, SrcPos) -> Answer[ans])
    }

    pub def fail() : Parser[r, ans] = 
        Parser((_, fk, _, pos) -> fk(pos))

    pub def return(x: ans): Parser[r, ans] =
        Parser((sk, fk, inp, pos) -> sk(x, fk, inp, pos))

    def apply1(parser: Parser[r, ans], sk: SuccessK[r, ans], fk: FailK[ans], inp: Str, pos: SrcPos): Result[ans, Str] = 
        match parser {
            case Parser(f) => f(sk, fk, inp, pos)
        }

    pub def bind(ma: Parser[r, a], k: a -> Parser[r,b]) : Parser[r,b] = 
        Parser((sk, fk, inp, pos) -> match apply1(ma, sk, fk, inp, pos) { 
                case Err(msg) => Err(msg)
                case Ok(ans) => apply1(k(ans), sk, fk, inp, pos)
            })



    pub def run(parser: Parser[ans, ans], input: Str): Result[ans, Str] = 
        let failk = _ -> Err("error");
        let successk = (x, _, _, _) -> Ok(x);
        match parser {
            case Parser(f) => f(successk, failk, input, 0)
        }

    pub def throwError(msg: Str) : Parser[r, ans] = 
        Parser((_,_,_,_) -> Err(msg))

    pub def anyChar() : Parser[r, Char] = 
        Parser((sk, fk, inp, pos) -> match String2.viewl(inp) {
                case None => fk(pos)
                case Some(ch, inp1) => sk(ch, fk, inp1, (pos+1))
            })


    

    // def apply1(parser: Parser[t], input: Str): Result[(Str, t), Str] = match parser { 
    //     case Parser(f) => f(input) 
    // }


    // pub def merror(msg: Str): Parser[t] = 
    //     Parser(_ -> Err(msg))

    // pub def return(x: t): Parser[t] =
    //     Parser(str -> Ok (str,x))

    // pub def bind (ma: Parser[t], fn: t -> Parser[u]): Parser[u] = 
    //     Parser(str -> match apply1(ma, str) {
    //         case Ok(str1, a) => apply1(fn(a), str1)
    //         case Err(msg) => Err(msg)
    //     })

}