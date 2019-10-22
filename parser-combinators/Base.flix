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


enum Parser[t] {
    case Parser(Str -> Result[(Str, t), Str])
}

namespace Parser {
    

    def getAnswer (input: Result[(Str,t), Str]): Result[t,Str] = match input {
        case Ok(_,a) => Ok(a)
        case Err(msg) => Err(msg)
    }


    pub def run(input: Str, parser: Parser[t]): Result[t, Str] = match parser { 
        case Parser(f) => f(input) |> getAnswer
    }

    pub def merror(msg: Str): Parser[t] = 
        Parser(_ -> Err(msg))

    pub def mreturn(x: t): Parser[t] =
        Parser(st -> Ok (st,x))

}