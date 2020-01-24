/*
 * Copyright 2020 Stephen Tetley
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

enum Validation[t, e] {
    case Okay(t),
    case Error(JoinList[e]),
    case Fatal(JoinList[e])
}

namespace Validation {



    pub def pure(x: t): Validation[t, e] = Okay(x)
    
    pub def error(err: e): Validation[t, e] = Error(ImportBase.singleton(err))

    pub def fatal(err: e): Validation[t, e] = Fatal(ImportBase.singleton(err))

    pub def validate(va: Validation[t, e]): Result[t, List[e]] =  
        match va {
            case Okay(a) => Result.Ok(a)
            case (Error(xs)) => Result.Err(ImportBase.toList(xs))
            case (Fatal(xs)) => Result.Err(ImportBase.toList(xs))
        }

    pub def fmap(f: t -> t1, va: Validation[t, e]): Validation[t1, e] = 
        match va {
            case Okay(a) => Okay(f(a))
            case Error(xs) => Error(xs)
            case Fatal(xs) => Fatal(xs)
        }


    pub def ap(vf: Validation[t -> t1, e], va: Validation[t, e]): Validation[t1, e1] =
        match vf {
            case Okay(f) => fmap(f, va)
            case Error(xs) => match va {
                case Okay(_) => Error(xs)
                case Error(ys) => Error(ImportBase.append(xs,ys))
                case Fatal(ys) => Fatal(ys)
            }
            case Fatal(xs) => match va {
                case Fatal(ys) => Fatal(ImportBase.append(xs,ys))
                case _ => Fatal(xs)
            }
        }

    pub def alt(va: Validation[t, e], vb: Validation[t, e]): Validation[t, e] = match va {
        case Okay(a) => Okay(a)
        case Error(xs) => match vb {
            case Okay(b) => Okay(b)
            case Error(ys) => Error(ImportBase.append(xs,ys))
            case Fatal(ys) => Fatal(ys)
        }
        case Fatal(xs) => match vb {
            case Fatal(ys) => Fatal(ImportBase.append(xs,ys))
            case _ => Fatal(xs)
        }
    }

    pub def withDefault(x: t, va: Validation[t, e]): Validation[t, e] = match va {
        case Okay(a) => Okay(a)
        case Error(_) => Okay(x)
        case Fatal(xs) => Fatal(xs)
    }

    pub def liftV2(f: (t1,t2) -> t3, v1: Validation[t1, e], v2: Validation[t2, e]): Validation[t3, e] = 
        ap(fmap(f, v1), v2)

    pub def liftV3(f: (t1,t2,t3) -> t4, v1: Validation[t1, e], v2: Validation[t2, e], v3: Validation[t3, e]): Validation[t4, e] = 
        ap(liftV2(f, v1, v2), v3)

    pub def liftV4(f: (t1,t2,t3,t4) -> t5, v1: Validation[t1, e], v2: Validation[t2, e], v3: Validation[t3, e], v4: Validation[t4, e]): Validation[t5, e] = 
        ap(liftV3(f, v1, v2, v3), v4)
    
    pub def liftV5(f: (t1,t2,t3,t4,t5) -> t6, v1: Validation[t1, e], v2: Validation[t2, e], v3: Validation[t3, e], v4: Validation[t4, e], v5: Validation[t5, e]): Validation[t6, e] = 
        ap(liftV4(f, v1, v2, v3, v4), v5)

    pub def liftV6(f: (t1,t2,t3,t4,t5,t6) -> t7, v1: Validation[t1, e], v2: Validation[t2, e], v3: Validation[t3, e], v4: Validation[t4, e], v5: Validation[t5, e], v6: Validation[t6, e]): Validation[t7, e] = 
        ap(liftV5(f, v1, v2, v3, v4, v5), v6)
    
    pub def liftV7(f: (t1,t2,t3,t4,t5,t6,t7) -> t8, v1: Validation[t1, e], v2: Validation[t2, e], v3: Validation[t3, e], v4: Validation[t4, e], v5: Validation[t5, e], v6: Validation[t6, e], v7: Validation[t7, e]): Validation[t8, e] = 
        ap(liftV6(f, v1, v2, v3, v4, v5, v6), v7)
    
    pub def liftV8(f: (t1,t2,t3,t4,t5,t6,t7,t8) -> t9, v1: Validation[t1, e], v2: Validation[t2, e], v3: Validation[t3, e], v4: Validation[t4, e], v5: Validation[t5, e], v6: Validation[t6, e], v7: Validation[t7, e], v8: Validation[t8, e]): Validation[t9, e] = 
        ap(liftV7(f, v1, v2, v3, v4, v5, v6, v7), v8)

    pub def liftV9(f: (t1,t2,t3,t4,t5,t6,t7,t8,t9) -> t10, v1: Validation[t1, e], v2: Validation[t2, e], v3: Validation[t3, e], v4: Validation[t4, e], v5: Validation[t5, e], v6: Validation[t6, e], v7: Validation[t7, e], v8: Validation[t8, e], v9: Validation[t9, e]): Validation[t10, e] = 
        ap(liftV8(f, v1, v2, v3, v4, v5, v6, v7, v8), v9)
    
    pub def liftV10(f: (t1,t2,t3,t4,t5,t6,t7,t8,t9,t10) -> t11, v1: Validation[t1, e], v2: Validation[t2, e], v3: Validation[t3, e], v4: Validation[t4, e], v5: Validation[t5, e], v6: Validation[t6, e], v7: Validation[t7, e], v8: Validation[t8, e], v9: Validation[t9, e], v10: Validation[t10, e]): Validation[t11, e] = 
        ap(liftV9(f, v1, v2, v3, v4, v5, v6, v7, v8, v9), v10)

    pub def pipeV2(v1: Validation[t1, e], v2: Validation[t2, e], f: (t1,t2) -> t3): Validation[t3, e] =
        liftV2(f, v1, v2)

}