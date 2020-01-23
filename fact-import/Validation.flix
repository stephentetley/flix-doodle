
namespace Validation {
        ///
    /// A Non-empty JoinList (JoinLists have cheap append)
    ///
    enum JoinList[a] {
        case One(a),
        case Join(JoinList[a], JoinList[a])
    }

    def singleton(x: a): JoinList[a] = One(x)

    def append(a: JoinList[a], b: JoinList[a]): JoinList[a] = Join(a,b)

    def toList(a: JoinList[a]): List[a] =
            toListHelper(a, Nil, xs -> xs)

    def toListHelper(xs: JoinList[a], acc:List[a], cont: List[a] -> List[a]): List[a] =
        match xs {
            case One(a) => cont(a :: acc)
            case Join(l, r) =>
                // Go Right first
                toListHelper(r, acc, acc1 ->
                    toListHelper(l, acc1, cont))
        }

    pub enum Validation[t, e] {
        case Okay(t),
        case Error(JoinList[e]),
        case Fatal(JoinList[e])
    }

    pub def pure(x: t): Validation[t, e] = Okay(x)
    
    pub def error(err: e): Validation[t, e] = Error(singleton(err))

    pub def fatal(err: e): Validation[t, e] = Fatal(singleton(err))

    pub def validate(va: Validation[t, e]): Result[t, List[e]] =  
        match va {
            case Okay(a) => Result.Ok(a)
            case (Error(xs)) => Result.Err(toList(xs))
            case (Fatal(xs)) => Result.Err(toList(xs))
        }

    pub def fmap(f: t -> t1, va: Validation[t, e]): Validation[t1, e] = 
        match va {
            case Okay(a) => Okay(f(a))
            case Error(xs) => Error(xs)
            case Fatal(xs) => Fatal(xs)
        }


    pub def ap(vf: Validation[t -> t1, e], va: Validation[t, e]): Validation[t, e1] = 
        match vf {
            case Okay(f) => fmap(f, va)
            case Error(xs) => match va {
                case Okay(_) => Error(xs)
                case Error(ys) => Error(append(xs,ys))
                case Fatal(ys) => Fatal(ys)
            }
            case Fatal(xs) => match va {
                case Fatal(ys) => Fatal(append(xs,ys))
                case _ => Fatal(xs)
            }
        }


    pub def alt(va: Validation[t, e], vb: Validation[t, e]): Validation[t, e] = 
        match va {
            case Okay(a) => Okay(a)
            case Error(xs) => match vb {
                case Okay(b) => Okay(b)
                case Error(ys) => Error(append(xs,ys))
                case Fatal(ys) => Fatal(ys)
            }
            case Fatal(xs) => match va {
                case Fatal(ys) => Fatal(append(xs,ys))
                case _ => Fatal(xs)
            }
        }

    pub def withDefault(x: t, va: Validation[t, e]): Validation[t, e] = 
        match va {
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
}