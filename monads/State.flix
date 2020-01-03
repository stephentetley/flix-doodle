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

enum State[s,a] {
    case State(s -> (s,a))
}

namespace State {

    pub def runState (s: s, ma: State[s, a]): (s,a) = match ma {
        case State(f) => f(s)
    }

    pub def return(a: a): State[s,a] = State(s -> (s,a))

    pub def bind(ma: State[s,a], k: a -> State[s,b]): State[s,b] = 
        State(s -> 
            let (s1,a) = runState(s, ma);
            runState(s1, k(a))
        )

    pub def get(): State[s,s] = State(s -> (s,s))

    pub def set(s1: s): State[s, Unit] = State(_ -> (s1, ()))

    pub def sets_(f: s -> s): State[s, Unit] = State(s -> (f(s), ()))
    
    pub def sets(f: s -> (s,a)): State[s, Unit] = State(s -> f(s))


    pub def fmap(f: a -> b, ma: State[s,a]): State[s,b] = 
        State(s -> 
            let (s1,a) = runState(s, ma);
            (s1, f(a))
        )

    pub def ap(mf: State[s, a->b], ma: State[s,a]): State[s,b] =
        State(s ->
            let (s1,f) = runState(s,mf);
            let (s2,a) = runState(s1,ma);
            (s2, f(a))
        )
    
    pub def liftA2(f: (a,b) -> c, ma: State[s,a], mb: State[s,b]): State[s,c] = 
        State(s ->
            let (s1,a) = runState(s,ma);
            let (s2,b) = runState(s1,mb);
            (s2, f(a,b))
        )
    
    pub def seqL(ma: State[s,a], mb: State[s,b]): State[s,b] = 
        State(s ->
            let (s1,_) = runState(s,ma);
            runState(s1,mb)            
        )

    pub def seqR(ma: State[s,a], mb: State[s,b]): State[s,a] = 
        State(s ->
            let (s1,a) = runState(s,ma);
            let (s2,_) = runState(s1,mb);
            (s2,a)
        )

    // No notion of failure / choice so no `mzero` & `mplus`

    /// TODO - mapM is writable with bind and return and shouldn't need to inspect the contents of State(...)
    pub def mapM(f: a -> State[s,b], xs: List[a]): State[s, List[b]] = 
        State(s -> mapMHelper(f, s, xs, Nil))
        
    def mapMHelper(f: a -> State[s,b], s: s, xs: List[a], ac: List[b]): (s, List[b]) = match xs {
        case Nil => (s, List.reverse(ac))
        case x :: rs => {
            let (s1,a) = runState(s, f(x));
            mapMHelper(f, s1, rs, (a :: ac))
        }
    }

}