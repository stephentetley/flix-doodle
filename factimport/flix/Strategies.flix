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


type alias RowResult[t] = Validation[t, String]

type alias RowIterator[src,row] = { hasNext: src -> Bool, getRow: src -> row }

namespace Strategies {


    pub def collect(iterator: RowIterator[src, row], rowreader: row -> RowResult[reln], source: src): Result[reln, String] =
        let failk = (n,msgs) -> ImportBase.collectErrors(n, msgs) |> Err;
        collectHelper(iterator, rowreader, source, 0, failk, relns -> Ok(relns))

    def collectHelper(iterator: RowIterator[src, row], rowreader: row -> RowResult[reln], src: src, n: Int32, fk: (Int32, List[String]) -> Result[reln, String], sk: reln -> Result[reln, String]): Result[reln, String] =
        let hasNext = iterator.hasNext;
        let getRow = iterator.getRow;
        if (!hasNext(src))
            sk(#{ })
        else {
            match getRow(src) |> (Validation.validate << rowreader) {
                case Err(xs) => fk(n, xs)
                case Ok(x) => collectHelper(iterator, rowreader, src, n+1, fk, relns -> sk(x <+> relns))
            }
        }

 }