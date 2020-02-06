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

///
/// Represents a source of text lines (internally an iterator on a text file).
///
opaque type LinesSource = ##flix.runtime.factimport.LinesIterator

type alias LineResult[t] = Validation[t, String]

namespace LinesImport {

    pub def newLinesSource(filepath: String): Result[LinesSource, String] =
        try {
            import new flix.runtime.factimport.LinesIterator(String) as newLinesIterator;
            let iterator = newLinesIterator(filepath);
            LinesSource(iterator) |> Ok
        } catch {
            case _: ##java.io.FileNotFoundException => Err("newTextLineSource - file not found")
            case _: ##java.io.IOException => Err("newTextLineSource - io exception")
        }

    def hasNext(src: LinesSource): Bool =
        let LinesSource(iterator) = src;
        import flix.runtime.factimport.LinesIterator.hasNext();
        iterator.hasNext()

    def nextRow(src: LinesSource): String =
        let LinesSource(iterator) = src;
        import flix.runtime.factimport.LinesIterator.next();
        iterator.next()

    ///
    /// Track line number for errors.
    ///
    pub def collect(f: String -> LineResult[reln], src: LinesSource): Result[reln, String] =
        let failk = (n,msgs) -> ImportBase.collectErrors(n, msgs) |> Err;
        collectHelper(f, src, 0, failk, relns -> Ok(relns))

    def collectHelper(f: String -> LineResult[reln], src: LinesSource, n: Int32, fk: (Int32, List[String]) -> Result[reln, String], sk: reln -> Result[reln, String]): Result[reln, String] =
        if (!hasNext(src))
            sk(#{ })
        else {
            match nextRow(src) |> (Validation.validate << f) {
                case Err(xs) => fk(n, xs)
                case Ok(x) => collectHelper(f, src, n+1, fk, relns -> sk(x <+> relns))
            }
        }

    pub def nonempty(s: String): LineResult[String] =
        if (String.length(s) > 0) Validation.pure(s) else Validation.error("nonempty")

}