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
/// Represents a Text line.
///
opaque type FileLine = Str

///
/// Represents a FileLine source (internally an iterator on a text file).
///
opaque type FileLineSource = ##flix.runtime.factimport.FileLineIterator

type alias FileLineResult[t] = Validation[t, Str]

namespace FileLineImport {

    pub def newFileLineSource(filepath: Str): Result[FileLineSource, Str] =
        try {
            let iterator = native new flix.runtime.factimport.FileLineIterator(filepath);
            FileLineSource(iterator) |> Ok
        } catch {
            case _: ##java.io.FileNotFoundException => Err("newTextLineSource - file not found")
            case _: ##java.io.IOException => Err("newTextLineSource - io exception")
        }

    def hasNext(src: FileLineSource): Bool =
        let FileLineSource(iterator) = src;
        native method flix.runtime.factimport.FileLineIterator.hasNext(iterator) as Bool

    def nextRow(src: FileLineSource): FileLine =
        let FileLineSource(iterator) = src;
        let textline = native method flix.runtime.factimport.FileLineIterator.next(iterator) as Str;
        FileLine(textline)

    ///
    /// Track line number for errors.
    ///
    pub def collect(f: FileLine -> FileLineResult[reln], src: FileLineSource): Result[reln, Str] =
        let failk = (n,msgs) -> ImportBase.collectErrors(n, msgs) |> Err;
        collectHelper(f, src, 0, failk, relns -> Ok(relns))

    def collectHelper(f: FileLine -> FileLineResult[reln], src: FileLineSource, n: Int32, fk: (Int32, List[Str]) -> Result[reln, Str], sk: reln -> Result[reln, Str]): Result[reln, Str] =
        if (!hasNext(src))
            sk(#{ })
        else {
            match nextRow(src) |> (Validation.validate << f) {
                case Err(xs) => fk(n, xs)
                case Ok(x) => collectHelper(f, src, n+1, fk, relns -> sk(x <+> relns))
            }
        }
}