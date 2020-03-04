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
/// Represents a FilePath.
///
opaque type FilePath = ##java.nio.file.Path

namespace FilePath {

    ///
    /// Returns a new FilePath.
    ///
    pub def new(uri: String): FilePath =
        import flix.runtime.library.System:pathsGet(String) as pathsGet;
        FilePath(pathsGet(uri) as & Pure)

    pub def toString(path: FilePath): String = 
        import flix.runtime.library.System:pathToString(##java.nio.file.Path) as toString;
        let FilePath(path1) = path;
        path1.toString() as & Pure

    pub def normalize(path: FilePath): FilePath = 
        import flix.runtime.library.System:pathNormalize(##java.nio.file.Path) as pathNoramlize;
        let FilePath(path1) = path;
        FilePath(path1.pathNoramlize() as & Pure)

    

 }