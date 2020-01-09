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

// Needs ReadShowWrapper from `parser-combinators`...

namespace Util {

    pub def showInt8(i: Int8): Str = 
        native method flix.runtime.library.ReadShowWrapper.ValueOfByte(i) as Str
        
    pub def showInt16(i: Int16): Str = 
        native method flix.runtime.library.ReadShowWrapper.ValueOfShort(i) as Str

    pub def showInt32(i: Int32): Str = 
        native method flix.runtime.library.ReadShowWrapper.ValueOfInt(i) as Str

    pub def showInt64(i: Int64): Str = 
        native method flix.runtime.library.ReadShowWrapper.ValueOfLong(i) as Str

}