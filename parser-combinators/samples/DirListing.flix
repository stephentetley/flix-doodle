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

/// Parser for Windows `dir` output

type alias TimeStamp = { day: Int32, month: Int32, year: Int32, hour: Int32, minute: Int32 }

/// Don't care about attributes for the moment 

type alias File = { name: String, size: Int64, modified: TimeStamp }
type alias Folder = { name: String, modified: TimeStamp }

namespace DirListing {

    
    pub def printTimeStamp(ts: TimeStamp): String = 
        "${ Int32.toString(ts.day)}/${Int32.toString(ts.month)}/${ Int32.toString(ts.year)} ${ Int32.toString(ts.hour)}:${Int32.toString(ts.minute)}"

    pub def pUkTimeStamp(): StringParser.Parser[TimeStamp] = 
        StringParser.pipe5(
            StringParser.seqLeft(StringParser.int32(), StringParser.char('/')),
            StringParser.seqLeft(StringParser.int32(), StringParser.char('/')),
            StringParser.int32(),
            StringParser.seqLeft(StringParser.int32(), StringParser.char(':')),
            StringParser.int32(),
            (d, mon, y, h, min) -> { day=d, month=mon, year=y, hour=h, minute=min }
        )

    pub def fileStats(): StringParser.Parser[(TimeStamp, Int64)] = 
        StringParser.tuple2(
            pUkTimeStamp(),
            StringParser.int64()
        )
    
    /// Note - Windows Dir Listing can run to muliple lines - not handled yet...
    pub def fileName(): StringParser.Parser[String] = 
        StringParser.fmap(String.trim, StringParser.restOfLine())

    pub def pFile(): StringParser.Parser[File] = 
        StringParser.pipe3(
            StringParser.horizon(13),
            StringParser.bounded(fileStats(), 36),
            fileName(),
            (_, x, name) -> { 
                let (ts, size) = x; 
                { name=name, size=size, modified=ts } 
            }
        )
    pub def pFolder(): StringParser.Parser[Folder] = 
        StringParser.pipe3(
            StringParser.horizon(13),
            StringParser.bounded(pUkTimeStamp(), 36),
            fileName(),
            (_, ts, name) -> { name=name, modified=ts }
        )

    pub def ignoreLine(): StringParser.Parser[Option[a]] = 
        StringParser.fmap(_ -> None, StringParser.restOfLine())

}