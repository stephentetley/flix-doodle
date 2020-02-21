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
    
type alias DateStamp = { day: Int32, month: Int32, year: Int32 }
type alias FlocRoot = { floc: String, description: String }
type alias Floc2 = { floc: String, description: String, parent: String }
type alias Equi = { equiId: String, description: String, floc: String, superoridinate: Option[String] }


namespace HierarchyParser {


    def taggedFunctionalLocation(): StringParser.Parser[String] = 
        StringParser.seqRight(
            StringParser.symbol("Functional Location"),
            StringParser.content(34)
        )

    def dateStamp(): StringParser.Parser[DateStamp] = 
        StringParser.pipe3(
            StringParser.seqLeft(StringParser.int32(), StringParser.char('.')),
            StringParser.seqLeft(StringParser.int32(), StringParser.char('.')),
            StringParser.int32(),
            (d,m,y) -> { day = d, month = m, year = y }
        )

    def validFrom(): StringParser.Parser[DateStamp] = 
        StringParser.seqRight(
            StringParser.symbol("Valid From"),
            dateStamp()
        )

    /// Doesn't parse trailing lineSeparator.
    pub def line1(): StringParser.Parser[(String, DateStamp)] = 
        StringParser.tuple2(
            taggedFunctionalLocation(),
            validFrom()
        )

    /// Doesn't parse trailing lineSeparator.
    pub def line2(): StringParser.Parser[String] = 
        StringParser.seqRight(
            StringParser.symbol("Description"),
            StringParser.restOfLine()
        )

    pub def indenting(): StringParser.Parser[String] = 
        StringParser.manyChars(StringParser.oneOf([' ', '-', '|']))

    pub def floc(): StringParser.Parser[String] = 
        StringParser.manyChars2(
            StringParser.upper(),
            StringParser.choice(
                [StringParser.upper(), StringParser.char('-'), StringParser.digit()]
            )
        )

    pub def equiId(): StringParser.Parser[String] = 
        StringParser.manyChars2(
            StringParser.alt(StringParser.char('$'), StringParser.digit()),
            StringParser.digit()            
        )

    pub def equiLinePart2(r : { equiId: String, description: String}): StringParser.Parser[{ | Equi }] = 
        let p1 = 
            StringParser.tuple2(
                StringParser.bounded(floc(), 41), 
                StringParser.fmap(Some, equiId())
            );
        let p2 = 
            StringParser.tuple2(
                floc(), 
                StringParser.return(None) 
            );
        StringParser.fmap(
            { (f1, s1) ->
                let r1 = { +floc = f1 | r }; 
                let r2 = { +superoridinate = s1 | r1};
                r2 },
            StringParser.alt(p1, p2)
        )

                

 }