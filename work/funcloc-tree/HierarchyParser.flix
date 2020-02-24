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
type alias FlocRow = { floc: String, description: String, parent: String }
type alias EquiRow = { equiId: String, description: String, floc: String, superordinate: Option[String] }


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


    pub def indenting(): StringParser.Parser[String] = 
        StringParser.manyChars(StringParser.oneOf([' ', '-', '|']))

    pub def funcLoc(): StringParser.Parser[String] = 
        StringParser.manyChars2(
            StringParser.upper(),
            StringParser.choice(
                [StringParser.upper(), StringParser.char('-'), StringParser.digit()]
            )
        )

    pub def equipmentId(): StringParser.Parser[String] = 
        StringParser.manyChars2(
            StringParser.alt(StringParser.char('$'), StringParser.digit()),
            StringParser.digit()            
        )

    /// Description is always bounded 
    def description(): StringParser.Parser[String] = 
        StringParser.content(41)


    /// Line parsers e.g for FactImport - LinesImport

    pub def funcLocHeaderLine(): StringParser.Parser[(String, DateStamp)] = 
        StringParser.tuple2(
            taggedFunctionalLocation(),
            validFrom()
        )

    pub def descriptionHeaderLine(): StringParser.Parser[String] = 
        StringParser.seqRight(
            StringParser.symbol("Description"),
            StringParser.restOfLine()
        )

    pub def flocRootLine(): StringParser.Parser[FlocRoot] = 
        StringParser.pipe2(
            StringParser.seqRight(StringParser.string("  "), StringParser.bounded(funcLoc(), 41)),
            StringParser.restOfLine(),
            (floc, descr) -> { floc = floc, description = descr }
        )

    pub def flocLine(): StringParser.Parser[FlocRow] = 
        StringParser.pipe3(
            StringParser.seqRight(indenting(), StringParser.bounded(funcLoc(), 41)),
            description(),
            funcLoc(),
            (floc, descr, parent) -> { floc = floc, description = descr, parent = parent }
        )

    pub def equiLine(): StringParser.Parser[EquiRow] = 
        StringParser.bind(
            StringParser.seqRight(indenting(), StringParser.bounded(equipmentId(), 19)), 
            equiId -> StringParser.bind(
                description(),
                descr -> equiLinePart2(equiId, descr)
            ))

    def equiLinePart2(equiId: String, descr: String): StringParser.Parser[EquiRow] = 
        let p1 = 
            StringParser.tuple2(
                StringParser.bounded(funcLoc(), 41), 
                StringParser.fmap(x -> Some(x), equipmentId())
            );
        let p2 = 
            StringParser.tuple2(
                funcLoc(), 
                StringParser.return(None) 
            );
        StringParser.fmap(
            { pair ->
                let (floc1, super1) = pair;
                { equiId = equiId, description = descr, floc = floc1, superordinate = super1 }
            },                
            StringParser.alt(p1, p2)
        )

                

 }