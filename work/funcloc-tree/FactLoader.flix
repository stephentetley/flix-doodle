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



rel FuncLoc2(floc: String, description: String)
rel FuncLoc3(floc: String, description: String, parent: String)
rel Equi3(eqnum: String, floc: String, description: String)
rel SubEqui(equiId: String, parent: String)

namespace FactLoader {

    pub def applyParser(p: StringParser.Parser[a], line: TextLine): LineResult[a] = 
        LinesImport.applyRes(StringParser.run(p), line)

    // FuncLoc2 (Func Loc root)

    def parseFuncLoc2[r](): StringParser.Parser[Option[#{ FuncLoc2 | r }]] = 
        StringParser.optional(
            StringParser.fmap(
                x -> { FuncLoc2(x.floc, x.description). }, 
                HierarchyParser.flocRootLine()
            )
        )


    pub def importFuncLoc2[r](path: String, cs: Charset): Result[#{ FuncLoc2 | r }, String] =
        match LinesImport.newLinesSource(path, cs) {
            case Err(msg) => Err(msg)
            case Ok(src) => 
                match LinesImport.choose(applyParser(parseFuncLoc2()), src) {
                    case Err(msg) => Err(msg)
                    case Ok(relns) => Ok(solve relns)
                }
        }
    
    // FuncLoc3 (other Func Locs)
    
    def parseFuncLoc3[r](): StringParser.Parser[Option[#{ FuncLoc3 | r }]] = 
        StringParser.optional(
            StringParser.fmap(
                x -> { FuncLoc3(x.floc, x.description, x.parent). }, 
                HierarchyParser.flocLine()
            )
        )
        

    pub def importFuncLoc3[r](path: String, cs: Charset): Result[#{ FuncLoc3 | r }, String] =
        match LinesImport.newLinesSource(path, cs) {
            case Err(msg) => Err(msg)
            case Ok(src) => 
                match LinesImport.choose(applyParser(parseFuncLoc3()), src) {
                    case Err(msg) => Err(msg)
                    case Ok(relns) => Ok(solve relns)
                }
        }

    // Equi (Func Loc root)

    pub def makeEquiFacts[r](equi: EquiRow): #{ Equi3, SubEqui | r } = 
        let equi3 = Equi3(equi.equiId, equi.description, equi.floc). ;
        match equi.superordinate {
            case None => equi3
            case Some(parent) => {
                let subequi = SubEqui(equi.equiId, parent). ;
                equi3 <+> subequi            
            }
        }

    def parseEqui[r](): StringParser.Parser[Option[#{ Equi3, SubEqui  | r }]] = 
        StringParser.optional(
            StringParser.fmap(
                makeEquiFacts, 
                HierarchyParser.equiLine()
            )
        )


    pub def importEquis[r](path: String, cs: Charset): Result[#{ Equi3, SubEqui  | r }, String] =
        match LinesImport.newLinesSource(path, cs) {
            case Err(msg) => Err(msg)
            case Ok(src) => 
                match LinesImport.choose(applyParser(parseEqui()), src) {
                    case Err(msg) => Err(msg)
                    case Ok(relns) => Ok(solve relns)
            }
        }
    

}