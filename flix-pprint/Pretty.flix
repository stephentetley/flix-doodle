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

 namespace Pretty {

    enum Doc {
        case Empty,
        case Text(Str),
        case Line(Bool),
        case Cat(Doc,Doc),
        case Nest(Int32,Doc),
        case Group(Doc),
        case Column(Int32 -> Doc),
        case Nesting(Int32 -> Doc)
    }

    // enum SimpleDoc {
    //     case SEmpty,
    //     case SText(Str, SimpleDoc),
    //     case SLine(Str, SimpleDoc)
    // }

    // def extend(s: Str, i: Int32): Str = 
    //     s + String.repeat(i, " ")


    // def flatten(columnPos: Int32, doc: Doc) : Doc =
    //     flattenHelper(columnPos, 0, doc, (x,y) -> (x,y)) |> snd

    // def flattenHelper (column: Int32, nest: Int32, doc: Doc, cont : (Int32, Doc) -> (Int32, Doc)) : (int * Doc) = 
    //     match doc {
    //         case 
    //     }

    // ************************************************************************
    // Primitive printers 
    
    ///
    /// The empty document
    ///
    pub def empty(): Doc = Empty

    ///
    /// `nest` renders the document `doc` with the current indentation level 
    /// increased by `i`
    ///
    pub def nest(i: Int32, doc: Doc): Doc = Nest (i, doc)

    ///
    /// Generate the document containing the literal string `s`.
    /// The input text should not contain newline characters.
    ///
    pub def text(s: Str): Doc = Text(s)

    ///
    /// Undocumented - used by `align`.
    /// `column` gives access to the current column position.
    ///
    pub def column(f: Int32 -> Doc): Doc = Column(f)

    ///
    /// Undocumented - used by `align`.
    /// `nesting` gives access to the current nesting position.
    /// The nesting position is not the same as the column position.
    pub def nesting (f: Int32 -> Doc): Doc = Nesting(f)

    ///
    /// Use the `group` combinator to specify alternate layouts.
    /// `(group doc)` undoes all linebreaks in doc.
    ///
    pub def group(doc: Doc): Doc = Group(doc)

    ////
    /// `line` advances to the next line and indents to the current nesting level.
    /// If the line break is undone by group `line` is rendered as a space.
    ///
    pub def line(): Doc = Line(false)
    
    ///
    /// `linebreak` advances to the next line and indents to the current nesting 
    /// level.
    /// If the line break is undone by group `linebreak` is rendered as empty.    
    ///
    pub def linebreak(): Doc = Line(true)

    ///
    /// This is 'char' in PPrint (Haskell).
    ///
    pub def character(ch: Char): Doc = 
        match ch {
            case '\n' => line()
            case '\r' => line()
            case _ => Char.toString(ch) |> text
        }
        
    ///
    /// `softline` behaves like `space` if the document it is part of fits the page.
    /// If it is too large it renders as `line`.
    ///
    pub def softline(): Doc = line() |> group

    ///
    /// `softbreak` behaves like `empty` if the document it is part of fits the page.
    /// If it is too large it renders as `line`.
    ///
    pub def softbreak(): Doc = linebreak() |> group

    ///
    /// Concatenate documents `x` and `y`.
    ///
    pub def beside(x: Doc, y: Doc): Doc = Cat(x,y)

    ///
    /// Concatenate two documents horizontally (no separating space).
    ///
    pub def <>(x: Doc, y: Doc): Doc = beside(x,y)

    ///
    /// Concatenate two documents horizontally with a separating space.
    ///
    pub def besideSpace(x: Doc, y: Doc): Doc = x <> character(' ') <> y

    ///
    /// Concatenate two documents horizontally with a separating space.
    /// 
    pub def <+>(x: Doc, y:Doc): Doc = besideSpace(x,y)

    ///
    /// Concatenate two documents with a soft line.
    /// This is (</>) in PPrint (Haskell).
    ///
    pub def <!>(x: Doc, y: Doc): Doc = x <> softline() <> y
    
    ///
    /// Concatenate two documents with a soft break.
    /// This is (<//>) in PPrint (Haskell).
    ///
    pub def <!!>(x: Doc, y: Doc): Doc = x <> softbreak() <> y

    ///
    /// Concatenate two documents separating with `line`.
    /// This is (<$>) in PPrint (Haskell).
    ///
    pub def <&>(x: Doc, y: Doc): Doc = x <> line() <> y

    ///
    /// Concatenate two documents separating with `linebreak`.
    /// This is (<$$>) in PPrint (Haskell).
    ///
    pub def <&&>(x: Doc, y: Doc): Doc = x <> linebreak() <> y



    // ************************************************************************
    // List concatenation 

    pub def foldDocs (op: (Doc, Doc) -> Doc, docs: List[Doc]): Doc = 
        match docs {
            case Nil => empty()
            case x :: rs => foldDocsHelper(op, x, rs, identity)
        }

    def foldDocsHelper (op: (Doc, Doc) -> Doc, acc: Doc, docs:List[Doc], cont: Doc -> Doc): Doc = 
        match docs {
            case Nil => cont(acc)
            case x :: rs => foldDocsHelper(op, op(acc,x), rs, cont)
        }

    pub def punctuate(separator: Doc, docs:List[Doc]): Doc =
        foldDocs((l,r) -> l <> separator <> r, docs)
    
    ///
    /// Separate documents horizontally with a space.
    ///
    /// Note - seems to be a problem using (<+>)...
    ///
    pub def hsep(docs: List[Doc]): Doc = foldDocs((x,y) -> besideSpace(x,y), docs)

    ///
    /// Concatenate documents horizontally (no space).
    ///
    pub def hcat(docs: List[Doc]): Doc= foldDocs((x,y) -> x <> y, docs)

    // ************************************************************************
    // Character printers

    ///
    /// Single left parenthesis: '('
    ///
    pub def lparen() : Doc = character('(')

    ///
    /// Single right parenthesis: ')'
    ///
    pub def rparen() : Doc = character(')')

    ///
    /// Single left angle: '<'
    ///
    pub def langle() : Doc = character('<')

    ///
    /// Single right angle: '>'
    ///
    pub def rangle() : Doc = character('>')

    ///
    /// Single left brace: '{'
    ///
    pub def lbrace(): Doc = character('{')
    
    ///
    /// Single right brace: '}'
    ///
    pub def rbrace(): Doc= character('}')
    
    ///
    /// Single left square bracket: '['
    ///
    pub def lbracket(): Doc = character('[')
    
    ///
    /// Single right square bracket: ']'
    ///
    pub def rbracket(): Doc = character(']')

    ///
    /// Single quote
    ///
    pub def squote(): Doc= character('\'')

    ///
    /// The document @dquote@ contains a double quote
    ///
    pub def dquote(): Doc = character('"')

    ///
    /// The document @semi@ contains a semi colon
    ///
    pub def semi(): Doc = character(';')

    ///
    /// The document @colon@ contains a colon
    ///
    pub def colon(): Doc = character(':')

    ///
    /// The document @comma@ contains a comma
    ///
    pub def comma() : Doc = character(',')

    ///
    /// The document @space@ contains a single space
    ///
    pub def space() : Doc = character(' ')

    ///
    /// The document @dot@ contains a single dot
    ///
    pub def dot(): Doc = character('.')

    ///
    /// The document @backslash@ contains a back slash
    ///
    pub def backslash(): Doc = character('\\')

    ///
    /// The document @equals@ contains an equal sign
    ///
    pub def equals(): Doc = character('=')

    ///
    /// Generate a document of n spaces
    ///
    pub def spaces(n:int): Doc = text <| String.repeat(n, " ")

    ///
    /// Enclose the document body between `l` (left) and `r` (right).
    ///
    pub def enclose(l: Doc, r: Doc, body: Doc): Doc = l <> body <> r

    ///
    /// Enclose in single quotes '...'
    ///
    pub def squotes(x: Doc): Doc = enclose(squote(), squote(), x)
    
    ///
    /// Enclose in double quotes "..."
    ///
    pub def dquotes(x: Doc): Doc = enclose(dquote(), dquote(), x)
    
    ///
    /// Enclose in angle braces {...}
    ///
    pub def braces(x: Doc): Doc = enclose(lbrace(), rbrace(), x)
    
    ///
    /// Enclose in square brackets (...)
    ///
    pub def parens(x: Doc): Doc = enclose(lparen(), rparen(), x)
    
    ///
    /// Enclose in angle brackets <...>
    ///
    pub def angles(x: Doc): Doc = enclose(langle(), rangle(), x)
    
    ///
    /// Enclose in square brackets [...]
    ///
    pub def brackets (x: Doc): Doc = enclose(lbracket(), rbracket(), x)
    
    
 }
